/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.time.StopWatch;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ICoreRunnable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.discovery.sync.ApplyProjectConfiguration;
import innowake.mining.plugin.module.ui.handler.PluginWorkingStorageHelper.SourceObjectTimestampHelper;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.io.MiningFileIndex.File;
import innowake.mining.shared.io.SecuredZipInputStream;
import innowake.ndt.natclipse.ui.wizard.NewErrorMessageWizard;
import innowake.product.base.core.api.ApiException;
import innowake.product.base.core.api.util.ResourceUtil2;


/**
 * Downloads {@link SourcePojo SourcePojos} from Mining server.
 */
public class SourceObjectDownloader implements ICoreRunnable {
	
	private static final Logger LOG = LoggerFactory.getLogger(SourceObjectDownloader.class);
	
	private final IProject project;
	private final MiningFileIndex existingFileIndex;
	private final SourceObjectTimestampHelper sourceObjectTimestampHelper;
	private final boolean forceFullDownload;
	
	@Nullable
	private IProgressMonitor monitor;
	
	/**
	 * Creates a {@link NewErrorMessageWizard} instance.
	 * 
	 * @param project the {@link IProject} to download the {@link SourcePojo SourcePojos} from
	 * @param connectionInfo the {@link ConnectionInfo}
	 * @param forceFullDownload the flag to enforce downloading the entire project contents instead of the deltas
	 * @throws IOException if an error occurs
	 * @throws ValidationException if there is an error while retrieving the {@link ProjectData}
	 */
	public SourceObjectDownloader(final IProject project, final ConnectionInfo connectionInfo, final boolean forceFullDownload)
			throws IOException, ValidationException {
		this.project = project;
		this.existingFileIndex = PluginWorkingStorageHelper.loadMiningFileIndex(project);
		final ProjectData projectData = MiningPreferences.getApiProject(project)
				.orElseThrow(() -> new ValidationException("Project data not found for %s ", project.getName()));
		this.sourceObjectTimestampHelper = new SourceObjectTimestampHelper(project, projectData.getProjectId(), connectionInfo);
		this.forceFullDownload = forceFullDownload;
	}

	@Override
	public void run(@Nullable final IProgressMonitor monitor) throws CoreException {
		this.monitor = monitor;
		if (monitor != null) {
			monitor.beginTask(DownloadSourceObjectsHandler.JOB_NAME, IProgressMonitor.UNKNOWN);
		}
		final StopWatch watch = new StopWatch();
		watch.start();
		Logging.info("Downloading source code files");
		try {
			if (forceFullDownload) {
				downloadSourceObjects(project, OptionalLong.empty());
			} else {
				final OptionalLong baseRevision = OptionalLong.of(existingFileIndex.getSourceCodeRevision().longValue());
				if ( ! downloadSourceObjects(project, baseRevision) && baseRevision.isPresent()) {
					/* sync has failed -- perform a full sync without setting baseRevision
					 * thus the server must send all files */
					Logging.warn("Incremental synchronization has failed. Performing full download of project.");
					downloadSourceObjects(project, OptionalLong.empty());
				}
			}
			
			watch.stop();
			Logging.info(String.format("Overall downloading of source code files took %s (H:mm:ss.SSS)", watch.toString()));
		} catch (final OperationCanceledException e) {
			Logging.info("Source download was canceled");
			LOG.trace(e::getLocalizedMessage, e);
		} catch (final Exception e) {
			Logging.error("Error occured while downloading source objects", e);
			throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, e.getLocalizedMessage(), e));
		}
	}

	private boolean downloadSourceObjects(final IProject project, final OptionalLong baseRevision) throws Exception {
		setDescription("Downloading source objects from the server");
		final Result<byte[]> byteArr = ApiClient.ioService(project)
				.exportSourceObjects()
				.progressUpdate((pos, len) -> setDescription(String.format("Downloaded %s/%s (%d%%)",
						FileUtils.byteCountToDisplaySize(pos),
						FileUtils.byteCountToDisplaySize(len),
						(int) ((double) pos / len * 100))))
				.setBaseRevision(baseRevision)
				.execute();
		final Optional<byte[]> optionalByteArr = byteArr.getValue();
		if ( ! optionalByteArr.isPresent()) {
			Logging.info("No Source Objects to download");
			return true;
		}
		
		checkCanceled();

		final InputStream inputStream = new ByteArrayInputStream(optionalByteArr.get());
		try (final ZipInputStream zipIn = new SecuredZipInputStream(inputStream, StandardCharsets.UTF_8)) {
			final ZipEntry miningFileIndexEntry = zipIn.getNextEntry();
			final MiningFileIndex miningFileIndex = buildMiningFileIndex(miningFileIndexEntry, zipIn);
			final Map<Long, IFile> projectFiles = getExistingFiles(project, existingFileIndex);
			final List<File> miningIndexFiles = assertNotNull(miningFileIndex.getFiles());
			final Set<String> existingFilesToKeep = new HashSet<>();
			final Set<String> missingFileContent = new HashSet<>();
			for (int i = 0; i < miningIndexFiles.size(); i++) {
				checkCanceled();
				setDescription(String.format("Moving files (%d/%d)", Integer.valueOf(i), Integer.valueOf(miningIndexFiles.size())));
				final File file = miningIndexFiles.get(i);
				final Long id = assertNotNull(file.getId());
				final String path = file.getPath().startsWith("/") ? file.getPath().substring(1) : file.getPath();
				if (projectFiles.containsKey(id)) {
					/* A file exists in the project that has the same ID as a file in the file index. Check whether we need to move
					 * the existing file to a new location. */
					final IFile sourceFile = projectFiles.get(id);
					final IFile targetFile = project.getFile(path);
					if ( ! path.equals(sourceFile.getProjectRelativePath().toString())) {
						if (targetFile.exists()) {
							/* A file already exists in the target location - so we can't execute a move.
							 * Just delete the source files. The contents of the existing target file will be updated below. */
							deleteFiles(Collections.singleton(sourceFile));
						} else {
							moveFile(project, sourceFile.getProjectRelativePath().toString(), file.getPath());
						}
					}
					projectFiles.remove(id);
				} else if ( ! project.getFile(path).exists()) {
					/* A file exists in the file index, but doesn't exist in the project yet. Create the new file */
					createFile(project, path);
					missingFileContent.add(path);
				} else {
					/* A file exists in the file index and also locally in the project. We must ensure to keep the existing file */
					existingFilesToKeep.add(path);
				}
			}
			setDescription("File cleanup");
			checkCanceled();
			/* Delete files that exist in the project but were not present in the file index */
			deleteFiles(projectFiles.values()
					.stream()
					.filter(iFile -> ! existingFilesToKeep.contains(iFile.getProjectRelativePath().toString()))
					.collect(Collectors.toList()));
			ZipEntry entry;
			checkCanceled();
			int bytes = 0;
			final String overallSize = humanReadableByteCount(optionalByteArr.get().length);
			while ((entry = zipIn.getNextEntry()) != null) {
				final String fileName = entry.getName();
				if ( ! fileName.equals(MiningFileIndex.NAME)) {
					checkCanceled();
					/* IFile.setContents() closes the stream :-( Since we want to read more entries from the zip file,
					 * that's no good. Therefore we need to copy the entry and pass ByteArrayInputStream to setContents() */
					final ByteArrayOutputStream out = new ByteArrayOutputStream();
					IOUtils.copy(zipIn, out);
					project.getFile(fileName).setContents(new ByteArrayInputStream(out.toByteArray()), true, false, null);
					missingFileContent.remove(fileName);
				}
				bytes += entry.getCompressedSize();
				setDescription(String.format("Writing file contents (%s of %s)", humanReadableByteCount(bytes), overallSize));
			}
			
			checkCanceled();
			setDescription("Storing file index");
			PluginWorkingStorageHelper.storeMiningFileIndex(project, PluginWorkingStorageHelper.mergeFileIndex(existingFileIndex, miningFileIndex));
			
			checkCanceled();
			if ( ! missingFileContent.isEmpty()) {
				/* expected to receive updated file content for some files, but they were missing
				 * return false to indicate that the sync has failed */
				Logging.warn("Expected to receive " + missingFileContent.size() + " additional files");
				return false;
			}
			
			checkCanceled();
			setDescription("Applying project configuration");
			miningIndexFiles.stream()
				.map(File::getTechnology)
				.distinct()
				.forEach(language -> ApplyProjectConfiguration.apply(language, project));
		}
		sourceObjectTimestampHelper.commit();
		return true;
	}
	
	private static String humanReadableByteCount(long bytes) {
	    if (-1000 < bytes && bytes < 1000) {
	        return bytes + " B";
	    }
	    final CharacterIterator ci = new StringCharacterIterator("kMGTPE");
	    while (bytes <= -999_950 || bytes >= 999_950) {
	        bytes /= 1000;
	        ci.next();
	    }
	    return String.format("%.1f %cB", Double.valueOf(bytes / 1000.0), Character.valueOf(ci.current()));
	}
	
	/**
	 * Extracts and parses a {@code MiningFileIndex} from a {@code ZipInputStream}.
	 *
	 * @param entry the zip entry containing the mining file index
	 * @param zipIn the ZipInputStream from which to read
	 * @return the parsed {@link MiningFileIndex}
	 * @throws IOException when reading from the stream fails
	 */
	private MiningFileIndex buildMiningFileIndex(@Nullable final ZipEntry entry, final ZipInputStream zipIn) throws IOException {
		setDescription("Building file index");
		if (entry == null || ! entry.getName().equals(MiningFileIndex.NAME)) {
			throw new IllegalStateException("First entry is not a " + MiningFileIndex.NAME + "but " + assertNotNull(entry).getName());
		}
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		IOUtils.copy(zipIn, out);

		final Gson gson = new GsonBuilder().create();
		return gson.fromJson(new String(out.toByteArray(), StandardCharsets.UTF_8), MiningFileIndex.class);
	}
	
	/**
	 * Returns all files that exist in the project and are listed in the given mining file index.
	 * <p>
	 * This returns a map of the server-side ID that is given in the mining file index to the local workspace file.
	 *
	 * @param project the project to work on
	 * @param miningFileIndex the miningFileIndex
	 * @return
	 */
	private Map<Long, IFile> getExistingFiles(final IProject project, final MiningFileIndex miningFileIndex) {
		final Map<Long, IFile> existingFiles = new HashMap<>();
		final List<File> files = miningFileIndex.getFiles();
		for (int i = 0; i < files.size(); i++) {
			setDescription(String.format("Retrieving existing files (%d/%d)", Integer.valueOf(i), Integer.valueOf(files.size())));
			final MiningFileIndex.File file = files.get(i);
			final IFile iFile = project.getFile(file.getPath());
			if (iFile.exists()) {
				existingFiles.put(file.getId(), iFile);
			}
		}
		
		return existingFiles;
	}

	private void createFile(final IProject project, final String path) throws CoreException, ApiException {
		final IFile file = project.getFile(path);
		final IContainer folder = file.getParent();
		if (folder instanceof IFolder) { /* false when the file is in the workspace root */
			/* ensure parent directory exists */
			ResourceUtil2.createFolder((IFolder) folder, null);
		}
		file.create(IOUtils.toInputStream(StringUtils.EMPTY, StandardCharsets.UTF_8), true, null);
		sourceObjectTimestampHelper.addFile(file);		/* This doesn't really update the SourceObjectTimestamp Map but gets updated after the commit */
	}

	private void moveFile(final IProject project, final String sourcePath, final String destinationPath) throws ApiException, CoreException {
		final IFile iFile = project.getFile(sourcePath);
		sourceObjectTimestampHelper.moveFile(iFile, sourcePath, destinationPath);
		final IPath iPath = project.getFullPath().append(destinationPath);
		final IFolder folder = ResourcesPlugin.getWorkspace().getRoot().getFolder(iPath.removeLastSegments(1));
		if ( ! folder.exists()) {
			try {
				Logging.info("Folder creation for : " + iPath);
				ResourceUtil2.createFolder(folder, null);
			} catch (final ApiException e) {
				final IResource[] members = folder.getParent().members();
				final Optional<IResource> fileWithMatchName = Arrays.stream(members)
						.filter(IFolder.class::isInstance)
						.filter(member -> member.getName().equalsIgnoreCase(folder.getName()))
						.findFirst();
				if (fileWithMatchName.isPresent()) {
					final String message = String.format(
							"Unable to download source code for this project, because the folder %s contains both %s and %s which creates a case-conflict."
									+ " The recommended way to solve this issue is to search for the folder named %s or %S in the original source code and rename it to avoid the conflict. "
									+ "Then re-run Discover Code on the sources.",
							iFile.getName(), folder.getName(), fileWithMatchName.get().getName(), folder.getName(), fileWithMatchName.get().getName());
					Logging.error(message, e);
					throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, message, e));
				} else {
					throw e;
				}

			} catch (final Exception e) {
				Logging.error("Exception while creating folder : {} ", e);
			}
		}

		if (iFile.exists() && folder.exists()) {
			Logging.info("Source file to be moved to : " + iPath);
			iFile.move(iPath, true, false, null);
		}
		deleteDirectoryIfEmpty(iFile.getParent());
	}
	
	private void deleteFiles(final Collection<IFile> files) {
		final Integer numberOfFiles = Integer.valueOf(files.size());
		final AtomicInteger counter = new AtomicInteger(1);
		files.forEach(file -> {
			checkCanceled();
			setDescription(String.format("Deleting files (%d/%d)", Integer.valueOf(counter.getAndIncrement()), numberOfFiles));
			try {
				sourceObjectTimestampHelper.removeFile(file.getProjectRelativePath().toString());
				file.delete(true, null);
				deleteDirectoryIfEmpty(file.getParent());
			} catch (final CoreException e) {
				Logging.error(String.format("Error occured while deleting source object %s file", file.getName()), e);
				throw new IllegalStateException(e);
			}
		});
	}
	
	/**
	 * Recursively deletes empty directories. It checks if the given directory is empty and if yes, removes it.
	 * Then continues recursively for the parent directory.
	 *
	 * @param dir the directory to check and delete
	 * @throws CoreException when deletion fails
	 */
	private void deleteDirectoryIfEmpty(@Nullable final IContainer dir) throws CoreException {
		/* checking for instanceof IFolder, so we don't accidentally delete an empty IProject or something else */
		if ((dir instanceof IFolder) && dir.exists() && dir.members().length == 0) {
			dir.delete(false, null);
			deleteDirectoryIfEmpty(dir.getParent());
		}
	}

	private void setDescription(final String description) {
		if (monitor != null) {
			monitor.setTaskName(description);
		}
	}

	private void checkCanceled() {
		if (monitor != null && monitor.isCanceled()) {
			throw new OperationCanceledException();
		}
	}
}