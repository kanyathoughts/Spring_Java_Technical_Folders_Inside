/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

import com.google.gson.Gson;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * Helper class that allows to store, load and update the MiningFileIndex and SourceObjectTimestamp for a project.
 * <p>
 * The index is stored in the mining-plugin's private working storage provided by Eclipse.
 */
public class PluginWorkingStorageHelper {
	
	private static final String SOURCE_OBJECT_TIMESTAMP = ".source-object-timestamp";
	
	/**
	 * Loads and returns the stored MiningFileIndex for the given Project.
	 * If no data is currently stored, an empty MiningFileIndex object is returned.
	 *
	 * @param project the Eclipse project for which to load the index file
	 * @return the loaded index file or a new index
	 * @throws IOException when loading the file from disk fails
	 */
	public static MiningFileIndex loadMiningFileIndex(final IProject project) throws IOException {
		return loadFile(project, new MiningFileIndex(), MiningFileIndex.NAME);
	}
	
	private static Map<String, Map<String, String>> loadSourceObjectTimestampData(final IProject project) throws IOException {
		return loadFile(project, new HashMap<String, Map<String, String>>(), SOURCE_OBJECT_TIMESTAMP);
	}
	
	@SuppressWarnings("unchecked")
	private static <T> T loadFile(final IProject project, final T object, final String fileName) throws IOException {
		final Gson gson = new Gson();
		final IPath workingLocation = project.getWorkingLocation(MiningPlugin.ID);
		final File indexFile = workingLocation.append(fileName).toFile();
		
		if (indexFile.exists()) {			
			try (final BufferedReader reader = Files.newBufferedReader(indexFile.toPath(), StandardCharsets.UTF_8);) {
				final T loadedContent = (T) gson.fromJson(reader, object.getClass());
				return loadedContent == null ? object : loadedContent;
			}
		} else {
			return object;
		}
	}
	
	/**
	 * Stores the MiningFileIndex for the given Project.
	 *
	 * @param project the project for which to store the index
	 * @param miningFileIndex the index to store
	 * @throws IOException when writing the index to disk fails
	 */
	public static void storeMiningFileIndex(final IProject project, final MiningFileIndex miningFileIndex) throws IOException {
		storeFile(project, miningFileIndex, MiningFileIndex.NAME);
	}
	
	private static void storeSourceObjectTimestampData(final IProject project, final Map<String, Map<String, String>> sourceObjectTimestampData) throws IOException {
		storeFile(project, sourceObjectTimestampData, SOURCE_OBJECT_TIMESTAMP);
	}
	
	private static void storeFile(final IProject project, final Object object, final String fileName) throws IOException {
		final Gson gson = new Gson();
		final IPath workingLocation = project.getWorkingLocation(MiningPlugin.ID);
		final File indexFile = workingLocation.append(fileName).toFile();
		
		try (final BufferedWriter writer = Files.newBufferedWriter(indexFile.toPath(), StandardCharsets.UTF_8, 
				StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
			gson.toJson(object, writer);
			writer.flush();
		}
	}
	
	private static Map<String, String> mergeSourceObjectTimestampData(final IProject project, final Map<String, String> dataToAdd, 
			final Set<String> dataToRemove, final Long miningProjectId) throws IOException {
		final Map<String, Map<String, String>> existingData = loadSourceObjectTimestampData(project);
		final Map<String, String> projectData;
		if (existingData.containsKey(miningProjectId.toString())) {
			projectData = existingData.get(miningProjectId.toString());
		} else {
			projectData = new HashMap<>();
			existingData.put(miningProjectId.toString(), projectData);
		}
		dataToAdd.entrySet().forEach(entry -> projectData.put(entry.getKey(), entry.getValue()));
		dataToRemove.forEach(projectData::remove);
		storeSourceObjectTimestampData(project, existingData);
		return projectData;
	}
	
	private static Map<String, String> clearSourceObjectTimestampData(final IProject project, final Long miningProjectId) throws IOException {
		final Map<String, Map<String, String>> existingData = loadSourceObjectTimestampData(project);
		existingData.put(miningProjectId.toString(), new HashMap<>());
		storeSourceObjectTimestampData(project, existingData);
		return existingData.get(miningProjectId.toString());
	}
	
	/**
	 * Merges an existing mining file index with an updated mining file index by applying changes to existing files from the updated index,
	 * and leaving all files not touched by the update as-is.
	 * <p>
	 * This method will
	 * <ul>
	 * <li> Remove all existing files underneath the scope of the update file index and then import the updated files from the index
	 * <li> If the updated file has no scope, it will remove all existing files that have the same name as an updated file and then import all updated files
	 * </ul>
	 * Note: {@code existingFileIndex} is modified by this method!
	 *
	 * @param existingFileIndex the existing file index that will be updated
	 * @param updatedFileIndex the update to merge into the existing file index
	 * @return the modified existing file index
	 */
	public static MiningFileIndex mergeFileIndex(final MiningFileIndex existingFileIndex, final MiningFileIndex updatedFileIndex) {
		existingFileIndex.setSourceCodeRevision(assertNotNull(updatedFileIndex.getSourceCodeRevision()));
		
		if ("/".equals(updatedFileIndex.getScope())) {
			/* the updated file index includes all files in the project, so remove all files from the existing file index */
			existingFileIndex.getFiles().clear();
		} else {
			final Iterator<MiningFileIndex.File> existingFiles = existingFileIndex.getFiles().iterator();
			if (existingFiles.hasNext()) {
				final Set<String> updatedFilePaths = updatedFileIndex.getFiles().stream().map(MiningFileIndex.File::getPath).collect(Collectors.toSet());
				while (existingFiles.hasNext()) {
					final MiningFileIndex.File existingFile = existingFiles.next();
					/* remove all files with the same path that already exist in the existing file index and are also in the updated file index */
					if (updatedFilePaths.contains(existingFile.getPath())) {
						existingFiles.remove();
					} else {
						/* remove all files from the existing file index that are underneath the scope of the updated file index, as we will re-add these below. */
						final String scope = scopeToPath(updatedFileIndex.getScope());
						if (StringUtils.isNotEmpty(scope) && existingFile.getPath().startsWith(scope)) {
							existingFiles.remove();
						}
					}
				}
			}			
		}
		existingFileIndex.getFiles().addAll(updatedFileIndex.getFiles());
		
		return existingFileIndex;
	}
	
	@Nullable
	private static String scopeToPath(@Nullable final String scope) {
		String adjustedScope = scope;
		if (StringUtils.isNotBlank(adjustedScope) && /* for Eclipse compiler sake */ adjustedScope != null) {
			/* trim a leading "/" from scope, because our paths in the database do not start with "/"
			 * and add a trailing "/" to ensure we only match full path segments, and not partial folder names */
			adjustedScope = adjustedScope.startsWith("/") ? adjustedScope.substring(1) : adjustedScope;
			adjustedScope = adjustedScope.endsWith("/") ? adjustedScope : adjustedScope + "/";
		}
		return adjustedScope;
	}
	
	/**
	 * Helper class to access SourceObjectTimestamp data.
	 */
	public static class SourceObjectTimestampHelper {
		
		private final Map<String, String> dataToAdd = new ConcurrentHashMap<>();
		private final Set<String> dataToRemove = ConcurrentHashMap.newKeySet();
		
		@Nullable
		private Map<String, String> existingData;
		
		private final IProject project;
		private final Long miningProjectId;
		private final boolean revisionMismatch;
		
		/**
		 * Constructor.
		 * 
		 * @param project the IProject
		 * @param miningProjectId This is the mining project ID where this eclipse project points to
		 * @param connectionInfo the {@link ConnectionInfo}
		 */
		public SourceObjectTimestampHelper(final IProject project, final Long miningProjectId, final ConnectionInfo connectionInfo) {
			this.project = project;
			this.miningProjectId = miningProjectId;

			final Optional<ProjectPojo> miningProject = MiningServiceExecutor
					.create(() -> ApiClient.projectService(connectionInfo).findProjectById().setProjectId(miningProjectId)).execute();
			final Long serverRevision = miningProject.orElseThrow(() -> new IllegalStateException("Error while retrieving Mining project data"))
					.getSourceCodeRevision();
			if (serverRevision == null) {
				revisionMismatch = true;
			} else {
				try {
					revisionMismatch = ! loadMiningFileIndex(project).getSourceCodeRevision().equals(serverRevision);
				} catch (final IOException e) {
					Logging.error("Error while accessing meta-data file: MiningFileIndex", e);
					throw new IllegalStateException(e);
				}
			}
		}
		
		/**
		 * Allows the {@link IFile} timestamp stored in the metadata file. 
		 * This data wont get persisted in the file until the {@link SourceObjectTimestampHelper#commit} is called.
		 *
		 * @param file to be added
		 */
		public void addFile(final IFile file) {
			dataToAdd.put(file.getProjectRelativePath().toString(), String.valueOf(file.getModificationStamp()));
		}
		
		/**
		 * Allows the file data is removed from the metadata file. 
		 * This data wont get persisted in the file until the {@link SourceObjectTimestampHelper#commit} is called.
		 *
		 * @param file SourceObject path to be removed
		 */
		public void removeFile(final String file) {
			dataToRemove.add(file);
		}
		
		/**
		 * Removes the delta of changed/added/deleted modules from the metadata file. 
		 * This data wont get persisted in the file until the {@link SourceObjectTimestampHelper#commit} is called.
		 *
		 * @param files current source files in the src folder
		 * @param existingFileIndex MiningFileIndex of previously executed metrics
		 */
		public void removeFiles(final List<IFile> files, final MiningFileIndex existingFileIndex) {
			final Set<String> sourceFilePaths = files.stream()
					.map(file -> {
						/* getProjectRelativePath() never starts with "/" */
						final String path = file.getProjectRelativePath().toString();
						return path.startsWith("/") ? path.substring(1) : path;
					})
					.collect(Collectors.toSet());

			for (final innowake.mining.shared.io.MiningFileIndex.File file : existingFileIndex.getFiles()) {
				final String path = file.getPath().startsWith("/") ? file.getPath().substring(1) : file.getPath();
				if ( ! sourceFilePaths.contains(path)) {
					this.removeFile(file.getPath());
				}
			}
		}
		
		/**
		 * Clears project data from the source-object-timestamp file.
		 */
		public void clearProjectData() {
			try {
				existingData = clearSourceObjectTimestampData(project, miningProjectId);
			} catch (final IOException e) {
				Logging.error(
						"Failed to store SourceObject timestamp data. Process resumes, however in the next Discovery execution even Unmodified SourceObjects "
						+ "get uploaded because of this failure. Cause : " + e.getMessage());
			}
		}
		
		/**
		 * Allows the file data is updated based on the paths provided into the metadata file. 
		 * This data wont get persisted in the file until the {@link SourceObjectTimestampHelper#commit} is called.
		 *
		 * @param file to be updated
		 * @param source the source path
		 * @param destination the destination path
		 */
		public void moveFile(final IFile file, final String source, final String destination) {
			dataToAdd.put(destination, String.valueOf(file.getModificationStamp()));
			dataToRemove.add(source);
		}
				
		/**
		 * Persists the information into the metadata file.
		 */
		public void commit() {
			if (dataToAdd.isEmpty() && dataToRemove.isEmpty()) {
				return;
			}
			try {
				existingData = mergeSourceObjectTimestampData(project, dataToAdd, dataToRemove, miningProjectId);
			} catch (final IOException e) {
				Logging.error(
						"Failed to store SourceObject timestamp data. Process resumes, however in the next Discovery execution even Unmodified SourceObjects "
						+ "get uploaded because of this failure. Cause : "	+ e.getMessage());
			}
		}
		
		/**
		 * Checks whether metadata is available needed by the check in {@link #isModified(IFile)}. If this method returns {@code true} it is not necessary to 
		 * call {@link #isModified(IFile)} for each file.
		 *
		 * @return {@code true} if the metadata for file modification check is available else {@code false}
		 * @throws IOException the exception while accessing the metadata file
		 */
		public boolean isModified() throws IOException {
			if (revisionMismatch) {
				return true;
			}
			if (existingData == null) {
				existingData = loadSourceObjectTimestampData(project).get(miningProjectId.toString());
				if (existingData == null) {
					return true;
				}
			}
			return false;
		}

		/**
		 * Checks whether the {@link IFile} is modified since the upload. It checks the metadata file to identify it.
		 *
		 * @param file the IFile
		 * @return {@code true} if the file has been modified else {@code false}
		 * @throws IOException the exception while accessing the metadata file
		 */
		public boolean isModified(final IFile file) throws IOException {
			final Map<String, String> existingData = Assert.assertNotNull(this.existingData); /* can't be null here but to make eclipse happy */
			final String path = file.getProjectRelativePath().toString();
			if ( ! existingData.containsKey(path)) {
				return true;
			}
			try {
				final long existingTimestamp = Long.parseLong(existingData.get(path));
				return existingTimestamp != file.getModificationStamp();
			} catch (final NumberFormatException e) {
				Logging.error("Error while retrieving the stored timestamp", e);
				return true;
			}
		}
		
	}
}
