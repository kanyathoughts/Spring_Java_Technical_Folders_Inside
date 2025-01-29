/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base;

import static innowake.mining.plugin.base.ResourceUtil.getFilesRecursively;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.swt.widgets.Shell;
import org.junit.Before;
import org.mockito.Mockito;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.IntegrationBaseTest;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.module.importer.SourceObjectImporter;
import innowake.mining.plugin.module.ui.handler.SourceObjectDownloader;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;
import innowake.mining.plugin.preferences.PropertyUtils;
import innowake.mining.plugin.preferences.SecureScopedPreferenceStore;

/**
 * Base class for tests handling resources with Mining server.
 */
public abstract class AbstractRemoteResourcesTest extends IntegrationBaseTest {

	protected static final String MINING_TEST_PLUGIN = "mining-test-plugin";
	protected static final String RESOURCES_FOLDER = "src/test/resources";
	protected static final NullProgressMonitor MONITOR = new NullProgressMonitor();
	protected static final Long CLIENT_ID = Long.valueOf(1);
	protected static final Long PROJECT_ID = Long.valueOf(1);
	protected static final ProjectData PROJECT_DATA = new ProjectData(PROJECT_ID, "Test", CLIENT_ID, "Test");
	protected static final Charset WORKSPACE_CHARSET = Charset.forName("cp1252");
	
	protected static final ConnectionInfo CONNECTION_INFO = getConnectionInfo();
	
	@Nullable
	private IProject targetProject;

	/**
	 * Test setup:
	 * <ul>
	 * <li>Creates a target project which contains all test resources determined by {@link #getTestFolder()}.
	 * <li>Creates a filter for log events that throws an {@link IllegalStateException} if {@link MiningPlugin} logs an error.
	 * <li>Invokes {@link #preprocess()}
	 * </ul>
	 */
	@Before
	public void setup() {
		targetProject = getTargetWorkspace().getProject(MINING_TEST_PLUGIN);
		
		try {
			preprocess();
		} catch (final CoreException | IOException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Provides the name of the folder with the test resources.
	 *
	 * @return the name of the folder with the test resources
	 */
	protected abstract String getTestFolder();
	
	/**
	 * Copies resources determined by {@link #getTestFolder()} to the target project and deletes all modules in Mining server with project ID {@code 1}.
	 * 
	 * @throws IOException if an error occurs 
	 * @throws CoreException if an error occurs 
	 */
	protected void preprocess() throws CoreException, IOException {
		copyResources();
		deleteAllModules();
	}
	
	
	/**
	 * Provides a collection of directory or file names that will <b>not</b> be copied from
	 * your current workspace. This is helpful to save on test startup time. Override 
	 * this method if you want to exclude more or less directories or files. Per default 
	 * this method returns <code>{".svn"}</code>
	 *
	 * @return a collection of directory or file names
	 */
	protected List<String> getCopyFilter() {
		return Arrays.asList(".svn");
	}
	
	/**
	 * Provides the target projects in which tests run. The project is already configured to use with Mining server connection from
	 * {@link #getConnectionInfo()}, Client ID {@code 1} and {@link Project} ID {@code 1}. 
	 *
	 * @return the project
	 */
	protected IProject getTargetProject() {
		return Assert.assertNotNull(targetProject);
	}
	
	/**
	 * Refreshes the local workspace. 
	 * 
	 * @throws CoreException if an error occurs.
	 * @throws InterruptedException if an error occurs.
	 */
	protected void refreshLocal() throws CoreException, InterruptedException {
		final IProject currentProject = getTargetWorkspace().getProject(MINING_TEST_PLUGIN);
		currentProject.refreshLocal(IResource.DEPTH_INFINITE, MONITOR);
		final IJobManager jobManager = Job.getJobManager();
        jobManager.join(ResourcesPlugin.FAMILY_MANUAL_BUILD, MONITOR);
        jobManager.join(ResourcesPlugin.FAMILY_AUTO_BUILD, MONITOR);
		Thread.sleep(30000);
	}
	
	/**
	 * @return the absolute path of the test project in the source workspace
	 */
	protected static Path getSourceTestProjectPath() {
		/* This relies on the execution directory */
		return Paths.get("").toAbsolutePath();
	}
	
	/**
	 * @return the absolute path of the source workspace
	 */
	protected static Path getSourceWorkspacePath() {
		return Assert.assertNotNull(getSourceTestProjectPath().getParent(), "A project path should always have a parent: the workspace path.");
	}

	/**
	 * Returns the absolute path of the project in the source workspace.
	 * 
	 * @param name the name of the project
	 * @return the absolute path of the project in the source workspace
	 */
	protected static Path getSourceProjectPath(final String name) {
		return getSourceWorkspacePath().resolve(name);
	}	
	
	/**
	 * @return the target workspace
	 */
	protected static IWorkspaceRoot getTargetWorkspace() {
		return ResourcesPlugin.getWorkspace().getRoot();
	}
	
	/**
	 * @return the absolute path of the target workspace
	 */
	protected static Path getTargetWorkspacePath() {
		return Paths.get(getTargetWorkspace().getLocationURI());
	}
	
	/**
	 * Returns the absolute path of the project in the target workspace.
	 * 
	 * @param name the name of the project
	 * @return the absolute path of the project in the target workspace
	 */
	protected static Path getTargetProjectPath(final String name) {
		return getTargetWorkspacePath().resolve(name);
	}

	/**
	 * @return the absolute path of the test project in the target workspace
	 */
	protected static final Path getTargetTestResourcesProjectPath() {
		return getTargetProjectPath(MINING_TEST_PLUGIN);
	}
	
	/**
	 * Asserts that the folder with the given path axists and contains the given number of files.
	 *
	 * @param folder path of the folder
	 * @param files number of files in the folder
	 * @throws IOException if an error occurs
	 */
	protected void assertFolder(final Path folder, final int files) throws IOException {
		assertTrue(String.format("Folder %s missing", folder), Files.exists(folder));
		assertEquals(String.format("Number of Files in %s", folder), files, childCount(folder));
	}

	/**
	 * Asserts that the folder with the given path axists and contains the given files.
	 *
	 * @param folder path of the folder
	 * @param files files in the folder
	 * @throws IOException if an error occurs
	 */
	protected void assertFolder(final Path folder, final String... files) throws IOException {
		assertTrue(String.format("Folder %s missing", folder), Files.exists(folder));
		for (final String file : files) {
			assertTrue(String.format("File %s missing in %s", file, folder), Files.exists(folder.resolve(file)));
		}
		assertEquals(String.format("Number of Files in %s", folder), files.length, childCount(folder));
	}
	
	protected void uploadSourceObjects() {
		final String rootPath = getTargetProjectPath(MINING_TEST_PLUGIN).resolve(getTestFolder()).toString();
		try {
			final List<IFile> files = getFilesRecursively(getTargetProject().getFolder(getTestFolder()));
			new SourceObjectImporter(CONNECTION_INFO, getTargetProject(), PROJECT_ID, Mockito.mock(Shell.class), rootPath, files, true).run(MONITOR);
		} catch (final CoreException e) {
			throw new IllegalStateException(e);
		}
	}
	
	protected void downloadSourceObjects() {
		final IProject project = getTargetProject();
		try {
			/* Instantiates SourceObjectDownloader with forceFullDownload flag as false
			 * to download just the source code deltas instead of entire project contents.*/
			new SourceObjectDownloader(project, CONNECTION_INFO, false).run(MONITOR);
		} catch (final CoreException | IOException | ValidationException e) {
			throw new IllegalStateException(e);
		}
	}
	
	private long childCount(final Path folder) throws IOException {
		try (final Stream<Path> list = Files.list(folder)) {
			return list.count();
		}
	}
	
	protected void copyResources() throws CoreException, IOException {
		final IProject targetProjectNotNull = getTargetProject();
		/* A project may exists if you don't clean the workspace in the run configuration, or several tests are executed consecutively. 
		 * It's important to make the copy while the project is non existent, so it picks up the changes later. 
		 * Otherwise we end up with resources out of sync or unpredictable classpath issues. */
		if (targetProjectNotNull.exists()) {
			targetProjectNotNull.close(MONITOR);
			targetProjectNotNull.delete(true, MONITOR);
		}

		final File targetProjectAsFile = getTargetProjectPath(MINING_TEST_PLUGIN).toFile();
		/* Try several times to delete. Sometime this is successful. */
		for (int i = 0; i < 3 && targetProjectAsFile.exists(); i++) {
			FileUtils.deleteQuietly(targetProjectAsFile);
		}
		if (targetProjectAsFile.exists()) {
			Logging.error(String.format("Deletion of folder %s failed in test %s", targetProjectAsFile.getPath(), getClass().getName()));
		}

		final File sourceFolderAsFile = getSourceProjectPath(MINING_TEST_PLUGIN).resolve(RESOURCES_FOLDER).resolve(getTestFolder()).toFile();
		final File targetFolderAsFile = getTargetProjectPath(MINING_TEST_PLUGIN).resolve(getTestFolder()).toFile();
		FileUtils.copyDirectory(sourceFolderAsFile, targetFolderAsFile, getFileFilter());

		if ( ! targetProjectNotNull.exists() ) {
			targetProjectNotNull.create(MONITOR);
		}
		targetProjectNotNull.open(MONITOR);

		try {
			final SecureScopedPreferenceStore store = MiningPreferences.getProjectStore(getTargetProject());
			store.setValue(MiningPreferences.KEY_PROJECT_SETTINGS, true);
			store.setValue(MiningPreferences.KEY_API_SERVER, CONNECTION_INFO.getUrl());
			store.setSecureValue(MiningPreferences.KEY_ACCESS_TOKEN, Assert.assertNotNull(CONNECTION_INFO.getToken()));
			store.setValue(MiningPreferences.KEY_PROJECT, PropertyUtils.toString(PROJECT_DATA));
			store.save();
		} catch (final StorageException | IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
	private void deleteAllModules() {
		try {
			final Result<Void> result = MiningApiClient.moduleService(CONNECTION_INFO)
					.deleteAllModules()
					.setProjectId(PROJECT_ID)
					.execute();
			
			if ( ! result.isValid()) {
				throw new IllegalStateException(String.format("Could not delete modules for project:%n%s", result.getExtendedStatusMessage()));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
	/*
	 * Provides a {@link FileFilter} excluding the files listed in {@link #getCopyFilter()}.
	 * If {@link #getCopyFilter()} returns an empty collection this method returns 
	 * {@link FileFilterUtils#trueFileFilter()}.
	 *
	 * @return a {@link FileFilter} excluding the files listed in {@link #getCopyFilter()}.
	 */
	private FileFilter getFileFilter() {
		return getCopyFilter().stream()
				.map(FileFilterUtils::nameFileFilter)
				.map(FileFilterUtils::notFileFilter)
				.collect(
						Collectors.reducing(FileFilterUtils.trueFileFilter(), FileFilterUtils::and)
				);
	}
	
}
