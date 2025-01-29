/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.widgets.Shell;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.IntegrationBaseTest;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests {@link SourceObjectImporter}.
 */
public class SourceObjectImportTest extends IntegrationBaseTest {
	
	private static final String SOURCE_OBJECT_IMPORT_PROJECT = "SourceObjectImportProject";
	private static final Long PROJECT_ID = Long.valueOf(1);
	private static final String ROOT_PATH = "src/cobol/programs";
	private static final String PATH_1 = ROOT_PATH + "/NONEXIST1.cbl";
	private static final String PATH_2 = ROOT_PATH + "/NONEXIST2.cbl";

	/**
	 * Setup.
	 */
	@Before
	public void setup() {
		deleteAllModules();
		assertEquals(0, SourceObjectImportUtility.getSourceObjects().size());
	}

	/**
	 * Imports and counts {@link SourcePojo SourcePojos}.
	 */
	@Test
	public void testCreateAndUpdate() {
		final IFile file1 = mockFile(PATH_1, "blah");
		final IFile file2 = mockFile(PATH_2, "blub");
		
		importSourceCode(Arrays.asList(file1, file2));

		final IFile file3 = mockFile(PATH_2, "blub2");
		importSourceCode(Arrays.asList(file3));
		assertEquals(1, SourceObjectImportUtility.getSourceObjects().size());
	}
	
	/**
	 * Imports and tests {@link Technology} and {@link Type}.
	 */
	@Test
	public void testTechnologyAndType() {
		final String content = "blah";
		final IFile file1 = mockFile(PATH_1, content);
		importSourceCode(Arrays.asList(file1));
		Map<String, SourceObjectImportUtility.FileIndexEntry> sourceIndex = SourceObjectImportUtility.getSourceObjects();
		assertEquals(1, sourceIndex.size());
		assertEquals(Technology.COBOL, sourceIndex.get(PATH_1).getFile().getTechnology());
		assertEquals(Type.PROGRAM, sourceIndex.get(PATH_1).getFile().getType());
		assertEquals(content, sourceIndex.get(PATH_1).getContent());
	}

	/**
	 * Test creating and updating sourceAttachmentLink on {@link Module}.
	 */
	@Test
	public void testCreateSourceObjectLinkOnModule() {
		Map<String, SourceObjectImportUtility.FileIndexEntry> sourceIndex;
		
		importModulesWithoutSourceCode("src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16.xlsx");
		assertEquals(0, SourceObjectImportUtility.getSourceObjects().size());
		
		final String path = ROOT_PATH + "/MMRS7101.cbl";
		final String content = "blah";
		final IFile file1 = mockFile(path, content);
		importSourceCode(Arrays.asList(file1));
		
		sourceIndex = SourceObjectImportUtility.getSourceObjects();
		assertEquals(1, sourceIndex.size());
		assertEquals(content, sourceIndex.get(path).getContent());
		
		final String content2 = "blub";
		final IFile file2 = mockFile(path, content2);
		importSourceCode(Arrays.asList(file2));
		
		sourceIndex = SourceObjectImportUtility.getSourceObjects();
		assertEquals(1, sourceIndex.size());
		assertEquals(content2, sourceIndex.get(path).getContent());
	}
	
	private IFile mockFile(final String path, final String content) {
		final IFile mock = mock(IFile.class);
		doReturn(Path.fromOSString(path)).when(mock).getProjectRelativePath();
		final IPath mockPath = mock(IPath.class);
		doReturn(new File("temp")).when(mockPath).toFile();
		doReturn(mockPath).when(mock).getLocation();
		try {
			doReturn(IOUtils.toInputStream(content, StandardCharsets.UTF_8)).when(mock).getContents();
		} catch (final CoreException e) {
			throw new IllegalStateException(e);
		}
		return mock;
	}
	
	private void importSourceCode(final List<IFile> files) {
		try {
			final IProgressMonitor progressMonitor = new NullProgressMonitor();
			final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(SOURCE_OBJECT_IMPORT_PROJECT);
			if ( ! project.exists()) {
				project.create(progressMonitor);
			}
			if ( ! project.isOpen()) {
				project.open(progressMonitor);
			}
			final Shell shell = Mockito.mock(Shell.class);
			new SourceObjectImporter(getConnectionInfo(), project, PROJECT_ID, shell, ROOT_PATH, files, true).run(progressMonitor);
		} catch (final CoreException e) {
			throw new IllegalStateException(e);
		}
	}
	
	private void deleteAllModules() {
		try {
			final Result<Void> result = MiningApiClient.moduleService(getConnectionInfo())
					 .deleteAllModules()
					 .setProjectId(PROJECT_ID)
					 .execute();

			if ( ! result.isValid()) {
				throw new IllegalStateException(String.format("Could not delete modules for project:\n%s", result.getExtendedStatusMessage()));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	private void importModulesWithoutSourceCode(final String path) {
		final String pathAsString = Paths.get(System.getProperty("user.dir"), path).toString();
		try {
			final Result<Void> result;
			try (final FileInputStream inputStream = FileUtils.openInputStream(new File(pathAsString))) {
				result = MiningApiClient.ioService(getConnectionInfo())
						.importExcel()
						.setProjectId(PROJECT_ID)
						.setInputStreamId(path)
						.setInputStream(inputStream)
						.execute();
			}
			
			if ( ! result.isValid()) {
				throw new IllegalStateException(result.getExtendedStatusMessage());
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
}
