/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Unit tests for {@link PluginWorkingStorageHelper}.
 */
public class MiningFileIndexHelperTest {

	/* dummy initialization required because these are @NonNull */
	private File tempDirectory = new File("");
	private IProject mockProject = Mockito.mock(IProject.class);
	
	@Before
	public void setupMockProject() throws IOException {
		tempDirectory = Files.createTempDirectory("mining-plugin-test").toFile();
		mockProject = Mockito.mock(IProject.class);
		Mockito.when(mockProject.getWorkingLocation(ArgumentMatchers.anyString()))
				.thenReturn(new Path(tempDirectory.getAbsolutePath()));
	}
	
	@Test
	public void testStoreAndLoad() throws IOException {
		final MiningFileIndex createdFileIndex = new MiningFileIndex();
		createdFileIndex.setSourceCodeRevision(Long.valueOf(42));
		createdFileIndex.setVersion(1);
		createdFileIndex.setScope("test");
		createdFileIndex.setFiles(Arrays.asList(
				new MiningFileIndex.File(Long.valueOf(24), "test/foo.cbl", Technology.COBOL, Type.PROGRAM, 13, 12, false),
				new MiningFileIndex.File(Long.valueOf(25), "test/bar.job", Technology.JCL, Type.JOB, 15, 14, false)
		));
		
		PluginWorkingStorageHelper.storeMiningFileIndex(mockProject, createdFileIndex);
		
		final MiningFileIndex loadedFileIndex = PluginWorkingStorageHelper.loadMiningFileIndex(mockProject);
		
		assertEquals(createdFileIndex.getSourceCodeRevision(), loadedFileIndex.getSourceCodeRevision());
		assertEquals(createdFileIndex.getVersion(), loadedFileIndex.getVersion());
		assertEquals(createdFileIndex.getScope(), loadedFileIndex.getScope());
		assertEquals(createdFileIndex.getFiles(), loadedFileIndex.getFiles());
	}
	
	@Test
	public void testMerge() {
		final MiningFileIndex createdFileIndex = new MiningFileIndex();
		createdFileIndex.setSourceCodeRevision(Long.valueOf(42));
		createdFileIndex.setVersion(1);
		createdFileIndex.setScope("test");
		createdFileIndex.setFiles(new ArrayList<>(Arrays.asList(
				new MiningFileIndex.File(Long.valueOf(24), "test/foo.cbl", Technology.COBOL, Type.PROGRAM, 13, 12, false),
				new MiningFileIndex.File(Long.valueOf(25), "test/bar.job", Technology.JCL, Type.JOB, 15, 14, false),
				new MiningFileIndex.File(Long.valueOf(25), "test-foobar/baz.cpy", Technology.COBOL, Type.COPYBOOK, 15, 14, false)
		)));
		
		final MiningFileIndex updatedFileIndex = new MiningFileIndex();
		updatedFileIndex.setSourceCodeRevision(Long.valueOf(43));
		updatedFileIndex.setVersion(1);
		updatedFileIndex.setScope("test");
		updatedFileIndex.setFiles(Arrays.asList(
				new MiningFileIndex.File(Long.valueOf(26), "test/qux.cpy", Technology.COBOL, Type.COPYBOOK, 15, 14, false)
		));
		
		final MiningFileIndex mergedFileIndex = PluginWorkingStorageHelper.mergeFileIndex(createdFileIndex, updatedFileIndex);
		assertEquals(mergedFileIndex.getSourceCodeRevision(), Long.valueOf(43));
		assertEquals(mergedFileIndex.getVersion(), 1);
		/* it should have deleted all files in the "test" folder due to the scope setting and then added the one new file,
		 * it should have left all files in the "test-foobar" folder as-is */
		assertEquals(mergedFileIndex.getFiles(), Arrays.asList(
				new MiningFileIndex.File(Long.valueOf(25), "test-foobar/baz.cpy", Technology.COBOL, Type.COPYBOOK, 15, 14, false),
				new MiningFileIndex.File(Long.valueOf(26), "test/qux.cpy", Technology.COBOL, Type.COPYBOOK, 15, 14, false)
		));
	}
	
	@After
	public void cleanUp() throws IOException {
		if (tempDirectory.exists()) {
			FileUtils.deleteDirectory(tempDirectory);			
		}
	}
}
