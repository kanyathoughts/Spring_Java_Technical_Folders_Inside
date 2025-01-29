/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.discovery.categorize;

import java.io.IOException;
import java.nio.file.Path;

import org.eclipse.core.runtime.CoreException;
import org.junit.Test;


/**
 * Tests Discover Code for duplicate files.
 * It also tests if a default discovery-config.yml is created
 * when there is none.
 */
public class DuplicateFileTest extends AbstractDiscoverCodeTest {

	@Override
	protected String getTestFolder() {
		return "undiscovered-duplicate";
	}
	
	@Test
	public void test() throws IOException, CoreException, InterruptedException {
		final Path project = getTargetTestResourcesProjectPath();
		final Path cobolProgramA = project.resolve("src/cobol/A/programs/");
		
		refreshLocal();
		
		assertFolder(cobolProgramA, 1);
		assertFolder(cobolProgramA, "MEE5377F.cbl");
		
		final Path cobolProgramB = project.resolve("src/cobol/B/programs/");
		
		assertFolder(cobolProgramB, 1);
		assertFolder(cobolProgramB, "MEE5377F.cbl");
	}
}
