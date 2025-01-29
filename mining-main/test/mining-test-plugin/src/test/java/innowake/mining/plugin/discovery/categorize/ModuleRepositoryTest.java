/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.discovery.categorize;

import java.io.IOException;
import java.nio.file.Path;

import org.eclipse.core.runtime.CoreException;
import org.junit.Test;

/**
 * Tests results of Discover Code. 
 * Validated against resulting target workspace of innowake.components.discovery.metrics.ModuleRepositoryTest.
 */

public class ModuleRepositoryTest extends AbstractDiscoverCodeTest {
	
	@Override
	protected String getTestFolder() {
		return "undiscovered-pricing";
	}
	
	@Test
	public void test() throws IOException, CoreException, InterruptedException {
		final Path projectPath = getTargetTestResourcesProjectPath();
		refreshLocal();

		assertFolder(projectPath.resolve("src/asm/RelationshipPricing/ASSEM.SOURCE/"), 87);
		assertFolder(projectPath.resolve("src/cobol/RelationshipPricing/COBOL.COPYLIB/copies/"), 299);
		assertFolder(projectPath.resolve("src/cobol/RelationshipPricing/COBOL.SOURCE/programs/"), 100);
		assertFolder(projectPath.resolve("src/jcl/RelationshipPricing/SAMPLE.DATA/controlcards/"), 88);
		assertFolder(projectPath.resolve("src/jcl/RelationshipPricing/SAMPLE.JCL/jobs/"), 97);
		assertFolder(projectPath.resolve("src/maybe/sql/RelationshipPricing/SAMPLE.DATA/"), 4);
		assertFolder(projectPath.resolve("src/maybe/sql/RelationshipPricing/SSFILE.SOURCE/"), 4);
		assertFolder(projectPath.resolve("undiscovered-entities/RelationshipPricing/ASSEM.COPYLIB/"), 13);
		assertFolder(projectPath.resolve("undiscovered-entities/RelationshipPricing/ASSEM.MACLIB/"), 1);
		assertFolder(projectPath.resolve("undiscovered-entities/RelationshipPricing/COBOL.COPYLIB/"), 53);
		assertFolder(projectPath.resolve("undiscovered-entities/RelationshipPricing/SAMPLE.DATA/"), 85);
		assertFolder(projectPath.resolve("undiscovered-entities/RelationshipPricing/SAMPLE.JCL/"), 1);
		assertFolder(projectPath.resolve("undiscovered-entities/RelationshipPricing/SSFILE.SOURCE/"), 485);
	}
}
