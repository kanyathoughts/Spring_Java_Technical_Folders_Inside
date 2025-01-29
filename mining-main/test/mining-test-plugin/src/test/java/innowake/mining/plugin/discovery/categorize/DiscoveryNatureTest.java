/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.plugin.discovery.categorize;


import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.nio.file.Path;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.JavaCore;
import org.junit.Test;

import innowake.base.eclipse.common.core.NatureDescription;

/**
 * Test to execute discover code and check the configuration of the Eclipse test project for all the language natures being configured
 */
public class DiscoveryNatureTest extends AbstractDiscoverCodeTest {

	@Override
	protected String getTestFolder() {
		return "undiscovered-nature";
	}

	/* a unique nature id for the cobol nature */
	private static final String COBOL_NATURE_ID = "innowake.ndt.cobolclipse.cobolnature";
	/* a unique nature id for the batch nature */
	private static final String BATCH_NATURE_ID = "innowake.ndt.batchclipse.nature";
	/* a unique nature id for the natural nature */
	private static final String NATURAL_NATURE_ID = "innowake.natclipse.natnature";
	/* a unique nature id for the java nature */
	private static final String JAVA_NATURE_ID = "org.eclipse.jdt.core.javanature";
	
	@Test
	public void testProjectNature() throws IOException, CoreException {
		final Path project = getTargetTestResourcesProjectPath();
		
		assertFolder(project.resolve("src/cobol/undiscovered-cobol/programs"), 2);
		assertFolder(project.resolve("src/java/undiscovered-java"), 2);
		assertFolder(project.resolve("src/jcl/undiscovered-batch/jobs"), 1);
		assertFolder(project.resolve("src/jcl/undiscovered-batch/procs"), 1);
		assertFolder(project.resolve("src/natural/undiscovered-natural/programs"), 1);
		assertNotNull(getTargetProject().getNature(COBOL_NATURE_ID));
		assertNotNull(getTargetProject().getNature(BATCH_NATURE_ID));
		assertNotNull(getTargetProject().getNature(NATURAL_NATURE_ID));
		NatureDescription.addNature(getTargetProject(), "Java", JavaCore.NATURE_ID, JavaCore.BUILDER_ID, false, null);
		assertNotNull(getTargetProject().getNature(JAVA_NATURE_ID));
	}
}
