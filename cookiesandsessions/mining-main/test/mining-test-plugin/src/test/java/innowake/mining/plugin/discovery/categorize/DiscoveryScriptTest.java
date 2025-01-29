/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.discovery.categorize;

import java.io.IOException;
import java.nio.file.Path;

import org.eclipse.core.runtime.CoreException;
import org.junit.Test;

/**
 * Test for Discover Code.
 */
public class DiscoveryScriptTest extends AbstractDiscoverCodeTest {
	
	@Override
	protected String getTestFolder() {
		return "undiscovered";
	}
	
	@Test
	public void assertCategorization() throws IOException, CoreException, InterruptedException {
		final Path project = getTargetTestResourcesProjectPath();
		final Path cobol = project.resolve("src/cobol");
		final Path cobolPrograms = cobol.resolve("programs");
		final Path cobolCopies = cobol.resolve("copies");
		final Path cicsMaps = project.resolve("src/cics").resolve("maps");
		refreshLocal();
		
		assertFolder(cobolPrograms, 13);
		assertFolder(cobolPrograms, 
			"DRZWR011.cbl",
			"EXECP.cbl", 
			"KVUDR401.cbl",
			"M4328FN.cbl", 
			"MEE2766A.cbl", 
			"MEE5377F.cbl", 
			"MEE6165D.cbl", 
			"MEE6166B.cbl", 
			"MEE6358A.cbl",
			"MEE6705A.cbl", 
			"UNKNOWN.cbl",
			"Display.cbl",
			"DisplayWithExtension.cbl");
		
		assertFolder(cobolCopies, 2);
		assertFolder(cobolCopies, "CPY2766A.cpy", "MTEMAP7C.cpy");
		
		assertFolder(cicsMaps, 2);
		assertFolder(cicsMaps, "MEE6283M.map", "MTEMAP7.map");
		
		final Path jcl = project.resolve("src/jcl");
		final Path jobs = jcl.resolve("jobs");
		final Path procs = jcl.resolve("procs");
		
		assertFolder(jobs, 2);
		assertFolder(jobs, "IFELSE01.job", "WMEE2880.job");
		
		assertFolder(procs, 2);
		assertFolder(procs, "BCOBBAT.proc", "DTNAT2M4.proc");
		
		final Path asm = project.resolve("src/asm");
		
		assertFolder(asm, 1);
		assertFolder(asm, "UIM-EWG961D.asm");
		
		final Path undiscovered = project.resolve("undiscovered-entities");
		assertFolder(undiscovered, 1);
		assertFolder(undiscovered, "SOMETHING");
		
	}
	
}
