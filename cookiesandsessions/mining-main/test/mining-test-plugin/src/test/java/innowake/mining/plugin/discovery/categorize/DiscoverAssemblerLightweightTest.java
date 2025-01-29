/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.discovery.categorize;

import java.io.IOException;
import java.nio.file.Path;

import org.eclipse.core.runtime.CoreException;
import org.junit.Test;

/**
 * Tests the AssemblerFileTypeDetection implementation using the ndt-core
 * lightweight parser.
 */
public class DiscoverAssemblerLightweightTest extends AbstractDiscoverCodeTest {
	
	@Override
	protected String getTestFolder() {
		return "undiscovered-assembler";
	}
	
	@Test
	public void test() throws IOException, CoreException, InterruptedException {
		final Path asm = getTargetTestResourcesProjectPath().resolve("src/asm");
		final Path asmMaybe = getTargetTestResourcesProjectPath().resolve("src/maybe/asm");
		final Path undiscoveredEntities = getTargetTestResourcesProjectPath().resolve("undiscovered-entities");
		
		refreshLocal();
		
		/*
		 * not be identified as assembler: 
		 * RFA100 because token count < 10 
		 */
		assertFolder(asm, 5);
		assertFolder(asm, "RFE105.mac", "RFE110.mac", "RFE115.mac", "RFE125.mac", "RFE130.mac");
		
		/*
		 * Identified as maybe assembler by main mode of categorizer 
		 */
		assertFolder(asmMaybe, 2);
		assertFolder(asmMaybe, "RFA042.asm", "RFA044.asm");
		
		/*
		 * RFA1001 because token count 10 but unknown count 7 (if more than 50% unknown, no identification as assembler)
		 */
		assertFolder(undiscoveredEntities, 3);
		assertFolder(undiscoveredEntities, "RFA043", "RFA100", "RFA1001");
	}
}
