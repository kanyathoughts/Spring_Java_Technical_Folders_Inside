/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.discovery.config;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.Collections;
import org.junit.Test;
import innowake.mining.plugin.discovery.sync.CobolProjectConfigurator;
import innowake.mining.shared.model.discovery.ResolveTarget;

public class ProjectConfiguratorTest {
	
	@Test
	public void testSubType() {
		final CobolProjectConfigurator assembler = new CobolProjectConfigurator();
		
		assertTrue(assembler.accepts(Arrays.asList(ResolveTarget.COBOL_PROGRAM)));
		assertTrue(assembler.accepts(Arrays.asList(ResolveTarget.NATURAL, ResolveTarget.COBOL_PROGRAM)));
		assertTrue(assembler.accepts(Arrays.asList(ResolveTarget.NATURAL_DDM, ResolveTarget.COBOL_PROGRAM)));
		assertFalse(assembler.accepts(Arrays.asList(ResolveTarget.NATURAL_DDM)));
		assertFalse(assembler.accepts(Collections.emptyList()));
	}

}
