/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core;

import innowake.ndt.core.assembling.AssemblerConfiguration;
import innowake.ndt.core.parsing.replacement.ReplacementMode;

/**
 * Assembler configuration for Pl1.
 */
public class PL1AssemblerConfiguration extends AssemblerConfiguration {

	/**
	 * Constructor to initialize {@link PL1AssemblerConfiguration}.
	 * 
	 * @param replacementMode of type {@link ReplacementMode}
	 * @param keepIntermediateContents {@code true} to keep intermediate contents, {@code false} otherwise
	 */
	public PL1AssemblerConfiguration(final ReplacementMode replacementMode, final boolean keepIntermediateContents) {
		super(ReplacementMode.FAST, true);
	}

}
