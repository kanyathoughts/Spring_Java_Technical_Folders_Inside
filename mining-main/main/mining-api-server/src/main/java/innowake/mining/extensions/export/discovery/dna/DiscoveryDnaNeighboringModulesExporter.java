/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.discovery.dna;

import org.springframework.stereotype.Service;

/**
 * Creates a DiscoveryDnaExporter job to export DNA neighboring modules.
 */
@Service
public class DiscoveryDnaNeighboringModulesExporter extends DiscoveryDnaExporter {
	
	@Override
	public String getDescription() {
		return File.NEIGHBORING_MODULES.getDescription();
	}

	@Override
	protected File[] getFiles() {
		return  new File[] {File.NEIGHBORING_MODULES};
	}

	@Override
	protected String getLabel() {
		return File.NEIGHBORING_MODULES.getLabel();
	}

	@Override
	public String getIdentifier() {
		return "discovery-dna-export-modules-in-cluster";
	}
}
