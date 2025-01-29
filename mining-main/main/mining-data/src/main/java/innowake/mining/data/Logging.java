/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data;

import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.model.discovery.ModelArtifact;

/**
 * Container class for logging constants.
 */
public class Logging {

	/**
	 * {@value #DATA} logs messages for all data relevant packages. 
	 */
	public static final String DATA = "innowake.mining.data";
	
	/**
	 * {@value #IO} logs messages for import and export. 
	 */
	public static final String IO = "innowake.mining.data.io";
	
	/**
	 * {@value #MIGRATION} logs messages during data schema migrations.
	 */
	public static final String MIGRATION = "innowake.mining.data.migration";

	/**
	 * {@value #DISCOVERY_MODULE_REPO} logs modifications of the {@link IModuleRepository} and {@link ModelArtifact}.
	 */
	public static final String DISCOVERY_MODULE_REPO = "innowake.mining.data.discovery.ModuleRepository";

	private Logging() {}
}
