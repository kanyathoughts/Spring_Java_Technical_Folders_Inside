/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.shared.model.ModuleType;

import java.io.Serializable;
import java.util.List;

/**
 * Configuration for reachability analysis functional block upper bound and lower bound module types.
 */
public class ReachabilityAnalysisConfig implements Serializable {

	/**
	 * The name of the configuration in the Database.
	 */
	public static final String CONFIG_NAME = "ReachabilityAnalysisConfig";

	private final List<ModuleType> upperBoundModuleTypes;
	private final List<ModuleType> lowerBoundModuleTypes;

	/**
	 * Constructor for ReachabilityAnalysisConfig.
	 *
	 * @param upperBoundModuleTypes Upper bound module types
	 * @param lowerBoundModuleTypes Lower bound module types
	 */
	@JsonCreator
	public ReachabilityAnalysisConfig(@JsonProperty("upperBoundModuleTypes") List<ModuleType> upperBoundModuleTypes,
			@JsonProperty("lowerBoundModuleTypes") List<ModuleType> lowerBoundModuleTypes) {
		this.upperBoundModuleTypes = upperBoundModuleTypes;
		this.lowerBoundModuleTypes = lowerBoundModuleTypes;
	}

	/**
	 * Returns the default configuration for the upper and lower bounds of reachability analysis.
	 *
	 * @return the default configuration for the upper and lower bounds of reachability analysis
	 */
	public static ReachabilityAnalysisConfig defaultConfig() {
		final var upperBounds = List.of(
				ModuleType.ECL_JOB,
				ModuleType.JCL_JOB,
				ModuleType.JAVA_TYPE,
				ModuleType.SQL_STORED_PROCEDURE,
				ModuleType.SERVICE);
		final var lowerBounds = List.of(
				ModuleType.SQL_TABLE,
				ModuleType.RESOURCE_FILE,
				ModuleType.RESOURCE_VSAM_FILE,
				ModuleType.RESOURCE_GDG_FILE,
				ModuleType.CICS_TSQ,
				ModuleType.CICS_TDQ,
				ModuleType.CICS_BMS_MAP,
				ModuleType.IMS_DBD,
				ModuleType.IMS_MFS,
				ModuleType.RESOURCE_TPFDF_DATASET);
		return new ReachabilityAnalysisConfig(upperBounds, lowerBounds);
	}

	public List<ModuleType> getUpperBoundModuleTypes() {
		return upperBoundModuleTypes;
	}

	public List<ModuleType> getLowerBoundModuleTypes() {
		return lowerBoundModuleTypes;
	}
}
