/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql;

/**
 * Names of GraphQL queries in Mining.
 */
public class MiningGraphQLQueries {
	
	public static final String MODULES = "modules";
	public static final String STATEMENTS = "statements";
	public static final String ANNOTATIONS = "annotations";
	public static final String DATA_DICTIONARY = "dataDictionaries";
	public static final String DNA_MODULES_IN_CLUSTER = "dnaModulesInCluster";
	public static final String MODULE_DEPENDENCIES = "moduleDependencies";
	public static final String JOBS = "jobs";
	public static final String REACHABILITY_DATA = "reachabilityData";
	public static final String FUNCTIONAL_BLOCKS = "functionalBlocks";
	public static final String ERROR_MARKERS = "errorMarkers";

	private MiningGraphQLQueries() { }
	
}
