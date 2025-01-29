/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition.usages;

import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SortByAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;

/**
 * Definition of built-in data point usages.
 */
public final class Usages {
	
	/* new usages should follow the naming convention of existing usages */
	
	/* !!! new usages must be added to innowake.mining.server.controller.DataPointController.UsagesModel !!!
	 * 
	 * otherwise they don't appear in the generated type definitions for the TypeScript client (i.e. mining-ui) */

	/* =========================================================
	 * Usage of the data point in a specific GraphQL Query
	 * =========================================================
	 */

	/**
	 * Defines attributes that are applicable only when the data point is used as part of a specific top-level GraphQL query.
	 */
	public static final String GRAPHQL_QUERY_PREFIX = "graphql.query.";
	
	/**
	 * Defines attributes that are applicable only when the data point is used as part of a specific top-level GraphQL query for Module.
	 */
	public static final String GRAPHQL_QUERY_MODULES = GRAPHQL_QUERY_PREFIX + "modules";
	
	/**
	 * Defines attributes that are applicable only when the data point is used as part of a specific top-level GraphQL query for Annotation.
	 */
	public static final String GRAPHQL_QUERY_ANNOTATIONS = GRAPHQL_QUERY_PREFIX + "annotations";
	
	/**
	 * Defines attributes that are applicable only when the data point is used as part of a specific top-level GraphQL query for DataDictionaryEntry.
	 */
	public static final String GRAPHQL_QUERY_DATA_DICTIONARIES = GRAPHQL_QUERY_PREFIX + "dataDictionaries";

	/**
	 * Defines attributes that are applicable only when the data point is used as part of a specific top-level GraphQL query for DnaModulesInCluster.
	 */
	public static final String GRAPHQL_QUERY_DNA_MODULE_CLUSTER = GRAPHQL_QUERY_PREFIX + "dnaModulesInCluster";

	/**
	 * Defines attributes that are applicable only when the data point is used as part of a specific top-level GraphQL query for DependencyInformation.
	 */
	public static final String GRAPHQL_QUERY_DEPENDENCIES = GRAPHQL_QUERY_PREFIX + "moduleDependencies";

	/**
	 * Defines attributes that are applicable only when the data point is used as part of a specific top-level GraphQL query for ErrorMarker.
	 */
	public static final String GRAPHQL_QUERY_ERROR_MARKERS = GRAPHQL_QUERY_PREFIX + "errorMarkers";

	/* =========================================================
	 * Generic Usages for adding meta-information to data points
	 * =========================================================
	 */
	
	/**
	 * Defines attributes for displaying the data point in a read-only context.
	 * @see ViewModeAttributes
	 */
	public static final String VIEW_MODE = "general.viewMode";
	/**
	 * Determines that this data point is editable. Defines attributes used when editing the data point (e.g. constraints).
	 */
	public static final String EDIT_MODE = "general.editMode";
	/**
	 * Determines that this data point can be used for searching or filtering of results.
	 * Defines attributes how to use this data point for searching or filtering (e.g. what filter type to use and how to build queries).
	 * @see SearchFilterAttributes
	 */
	public static final String SEARCH_FILTER = "general.searchFilter";
	/**
	 * Defines attributes how the data point is represented when exported to CSV.
	 */
	public static final String EXPORT_FORMAT_CSV = "general.exportFormat.csv";
	
	/**
	 * Determines that this data point can be used for sorting of results and defines attributes that are required to do so.
	 * @see SortByAttributes
	 */
	public static final String SORT_BY = "general.sortBy";
	
	/* =========================================================
	 * Pages / Locations on Mining UI
	 * =========================================================
	 */

	/**
	 * The data point should be displayed (in some form) on the module details page.
	 */
	public static final String MINING_UI_MODULE_DETAILS = "miningUi.moduleDetails";
	/**
	 * The data point is selectable as a column on the Modules in DNA Cluster table.
	 */
	public static final String MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE = "miningUi.modulesInDnaCluster";
	/**
	 * The data point is selectable as a column on the Modules table.
	 */
	public static final String MINING_UI_MODULES_TABLE = "miningUi.modulesTable";
	/**
	 * The data point is selectable as a column on the Annotations table.
	 */
	public static final String MINING_UI_ANNOTATIONS_TABLE = "miningUi.annotationsTable";
	/**
	 * The data point is selectable as a column on the Reachability table.
	 */
	public static final String MINING_UI_REACHABILITY_TABLE = "miningUi.reachabilityTable";
	/**
	 * The data point is selectable on the GraphML export page.
	 */
	public static final String MINING_UI_GRAPHML_EXPORT = "miningUi.graphMlExport";
	/**
	 * The data point is selectable as a column on the Data Dictionary table.
	 */
	public static final String MINING_UI_DATADICTIONARY_TABLE = "miningUi.dataDictionaryTable";
	/**
	 * The data point is selectable as a column on the Custom Properties table.
	 */
	public static final String MINING_UI_CUSTOM_PROPERTIES_TABLE = "miningUi.customPropertiesTable";
	/**
	 * The data point is selectable as a column on the "Table Columns" table (on the Module Details page).
	 */
	public static final String MINING_UI_TABLE_COLUMNS_TABLE = "miningUi.tableColumnsTable";
	/**
	 * The data point is selectable as a column on the scheduler import table (on the Configuration page).
	 */
	public static final String MINING_UI_SCHEDULER_IMPORT_TABLE = "miningUi.schedulerImportTable";

	/**
	* Indicates that this data point should be included in the "All Taxonomies" saved search.
	*/
	public static final String ALL_TAXONOMIES_SAVED_SEARCH = "savedSearch.allTaxonomies";
	/**
	 * The datapoint is selectable as a column on the dependencies table.
	 */
	public static final String MINING_UI_DEPENDENCIES_TABLE = "miningUi.dependenciesTable";
	/**
	 * The data point is selectable as a column on the Module Error Marker table.
	 */
	public static final String MINING_UI_MODULE_ERROR_MARKER_TABLE = "miningUi.moduleErrorMarkerTable";
	/**
	* The data point can be selected as a column on the Metrics chart detail table.
	*/
	public static final String METRICS_CHART_DETAILS_MODULE = "miningUi.chartDetailsModule";
	/**
	* The data point can be selected as a column on the Metrics chart detail Sql table.
	*/
	public static final String METRICS_CHART_DETAILS_SQL = "miningUi.chartDetailsSql";

	/* =========================================================
	 * Data Export / Reporting
	 * =========================================================
	 */
	public static final String TAXONOMY_ASSIGNMENTS_EXPORT = "export.taxonomyAssignments";

	private Usages() {}
}
