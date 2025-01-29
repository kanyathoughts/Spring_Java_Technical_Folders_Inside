/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.DISCOVERY_LIGHT;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.EditModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SortByAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for inspecting mining data points.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class DataPointController extends BaseController {
	
	/**
	 * This class exists solely to generate swagger type definitions for the data point usages constants.
	 */
	@Schema
	private static class UsagesModel {
		
		/**
		 * Lists all usages defined in {@link Usages}.
		 */
		@Schema(allowableValues = { Usages.GRAPHQL_QUERY_PREFIX,
				Usages.GRAPHQL_QUERY_MODULES,
				Usages.GRAPHQL_QUERY_ANNOTATIONS,
				Usages.GRAPHQL_QUERY_DATA_DICTIONARIES,
				Usages.GRAPHQL_QUERY_DNA_MODULE_CLUSTER,
				Usages.GRAPHQL_QUERY_DEPENDENCIES,
				Usages.GRAPHQL_QUERY_ERROR_MARKERS,
				Usages.VIEW_MODE,
				Usages.EDIT_MODE,
				Usages.SEARCH_FILTER,
				Usages.EXPORT_FORMAT_CSV,
				Usages.SORT_BY,
				Usages.MINING_UI_MODULE_DETAILS,
				Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE,
				Usages.MINING_UI_MODULES_TABLE,
				Usages.MINING_UI_ANNOTATIONS_TABLE,
				Usages.MINING_UI_REACHABILITY_TABLE,
				Usages.MINING_UI_GRAPHML_EXPORT,
				Usages.MINING_UI_DATADICTIONARY_TABLE,
				Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE,
				Usages.MINING_UI_TABLE_COLUMNS_TABLE,
				Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE,
				Usages.ALL_TAXONOMIES_SAVED_SEARCH,
				Usages.MINING_UI_DEPENDENCIES_TABLE,
				Usages.METRICS_CHART_DETAILS_MODULE,
				Usages.METRICS_CHART_DETAILS_SQL,
				Usages.TAXONOMY_ASSIGNMENTS_EXPORT,
				Usages.MINING_UI_SCHEDULER_IMPORT_TABLE
		})
		public String usages;
		
		/**
		 * Lists all usages defined in {@link SearchFilterAttributes}
		 */
		@Schema(allowableValues = { SearchFilterAttributes.FILTER_MODE,
				SearchFilterAttributes.FILTER_MODE_TEXT,
				SearchFilterAttributes.FILTER_MODE_NUMBER,
				SearchFilterAttributes.FILTER_MODE_MULTI_SELECT,
				SearchFilterAttributes.FILTER_MODE_CUSTOM_PROPERTY_TAG,
				SearchFilterAttributes.RSQL_FRAGMENT,
				SearchFilterAttributes.SQL_FRAGMENT_EQ,
				SearchFilterAttributes.SQL_FRAGMENT_FALSE,
				SearchFilterAttributes.SQL_FRAGMENT_TRUE,
				SearchFilterAttributes.SQL_FRAGMENT_GTE,
				SearchFilterAttributes.SQL_FRAGMENT_GT,
				SearchFilterAttributes.SQL_FRAGMENT_NONE,
				SearchFilterAttributes.SQL_FRAGMENT_LTE,
				SearchFilterAttributes.SQL_FRAGMENT_LT,
				SearchFilterAttributes.SQL_FRAGMENT_IN,
				SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
				SearchFilterAttributes.SQL_FRAGMENT_FLAG_TO_LOWERCASE,
				SearchFilterAttributes.SQL_FRAGMENT_FLAG_BEGINS_WITH,
				SearchFilterAttributes.SQL_FRAGMENT_FLAG_ENDS_WITH,
				SearchFilterAttributes.SQL_FRAGMENT_FLAG_ESCAPE_LUCENE,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_AGGREGATED_VALUES,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_TAXONOMY_AGGREGATED_VALUES,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_KEY_FIELD,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FILTER,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES,
				SearchFilterAttributes.MULTI_SELECT_SHOW_NONE_OPTION,
				SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_REFERENCES_AGGREGATED_VALUES,
				SearchFilterAttributes.MULTI_SELECT_FIXED_VALUES,
				SearchFilterAttributes.FILTER_MODE_TREE_SELECT
		})
		public String searchFilterAttributes;
		
		/**
		 * Lists all usages defined in {@link SortByAttributes}
		 */
		@Schema(allowableValues = SortByAttributes.SQL_FRAGMENT_ORDER_BY)
		public String sortByAttributes;
		
		/**
		 * Lists all usages defined in {@link ViewModeAttributes}
		 */
		@Schema(allowableValues = { ViewModeAttributes.DISPLAY_AS,
				ViewModeAttributes.DISPLAY_AS_HTML,
				ViewModeAttributes.DISPLAY_AS_LINK,
				ViewModeAttributes.DISPLAY_AS_DATE,
				ViewModeAttributes.DISPLAY_AS_TAG,
				ViewModeAttributes.DISPLAY_AS_LINK_OPEN_MODAL,
				ViewModeAttributes.LINK_TEMPLATE,
				ViewModeAttributes.TOGETHER_WITH,
				ViewModeAttributes.LABEL_MAPPING
		})
		public String viewModeAttributes;
		
		/**
		 * Lists all usages defined in {@link TableAttributes}
		 */
		@Schema(allowableValues = { TableAttributes.CATEGORY, 
				TableAttributes.DEFAULT_COLUMN_INDEX,
				TableAttributes.HIDDEN_BY_DEFAULT
		})
		public String tableAttributes;
		
		/**
		 * Lists all usages defined in {@link EditModeAttributes}
		 */
		@Schema(allowableValues = {EditModeAttributes.EDIT_AS,
				EditModeAttributes.EDIT_AS_TEXT,
				EditModeAttributes.EDIT_AS_TEXT_AREA,
				EditModeAttributes.EDIT_ENDPOINT,
				EditModeAttributes.EDIT_ENDPOINT_FIELD_NAME,
				EditModeAttributes.TOGETHER_WITH
		})
		public String editModeAttributes;
	}
	
	@Autowired
	private DataPointRegistry dataPointRegistry;
	
	/**
	 * URL for all data points (optionally filtered by usage)
	 */
	public static final String DATA_POINTS_URL = "/v2/projects/{projectId}/datapoints";
	
	/**
	 * URL for known types
	 */
	public static final String DATA_POINTS_TYPES_URL = "/v2/projects/{projectId}/datapoints/types";
	
	/**
	 * URL for data points for type
	 */
	public static final String DATA_POINTS_FOR_TYPE_URL = "/v2/projects/{projectId}/datapoints/for-type/{typeName}";
	
	/**
	 * URL for all data points usages
	 */
	public static final String DATA_POINTS_USAGES_URL = "/v2/projects/{projectId}/datapoints/usages";
	
	/**
	 * Lists all known data points for a project. Can optionally be filtered by usage.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId id of the project for which the known types should be listed
	 * @param usages the data point usages to return, or all usages if empty
	 * @return a list of type names
	 */
	@GetMapping(value = DATA_POINTS_URL)
	@Operation(summary  = "List all known data points for a project. Can optionally be filtered by usage.",
				operationId  = "getDataPoints")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role(value = {VIEWER})
	public Map<String, Map<String, MiningDataPointDefinition>> getDataPoints(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the id of the project for which data types should be returned", required = true)
			@PathVariable final EntityId projectId,
			@Parameter(description = "the data point usages that should be included, multiple usages can be given (if not specified, all usages are returned)",
					required = false)
			@RequestParam final Optional<List<String>> usages) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return dataPointRegistry.getDataPointDefinitionsWithUsage(Optional.ofNullable(projectService.getNid(projectId)), usages.orElse(Collections.emptyList()));
	}
	
	/**
	 * Lists all known data types containing data points for a project
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId id of the project for which the known types should be listed
	 * @return a list of type names
	 */
	@GetMapping(value = DATA_POINTS_TYPES_URL)
	@Operation(summary = "List all known data types containing data points for a project",
				 operationId = "getDataTypes")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role(value = {VIEWER})
	public List<String> getDataTypes(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the id of the project for which data types should be returned")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return new ArrayList<>(dataPointRegistry.getTypeDefinitions(Optional.ofNullable(projectService.getNid(projectId))).keySet());
	}
	
	/**
	 * Lists all data points for a type, recursively including nested types. Can optionally be filtered by usage.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId id of the project for which the data points should be listed
	 * @param typeName name of the type for which the data points should be listed
	 * @param usages the data point usages to return, or all usages if empty
	 * @return a {@linkplain ResponseEntity response} containing list of {@linkplain MiningDataPointDefinitionWithPath data points including the path} from
	 * the requested root type
	 */
	@GetMapping(value = DATA_POINTS_FOR_TYPE_URL)
	@Operation(description = "List all data points for a type, recursively including nested types. Can optionally be filtered by usage.",
				operationId = "getDataPointsForType")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or type does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role(value = {VIEWER})
	public ResponseEntity<List<MiningDataPointDefinitionWithPath>> getDataPointsForType(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the id of the project for which the data points should be listed")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the name of the type for which the data points should be listed", required = true)
			@PathVariable final String typeName,
			@Parameter(description = "the data point usages that should be included, multiple usages can be given  (all usages are returned, if not specified)",
					required = false)
			@RequestParam final Optional<List<String>> usages) {
		validate(request, "typeName");
		response.setStatus(HttpStatus.OK.value());
		final List<MiningDataPointDefinitionWithPath> dataPoints = dataPointRegistry.getDataPointsForTypeRecursivelyWithUsage(Optional.ofNullable(projectService.getNid(projectId)),
				typeName, usages.orElse(Collections.emptyList()));
		return dataPoints.isEmpty() ? ResponseEntity.notFound().build() : ResponseEntity.ok().body(dataPoints);
	}
	
	@SuppressWarnings("unused")
	@GetMapping(value = DATA_POINTS_USAGES_URL)
	@Operation(summary = "This endpoint exists only to generate type definitions via swagger-codegen and can not actually be called", 
						operationId = "getDataPointUsages")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "403", description = "always")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role(value = {VIEWER})
	public UsagesModel getDataPointUsages(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the id of the project")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.FORBIDDEN.value());
		
		/* this endpoint exists only to generate type definitions via swagger-codegen and can not actually be called */
		return null;
	}
}
