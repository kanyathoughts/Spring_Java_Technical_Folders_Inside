/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import graphql.GraphQLContext;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojo;

/**
 * Data Point Source for Functional Blocks.
 * @see FunctionalBlocksGraphQlController
 * @see FunctionalBlockPojo
 */
@Component
public class FunctionalBlocksDataPointSource implements MiningDataPointSource {

	private static final Logger LOG = LoggerFactory.getLogger(MiningDataPointSource.class);

	private static final String BASE_DATA = "Base Data";
	/**
	 * GraphQL schema type name for {@link ReachabilityDataPojo}.
	 */
	private static final String REACHABILITY_DATA = "ReachabilityData";
	/**
	 * GraphGL schema type name for {@link FunctionalBlockPojo}
	 */
	public static final String FUNCTIONAL_BLOCK_TYPE_NAME = "FunctionalBlock";
	public static final String FUNCTIONAL_BLOCK_LINK_TYPE_NAME = "FunctionalBlockLink";

	@Autowired
	@Lazy
	private FilterObjectService filterObjectService;
	@Value("${dbcutter-db-postgres.enabled}")
	private boolean dbcutterDbPostgresEnabled;

	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {

		builder.defineType(FUNCTIONAL_BLOCK_TYPE_NAME).representedBy(FunctionalBlockPojo.class).add();
		builder.defineType(FUNCTIONAL_BLOCK_LINK_TYPE_NAME).representedBy(FunctionalBlockLink.class).withDefaultProperties().add();
		builder.defineDataPointsFromSchemaMappingAnnotations(FunctionalBlocksGraphQlController.class);
		/* defining "aggregations" datapoint directly on the Paged<FunctionalBlock> type, so aggregations can be applied to all queries
		 * that return a (paged) set of functional blocks */
		builder.defineAggregations("PAGED_FunctionalBlock", "aggregations", FunctionalBlockFieldName.class);

		if (dbcutterDbPostgresEnabled) {
			try {
				/* Enable sqlDetails datapoint only when the dbcutter connection is available */
				final Method sqlDetails =
						FunctionalBlocksGraphQlController.class.getMethod("sqlDetails", ReachabilityDataPojo.class, GraphQLContext.class);
				builder.defineDataPointForGivenMethod(FunctionalBlocksGraphQlController.class, sqlDetails);
				builder.extend(REACHABILITY_DATA, "sqlDetails")
						.withDisplayName("SQL Details")
						.withDescription("SQL Details for the Reachability Data")
						.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
						.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
						.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "12")
						.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.HIDDEN_BY_DEFAULT, "true")
						.add();
			} catch (final NoSuchMethodException e) {
				LOG.error("Could not find method getSqlDetails in FunctionalBlocksGraphQlController", e);
			}
		} else {
			LOG.debug("DBCutter connection is not available. Skipping sqlDetails data point.");
		}

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "uid")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.eq(FunctionalBlockInquiryBuilder::byUid);
				f.in(FunctionalBlockInquiryBuilder::byUids);
				f.notEq(FunctionalBlockInquiryBuilder::notByUid);
			})
			.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "type")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.eq(FunctionalBlockInquiryBuilder :: withType);
				f.in(FunctionalBlockInquiryBuilder :: withTypes);
			})
			.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "outdatedBlock")
				.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
					f.<FunctionalBlockInquiryBuilder, Boolean>eq((b, value) -> b.withFlag(FunctionalBlockFlag.OUTDATED, value));
				})
				.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "blocksWithDeletedUB")
				.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
					f.<FunctionalBlockInquiryBuilder, Boolean>eq((b, value) -> b.withFlag(FunctionalBlockFlag.DELETED, value));
				})
				.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "parents")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, "FilterObject_functionalBlocks", f -> {
				f.<FunctionalBlockInquiryBuilder, Map<String, Object>>eq((b, filterObject) ->
					b.withParent(peer -> filterObjectService.applyFilterObject(null, FunctionalBlocksGraphQlController.QUERY_NAME, filterObject, peer)));
				f.<FunctionalBlockInquiryBuilder, Map<String, Object>>notEq((b, filterObject) ->
					b.notWithParent(peer -> filterObjectService.applyFilterObject(null, FunctionalBlocksGraphQlController.QUERY_NAME, filterObject, peer)));
			})
			.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "children")
				.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, "FilterObject_functionalBlocks", f ->
						f.<FunctionalBlockInquiryBuilder, Map<String, Object>> eq((b, filterObject) -> b.withChildForMergedAndSingleReachabilityBlocks(
						child -> filterObjectService.applyFilterObject(null, FunctionalBlocksGraphQlController.QUERY_NAME, filterObject, child))))
				.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "peers")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, "FilterObject_functionalBlocks", f -> {
				f.<FunctionalBlockInquiryBuilder, Map<String, Object>>eq((b, filterObject) ->
					b.withPeer(peer -> filterObjectService.applyFilterObject(null, FunctionalBlocksGraphQlController.QUERY_NAME, filterObject, peer)));
				f.<FunctionalBlockInquiryBuilder, Map<String, Object>>notEq((b, filterObject) ->
					b.notWithPeer(peer -> filterObjectService.applyFilterObject(null, FunctionalBlocksGraphQlController.QUERY_NAME, filterObject, peer)));
			})
			.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "status")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.<FunctionalBlockInquiryBuilder, FunctionalBlockStatus>eq(FunctionalBlockInquiryBuilder :: withStatus);
				f.<FunctionalBlockInquiryBuilder, FunctionalBlockStatus>notEq(FunctionalBlockInquiryBuilder :: notWithStatus);
			})
			.add();

		builder.extend("ResolvedModulePart", "moduleId")
				.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
					f.eq(FunctionalBlockInquiryBuilder::withResolvedModulePart);
					f.in(FunctionalBlockInquiryBuilder::withResolvedModuleParts);
				}).add();
		
		builder.extend("GeneratedFrom", "annotationId")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.eq(FunctionalBlockInquiryBuilder::generatedFromAnnotation);
				f.in(FunctionalBlockInquiryBuilder::generatedFromAnnotations);
			})
			.add();

		builder.extend("Taxonomy", "id")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, ScalarType.ENTITY_ID, f -> {
				f.eq(FunctionalBlockInquiryBuilder::withResolvedModulePartHavingTaxonomy);
				f.in(FunctionalBlockInquiryBuilder::withResolvedModulePartHavingTaxonomies);
			})
			.add();
		
		builder.defineType().representedBy(ModulePojo.class).withDefaultProperties().add();
		builder.extend("Module", "name")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.eq(FunctionalBlockInquiryBuilder::withResolvedModulePartModuleName);
				f.in(FunctionalBlockInquiryBuilder::withResolvedModulePartModuleNames);
			})
			.add();

		builder.extend("Module", "technology")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.eq(FunctionalBlockInquiryBuilder::withResolvedModulePartTechnology);
				f.in(FunctionalBlockInquiryBuilder::withResolvedModulePartTechnologies);
			})
			.add();

		builder.extend("Module", "type")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.eq(FunctionalBlockInquiryBuilder::withResolvedModulePartType);
				f.in(FunctionalBlockInquiryBuilder::withResolvedModulePartTypes);
			})
			.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "lowerBoundAccessTypes")
				.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
					f.<FunctionalBlockInquiryBuilder, String>eq(FunctionalBlockInquiryBuilder::withLowerBoundAccessType);
					f.<FunctionalBlockInquiryBuilder, Collection<String>>in(FunctionalBlockInquiryBuilder::withLowerBoundAccessTypes);
				})
				.add();

		
		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "name")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.<FunctionalBlockInquiryBuilder, String>eq(this::searchByName);
				f.in(FunctionalBlockInquiryBuilder::withNames);
			})
			.withUsage(Usages.SORT_BY)
			.withSorting(MiningGraphQLQueries.FUNCTIONAL_BLOCKS, FunctionalBlockService.FunctionalBlockOrderBuilder::sortName)
			.add();
		
		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "updated")
		.withUsage(Usages.SORT_BY)
		.withSorting(MiningGraphQLQueries.FUNCTIONAL_BLOCKS, FunctionalBlockService.FunctionalBlockOrderBuilder::sortByLastModified)
		.add();
		
		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME, "deepName")
		.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
			f.<FunctionalBlockInquiryBuilder, String>eq(this::searchByName);
			f.<FunctionalBlockInquiryBuilder, Collection<String>>in(this::searchByNames);
		})
		.add();

		/* data points for reachability data table */

		builder.defineAlias(REACHABILITY_DATA, "upperBoundModuleName")
				.forDataPoint("upperBoundModules")
				.withSubSelection("name")
				.add();

		builder.extend(REACHABILITY_DATA, "upperBoundModuleName")
				.withDisplayName("Upper Bound")
				.withDescription("Module name of the Upper Bound Module")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "0")
				.withUsage(Usages.VIEW_MODE)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LINK_TEMPLATE, "/project-${$projectId}/module-${linkHash}/details/overview")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "upperBoundModuleName.linkHash")
				.withUsage(Usages.SORT_BY)
				.withSorting(MiningGraphQLQueries.REACHABILITY_DATA, FunctionalBlockService.ReachabilityDataOrderBuilder::sortByUpperBoundModuleName)
				.add();
		
		builder.defineAlias(REACHABILITY_DATA, "upperBoundModuleTechnology")
				.forDataPoint("upperBoundModules")
				.withSubSelection("technology")
				.withDisplayName("Upper Bound Technology")
				.withDescription("Technology of the Upper Bound Module")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "1")
				.add();
	
		builder.defineAlias(REACHABILITY_DATA, "upperBoundModuleType")
				.forDataPoint("upperBoundModules")
				.withSubSelection("type")
				.withDisplayName("Upper Bound Type")
				.withDescription("Type of the Upper Bound Module")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "2")
				.add();
	
		builder.defineAlias(REACHABILITY_DATA, "lowerBoundModuleName")
				.forDataPoint("lowerBoundModules")
				.withSubSelection("name")
				.add();

		builder.extend(REACHABILITY_DATA, "lowerBoundModuleName")
				.withDisplayName("Lower Bound")
				.withDescription("Module name of the lower Bound Module")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "7")
				.withUsage(Usages.VIEW_MODE)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LINK_TEMPLATE, "/project-${$projectId}/module-${linkHash}/details/overview")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "lowerBoundModuleName.linkHash")
				.withUsage(Usages.SORT_BY)
				.withSorting(MiningGraphQLQueries.REACHABILITY_DATA, FunctionalBlockService.ReachabilityDataOrderBuilder::sortByLowerBoundModuleName)
				.add();
	
		builder.defineAlias(REACHABILITY_DATA, "lowerBoundModuleTechnology")
				.forDataPoint("lowerBoundModules")
				.withSubSelection("technology")
				.withDisplayName("Lower Bound Technology")
				.withDescription("Technology of the Lower Bound Module")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "8")
				.add();
		
		builder.defineAlias(REACHABILITY_DATA, "lowerBoundModuleType")
				.forDataPoint("lowerBoundModules")
				.withSubSelection("type")
				.withDisplayName("Lower Bound Type")
				.withDescription("Type of the Upper Bound Module")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "9")
				.add();
		builder.defineAlias(REACHABILITY_DATA, "accessModulesName")
				.forDataPoint("accessModules")
				.withSubSelection("name")
				.withDisplayName("Accessing Module")
				.withDescription("Module name of the Access Module")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "5")
				.withUsage(Usages.VIEW_MODE)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LINK_TEMPLATE, "/project-${$projectId}/module-${linkHash}/details/overview")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "accessModulesName.linkHash")
				.add();

		builder.defineAlias(REACHABILITY_DATA, "upperBoundTaxonomies")
				.forDataPoint("moduleTaxonomies")
				.withParameter("moduleType", "UpperBound")
				.withDisplayName("Upper Bound Taxonomy")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, "Taxonomy")
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "3")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_TAG)
				.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TREE_SELECT)
				.add();

		builder.extend(REACHABILITY_DATA, "upperBoundTaxonomies")
				.withFiltering(MiningGraphQLQueries.REACHABILITY_DATA, ScalarType.ENTITY_ID, f -> {
					f.eq(FunctionalBlockService.ReachabilityDataOrderBuilder::ofUpperBoundTaxonomy);
					f.in(FunctionalBlockService.ReachabilityDataOrderBuilder::ofUpperBoundTaxonomies);
				})
				.add();


		builder.defineAlias(REACHABILITY_DATA, "lowerBoundTaxonomies")
				.forDataPoint("moduleTaxonomies")
				.withParameter("moduleType", "LowerBound")
				.withDisplayName("Lower Bound Taxonomy")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, "Taxonomy")
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "10")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_TAG)
				.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TREE_SELECT)
				.add();

		builder.extend(REACHABILITY_DATA, "lowerBoundTaxonomies")
				.withFiltering(MiningGraphQLQueries.REACHABILITY_DATA, ScalarType.ENTITY_ID, f -> {
					f.eq(FunctionalBlockService.ReachabilityDataOrderBuilder::ofLowerBoundTaxonomy);
					f.in(FunctionalBlockService.ReachabilityDataOrderBuilder::ofLowerBoundTaxonomies);
				})
				.add();

		builder.defineAlias(REACHABILITY_DATA, "pathTaxonomies")
				.forDataPoint("moduleTaxonomies")
				.withParameter("moduleType", "PathTaxonomies")
				.withDisplayName("Path Taxonomy")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, "Taxonomy")
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "11")
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.HIDDEN_BY_DEFAULT, "true")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_TAG)
				.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TREE_SELECT)
				.add();

		builder.extend(REACHABILITY_DATA, "pathTaxonomies")
				.withFiltering(MiningGraphQLQueries.REACHABILITY_DATA, ScalarType.ENTITY_ID, f -> {
					f.eq(FunctionalBlockService.ReachabilityDataOrderBuilder::ofPathTaxonomy);
					f.in(FunctionalBlockService.ReachabilityDataOrderBuilder::ofPathTaxonomies);
				})
				.add();

		builder.defineAlias(REACHABILITY_DATA, "intermediateModuleName")
				.forDataPoint("intermediateModulesData")
				.withSubSelection("name")
				.withDisplayName("Intermediate Modules")
				.withDescription("List of intermediate Modules between upper bound and access module")
				.withUsage(Usages.MINING_UI_REACHABILITY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.CATEGORY, BASE_DATA)
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "4")
				.withUsageAttribute(Usages.MINING_UI_REACHABILITY_TABLE, TableAttributes.HIDDEN_BY_DEFAULT, "true")
				.withUsage(Usages.VIEW_MODE)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LINK_TEMPLATE, "/project-${$projectId}/module-${linkHash}/details/overview")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "intermediateModuleName.linkHash")
				.add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME,"referencedDataDictionaries")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.in(FunctionalBlockInquiryBuilder::withReferenceToDataDictionaries);
			}).add();

		builder.extend(FUNCTIONAL_BLOCK_TYPE_NAME,"referencedDataDictionaryNames")
			.withFiltering(FunctionalBlocksGraphQlController.QUERY_NAME, f -> {
				f.in(FunctionalBlockInquiryBuilder::withReferenceToDataDictionaryNames);
			}).add();
	}

	private void searchByName(final FunctionalBlockInquiryBuilder b, final String value) {
		String transformedValue = StringUtils.trimToEmpty(value);
		if (transformedValue.length() > 1) {
			if (transformedValue.charAt(0) == '*') {
				transformedValue = '%' + transformedValue.substring(1);
			}
			if (transformedValue.charAt(transformedValue.length() - 1) == '*') {
				transformedValue = transformedValue.substring(0, transformedValue.length() - 1) + '%';
			}
		}
		b.withName(transformedValue);
	}
	
	private void searchByNames(final FunctionalBlockInquiryBuilder b, final Collection<String> values) {
		final List<String> transformedValues = new ArrayList<>();
		for (final String value : values) {
			String transformedValue = StringUtils.trimToEmpty(value);
			if (transformedValue.length() > 1) {
				if (transformedValue.charAt(0) == '*') {
					transformedValue = '%' + transformedValue.substring(1);
				}
				if (transformedValue.charAt(transformedValue.length() - 1) == '*') {
					transformedValue = transformedValue.substring(0, transformedValue.length() - 1) + '%';
				}
			}
			transformedValues.add(transformedValue);
		}
		b.withNames(transformedValues);
	}
}

