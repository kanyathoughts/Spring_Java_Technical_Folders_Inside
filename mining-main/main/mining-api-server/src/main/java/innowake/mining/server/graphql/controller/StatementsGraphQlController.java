/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.StatementPgDao.StatementQueryBuilder;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.StatementInquiryBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.StatementPojo;

/**
 * Controller for the "statements" and "sqlStatements" GraphQl Queries.
 */
@Controller
@Order(4)
public class StatementsGraphQlController implements MiningDataPointSource {
	
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private FilterObjectService filterObjectService;

	/**
	 * Query for statements of a module.
	 * 
	 * @param projectId the ID of the project that contains the module
	 * @param page the page number
	 * @param size the number of elements
	 * @param filterObject containing conditions to filter by
	 * @return the list of statements contained in the module
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public Paged<StatementPojo> statements(@Argument final Long projectId, 
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size, 
			@Argument(name = "filterObject") @Nullable final Map<String, Object> filterObject
			) {
		
		final int intPage = Optional.ofNullable(page).orElse(0).intValue();
		final int intSize = Optional.ofNullable(size).orElse(0).intValue();

		return moduleService.findStatements(Pagination.at(intPage, intSize), q -> {
			q.ofProject(EntityId.of(projectId));
			if (filterObject != null) {
				filterObjectService.applyFilterObject(projectId, MiningGraphQLQueries.STATEMENTS, filterObject, q);
			}
		});
	}

	@MiningDataPoint(displayName = "Length")
	@SchemaMapping(typeName = MiningEnitityNames.STATEMENT)
	public int textLength(final StatementPojo statement) {
		return statement.getText().length();
	}

	@SchemaMapping(typeName = MiningEnitityNames.STATEMENT)
	public ModulePojo module(final StatementPojo annotation) {
		return moduleService.getModule(annotation.getModule());
	}

	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		builder.defineType(MiningEnitityNames.STATEMENT)
				.representedBy(StatementPojo.class)
				.withDefaultProperties()
		.add();

		builder.defineDataPointsFromSchemaMappingAnnotations(StatementsGraphQlController.class);

		for (final Map.Entry<String, String> property : StatementPojo.SQL_PROPERTY_KEYS.entrySet()) {
			
			builder.defineDataPoint(MiningEnitityNames.STATEMENT, property.getKey())
				.type(MiningDataPointDefinition.ScalarType.JSON)
				.withCustomFetch(env -> {
					final StatementPojo pojo = env.getSource();
					final var properties = pojo.getProperties();
					return properties == null ? "" : properties.get(property.getKey());
				})
				.withDisplayName(property.getValue())
			.add();
		}

		builder.extend(MiningEnitityNames.MODULE, "id")
				.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> {
					f.<StatementInquiryBuilder, Long>eq((q, v) -> q.ofModule(EntityId.of(v)));
				})
				.add();
		builder.extend(MiningEnitityNames.MODULE, "name")
				.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> {
					f.<StatementInquiryBuilder, String>eq(StatementInquiryBuilder::withModuleName);
				})
				.add();
		builder.extend(MiningEnitityNames.MODULE, "path")
				.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> {
					f.<StatementInquiryBuilder, String>eq(StatementInquiryBuilder::withModulePath);
				})
				.add();

		builder.extend(MiningEnitityNames.STATEMENT, "technology")
			.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> f.eq(StatementInquiryBuilder::withTechnology))
			.add();
		builder.extend(MiningEnitityNames.STATEMENT, "text")
				.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> f.eq(StatementInquiryBuilder::withText))
				.withUsage(Usages.METRICS_CHART_DETAILS_SQL)
				.add();
		builder.extend(MiningEnitityNames.STATEMENT, "type")
			/* for  "content_type": { "notIn": [ "ALTER*", "CREATE*"] } */
			.type(String.class)
			.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> {
				f.<StatementQueryBuilder, String>eq((q, value) -> q.withType(FilterOperators.OPERATOR_EQ, value));
				f.<StatementQueryBuilder, String>notEq((q, value) -> q.withType(FilterOperators.OPERATOR_NOT_EQ, value));
				f.<StatementQueryBuilder, List<String>>in((q, value) -> q.withType(FilterOperators.OPERATOR_IN, value));
				f.<StatementQueryBuilder, List<String>>notIn((q, value) -> q.withType(FilterOperators.OPERATOR_NOT_IN, value));
			})
			.withUsage(Usages.METRICS_CHART_DETAILS_SQL)
			.add();

		builder.extend(MiningEnitityNames.STATEMENT, "textLength")
			.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> {
				f.<StatementQueryBuilder, Integer>eq((q, value) -> q.withTextLength(FilterOperators.OPERATOR_EQ, value));
				f.<StatementQueryBuilder, Integer>gte((q, value) -> q.withTextLength(FilterOperators.OPERATOR_GTE, value));
				f.<StatementQueryBuilder, Integer>gt((q, value) -> q.withTextLength(FilterOperators.OPERATOR_GT, value));
				f.<StatementQueryBuilder, Integer>lte((q, value) -> q.withTextLength(FilterOperators.OPERATOR_LTE, value));
				f.<StatementQueryBuilder, Integer>lt((q, value) -> q.withTextLength(FilterOperators.OPERATOR_LT, value));
			})
			.add();

		builder.extend(MiningEnitityNames.STATEMENT, StatementPojo.PROPERTY_KEY_HALSTEAD_COMPLEXITY)
			.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> {
				f.<StatementQueryBuilder, Integer>eq((q, value) -> q.withHalsteadComplexity(FilterOperators.OPERATOR_EQ, value));
				f.<StatementQueryBuilder, Integer>gte((q, value) -> q.withHalsteadComplexity(FilterOperators.OPERATOR_GTE, value));
				f.<StatementQueryBuilder, Integer>gt((q, value) -> q.withHalsteadComplexity(FilterOperators.OPERATOR_GT, value));
				f.<StatementQueryBuilder, Integer>lte((q, value) -> q.withHalsteadComplexity(FilterOperators.OPERATOR_LTE, value));
				f.<StatementQueryBuilder, Integer>lt((q, value) -> q.withHalsteadComplexity(FilterOperators.OPERATOR_LT, value));
			})
			.add();

		/* filter statements by taxonomy id - only filtering for now */
		builder.defineDataPoint(MiningEnitityNames.STATEMENT, "taxonomy")
			.type(EntityId.class)
			.withFiltering(MiningGraphQLQueries.STATEMENTS, f -> {
				f.eq(StatementInquiryBuilder::ofTaxonomy);
				f.in(StatementInquiryBuilder::ofTaxonomies);
			})
			.add();
	}
}
