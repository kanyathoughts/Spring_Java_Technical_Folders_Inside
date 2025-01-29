/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.config;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.SortDirection;
import org.apache.commons.text.CaseUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import graphql.schema.DataFetchingEnvironment;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.data.event.MiningDataPointSourceInvalidatedEvent;
import innowake.mining.server.aspect.WithSystemUser;
import innowake.mining.server.event.ProjectCreatedEvent;
import innowake.mining.server.event.ProjectDeletedEvent;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.service.UserRoleService;

/**
 * Provide data points for Taxonomies on Modules
 */
@Component
@Order(3)
public class TaxonomyDataPointSource implements MiningDataPointSource {

	/**
	 * Each Module gets a data point of this type, which is an aggregate type containing all Taxonomy types of the Module's project
	 */
	public static final String TAXONOMIES_TYPE_NAME = "Taxonomies";
	/**
	 * Name of the data point on each Module that contains the Taxonomies.
	 */
	public static final String TAXONOMIES_DATA_POINT_NAME = "taxonomy";

	@Autowired
	private ProjectService projectService;
	
	@Autowired
	private TaxonomyService taxonomyService;
	
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Autowired
	private UserRoleService user;

	@WithSystemUser
	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		final List<ProjectPojo> projects = builder.getProjectId()
				.map(projectId -> Collections.singletonList(projectService.get(EntityId.of(projectId))))
				.orElseGet(() -> projectService.find(user.isAdmin() ? q -> q.withIdAbove(0L).filterMarkedForDeletion(Boolean.FALSE) : q -> q
						.withIds(user.getProjectIds(), user.getClientAdminIds()).filterMarkedForDeletion(Boolean.FALSE)));

		for (final ProjectPojo project : projects) {
			final List<TaxonomyTypePojo> taxonomies = taxonomyService.findTypes(q -> q.ofProject(project.identity()).sortName(SortDirection.ASCENDING));
			if (! taxonomies.isEmpty()) {
				builder.defineType(TAXONOMIES_TYPE_NAME)
						.onlyOnProjects(project.getId())
						.add();
				builder.defineDataPoint(MiningEnitityNames.MODULE, TAXONOMIES_DATA_POINT_NAME)
					.onlyOnProjects(project.getId())
					.type(TAXONOMIES_TYPE_NAME)
					.withCustomFetch(DataFetchingEnvironment::getSource)
					.add();
				for (final TaxonomyTypePojo tt : taxonomies) {
					final String name = toCamelCase(tt.getName());
					final String escapedName = escapeName(tt.getName());
					final String property = "taxonomy_" + name;
					
					builder.defineDataPoint(TAXONOMIES_TYPE_NAME, name)
						.<ModuleService.ModuleInquiryBuilder>withDynamicFieldBuilder(MiningEnitityNames.MODULE, 
								(alias, args, q) -> q.includeTaxonomies(property, tt.getId()))
						.withFiltering(MiningGraphQLQueries.MODULES, f -> {
							f.<ModuleService.ModuleInquiryBuilder, String>eq((q, val) -> q.filterArray(property, Collections.singleton(val)));
							f.<ModuleService.ModuleInquiryBuilder, Collection<String>>in((q, vals) -> q.filterArray(property, vals));
							f.<ModuleService.ModuleInquiryBuilder, Void>isAbsent((q, vals) -> q.filterNull(property, true));
						})
						.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
							f.<AnnotationService.AnnotationInquiryBuilder, Collection<String>>in((q, vals) -> {
								q.withTaxonomyNames(vals);
								q.withTaxonomyTypeName(escapedName);
							});
							f.<AnnotationService.AnnotationInquiryBuilder, String>eq((q, val) -> {
								q.withTaxonomyNames(List.of(val));
								q.withTaxonomyTypeName(escapedName);
							});
							f.<AnnotationService.AnnotationInquiryBuilder, String>isAbsent((q, vals) -> {
								//query all that don't have this typeName
								q.notWithTaxonomyTypeName(escapedName);
							});
						})
						.onlyOnProjects(project.getId())
						.arrayOfType(ScalarType.STRING)
						.withDisplayName(tt.getName())
						.withUsage(Usages.TAXONOMY_ASSIGNMENTS_EXPORT)
						.withUsage(Usages.ALL_TAXONOMIES_SAVED_SEARCH)
						.withUsage(Usages.GRAPHQL_QUERY_MODULES)
						.withUsage(Usages.MINING_UI_MODULES_TABLE)
						.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, tt.getCategory().getName())
						.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
						.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.MULTI_SELECT_SHOW_NONE_OPTION, "true")
						.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
								SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_TAXONOMY_AGGREGATED_VALUES)
						.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "NAME")
						.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE,
								SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FILTER, "{\"TYPE_NAME\": { \"eq\": \"" + tt.getName() +  "\" } }")
						.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
						.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, tt.getCategory().getName())
						.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE,
								SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
						.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, SearchFilterAttributes.MULTI_SELECT_SHOW_NONE_OPTION, "true")
						.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
								SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_TAXONOMY_AGGREGATED_VALUES)
						.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "NAME")
						.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE,
								SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FILTER, "{\"TYPE_NAME\": { \"eq\": \"" + tt.getName() +  "\" } }")
						.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_TAG)
						.withCustomFetch(env -> env.<ModulePojo>getSource().dynamic(property))
						.add();
				}
			}
		}
	}

	public static String toCamelCase(final String s) {
		/* If the string contains no delimiters, we expect it's already camel-case.
		 * Then we only make sure the first letter is lower case.
		 * Otherwise in this case, toCamelCase() would turn it into an all lower case string.
		 */
		if (s.isEmpty()) {
			throw new IllegalArgumentException("String must not be empty.");
		} else if (s.chars().anyMatch(ch -> ch == ' ' || ch == '-' || ch == '.')) {
			/* this only to ensure that "common delimiters" like space, '-' and '.' are rendered nicely as camel case,
			 * if the Taxonomy name contains other invalid characters they are being removed by the data point builder */
			return CaseUtils.toCamelCase(s, false, ' ', '-', '.');
		} else {
			return s.substring(0, 1).toLowerCase() + s.substring(1);
		}
	}

	private static String escapeName(final String s) {
		return s.replace("\"", "\\\"");
	}
	
	@EventListener
	public void onTaxonomiesModified(final TaxonomiesModifiedEvent event) {
		eventPublisher.publishEvent(new MiningDataPointSourceInvalidatedEvent(this, event.getProjectId()));
	}
	
	@EventListener
	public void onProjectCreated(final ProjectCreatedEvent event) {
		eventPublisher.publishEvent(new MiningDataPointSourceInvalidatedEvent(this, event.getProjectId()));
	}
	
	@EventListener
	public void onProjectDeleted(final ProjectDeletedEvent event) {
		eventPublisher.publishEvent(new MiningDataPointSourceInvalidatedEvent(this, event.getProjectId()));
	}

}
