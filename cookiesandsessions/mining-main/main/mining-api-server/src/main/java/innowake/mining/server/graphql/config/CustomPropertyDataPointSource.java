/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.graphql.config;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import graphql.schema.DataFetchingEnvironment;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointBuilder.DataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.data.event.MiningDataPointSourceInvalidatedEvent;
import innowake.mining.server.aspect.WithSystemUser;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.event.ProjectCreatedEvent;
import innowake.mining.server.event.ProjectDeletedEvent;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.CustomPropertiesService.CustomPropertiesInquiryBuilder;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.AnnotationService.AnnotationInquiryBuilder;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.CustomPropertyDataType;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.service.UserRoleService;

/**
 * Registers custom properties as data points.
 *
 * Example: if Module has a custom property class "Foo" and that class defines a property named "bar", then I would be able to query it like so:
 * <pre>
 * query {
 *   modules(projectId: 1) {
 *     content {
 *       customProperties {
 *         Foo {
 *           bar
 *         }
 *       }
 *     }
 *   }
 * }
 * </pre>
 */
@Component
public class CustomPropertyDataPointSource implements MiningDataPointSource {

	private static final Logger LOG = LoggerFactory.getLogger(CustomPropertyDataPointSource.class);
	
	@Autowired
	private ProjectService projectService;

	@Autowired
	private CustomPropertiesService customPropertiesService;

	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Autowired
	private UserRoleService user;
	
	private final Map<String, CustomPropertyQuery> queries = Arrays.stream(CustomPropertyQuery.values())
			.collect(Collectors.toMap(CustomPropertyQuery::getEntityName, q -> q));

	@WithSystemUser
	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		final List<ProjectPojo> projects = builder.getProjectId()
				.map(projectId -> Collections.singletonList(projectService.get(EntityId.of(projectId))))
				.orElseGet(() -> projectService.find(user.isAdmin() ? q -> q.withIdAbove(0l).filterMarkedForDeletion(Boolean.FALSE) : q -> q
						.withIds(user.getProjectIds(), user.getClientAdminIds()).filterMarkedForDeletion(Boolean.FALSE)));

		for (final ProjectPojo project : projects) {
			final Long projectId = project.getId();
			final Map<String, Set<String>> customPropertyClassMap = project.getCustomPropertyClasses();

			for (final Map.Entry<String, Set<String>> entry : customPropertyClassMap.entrySet()) {
				final String entityName = entry.getKey();
				final Set<String> customPropertyClasses = entry.getValue();
				final String containerTypeName = entityName + "_CustomProperties"; // e.g. "Annotation_CustomProperties"
				builder.defineType(containerTypeName).onlyOnProjects(projectId).add();
				builder.defineDataPoint(entityName, "customProperties").type(containerTypeName)
					.onlyOnProjects(projectId)
					.withCustomFetch(DataFetchingEnvironment::getSource)
					.add();
				for (final String customPropertyClass : customPropertyClasses) {
					final String typeName = entityName + "_CustomProperties_" + customPropertyClass; // e.g. "Annotation_CustomProperties_Foo"
					builder.defineType(typeName).onlyOnProjects(projectId).add();
					final DataPointBuilder dp = builder
						.defineDataPoint(containerTypeName, customPropertyClass)
						.type(typeName)
						.onlyOnProjects(projectId)
						.withCustomFetch(DataFetchingEnvironment::getSource);
					/* Add default Custom Property usages based on the entity name, if any defined. */
					Optional.ofNullable(queries.get(entityName)).map(CustomPropertyQuery::getDefaultUsages).ifPresent(usages -> usages.forEach(dp::withUsage));
					dp.add();
					final List<CustomPropertyMetadata> customProperties = customPropertiesService
							.findPropertyDefinitions(q -> q.withParent(customPropertyClass));
					customProperties.forEach(prop -> defineCustomPropertyDataPoint(builder, customPropertyClass, typeName, prop, projectId, entityName));
				}
			}
		}
	}

	@Nullable
	public static DataPointBuilder setType(final DataPointBuilder builder, final CustomPropertyDataType type) {
		switch (type) {
			case BOOLEAN:
				return builder.type(ScalarType.BOOLEAN);
			case INTEGER:
				return builder.type(ScalarType.INT);
			case SHORT:
				return builder.type(ScalarType.INT);
			case LONG:
				return builder.type(ScalarType.LONG);
			case FLOAT:
				return builder.type(ScalarType.FLOAT);
			case DOUBLE:
				return builder.type(ScalarType.FLOAT);
			case DATETIME:
				return builder.type(ScalarType.DATETIME);
			case STRING:
				return builder.type(ScalarType.STRING);
			case EMBEDDEDLIST:
				return builder.arrayOfType(ScalarType.STRING);
			case EMBEDDEDMAP:
				return builder.type(ScalarType.JSON);
			case BYTE:
				return builder.type(ScalarType.INT);
			case DATE:
				return builder.type(ScalarType.DATETIME);
			case DECIMAL:
				return builder.type(ScalarType.LONG);
			default:
				return null;
		}
	}
	
	private void defineCustomPropertyDataPoint(final MiningDataPointBuilder builder, final String customPropertyClassName, final String containerTypeName,
			final CustomPropertyMetadata customProperty, final Long projectId, final String entityName) {
		final DataPointBuilder dpBuilder = setType(builder.defineDataPoint(containerTypeName, customProperty.getName()), customProperty.getDataType());
		if (dpBuilder == null) {
			LOG.error("Ignoring custom property " + customProperty.getName() + " on " + containerTypeName
					+ " because type " + customProperty.getDataType().name() + " is not supported.");
			return;
		}
		dpBuilder.onlyOnProjects(projectId)
			.withDisplayName(customProperty.getLabel())
			.withDescription(StringUtils.trimToEmpty(customProperty.getDescription()))
			.withCustomFetch(env -> env.<MiningPojo>getSource().getCustomProperties().getOptional(customPropertyClassName, customProperty.getName()));
		Optional.ofNullable(queries.get(entityName)).ifPresent(query -> {
			query.getDefaultUsages().forEach(usage -> {
				dpBuilder.withUsage(usage);
				dpBuilder.withUsageAttribute(usage, TableAttributes.CATEGORY,
						StringUtils.isEmpty(customProperty.getCustomCategory()) ? "Custom Properties" : customProperty.getCustomCategory());
			});
			if (CustomPropertyDataType.EMBEDDEDLIST == customProperty.getDataType()) {
				/* multi value */
				dpBuilder.withFiltering(query.getQueryName(), f -> {
					/* contains value */
					f.<CustomPropertiesInquiryBuilder, String>eq((q, v) -> q
						.withCustomPropertyAny(Arrays.asList(customPropertyClassName, customProperty.getName()), Collections.singletonList(v)));
					/* contains any of */
					f.<CustomPropertiesInquiryBuilder, List<String>>in((q, v) -> q
						.withCustomPropertyAny(Arrays.asList(customPropertyClassName, customProperty.getName()), v));
					/* none assigned */
					f.<CustomPropertiesInquiryBuilder, Void>isAbsent((q, v) -> q
						.withCustomPropertyPresent(Arrays.asList(customPropertyClassName, customProperty.getName()), false));
				});
			} else {
				/* single value */
				dpBuilder.withFiltering(query.getQueryName(), f -> {
					/* eq -> starts with */
					f.<CustomPropertiesInquiryBuilder, String>eq((q, v) -> q
						.withCustomPropertyLike(Arrays.asList(customPropertyClassName, customProperty.getName()), v + "%"));
				});
			}
			query.getDefaultUsages().forEach(usage -> {
				dpBuilder.withUsageAttribute(usage, TableAttributes.CATEGORY,
						StringUtils.isEmpty(customProperty.getCustomCategory()) ? "Custom Properties" : customProperty.getCustomCategory());
				if (CustomPropertyFieldType.TAG == customProperty.getFieldType()) {
					dpBuilder.withUsageAttribute(usage,
							SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT);
					dpBuilder.withUsageAttribute(usage,
							SearchFilterAttributes.MULTI_SELECT_SHOW_NONE_OPTION, "true");
					dpBuilder.withUsageAttribute(usage,
							SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE, SearchFilterAttributes.FILTER_MODE_CUSTOM_PROPERTY_TAG);
					dpBuilder.withUsageAttribute(usage,
							SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, customProperty.getAutoCompletionKey());
				} else if (CustomPropertyDataType.EMBEDDEDLIST == customProperty.getDataType() &&
								CustomPropertyFieldType.DEFAULT.equals(customProperty.getFieldType())) {
					dpBuilder.withUsageAttribute(usage,
							SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT);
				} else {
					dpBuilder.withUsageAttribute(usage,
							SearchFilterAttributes.FILTER_MODE,
							customProperty.getDataType().isNumeric() ? SearchFilterAttributes.FILTER_MODE_NUMBER : SearchFilterAttributes.FILTER_MODE_TEXT)
					.withUsage(Usages.SORT_BY)
					.withSorting(MiningGraphQLQueries.MODULES, ModuleService.ModuleInquiryBuilder::sortCustomProperties)
					.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortByCustomProperties)
					.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortCustomProperties);
				}
			});
		});
		if (CustomPropertyFieldType.TAG == customProperty.getFieldType()) {
			dpBuilder.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_TAG);
		} else if (CustomPropertyFieldType.URL == customProperty.getFieldType()) {
			dpBuilder.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_EXTERNAL_LINK);
		}
		dpBuilder.add();
	}

	@EventListener
	public void onCustomPropertiesModified(final CustomPropertiesModifiedEvent event) {
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
