/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.graphql.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import graphql.execution.DataFetcherResult;
import graphql.schema.DataFetchingEnvironment;
import graphql.schema.SelectedField;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.server.aspect.WithSystemUser;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.discovery.feature.matrix.Dependency;
import innowake.mining.server.discovery.feature.matrix.FeatureMatrix;
import innowake.mining.server.discovery.feature.matrix.FeatureMatrixRoot;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

/**
 * Controller for the {@linkplain DependencyInformation} GraphQl Query.
 */
@Controller
public class DependencyGraphQlController implements MiningDataPointSource {

	private static final String FILE_NAME = "discoveryFeatureMatrix.json";
	
	@Autowired
	private FilterObjectService filterObjectService;
	
	@SchemaMapping(typeName = MiningEnitityNames.DEPENDENCY_INFORMATION)
	public static class DependencyInformation {
		private final RelationshipDirection direction;
		private final ModuleRelationshipPojo relation;
		private final ModuleBasePojo src;
		private final ModuleBasePojo dst;
		
		public DependencyInformation(final ModuleRelationshipPojo relation) {
			src = relation.getSrcModuleDetails().orElseThrow(() -> new IllegalArgumentException("Source details not available"));
			dst = relation.getDstModuleDetails().orElseThrow(() -> new IllegalArgumentException("Destination details not available"));
			direction = relation.getDirection().orElseThrow(() -> new IllegalArgumentException("Relationship direction not determined"));
			this.relation = relation;
		}
		
		public UUID getRelationshipId() {
			return relation.getId();
		}
		
		@MiningDataPoint(displayName = "Relationship Type", description = "Relationship of the dependency (Calls or Includes or Read writes or References)")
		@Usage(value = Usages.MINING_UI_DEPENDENCIES_TABLE, attributes = {
				@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "2"),
				@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_FIXED_VALUES, value = "CALLS,INCLUDES,REFERENCES,ACCESSES"),
				@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
		})
		@Usage(value = Usages.SEARCH_FILTER, attributes = {
				@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT) })
		public RelationshipType getRelationship() {
			return relation.getRelationship();
		}
		
		@MiningDataPoint(displayName = "Direction", description = "Direction of the dependency (incoming or outgoing)")
		@Usage(value = Usages.MINING_UI_DEPENDENCIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "5"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_FIXED_VALUES, value = "OUT,IN"),
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
		})
		@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
		})
		public RelationshipDirection getDirection() {
			return direction;
		}

		@MiningDataPoint(description = "Location of the dependency in the source module")
		@Nullable
		public ModuleLocation getFromModuleLocation() {
			return relation.getSrcLocation().orElse(null);
		}

		@MiningDataPoint(description = "Location of the dependency target in the target module")
		@Nullable
		public ModuleLocation getToModuleLocation() {
			return relation.getDstLocation().orElse(null);
		}

		@MiningDataPoint(description = "Properties of the dependency", scalarType = ScalarType.JSON)
		@Usage(value = Usages.SEARCH_FILTER, attributes = {
				@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_TEXT)
		})
		public Map<String, Object> getProperties() {
			return relation.getProperties().orElse(Collections.emptyMap());
		}

		@MiningDataPoint(description = "Target of the dependency")
		public ModuleBasePojo getTarget() {
			return direction == RelationshipDirection.IN ? src : dst;
		}

		@MiningDataPoint(description = "Module id of the dependency target")
		public Long getTargetId() {
			return getTarget().getId();
		}

		@MiningDataPoint(displayName = "Target Name", description = "Name of the dependency target for outgoing dependencies or source for incoming dependencies")
		@Usage(value = Usages.MINING_UI_DEPENDENCIES_TABLE, attributes = {
				@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "1"),
				@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
		})
		@Usage(value = Usages.VIEW_MODE, attributes = {
				@UsageAttribute(key = ViewModeAttributes.DISPLAY_AS, value = ViewModeAttributes.DISPLAY_AS_LINK),
				@UsageAttribute(key = ViewModeAttributes.LINK_TEMPLATE, value = "/project-${$projectId}/module-${targetId}/details/overview"),
				@UsageAttribute(key = ViewModeAttributes.TOGETHER_WITH, value = "targetId"),
		})
		public String getTargetName() {
			return getTarget().getName();
		}

		@MiningDataPoint(displayName = "Missing/Identified", description = "Identification of the target module")
		@Usage(value = Usages.MINING_UI_DEPENDENCIES_TABLE, attributes = {
				@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "4"),
				@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
		})
		public Identification getIdentification() {
			return getTarget().getIdentification();
		}

		@MiningDataPoint(displayName = "Number of Errors", description = "Number of errors in the target module")
		@Usage(value = Usages.MINING_UI_DEPENDENCIES_TABLE, attributes = {
				@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "3"),
				@UsageAttribute(key = TableAttributes.CATEGORY, value = "Errors")
		})
		public int getErrorCount() {
			return getTarget().getErrorCount();
		}
	}
	
	@WithSystemUser
	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		builder.defineType(MiningEnitityNames.DEPENDENCY_INFORMATION)
			.representedBy(DependencyInformation.class)
			.withDefaultProperties()
			.add();
		
		builder.defineDataPointsFromSchemaMappingAnnotations(DependencyGraphQlController.class);
		
		builder.extend(MiningEnitityNames.DEPENDENCY_INFORMATION, "direction")
			.withFiltering(MiningGraphQLQueries.MODULE_DEPENDENCIES, f -> {
				/* handled in the query */
				f.eq((q, v) -> {});
				f.in((q, v) -> {});
			})
			.add();
		
		builder.extend(MiningEnitityNames.DEPENDENCY_INFORMATION, "relationship")
			.withFiltering(MiningGraphQLQueries.MODULE_DEPENDENCIES, f -> {
				f.<ModuleService.ModuleRelationshipInquiryBuilder, RelationshipType>eq((q, value) -> q.withType(value));
				f.<ModuleService.ModuleRelationshipInquiryBuilder, List<RelationshipType>>in((q, values) -> q.withTypes(values));
			})
			.add();
		
		builder.extend(MiningEnitityNames.DEPENDENCY_INFORMATION, "properties")
			.withFiltering(MiningGraphQLQueries.MODULE_DEPENDENCIES, f -> {
				f.<ModuleService.ModuleRelationshipInquiryBuilder, String>eq((q, value) -> q.withProperties(value));
			})
			.add();
		
		final ObjectMapper mapper = new ObjectMapper();
		FeatureMatrixRoot featureMatrixRoot;
		try {
			featureMatrixRoot = mapper.readValue(getClass().getClassLoader().getResource(FILE_NAME), FeatureMatrixRoot.class);
			for (final FeatureMatrix feature : featureMatrixRoot.getFeatureMatrices()) {
				for (final Dependency dependency : feature.getDependencies()) {
					for (final RelationshipType relation : dependency.getRelationship()) {
						dependency.getAttributes().keySet()
								.forEach(attribute -> builder.defineDataPoint(MiningEnitityNames.DEPENDENCY_INFORMATION, relation + "_" + attribute)
										.type(ScalarType.STRING)
										.withCustomFetch(d -> d.<DependencyInformation>getSource().getProperties().get(attribute))
										.withDisplayName(attribute)
										.withUsage(Usages.MINING_UI_DEPENDENCIES_TABLE)
										.withUsageAttribute(Usages.MINING_UI_DEPENDENCIES_TABLE,
												TableAttributes.CATEGORY, relation.name() + " Properties")
										.add());
					}
				}
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	@Autowired
	private ModuleService moduleService;

	private List<ModuleService.RelationshipField> getDistinctSelection(List<SelectedField> fields) {
		final var distinct = new ArrayList<ModuleService.RelationshipField>();
		for (final var field : fields) {
			if (field.getObjectTypeNames().contains("DependencyInformation")) {
				if ("relationship".equals(field.getName())) {
					distinct.add(RelationshipField.TYPE);
				} else if ("direction".equals(field.getName())) {
					distinct.add(RelationshipField.DIRECTION);
				} else if ("target".equals(field.getName()) || "targetId".equals(field.getName()) || "targetName".equals(field.getName())) {
					distinct.add(RelationshipField.SOURCE);
					distinct.add(RelationshipField.DESTINATION);
				} else if ("fromModuleLocation".equals(field.getName())) {
					distinct.add(RelationshipField.SOURCE_LOCATION);
				} else if ("toModuleLocation".equals(field.getName())) {
					distinct.add(RelationshipField.DESTINATION_LOCATION);
				}
			}
		}
		return distinct;
	}

	/**
	 * Query for {@linkplain DependencyInformation dependencies} of a module.
	 * @param projectId the ID of the project that contains the module
	 * @param moduleId the ID of the module that contains the dependencies
	 * @param page the page number
	 * @param size the number of elements 
	 * @param filterObject filtering conditions in object format
	 * @param env GraphQL context
	 * @return the list of dependencies
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public DataFetcherResult<Paged<DependencyInformation>> moduleDependencies(
			@Argument final Long projectId, @Argument final Long moduleId,
			@Argument @Nullable final Integer page, @Argument @Nullable final Integer size,
			@Argument(name = "filterObject") @Nullable final Map<String, Object> filterObject,
			DataFetchingEnvironment env) {
		final var project = EntityId.of(projectId);
		final var module = EntityId.of(moduleId);
		final var filters = new NestedMap(filterObject);
		return DataFetcherResult.<Paged<DependencyInformation>>newResult().data(
			moduleService.findRelationships(Pagination.at(page == null ? 0 : page , size == null ? 0 : size), q -> {
				final var direction = filters.getSubAt("content_direction");
				q.ofProject(project)
					.ofModuleInDirection(module, direction.<String>getOptional("eq").map(RelationshipDirection::valueOf)
						.or(() -> direction.<Collection<String>>getOptional("in").flatMap(RelationshipDirection::inOf))
						.orElse(RelationshipDirection.BOTH))
					.setBaseModule(module)
					.includeModuleDetails(true, true);
				if (! filters.containsKey("content_relationship")) {
					q.withTypes(RelationshipType.DEPENDENCY_TYPES);
				}
				if (filterObject != null) {
					filterObjectService.applyFilterObject(projectId, MiningGraphQLQueries.MODULE_DEPENDENCIES, filterObject, q);
				}
				q.distinct(getDistinctSelection(env.getSelectionSet().getFields()));
				//This sorting is the default, it should be applied if no other sorting is defined
				q.sortByRelationshipDirection(SortDirection.DESCENDING);
				q.sortType(SortDirection.ASCENDING);
				q.sortNid(SortDirection.ASCENDING);
			}).map(relation -> new DependencyInformation(relation))).build();
	}
}
