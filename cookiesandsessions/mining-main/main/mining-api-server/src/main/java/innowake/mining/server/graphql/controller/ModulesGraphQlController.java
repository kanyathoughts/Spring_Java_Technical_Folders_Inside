/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.shared.access.*;
import innowake.mining.shared.entities.*;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.*;
import jline.internal.Log;
import org.apache.commons.text.CaseUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.BatchMapping;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Controller;

import graphql.execution.DataFetcherResult;
import graphql.schema.DataFetchingEnvironment;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.ModulePgDao.ModuleQueryBuilder;
import innowake.mining.data.access.postgres.PgUtil;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.data.datapoints.SortObjectService;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.controller.cfg.ControlFlowSupport;
import innowake.mining.server.graphql.GraphQlUtil;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.server.opensearch.OpenSearchService;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.ModuleService.MetricField;
import innowake.mining.shared.access.ModuleService.ModuleInquiryBuilder;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SortByAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.GraphMLAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AnnotationCategory.DatabaseAnnotationCategory;
import innowake.mining.shared.model.AnnotationCategory.RuleAnnotationCategory;
import innowake.mining.shared.model.dependency.graph.NodeType;
import innowake.mining.shared.service.UserRoleService;

/**
 * Defines data points for {@link ModulePojo}.
 *
 * Implementation had to be moved to a separate class to fix circular dependency
 * on {@link DataPointRegistry}.
 */
@Controller
@Order(2)
public class ModulesGraphQlController {
	
	private static final int MODULE_QUERY_PAGING_SIZE_THRESHOLD = 1_000;

	@Autowired
	private FilterObjectService filterObjectService;
	
	@Autowired
	private SortObjectService sortObjectService;
	
	@Autowired
	private DataDictionaryService dataDictionaryService;
	
	@Autowired
	private TaxonomyService taxonomyService;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private ProjectService projectService;
	
	@Autowired
	private UserRoleService userRoleService;

	@Autowired
	private AstService astService;

	@Autowired
	private FieldInfoService fieldInfoService;

	@Autowired
	private AnnotationService annotationService;
	
	@Autowired
	private DataPointRegistry dpRegistry;

	@Autowired
	private FunctionalBlockService functionalBlockService;
	
	/**
	 * Query for {@linkplain ModulePojo modules} of a project.
	 *
	 * @param projectId the ID of the project that contains the annotation
	 * @param page the page number
	 * @param size the number of elements
	 * @param sortObject sort conditions in object format
	 * @param filterObject filtering conditions in object format
	 * @param env the DataFetchingEnvironment - injected automatically
	 * @param useOpenSearch flag to use {@link OpenSearchService}
	 * @return the list of annotations
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public DataFetcherResult<Paged<ModulePojo>> modules(@Argument final Long projectId,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			@Argument(name = "sortObject") @Nullable final List<Map<String, String>> sortObject,
			@Argument(name = "filterObject") @Nullable final Map<String, Object> filterObject,
			@Argument @Nullable final Boolean useOpenSearch,
			final DataFetchingEnvironment env) {
		
		final int intPage = Optional.ofNullable(page).orElse(0).intValue();
		final int intSize = Optional.ofNullable(size).orElse(0).intValue();
		
		final Paged<ModulePojo> result;
		/* For better performance when paging is enabled and the page size is less than the the threshold, we first fetch the module IDs
		 * and then perform the module query with the modules IDs as filter */
		if (0 < intSize && intSize < MODULE_QUERY_PAGING_SIZE_THRESHOLD) {
			final Paged<EntityId> ids = moduleService.findModuleIds(Pagination.at(intPage, intSize),
																	createQueryBuilder(projectId, sortObject, filterObject, env, null));
			final var modules = ids.getContent().isEmpty() ?
													List.<ModulePojo>of() : 
													 moduleService.findModules(createQueryBuilder(projectId, sortObject, null, env, ids.getContent()));
			result = new Paged<>(modules, ids.getLimit(), ids.getOffset(), ids.getTotalElements(), ids.getFirstElement(), ids.getLastElement());
		} else {
			result = moduleService.findModules(Pagination.at(intPage, intSize), createQueryBuilder(projectId, sortObject, filterObject, env, null));
		}
		
		return DataFetcherResult.<Paged<ModulePojo>>newResult()
				.data(result)
				.localContext(new ControllerLocalContext(projectId, userRoleService.getProjectIds(), userRoleService.getClientAdminIds(),
						userRoleService.isAdmin()))
				.build();
	}

	private BuildingConsumer<ModuleInquiryBuilder> createQueryBuilder(final Long projectId, @Nullable final List<Map<String, String>> sortObject,
			@Nullable final Map<String, Object> filterObject, final DataFetchingEnvironment env, @Nullable final List<EntityId> moduleIds) {
		return q -> {
			q.ofProject(EntityId.of(projectId));
			GraphQlUtil.applyDynamicSelection(dpRegistry, projectId, MiningEnitityNames.MODULE, new String[] { "content" }, env, q, Stream.concat(
					sortObject != null ? sortObject.stream().flatMap(s -> s.keySet().stream()) : Stream.empty(),
					filterObject != null ? filterObject.keySet().stream() : Stream.empty()).map(s -> s.split("_")));
			if (env.getSelectionSet().contains("content/content")) {
				q.includeContent(true);
			}
			/* When moduleIds is set then we can filter by those ids only. */
			if (moduleIds != null) {
				q.byIds(moduleIds);
			} else if (filterObject != null) {
				filterObjectService.applyFilterObject(projectId, MiningGraphQLQueries.MODULES, filterObject, q);
			}
			if (sortObject != null && ! sortObject.isEmpty()) {
				sortObjectService.applySortObject(projectId, MiningGraphQLQueries.MODULES, sortObject, q);
			} else {
				/* the default sorting */
				q.sortName(SortDirection.ASCENDING);
			}
		};
	}

	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	public Long projectId(final ModulePojo module) {
		return projectService.getNid(module.getProject());
	}

	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	public int dataDictionaryEntryCount(final ModulePojo module) {
		return dataDictionaryService.count(q -> q.ofModule(module.identity())).intValue();
	}

	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	public String contentHash(final ModulePojo module) {
		return module.getContentHash().map(BinaryValue::toString).orElse(null);
	}

	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	@MiningDataPoint(displayName = "In Codebase", description = "Whether the source file of this Module is present in the codebase " +
			"or Module is contained within another Module.")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
	})
	@Usage(Usages.METRICS_CHART_DETAILS_MODULE)
	@Usage(value = Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
	})
	public boolean inCodebase(final ModulePojo module) {
		if (module.isSourceCodeAvailable()) {
			return true;
		}
		return module.getParent().map(parent -> moduleService.getModule(parent).isSourceCodeAvailable()).orElse(false);
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	@MiningDataPoint(displayName = "Path", description = "Path of the file containing the Module")
	@Usage(value = Usages.GRAPHQL_QUERY_MODULES, attributes = {
			@UsageAttribute(key = SortByAttributes.SQL_FRAGMENT_ORDER_BY, value = "path")
	})
	@Usage(value = Usages.GRAPHQL_QUERY_DNA_MODULE_CLUSTER, attributes = {
			@UsageAttribute(key = SortByAttributes.SQL_FRAGMENT_ORDER_BY, value = "path")
	})
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
	})
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
	})
	@Usage(Usages.MINING_UI_MODULE_DETAILS)
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT, attributes = {
			@UsageAttribute(key = GraphMLAttributes.SQL_FRAGMENT, value = "path")
	})
	@Usage(Usages.METRICS_CHART_DETAILS_MODULE)
	@Usage(Usages.SORT_BY)
	@Usage(value = Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "1")
	})
	public @Nullable String path(final ModulePojo module) {
		return module.getPath().orElse(module.getParentPath().orElse(null));
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	@MiningDataPoint(displayName = "Taxonomies", description = "Taxonomies of the Module")
	public List<TaxonomyPojo> taxonomies(final ModulePojo module) {
		return taxonomyService.find(q -> q.ofModule(module.identity()));
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.MODULE_RELATIONSHIP)
	public static class ModuleRelation extends ModuleRelationshipBasePojo {
		private final RelationshipDirection direction;
		private final ModulePojo module;
		
		public ModuleRelation(final ModuleRelationshipBasePojo relation, final ModulePojo module) {
			super(relation.getId(), relation.getSrcModule(), relation.getSrcLocation().orElse(null), relation.getDstModule(),
					relation.getDstLocation().orElse(null), relation.getRelationship(), relation.getProperties().orElse(null));
			this.direction = relation.getDstModule().equals(module.getUid()) ? RelationshipDirection.OUT : RelationshipDirection.IN;
			this.module = module;
		}

		public RelationshipType getType() {
			// Intentional duplicate getter method, as we generate the GraphQL API from it
			return getRelationship();
		}

		public RelationshipDirection getDirection() {
			return direction;
		}
		
		public ModulePojo getModule() {
			return module;
		}
	}

	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	@MiningDataPoint(displayName = "Missing Dependencies", description = "List of all Dependency modules that are missing in the codebase")
	public List<ModulePojo> missingDependencies(final ModulePojo module) {
		return moduleService.findModules(q -> q.withIdentified(false).withSourceRelationshipsFrom(r -> r.byUid(module.getUid())));
	}
	
	@SuppressWarnings("unchecked")
	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	@MiningDataPoint(displayName = "Dependencies", description = "List of Dependencies of the Module")
	public List<ModuleRelation> dependencies(final ModulePojo module, final DataFetchingEnvironment env,
			@Argument final @Nullable RelationshipDirection direction,
			@Argument final List<RelationshipType> type,
			@Argument(name = "properties") @MiningDataPoint(scalarType = ScalarType.JSON) @Nullable final Map<String, Object> properties,
			@Argument(name = "filterObject") @MiningDataPoint(referenceTypeName = "FilterObject_modules") @Nullable final Map<String, Object> filterObject) {
		return moduleService.findModules(q -> {
				q.withRelationship(module.identity(), 
						direction == null ? RelationshipDirection.BOTH : direction,
						type.isEmpty() ? RelationshipType.DEPENDENCY_TYPES : type);
				if (properties != null) {
					final Set<String> requiredProperties = new HashSet<>();
					final Set<String> filteredProperties = new HashSet<>();
					properties.forEach((k, v) -> {
						if (v == null) {
							filteredProperties.add(k);
						} else if (v instanceof List) {
							final var values = (List<String>) v;
							if (values.isEmpty()) {
								requiredProperties.add(k);
							} else {
								q.withRelationshipPropertyAny(k, (List<String>) v);
							}
						} else {
							q.withRelationshipProperty(k, (String) v, false);
						}
					});
					if (! requiredProperties.isEmpty()) {
						q.withRelationshipProperties(requiredProperties, true);
					}
					if (! filteredProperties.isEmpty()) {
						q.withRelationshipProperties(filteredProperties, false);
					}
				}
				q.includeContent(env.getSelectionSet().contains("module/content"));
				if (filterObject != null) {
					filterObjectService.applyFilterObject(module.getProjectNid(), MiningGraphQLQueries.MODULES, filterObject, q);
				}
			}).stream()
				.flatMap(m -> m.dynamicRelations().map(relations -> relations.stream().map(relation -> new ModuleRelation(relation, m))).orElse(null))
				.collect(Collectors.toList());
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	@MiningDataPoint(displayName = "Complexity Level", description = "Complexity Ranking of the Module")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Metrics")
	})
	@Usage(Usages.METRICS_CHART_DETAILS_MODULE)
	public ComplexityLevel complexityLevel(final ModulePojo module) {
		final var sourceMetrics = module.getSourceMetrics();
		if (sourceMetrics.isEmpty()) {
			return ComplexityLevel.UNKNOWN;
		}
		final Integer complexity = sourceMetrics.get().getComplexityMcCabe();
		if (complexity == null) {
			return ComplexityLevel.UNKNOWN;
		} else if (complexity.intValue() <= 10) {
			return ComplexityLevel.LOW;
		} else if (complexity.intValue() <= 20) {
			return ComplexityLevel.MEDIUM;
		} else if (complexity.intValue() <= 50) {
			return ComplexityLevel.HIGH;
		}
		return ComplexityLevel.VERY_HIGH;
	}

	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	public List<FieldInfoPojo> fieldInfos(final ModulePojo module, final DataFetchingEnvironment env) {
		final ControllerLocalContext context = env.getLocalContext();
		return fieldInfoService.find(q -> q.ofProject(EntityId.of(context.getQueriedProjectId())).ofModule(assertNotNull(module.identity())));
	}

	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	@MiningDataPoint(displayName = "Number of Errors", description = "Number of errors for the Module")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Errors"),
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_NUMBER)
	})
	@Usage(value = Usages.GRAPHQL_QUERY_MODULES)
	@Usage(Usages.SORT_BY)
	public int errorCount(final ModulePojo module) {
		return module.getErrors();
	}

	@SchemaMapping(typeName = MiningEnitityNames.MODULE)
	public List<FunctionalBlockPojo> reachabilityBlocks(final ModulePojo module) {
		return functionalBlockService.find(q -> q
				.withType(FunctionalBlockType.RA_TOP_DOWN)
				.withResolvedModulePart(module.identity()).sortName(SortDirection.ASCENDING));
	}

	@BatchMapping(typeName = MiningEnitityNames.MODULE)
	@MiningDataPoint(displayName = "Percentage of annotated Statements", description = "Displays the amount of Statements covered by Annotations")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Annotations"),
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
	})
	@Usage(Usages.SORT_BY)
	public Map<ModulePojo, Double> numberOfAnnotatedStatementNodes(final List<ModulePojo> modules) {
		if (modules.isEmpty()) {
			return Collections.emptyMap();
		}

		final HashMap<ModulePojo, Double> resultsMap = new HashMap<>();

		modules.stream()
				.forEach(m -> {
					try {
						resultsMap.put(m, Double.valueOf(calculateAnnotatedCode(m)));
					} catch (final Exception e) {
						Log.error("Unable to calculate code coverage for module " + m.getName() + " due to " + e);
					}
				});

		return resultsMap;
	}

	private double calculateAnnotatedCode(final ModulePojo m) {

		/* Retrieve all AstNode locations, merge them and sort them */
		final List<AstNodePojo> statementAstNodes = astService.find(q -> q.ofModule(m.identity()).withSuperTypes("Statement"));
		final List<ModuleLocation> locationsAst = statementAstNodes.stream().map(ast -> {
			final AstNodeLocation location = ast.getLocation();
			if (location != null) {
				final Optional<Integer> offset = location.getRootRelativeOffset();
				final Optional<Integer> length = location.getRootRelativeLength();
				if (offset.isPresent() && length.isPresent()) {
					return new ModuleLocation(offset.get(), length.get());
				} else {
					throw new IllegalStateException("No AstNode location available to calculate AnnotationCoverage");
				}
			} else {
				throw new IllegalStateException("No AstNode location available to calculate AnnotationCoverage");
			}
		}).sorted(Comparator.comparing(ModuleLocation::getOffset)).toList();
		final List<ModuleLocation.MergedModuleLocation> mergedLocationsAst = ModuleLocation.merge(locationsAst);

		/* Retrieve all Annotation locations, merge them and sort them */
		final List<AnnotationPojo> moduleAnnotations = annotationService.find(q -> q.ofModule(m.identity()));
		final List<ModuleLocation> locationsAnno = moduleAnnotations.stream().map(mod -> mod.getLocation().orElse(null))
				.sorted(Comparator.comparing(moduleLocation -> moduleLocation != null ? moduleLocation.getOffset() : 0)).toList();
		final List<ModuleLocation.MergedModuleLocation> mergedLocationsAnno = ModuleLocation.merge(locationsAnno);

		final float amountOfStatements = statementAstNodes.size();
		if (amountOfStatements == 0) {
			return 0;
		} else {
			final float result = countAnnoLines(mergedLocationsAst, mergedLocationsAnno) / amountOfStatements;
			return Math.round(result * Math.pow(10, 2)) / Math.pow(10, 2);
		}
	}

	private Integer countAnnoLines(final List<ModuleLocation.MergedModuleLocation> mergedLocationsAst, final List<ModuleLocation.MergedModuleLocation> mergedLocationsAnno) {
		int coveredStatementNodes = 0;

		for (final ModuleLocation.MergedModuleLocation astLoc : mergedLocationsAst) {
			for (final ModuleLocation.MergedModuleLocation annoLoc : mergedLocationsAnno) {
				if (locationsOverlap(astLoc, annoLoc)) {
					coveredStatementNodes += astLoc.getMergedLocations();
					break;
				}
			}
		}
		return coveredStatementNodes;
	}

	private boolean locationsOverlap(final ModuleLocation.MergedModuleLocation astLoc, final ModuleLocation.MergedModuleLocation annoLoc) {
		return astLoc.getOffset() <= annoLoc.getEndOffset() && annoLoc.getOffset() <= astLoc.getEndOffset();
	}

	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public Paged<ErrorMarkerPojo> errorMarkers(@Argument final Long projectId,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			@Argument(name = "filterObject") @Nullable final Map<String, Object> filterObject,
			final DataFetchingEnvironment env) {
		return moduleService.findErrorMarkers(Pagination.at(Optional.ofNullable(page).orElse(0),
				Optional.ofNullable(size).orElse(0)), q -> {
			q.ofProject(EntityId.of(projectId));
			if (filterObject != null) {
				filterObjectService.applyFilterObject(projectId, "errorMarkers", filterObject, q);
			}
		});
	}

	@BatchMapping(typeName = MiningEnitityNames.ERROR_MARKER)
	@MiningDataPoint(displayName = "Source Code", description = "The Source Code of the error marker")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "3")
	})
	@Usage(value = Usages.VIEW_MODE, attributes = {
			@UsageAttribute(key = ViewModeAttributes.TOGETHER_WITH, value = "location.rootRelativeOffset")
	})
	public Map<ErrorMarkerPojo, String> errorText(final List<ErrorMarkerPojo> errorMarkerPojo) {
		final Map<EntityId, String> modulePojoMap = moduleService.getContents(errorMarkerPojo.stream().map(ErrorMarkerPojo::getModule)
				.collect(Collectors.toSet()));
		return errorMarkerPojo.parallelStream()
				.collect(Collectors.toUnmodifiableMap(Function.identity(), e -> {
					final AstNodeLocation location = e.getLocation();
					if (location != null && ! location.isLocationInsideIncludeComponent().orElse(true)
					&& location.getRootRelativeOffset().isPresent() && location.getRootRelativeLength().isPresent()) {
						final String content = modulePojoMap.get(e.getModule());
						if (content != null) {
							final int startOffset = location.getRootRelativeOffset().get();
							final int length = location.getRootRelativeLength().get();
							final int endOffset = content.length() >= startOffset + length + 1 ? startOffset + length + 1 :  startOffset + length;
							final String errorText = content.substring(startOffset, endOffset);
							return length > 1 ? errorText.trim() : errorText;
						}
					}
					return "No Data";
				}));
	}

	@Component
	@Order(2)
	public static class DataPointSource implements MiningDataPointSource {

		@Override
		public void provideDataPoints(final MiningDataPointBuilder builder) {
			/* add ModulePojo to schema */
			builder.defineType(MiningEnitityNames.MODULE)
					.representedBy(ModulePojo.class)
					.withDefaultProperties()
				.add();

			builder.defineType(MiningEnitityNames.MODULE_RELATIONSHIP)
				.representedBy(ModuleRelationshipPojo.class)
				.withDefaultProperties()
				.add();

			builder.defineType(MiningEnitityNames.MODULE_RELATIONSHIP_BASE)
				.representedBy(ModuleRelationshipBasePojo.class)
				.withDefaultProperties()
				.add();

			builder.defineType(MiningEnitityNames.ANNOTATION_TYPE)
					.representedBy(AnnotationType.class)
					.withDefaultProperties()
					.add();

			builder.defineType(MiningEnitityNames.WORKING_STATE)
					.representedBy(WorkingState.class)
					.withDefaultProperties()
					.add();

			/* define queries and additional custom data points */
			builder.defineDataPointsFromSchemaMappingAnnotations(ModulesGraphQlController.class);


			builder.extend(MiningEnitityNames.MODULE_RELATIONSHIP, "properties")
				.type(MiningDataPointDefinition.ScalarType.JSON)
				.add();

			builder.extend(MiningEnitityNames.MODULE_RELATIONSHIP_BASE, "properties")
				.type(MiningDataPointDefinition.ScalarType.JSON)
				.add();

			builder.extend(MiningEnitityNames.MODULE, "uid")
					.withDisplayName("Module UID")
					.withUsage(Usages.MINING_UI_GRAPHML_EXPORT)
					.add();
			builder.defineType(MiningEnitityNames.ERROR_MARKER)
					.representedBy(ErrorMarkerPojo.class)
					.withDefaultProperties()
					.add();

			/* just define the type and overwrite the existing getId(): Long of MiningPojo. */
			builder.extend(MiningEnitityNames.MODULE, "id")
				.withFiltering(MiningGraphQLQueries.MODULES, filters -> {
					filters.eq(ModuleService.ModuleInquiryBuilder::byNid);
					filters.in(ModuleService.ModuleInquiryBuilder::byNids);
				})
				.withSorting(MiningGraphQLQueries.MODULES, ModuleService.ModuleInquiryBuilder::sortId)
				.withDisplayName("Module Id")
				.withDescription("The unique id of the Module")
				.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsage(Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsage(Usages.MINING_UI_GRAPHML_EXPORT)
				.add();
			
			builder.extend(MiningEnitityNames.MODULE, "linkHash")
					.withDisplayName("Link Hash")
					.withDescription("Unique link hash of the Module")
				.add();

			builder.extend(MiningEnitityNames.MODULE, "errorCount")
				.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.<ModuleQueryBuilder, Integer>eq((q, value) -> q.withErrors(FilterOperators.OPERATOR_EQ, value));
					f.<ModuleQueryBuilder, Integer>gte((q, value) -> q.withErrors(FilterOperators.OPERATOR_GTE, value));
					f.<ModuleQueryBuilder, Integer>lte((q, value) -> q.withErrors(FilterOperators.OPERATOR_LTE, value));
				})
				.withSorting(MiningGraphQLQueries.MODULES, ModuleService.ModuleInquiryBuilder::sortErrorCount)
				.add();

			builder.extend(MiningEnitityNames.MODULE, "name")
				.withDisplayName("Module Name").withDescription("The name of the Module")
				.withUsage(Usages.SORT_BY)
				.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
				.withUsage(Usages.VIEW_MODE)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LINK_TEMPLATE, "/project-${$projectId}/module-${linkHash}/details/overview")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "linkHash")
				/* Modules table */
				.withFiltering(MiningGraphQLQueries.MODULES, filters -> {
					filters.<ModuleService.ModuleInquiryBuilder, String>eq((q, v) -> q.withName(PgUtil.pgPattern(v.trim()) + '%', true));
					filters.<ModuleService.ModuleInquiryBuilder, List<String>>in((q, v) -> q.withNames(v, true));
				})
				.withSorting(MiningGraphQLQueries.MODULES, ModuleService.ModuleInquiryBuilder::sortName)
				.withUsage(Usages.MINING_UI_MODULES_TABLE)
				.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "0")
				/* Annotations table */
	//			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, filters -> filters.eq(AnnotationService.AnnotationInquiryBuilder::withModuleName))
				.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationService.AnnotationInquiryBuilder::sortByModuleName)
				.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
				.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "0")
				/* Data Dictionary table */
				.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortModuleName)
				/* DNA table */
	//			.withFiltering(MiningGraphQLQueries.DNA_MODULES_IN_CLUSTER, filters -> 
	//				filters.<ModuleService.ModuleInquiryBuilder, String>eq((q, v) -> q.withName(v, true)))
				.withSorting(MiningGraphQLQueries.DNA_MODULES_IN_CLUSTER, ModuleService.ModuleInquiryBuilder::sortName)
				.withUsage(Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE)
				.withUsageAttribute(Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "0")
				.withUsage(Usages.MINING_UI_GRAPHML_EXPORT)
				.withUsage(Usages.METRICS_CHART_DETAILS_MODULE)
				.withUsage(Usages.METRICS_CHART_DETAILS_SQL)
				/* Dependencies Table */
				.withUsageAttribute(Usages.MINING_UI_DEPENDENCIES_TABLE, TableAttributes.CATEGORY, "Base Data")
				.add();
			/*
			@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT, attributes = {
					@UsageAttribute(key = GraphMLAttributes.SQL_FRAGMENT, value = "name")
			})
			*/
			
			builder.extend(MiningEnitityNames.MODULE, "requiresReview")
				.withUsage(Usages.SORT_BY)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
				.withFiltering(MiningGraphQLQueries.MODULES, filters -> {
					filters.<ModuleService.ModuleInquiryBuilder, Object>isTrue((q, v) -> q.withRequiresReview(true));
					filters.<ModuleService.ModuleInquiryBuilder, Object>isFalse((q, v) -> q.withRequiresReview(false));
					filters.<ModuleService.ModuleInquiryBuilder, Boolean>eq((q, v) -> q.withRequiresReview(v));
					filters.<ModuleService.ModuleInquiryBuilder, List<Boolean>>in((q, v) -> {
						if (v.contains(true) && ! v.contains(false)) {
							q.withRequiresReview(true);
						} else if (v.contains(false) && ! v.contains(true)) {
							q.withRequiresReview(false);
						}
					});
				})
				.withSorting(MiningGraphQLQueries.MODULES, ModuleService.ModuleInquiryBuilder::sortRequiresReview)
				.add();
			
			builder.extend(MiningEnitityNames.MODULE, "modifiedDate")
				.withSorting(MiningGraphQLQueries.MODULES, ModuleService.ModuleInquiryBuilder::sortModified)
				.add();
			builder.extend(MiningEnitityNames.MODULE, "metricsDate")
				.withSorting(MiningGraphQLQueries.MODULES, ModuleService.ModuleInquiryBuilder::sortMetricsDate)
				.add();
			
			builder.extend(MiningEnitityNames.SOURCE_METRICS, "physicalLines")
				.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.<ModuleQueryBuilder, Integer>eq((q, value) -> q.withPhysicalLines(FilterOperators.OPERATOR_EQ, value));
					f.<ModuleQueryBuilder, Integer>gte((q, value) -> q.withPhysicalLines(FilterOperators.OPERATOR_GTE, value));
					f.<ModuleQueryBuilder, Integer>lte((q, value) -> q.withPhysicalLines(FilterOperators.OPERATOR_LTE, value));
				})
				.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortMetric(MetricField.PHYSICAL_LINES, d))
				.add();
			builder.extend(MiningEnitityNames.SOURCE_METRICS, "codeLines")
				.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.<ModuleQueryBuilder, Integer>eq((q, value) -> q.withCodeLines(FilterOperators.OPERATOR_EQ, value));
					f.<ModuleQueryBuilder, Integer>gte((q, value) -> q.withCodeLines(FilterOperators.OPERATOR_GTE, value));
					f.<ModuleQueryBuilder, Integer>lte((q, value) -> q.withCodeLines(FilterOperators.OPERATOR_LTE, value));
				})
				.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortMetric(MetricField.CODE_LINES, d))
				.add();
			builder.extend(MiningEnitityNames.SOURCE_METRICS, "commentLines")
				.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.<ModuleQueryBuilder, Integer>eq((q, value) -> q.withCommentLines(FilterOperators.OPERATOR_EQ, value));
					f.<ModuleQueryBuilder, Integer>gte((q, value) -> q.withCommentLines(FilterOperators.OPERATOR_GTE, value));
					f.<ModuleQueryBuilder, Integer>lte((q, value) -> q.withCommentLines(FilterOperators.OPERATOR_LTE, value));
				})
				.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortMetric(MetricField.COMMENT_LINES, d))
				.add();
			builder.extend(MiningEnitityNames.SOURCE_METRICS, "complexityMcCabe")
				.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.<ModuleQueryBuilder, Integer>eq((q, value) -> q.withComplexity(FilterOperators.OPERATOR_EQ, value));
					f.<ModuleQueryBuilder, Integer>gte((q, value) -> q.withComplexity(FilterOperators.OPERATOR_GTE, value));
					f.<ModuleQueryBuilder, Integer>gt((q, value) -> q.withComplexity(FilterOperators.OPERATOR_GT, value));
					f.<ModuleQueryBuilder, Integer>lte((q, value) -> q.withComplexity(FilterOperators.OPERATOR_LTE, value));
					f.<ModuleQueryBuilder, Integer>lt((q, value) -> q.withComplexity(FilterOperators.OPERATOR_LT, value));
				})
				.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortMetric(MetricField.COMPLEXITY_MCCABE, d))
				.add();
			builder.extend(MiningEnitityNames.SOURCE_METRICS, "deadCodeLines")
				.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.<ModuleQueryBuilder, Integer>eq((q, value) -> q.withDeadCodeLines(FilterOperators.OPERATOR_EQ, value));
					f.<ModuleQueryBuilder, Integer>gte((q, value) -> q.withDeadCodeLines(FilterOperators.OPERATOR_GTE, value));
					f.<ModuleQueryBuilder, Integer>lte((q, value) -> q.withDeadCodeLines(FilterOperators.OPERATOR_LTE, value));
				})
				.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortMetric(MetricField.DEAD_CODE_LINES, d))
				.add();
			
			builder.extend(MiningEnitityNames.MODULE, "creator")
				.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.eq(ModuleService.ModuleInquiryBuilder::withCreator);
					f.in(ModuleService.ModuleInquiryBuilder::withCreators);
				})
				.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortCreator(d))
					.withDisplayName("Creator")
					.withDescription("How the Module was created")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Base Data")
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "CREATOR")
					.withUsage(Usages.MINING_UI_MODULE_DETAILS)
					.withUsage(Usages.MINING_UI_GRAPHML_EXPORT)
					.withUsage(Usages.SORT_BY)
				.add();
			
			builder.extend(MiningEnitityNames.MODULE, "identification")
				.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.<ModuleService.ModuleInquiryBuilder, Object>eq((inquiryBuilder, filterValue) -> {
						if (filterValue instanceof Boolean) {
							inquiryBuilder.withIdentified((Boolean) filterValue);
						} else if (Identification.IDENTIFIED == filterValue) {
							inquiryBuilder.withIdentified(true);
						} else if (Identification.MISSING == filterValue) {
							inquiryBuilder.withIdentified(false);
						} else {
							throw new UnsupportedOperationException("Unsupported filter value for Module.identification: " + filterValue);
						}
					});
					f.<ModuleService.ModuleInquiryBuilder, List<Boolean>>in((q, v) -> {
						if (v.contains(true) && ! v.contains(false)) {
							q.withIdentified(true);
						} else if (v.contains(false) && ! v.contains(true)) {
							q.withIdentified(false);
						}
					});
				})
					.withDisplayName("File Identified / File Missing")
					.withDescription("Whether the Module is included in the code base or missing")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Base Data")
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "IDENTIFICATION")
					.withUsage(Usages.MINING_UI_GRAPHML_EXPORT)
					.withUsage(Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE, TableAttributes.CATEGORY, "Base Data")
					.add();
	
			builder.extend(MiningEnitityNames.MODULE, "technology")
					.withDisplayName("Technology")
					.withFiltering(MiningGraphQLQueries.MODULES, f -> {
						f.<ModuleQueryBuilder, Technology>eq((q, value) -> q.withTechnology(FilterOperators.OPERATOR_EQ, value));
						f.<ModuleQueryBuilder, Technology>notEq((q, value) -> q.withTechnology(FilterOperators.OPERATOR_NOT_EQ, value));
						f.<ModuleQueryBuilder, List<Technology>>in((q, value) -> q.withTechnology(FilterOperators.OPERATOR_IN, value));
						f.<ModuleQueryBuilder, List<Technology>>notIn((q, value) -> q.withTechnology(FilterOperators.OPERATOR_NOT_IN, value));
					})
					.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortTechnology(d))
					.add();
	
			builder.extend(MiningEnitityNames.MODULE, "type")
					.withDisplayName("Type")
					.withFiltering(MiningGraphQLQueries.MODULES, f -> {
						f.eq(ModuleService.ModuleInquiryBuilder::withType);
						f.in(ModuleService.ModuleInquiryBuilder::withTypes);
					})
					.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortType(d))
					.add();

			builder.extend(MiningEnitityNames.MODULE, "storage")
					.withDisplayName("Storage")
					.withFiltering(MiningGraphQLQueries.MODULES, f -> {
						f.eq(ModuleService.ModuleInquiryBuilder::withStorage);
					})
					.add();

			builder.extend(MiningEnitityNames.MODULE, "representation")
					.withDisplayName("Representation")
					.withFiltering(MiningGraphQLQueries.MODULES, f -> f.eq(ModuleService.ModuleInquiryBuilder::withRepresentation))
					.add();

			builder.defineAlias(MiningEnitityNames.MODULE, "missingDependencyNames")
					.forDataPoint("missingDependencies")
					.withSubSelection("name")
					.withDisplayName("Missing Dependencies")
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "missingDependencyNames.linkHash")
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK)
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LINK_TEMPLATE, "/project-${$projectId}/module-${linkHash}/details/overview")
					.withDescription("List of all Dependency Module names that are missing")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Dependencies")
					.add();

			builder.defineAlias(MiningEnitityNames.MODULE, "inboundDependencyNames")
					.forDataPoint("dependencies")
					.withSubSelection("module.name")
					.withParameter("direction", RelationshipDirection.IN)
					.withDisplayName("Inbound Dependencies")
					.withDescription("List of all Modules that directly depend on this Module")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Dependencies")
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "inboundDependencyNames.module.linkHash")
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK)
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LINK_TEMPLATE, "/project-${$projectId}/module-${linkHash}/details/overview")
				.add();
			builder.defineAlias(MiningEnitityNames.MODULE, "outboundDependencyNames")
					.forDataPoint("dependencies")
					.withSubSelection("module.name")
					.withParameter("direction", RelationshipDirection.OUT)
					.withDisplayName("Outbound Dependencies")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Dependencies")
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "outboundDependencyNames.module.linkHash")
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK)
					.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LINK_TEMPLATE, "/project-${$projectId}/module-${linkHash}/details/overview")
					.add();
			
			/* aliases added for Taxonomy assignment export (for backwards-compatibility with existing column names in the export file */
			builder.defineAlias(MiningEnitityNames.MODULE, "nameForTaxonomyExport")
					.forDataPoint("name")
					.withDisplayName("Module Name")
					.withUsage(Usages.TAXONOMY_ASSIGNMENTS_EXPORT)
					.withUsageAttribute(Usages.TAXONOMY_ASSIGNMENTS_EXPORT, TableAttributes.DEFAULT_COLUMN_INDEX, "0")
				.add();
			builder.defineAlias(MiningEnitityNames.MODULE, "pathForTaxonomyExport")
					.forDataPoint("path")
					.withDisplayName("Module Path")
					.withUsage(Usages.TAXONOMY_ASSIGNMENTS_EXPORT)
					.withUsageAttribute(Usages.TAXONOMY_ASSIGNMENTS_EXPORT, TableAttributes.DEFAULT_COLUMN_INDEX, "1")
				.add();
			builder.defineAlias(MiningEnitityNames.MODULE, "technologyForTaxonomyExport")
					.forDataPoint("technology")
					.withDisplayName("Technology")
					.withUsage(Usages.TAXONOMY_ASSIGNMENTS_EXPORT)
					.withUsageAttribute(Usages.TAXONOMY_ASSIGNMENTS_EXPORT, TableAttributes.DEFAULT_COLUMN_INDEX, "2")
				.add();
			builder.defineAlias(MiningEnitityNames.MODULE, "typeForTaxonomyExport")
					.forDataPoint("type")
					.withDisplayName("Type")
					.withUsage(Usages.TAXONOMY_ASSIGNMENTS_EXPORT)
					.withUsageAttribute(Usages.TAXONOMY_ASSIGNMENTS_EXPORT, TableAttributes.DEFAULT_COLUMN_INDEX, "3")
				.add();

			/* TODO For elusive reasons the static set of "supported types" used to be part of the Module class and therefore was also available via the modules() query.
			 * 		Despite this making little sense there are numerous usages of it in the UI, so at the moment this must still be provided for compatibility.
			 * 		It should however be refactored and removed as soon as possible.
			 * 		The correct way is to get this information from the getSupportedModuleTypes endpoint of the ControlFlowController once and cache it. 
			 */
			final var actuallySupportedTypes = ControlFlowSupport.getActuallySupportedTypes();
			builder.defineDataPoint(MiningEnitityNames.MODULE, "actuallySupported")
					.withDisplayName("Type-Technology is Supported")
					.withDescription("If the module type-technology combination is supported or not.")
					.type(ScalarType.BOOLEAN)
					.withCustomFetch(env -> {
						final ModulePojo pojo = env.<ModulePojo>getSource();
						final var combination = NodeType.of(pojo.getTechnology(), pojo.getType());
						return actuallySupportedTypes.contains(combination);
					})
					.add();

			builder.defineDataPoint(MiningEnitityNames.MODULE, "taxonomyCount")
					.withDisplayName("Number of assigned taxonomies")
					.withDescription("Number of all Taxonomies of the Module")
					.withUsage(Usages.GRAPHQL_QUERY_MODULES)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Taxonomies")
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_NUMBER)
					.type(ScalarType.LONG)
					.<ModuleService.ModuleInquiryBuilder>withDynamicFieldBuilder(MiningEnitityNames.MODULE,
							(alias, args, q) -> q.includeTaxonomyCount(alias, PgUtil.mapNullable((String) args.get("type"), UUID::fromString)))
					.withCustomFetch(GraphQlUtil.fetchDynamicProperty())
					.withOptionalParameter("type", ScalarType.STRING)
					.withFiltering(MiningGraphQLQueries.MODULES, f -> {
						f.<ModuleService.ModuleInquiryBuilder, Long>eq((q, v) -> q.filterBy("taxonomyCount", Comperator.EQUAL, v));
						f.<ModuleService.ModuleInquiryBuilder, Long>gte((q, v) -> q.filterBy("taxonomyCount", Comperator.GREATER_OR_EQUAL, v));
						f.<ModuleService.ModuleInquiryBuilder, Long>lte((q, v) -> q.filterBy("taxonomyCount", Comperator.LESSER_OR_EQUAL, v));
					})
					.add();
			/* Number of Annotations */
			buildAnnotationCount(builder, MiningEnitityNames.MODULE, "annotationCount", null, null, null)
					.withDisplayName("Number of Annotations")
					.withDescription("Number of all Annotations of a Module")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.add();

			builder.extend(MiningEnitityNames.TAXONOMY, "id")
					.type(EntityId.class)
					.withFiltering(MiningGraphQLQueries.MODULES, f -> {
						f.<ModuleQueryBuilder, EntityId>eq((q, value) -> q.withTaxonomy(FilterOperators.OPERATOR_EQ, value));
						f.<ModuleQueryBuilder, EntityId>notEq((q, value) -> q.withTaxonomy(FilterOperators.OPERATOR_NOT_EQ, value));
						f.<ModuleQueryBuilder, List<EntityId>>in((q, value) -> q.withTaxonomy(FilterOperators.OPERATOR_IN, value));
						f.<ModuleQueryBuilder, List<EntityId>>notIn((q, value) -> q.withTaxonomy(FilterOperators.OPERATOR_NOT_IN, value));
					})
					.add();

			builder.extend(MiningEnitityNames.TAXONOMY, "name")
					.withFiltering(MiningGraphQLQueries.MODULES, f -> {
						f.<ModuleQueryBuilder, String>eq((q, value) -> q.withTaxonomyName(FilterOperators.OPERATOR_EQ, value));
						f.<ModuleQueryBuilder, String>notEq((q, value) -> q.withTaxonomyName(FilterOperators.OPERATOR_NOT_EQ, value));
						f.<ModuleQueryBuilder, List<String>>in((q, value) -> q.withTaxonomyName(FilterOperators.OPERATOR_IN, value));
						f.<ModuleQueryBuilder, List<String>>notIn((q, value) -> q.withTaxonomyName(FilterOperators.OPERATOR_NOT_IN, value));
					})
					.add();

			/* dependency counts */
			buildDependencyCount(builder, MiningEnitityNames.MODULE, "dependencyCount", null, null, true)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Dependencies")
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.FILTER_MODE,
							SearchFilterAttributes.FILTER_MODE_NUMBER)
					.withDisplayName("Number of dependencies")
					.withDescription("Number of all direct incoming or outbound dependencies of the Module")
				.add();
			
			buildDependencyCount(builder, MiningEnitityNames.MODULE, "inboundDependencyCount", RelationshipDirection.IN, null, false)
					.withDisplayName("Number of inbound dependencies")
					.withDescription("Number of all direct incoming or outbound dependencies of the Module")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Dependencies")
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_NUMBER)
					.add();
			
			buildDependencyCount(builder, MiningEnitityNames.MODULE, "outboundDependencyCount", RelationshipDirection.OUT, null, false)
					.withDisplayName("Number of outbound dependencies")
					.withDescription("Number of all direct dependencies of the Module")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Dependencies")
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.FILTER_MODE,
							SearchFilterAttributes.FILTER_MODE_NUMBER)
					.add();
			
			for (final var direction : new RelationshipDirection[] { RelationshipDirection.IN, RelationshipDirection.OUT }) {
				final String strDirection = direction == RelationshipDirection.IN ? "inbound" : "outbound";
				for (final var ref : RelationshipType.values()) {
					final String description = strDirection + " " + ref.name().toLowerCase();
					buildDependencyCount(builder, MiningEnitityNames.MODULE,
							strDirection + CaseUtils.toCamelCase(ref.name(), true) + "Count", direction, ref, false)
							.withDisplayName("Number of " + description + " dependencies")
							.withDescription("Number of direct " + description + " dependencies of the Module").add();
					
					builder.defineDataPoint(MiningEnitityNames.MODULE,
							(direction == RelationshipDirection.IN ? "in" : "out") + CaseUtils.toCamelCase(ref.name(), true) + "Technology")
						.arrayOfType("Technology")
						.withCustomFetch(env -> { throw new UnsupportedOperationException("Select dependencies(...) { module { technology } } instead"); })
						.withDescription("Filter for the Technology of " + description + " dependencies of the Module")
						.withFiltering(MiningGraphQLQueries.MODULES, f -> {
							if (direction == RelationshipDirection.IN) {
								f.<ModuleService.ModuleInquiryBuilder, Technology>eq((q, v) -> q.withSourceRelationshipsFrom(m -> m.withTechnology(v), ref));
								f.<ModuleService.ModuleInquiryBuilder, List<Technology>>in((q, v) -> q.withSourceRelationshipsFrom(m -> m.withTechnologies(v), ref));
							} else {
								f.<ModuleService.ModuleInquiryBuilder, Technology>eq((q, v) -> q.withDestinationRelationshipsTo(m -> m.withTechnology(v), ref));
								f.<ModuleService.ModuleInquiryBuilder, List<Technology>>in((q, v) -> q.withDestinationRelationshipsTo(m -> m.withTechnologies(v), ref));
							}
						})
						.add();
					
					builder.defineDataPoint(MiningEnitityNames.MODULE,
							(direction == RelationshipDirection.IN ? "in" : "out") + CaseUtils.toCamelCase(ref.name(), true) + "Type")
						.arrayOfType("Type")
						.withCustomFetch(env -> { throw new UnsupportedOperationException("Select dependencies(...) { module { type } } instead"); })
						.withDescription("Filter for the Type of " + description + " dependencies of the Module")
						.withFiltering(MiningGraphQLQueries.MODULES, f -> {
							if (direction == RelationshipDirection.IN) {
								f.<ModuleService.ModuleInquiryBuilder, Type>eq((q, v) -> q.withSourceRelationshipsFrom(m -> m.withType(v), ref));
								f.<ModuleService.ModuleInquiryBuilder, List<Type>>in((q, v) -> q.withSourceRelationshipsFrom(m -> m.withTypes(v), ref));
							} else {
								f.<ModuleService.ModuleInquiryBuilder, Type>eq((q, v) -> q.withDestinationRelationshipsTo(m -> m.withType(v), ref));
								f.<ModuleService.ModuleInquiryBuilder, List<Type>>in((q, v) -> q.withDestinationRelationshipsTo(m -> m.withTypes(v), ref));
							}
						})
						.add();
				}
			}
			
			for (final RuleAnnotationCategory value : RuleAnnotationCategory.values()) {
				if (value != RuleAnnotationCategory.UNKNOWN) {
					buildAnnotationCount(builder, MiningEnitityNames.MODULE, "numberOf" + value.getName().replace(" ", "") + "s",
								AnnotationType.RULE, null, value.getName())
							.withDisplayName("Number of " + value.getName() + "s")
							.withDescription("Number of all annotation with Type - Rule and Category - " + value.getName())
							.withUsage(Usages.MINING_UI_MODULES_TABLE)
						.add(); 
				}
			}
			
			for (final DatabaseAnnotationCategory value : DatabaseAnnotationCategory.values()) {
				if (value != DatabaseAnnotationCategory.UNKNOWN) {
					buildAnnotationCount(builder, MiningEnitityNames.MODULE, "numberOf" + value.getName() + "DatabaseOperation",
								AnnotationType.DATABASE, null, value.getName())
							.withDisplayName("Number of " + value.getName() + " Database Operations")
							.withDescription("Number of all annotation with Type - Database and Category - " + value.getName())
							.withUsage(Usages.MINING_UI_MODULES_TABLE)
							.add();
				}
			}

			builder.defineAlias(MiningEnitityNames.MODULE, "reachabilityBlockNames")
					.forDataPoint("reachabilityBlocks")
					.withSubSelection("name")
					.add();

			builder.extend(MiningEnitityNames.MODULE, "reachabilityBlockNames")
					.withDisplayName("Reachability Block Name")
					.withDescription("Name of the Reachability Block to which the Module belongs")
					.withUsage(Usages.MINING_UI_MODULES_TABLE)
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Reachability Blocks")
					.withUsage(Usages.SEARCH_FILTER)
					.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
					.withUsage(Usages.SORT_BY)
					.withSorting(MiningGraphQLQueries.MODULES, ModuleService.ModuleInquiryBuilder::sortByReachabilityBlockName)
					.withFiltering(MiningGraphQLQueries.MODULES, f -> f.eq(ModuleService.ModuleInquiryBuilder::withReachabilityBlockName))
					.add();

			builder.extend(MiningEnitityNames.ERROR_MARKER, "module")
					.withFiltering(MiningGraphQLQueries.ERROR_MARKERS, f -> {
						f.eq(ModuleService.ErrorMarkerInquiryBuilder::ofModule);
					})
					.add();
		}

		private MiningDataPointBuilder.DataPointBuilder buildDependencyCount(final MiningDataPointBuilder dpBuilder, final String entity,
																			 final String name, @Nullable final RelationshipDirection direction, @Nullable final RelationshipType relationship, final boolean distinctModuleRelationships) {
			final var builder = dpBuilder.defineDataPoint(entity, name)
				.type(ScalarType.LONG)
				.<ModuleService.ModuleInquiryBuilder>withDynamicFieldBuilder(entity,
					(alias, args, q) -> q.includeDependencyCount(alias,
						direction == null ? PgUtil.mapNullable(args.get("direction"), o -> RelationshipDirection.valueOf(o.toString())) : direction,
						relationship == null ? PgUtil.mapNullable(args.get("type"), o -> RelationshipType.valueOf(o.toString())) : relationship,
						distinctModuleRelationships))
				.withCustomFetch(GraphQlUtil.fetchDynamicProperty());
			if (direction == null) {
				builder.withOptionalParameter("direction", RelationshipDirection.class);
			}
			if (relationship == null) {
				builder.withOptionalParameter("type", RelationshipType.class);
			}
			builder.withFiltering(MiningGraphQLQueries.MODULES, f -> {
					f.<ModuleService.ModuleInquiryBuilder, Long>eq((q, v) -> q.filterBy(name, Comperator.EQUAL, v));
					f.<ModuleService.ModuleInquiryBuilder, Long>gte((q, v) -> q.filterBy(name, Comperator.GREATER_OR_EQUAL, v));
					f.<ModuleService.ModuleInquiryBuilder, Long>lte((q, v) -> q.filterBy(name, Comperator.LESSER_OR_EQUAL, v));
				})
				.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortBy(name, d))
				.withUsage(Usages.GRAPHQL_QUERY_MODULES)
				.withUsage(Usages.SORT_BY);
			return builder;
		}
		
		public MiningDataPointBuilder.DataPointBuilder buildAnnotationCount(final MiningDataPointBuilder dpBuilder, final String entity,
				final String name, @Nullable final AnnotationType type, @Nullable final WorkingState state, @Nullable final String categoryName) {
			final var builder = dpBuilder.defineDataPoint(entity, name)
					.type(ScalarType.LONG)
					.<ModuleService.ModuleInquiryBuilder>withDynamicFieldBuilder(entity,
						(alias, args, q) -> q.includeAnnotationCount(alias,
							type == null ? PgUtil.mapNullable(args.get("type"), o -> AnnotationType.valueOf(o.toString())) : type,
							state == null ? PgUtil.mapNullable(args.get("state"), o -> WorkingState.valueOf(o.toString())) : state,
							categoryName == null ? (String) args.get("categoryName") : categoryName))
					.withCustomFetch(GraphQlUtil.fetchDynamicProperty());
				if (type == null) {
					builder.withOptionalParameter("type", AnnotationType.class);
				}
				if (state == null) {
					builder.withOptionalParameter("state", WorkingState.class);
				}
				if (categoryName == null) {
					builder.withOptionalParameter("categoryName", ScalarType.STRING);
				}
				builder.withFiltering(MiningGraphQLQueries.MODULES, f -> {
						f.<ModuleService.ModuleInquiryBuilder, Long>eq((q, v) -> q.filterBy(name, Comperator.EQUAL, v));
						f.<ModuleService.ModuleInquiryBuilder, Long>gte((q, v) -> q.filterBy(name, Comperator.GREATER_OR_EQUAL, v));
						f.<ModuleService.ModuleInquiryBuilder, Long>lte((q, v) -> q.filterBy(name, Comperator.LESSER_OR_EQUAL, v));
					})
					.<ModuleService.ModuleInquiryBuilder>withSorting(MiningGraphQLQueries.MODULES, (q, d) -> q.sortBy(name, d))
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, TableAttributes.CATEGORY, "Annotations")
					.withUsageAttribute(Usages.MINING_UI_MODULES_TABLE, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_NUMBER)
					.withUsage(Usages.GRAPHQL_QUERY_MODULES)
					.withUsage(Usages.SORT_BY);
				return builder;
		}
	}
}
