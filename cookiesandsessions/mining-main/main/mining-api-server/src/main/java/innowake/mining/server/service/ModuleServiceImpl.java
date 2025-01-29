/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.PersistenceException;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.jdbc.UncategorizedSQLException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.access.postgres.DataFlowPgDao;
import innowake.mining.data.access.postgres.DnaClusterPgDao;
import innowake.mining.data.access.postgres.ErrorMarkerPgDao;
import innowake.mining.data.access.postgres.MiningJobInfoPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.data.access.postgres.ModuleRelationshipPgDao;
import innowake.mining.data.access.postgres.ProjectPgDao;
import innowake.mining.data.access.postgres.StatementPgDao;
import innowake.mining.data.annotation.ProjectIdArgument;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojo;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ModuleUndiscoveredPojo;
import innowake.mining.shared.entities.ModuleUndiscoveredPojoPrototype;
import innowake.mining.shared.entities.SchemaInfoPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.hashing.LinkHash;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.HotSpot.FilterType;
import innowake.mining.shared.model.LinkedModule;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.ModuleStatisticsResponse;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipFieldName;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementFieldName;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;
import innowake.mining.shared.model.dependency.graph.NodeType;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Access to module related data.
 */
@Service
public class ModuleServiceImpl implements ModuleService {

	private final SourceService sourceService;
	private final ProjectPgDao projectDao;
	private final ModulePgDao moduleDao;
	private final DataFlowPgDao dataFlowDao;
	private final ModuleRelationshipPgDao moduleReferenceDao;
	private final StatementPgDao statementDao;
	private final ErrorMarkerPgDao errorMarkerDao;
	private final DnaClusterPgDao clusterDao;
	private final CustomPropertiesService customPropertiesService;
	private final MiningJobInfoPgDao miningJobInfoDao;
	private final JobManager jobManager;

	@Autowired
	public ModuleServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate, final SourceService sourceService,
							 final CustomPropertiesService customPropertiesService, final JobManager jobManager) {
		projectDao = new ProjectPgDao(jdbcTemplate);
		moduleDao = new ModulePgDao(jdbcTemplate);
		moduleReferenceDao = new ModuleRelationshipPgDao(jdbcTemplate);
		statementDao = new StatementPgDao(jdbcTemplate);
		errorMarkerDao = new ErrorMarkerPgDao(jdbcTemplate);
		miningJobInfoDao = new MiningJobInfoPgDao(jdbcTemplate);
		clusterDao = new DnaClusterPgDao(jdbcTemplate);
		dataFlowDao = new DataFlowPgDao(jdbcTemplate);
		this.customPropertiesService = customPropertiesService;
		this.sourceService = sourceService;
		this.jobManager = jobManager;
	}

	@Override
	@Transactional("postgres") /* create source, source_metrics, custom properties and module in one transaction */
	public EntityId create(final ModulePojoPrototype module) {
		final var path = StringUtils.trimToNull(module.path.orElse(null));
		final var name = module.name.getNonNull();
		final var project = module.project.getNonNull();

		try {
			setModuleProperties(module, true);

			final Optional<SourcePojo> sourcePojo = path == null ? Optional.empty()
					: sourceService.findOne(q -> q.ofProject(project).withPath(path).includeContent(true));
			final var content = module.content.orElse(null);
			if (sourcePojo.isPresent() && content != null) {
				throw new IllegalStateException("Source Attachment and content must not be present both");
			}
			if (content != null) {
				module.setContentHash(new BinaryValue(CityHash.cityHash128(ByteOrder.BIG_ENDIAN, content.getBytes(StandardCharsets.UTF_8))));
				if (path == null) {
					module.setSource(EntityId.of(sourceService.put(project, new BinaryString(content))));
				} else {
					final var source = new SourcePojoPrototype()
							.setContent(new BinaryString(content))
							.setName(name)
							.setPath(path)
							.setProject(project)
							.setTechnology(module.technology.getNonNull())
							.setType(module.type.getNonNull());
					module.setSource(sourceService.create(source));
				}
			} else if (path != null && sourcePojo.isPresent()) {
				module.setSource(sourcePojo.get().identity());
				module.setContentHash(sourcePojo.get().getContentHash());
			}

			final EntityId moduleId = moduleDao.put(module, true);

			final var sourceMetrics = module.sourceMetrics.orElse(null);
			if (sourceMetrics != null) {
				sourceMetrics.setModule(moduleId);
				moduleDao.putSourceMetrics(sourceMetrics);
			}

			customPropertiesService.validateProjectBound(module, module.project::getNonNull);

			return moduleId;
		} catch (final UncategorizedSQLException e) {
			throw new ConstraintViolationException(module, ExceptionUtils.getRootCauseMessage(e), e);
		}
	}

	@Override
	@Transactional("postgres") /* create or update of source_metrics + update module in one transaction */
	public EntityId update(final ModulePojoPrototype module) {
		setModuleProperties(module, false);

		final var id = moduleDao.put(module, false);

		customPropertiesService.validateModuleBound(module, module::identityProvisional);

		final var sourceMetrics = module.sourceMetrics.orElse(null);
		if (sourceMetrics != null) {
			sourceMetrics.setModule(id);
			moduleDao.putSourceMetrics(sourceMetrics);
		}

		return id;
	}

	private void setModuleProperties(final ModulePojoPrototype module, final boolean isNew) {
		/* update modified date whenever a module is updated or when a new module contains none */
		if ( ! isNew || module.modifiedDate.orElse(null) == null) {
			module.setModifiedDate(Instant.now());
		}

		if (isNew || ! module.linkHash.isPresent()) {
			/* Take properties for the linkHash calculation from prototype when a new module is created or when all properties for the linkHash calculation are defined */
			final boolean usePrototype = isNew || (module.name.isDefined() && module.path.isDefined() && module.technology.isDefined()
					&& module.type.isDefined() && module.parent.isDefined());

			final String name;
			final String path;
			final String  technology;
			final String  type;
			final EntityId parent;
			String parentPath;
			if (usePrototype) {
				name = module.name.getNonNull();
				path = module.path.orElse(null);
				technology = module.technology.getNonNull().toString();
				type = module.type.getNonNull().toString();
				parent = module.parent.orElse(null);
				parentPath = module.parentPath.orElse(null);
			} else {
				/* Sick: In the old dao the link hash was recalculated on every update */
				final ModuleLightweightPojo loadedModule = getModuleLightweight(module.identityProvisional());
				name = module.name.orElseNonNull(loadedModule.getName());
				path = module.path.orElse(loadedModule.getPath());
				technology = module.technology.orElseNonNull(loadedModule.getTechnology()).toString();
				type = module.type.orElseNonNull(loadedModule.getType()).toString();
				parent = module.parent.orElse(loadedModule.getParent());
				parentPath = module.parentPath.orElse(loadedModule.getParentPath());
			}

			final String parentLinkHash;
			/* WMIN-7868: Use linkHash of parent if parent path is not present */
			if (parent != null && parentPath == null) {
				final ModuleLightweightPojo parentModule = getModuleLightweight(parent);
				parentPath = parentModule.getPath();
				parentLinkHash = parentModule.getLinkHash();
			} else {
				parentLinkHash = null;
			}

			module.setLinkHash(LinkHash.calculateLinkHash(name, technology, type, path, parentPath, parentLinkHash));
		}
	}

	@Override
	public int deleteModule(final EntityId moduleId, final boolean deleteSource) {
		if (deleteSource) {
			moduleDao.findAnyModule(b -> b.byId(moduleId)).ifPresent(m -> m.getSource().ifPresent(sourceId -> sourceService.remove(EntityId.of(sourceId), null)));
		}

		final List<UUID> jobIds = miningJobInfoDao.findJobId(q -> q.ofModule(moduleId));
		jobManager.delete(q -> q.byIds(jobIds));

		dataFlowDao.setTraced(moduleId, false);

		return moduleDao.deleteModules(b -> b.byId(moduleId));
	}

	@Override
	public int deleteModules(final EntityId project, final boolean deleteSource, final boolean deleteDiscoveryOnly) {
		if (deleteSource) {
			final List<UUID> removedSources = sourceService.removeAll(q -> q.ofProject(project));
			if (! removedSources.isEmpty()) {
				projectDao.incrementSourceCodeRevision(project);
			}
		}

		final List<UUID> jobIds = miningJobInfoDao.findJobId(q -> q.ofModules(findModuleUids(q1 -> q1.ofProject(project))));
		jobManager.delete(q -> q.byIds(jobIds));

		if (project.isEmpty()) {
			throw new MiningEntityNotFoundException("Project with id must exists: " + project);
		}

		/* We always delete all undiscovered entities when deleting all modules of a project */
		deleteUndiscovered(q -> q.ofProject(project));

		/* We always delete all DNA entities and vertices */
		clusterDao.deleteSnapshots(project);

		return moduleDao.deleteModules(b -> {
			b.ofProject(project);
			if (deleteDiscoveryOnly) {
				b.withCreator(Creator.DISCOVERY);
			}
		});
	}

	@Override
	public int deleteModules(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		dataFlowDao.setTraced(moduleDao.findModuleIds(builder).stream().map(EntityId::getUid).collect(Collectors.toList()), false);

		return moduleDao.deleteModules(builder);
	}

	@Override
	public List<ModulePojo> findModules(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findModules(builder);
	}

	@Override
	public Paged<ModulePojo> findModules(final Pagination paging, final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findModules(paging, builder);
	}

	@Override
	public List<UUID> findRelatedModules(final EntityId module, final RelationshipType type, final RelationshipDirection direction) {
		return moduleDao.findReferencingModules(module, type, direction);
	}

	@Override
	public List<String> findNames(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findNames(builder);
	}

	@Override
	public ModuleLightweightPojo getModuleLightweight(final EntityId moduleId) {
		return findAnyModuleLightweight(q -> q.byId(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException(ModuleLightweightPojo.class, moduleId.toString()));
	}

	@Override
	public List<ModuleLightweightPojo> findModulesLightweight(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findModulesLightweight(builder);
	}

	@Override
	public Optional<ModuleLightweightPojo> findAnyModuleLightweight(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findAnyModuleLightweight(builder);
	}

	@Override
	public List<ModuleLightweightPojo> findModuleLightweightOrphaned(final EntityId projectId, final Creator creator) {
		return moduleDao.findModuleLightweightOrphaned(projectId, creator);
	}

	@Override
	public List<ModuleLightweightPojo> findModuleLightweightMissingOrChangedSource(final UUID projectId, final Creator creator) {
		return moduleDao.findModuleLightweightMissingOrChangedSource(projectId, creator);
	}

	@Override
	public List<LinkedModule> findLinkedModules(final BuildingConsumer<LinkedModuleInquiryBuilder> builder) {
		return moduleDao.findLinkedModules(builder);
	}

	@Override
	public Optional<ModulePojo> findAnyModule(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findAnyModule(builder);
	}

	@Override
	public ModulePojo getModule(final UUID moduleId) {
		return findAnyModule(q -> q.byUid(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException(ModulePojo.class, String.valueOf(moduleId)));
	}

	@Override
	public ModulePojo getModule(final EntityId moduleId) {
		return findAnyModule(q -> q.byId(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException(ModulePojo.class, String.valueOf(moduleId)));
	}

	@Override
	public String getContentSubstring(final EntityId moduleId, final Integer offset, final Integer length) {
		return moduleDao.getContentSubstring(moduleId, offset, length).orElseThrow(() -> new MiningEntityNotFoundException(ModulePojo.class, moduleId.toString()));
	}

	@Override
	public Map<EntityId, String> getContents(final Collection<EntityId> moduleIds) {
		return moduleDao.getContents(moduleIds);
	}

	@Override
	public Type getModuleType(final EntityId moduleId) {
		return moduleDao.getModuleType(moduleId).orElseThrow(() -> new MiningEntityNotFoundException(ModulePojo.class, moduleId.toString()));
	}

	@Override
	public String getModulePath(final EntityId moduleId) {
		return moduleDao.getModulePath(moduleId).orElseThrow(() -> new MiningEntityNotFoundException(ModulePojo.class, moduleId.toString()));
	}

	@Override
	public List<UUID> findModuleUids(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findModuleUids(builder);
	}

	@Override
	public List<EntityId> findModuleIds(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findModuleIds(builder);
	}

	@Override
	public Map<String, EntityId> findModuleIdsByLinkHash(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		final List<Pair<EntityId, String>> moduleIdsAndLinkHashes = moduleDao.findModuleIdsAndLinkHashes(builder);
		final Map<String, EntityId> ret = new HashMap<>(moduleIdsAndLinkHashes.size());
		for (final Pair<EntityId, String> moduleIdAndLinkHash : moduleIdsAndLinkHashes) {
			ret.put(moduleIdAndLinkHash.getRight(), moduleIdAndLinkHash.getLeft());
		}
		return ret;
	}

	@Override
	public Optional<EntityId> findAnyModuleId(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findAnyModuleId(builder);
	}

	@Override
	public Paged<EntityId> findModuleIds(final Pagination paging, final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findModuleIds(paging, builder);
	}

	@Override
	public List<String> findDuplicateModuleNames(final EntityId project, final Technology technology, final List<Type> types) {
		return moduleDao.findDuplicateModuleNames(project, technology, types);
	}

	@Override
	public List<EntityId> findDuplicateModuleIds(final EntityId project, final Technology technology, final Type typeToKeep, final Type typeToDelete) {
		return moduleDao.findDuplicateModuleIds(project, technology, typeToKeep, typeToDelete);
	}

	@Override
	public List<EntityId> findModulesWithUnresolvedDependencies(final EntityId project) {
		return moduleDao.findModulesWithUnresolvedDependencies(project);
	}

	@Override
	public List<EntityId> findModulesWithMetaData(final EntityId project) {
		return moduleDao.findModulesWithMetaData(project);
	}

	@Override
	public int updateModuleContentHashes(final EntityId project) {
		return moduleDao.updateModuleContentHashes(project);
	}

	@Override
	public int updateModuleDependencyHashes(final EntityId project) {
		return moduleDao.updateModuleDependencyHashes(project);
	}

	@Override
	public int updateModules(final BuildingConsumer<ModuleInquiryBuilder> builder, final ModulePojoPrototype values) {
		return moduleDao.updateModules(builder, values);
	}

	@Override
	public long countModules(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.countModules(builder);
	}

	@Override
	public long countSourceMetrics(final BuildingConsumer<SourceMetricsInquiryBuilder> builder) {
		return moduleDao.countSourceMetrics(builder);
	}

	@Override
	public long countSourceMetricsCodeLines(final BuildingConsumer<SourceMetricsInquiryBuilder> builder) {
		return moduleDao.countSourceMetricsCodeLines(builder);
	}

	@Override
	public Map<String, Long> countSourceMetricsCodeLinesByTechnology(final BuildingConsumer<SourceMetricsInquiryBuilder> builder) {
		return moduleDao.countSourceMetricsCodeLinesByTechnology(builder);
	}

	@Override
	public Map<String, Map<String, List<Integer>>> getComplexities(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.getComplexities(builder);
	}

	@Override
	public Optional<Table> getAggregations(final BuildingConsumer<ModuleAggregationInquiryBuilder<?>> builder) {
		return moduleDao.getAggregations(builder);
	}

	@Cacheable(cacheNames = "aggregatedValues", cacheResolver = "cacheResolver", keyGenerator = "projectKeyGenerator")
	@Override
	/* project is required for caching */
	public List<AggregationResult<ModuleFieldName>> getAggregations(@ProjectIdArgument final EntityId project, final AggregationRequest<ModuleFieldName> request) {
		return moduleDao.getAggregations(q -> {
			q.applyFilters(request.getFilterObject());
			request.getFields().forEach(q :: aggregate);
			request.getGroupBy().forEach(q :: groupBy);
			request.getOrderBy().forEach(q :: orderBy);
		}).map(rows -> AggregationResult.getResultFromTable(rows, request, ModuleFieldName.class))
				.orElse(Collections.emptyList());
	}

	@Override
	public Optional<Table> findRelationshipInvocations(final Map<ModuleFieldName, Map<String, Object>> filterObject) {
		return moduleDao.findRelationshipInvocations(q -> q.applyFilters(filterObject));
	}

	@Override
	public void putSourceMetrics(final SourceMetricsPojoPrototype sourceMetrics) {
		moduleDao.putSourceMetrics(sourceMetrics);
	}

	@Override
	public void updateSourceMetricsLinesOfDeadCode(final EntityId project) {
		moduleDao.resetSourceMetricsLinesOfDeadCode(project);
		moduleDao.updateSourceMetricsLinesOfDeadCode(project);
	}

	@Transactional("postgres")
	@Override
	public UUID createRelationship(final ModuleRelationshipPojoPrototype relationship) {
		final var uuid = createRelationship(relationship, false)
				.orElseThrow(() -> new PersistenceException(String.format("Failed to create %s relationship from %s to %s",
						relationship.type, relationship.srcModule, relationship.dstModule)));
		if (relationship.type.get() == RelationshipType.CONTAINS) {
			/* calculate link hash */
			update(new ModulePojoPrototype().withId(relationship.dstModule.getNonNull()));
		}

		return uuid;
	}

	@Override
	public Optional<UUID> createRelationship(final ModuleRelationshipPojoPrototype relationship, final boolean checkForDuplicates) {
		if (checkForDuplicates) {
			final var duplicates = findDuplicates(relationship);
			switch (duplicates.size()) {
			case 0: /* reference doesn't exists yet */
				break;
			case 1: /* reference already exists so upsert conditionalReferences only */
				final var ref = duplicates.get(0);
				final List<EntityId> fromModules = relationship.validIfReachedFrom.orElseNonNull(Collections::emptyList);
				if ( ! fromModules.isEmpty()) {
					moduleReferenceDao.putConditionalModules(ref, fromModules);
				}
				return Optional.empty();
			default:
				throw new IllegalStateException("Found more than one already existing module_relationship: " + duplicates);
			}
		}

		final var ref = moduleReferenceDao.create(relationship);
		if (relationship.validIfReachedFrom.isDefined()) {
			moduleReferenceDao.putConditionalModules(ref, relationship.validIfReachedFrom.orElseNonNull(Collections::emptyList));
		}

		return Optional.of(ref);
	}

	@Override
	public int deleteRelationship(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return moduleReferenceDao.delete(builder);
	}

	@Override
	public ModuleRelationshipPojo getRelationship(final UUID id) {
		return moduleReferenceDao.findAnyRelationship(q -> q.byId(id))
				.orElseThrow(() -> new MiningEntityNotFoundException(ModuleRelationshipPojo.class, id.toString()));
	}

	private List<UUID> findDuplicates(final ModuleRelationshipPojoPrototype moduleReference) {
		return moduleReferenceDao.findIds(query -> {
			query.ofSource(moduleReference.srcModule.getNonNull())
			.ofDestination(moduleReference.dstModule.getNonNull())
			.withType(moduleReference.type.getNonNull());

			if (moduleReference.srcLocation.isPresent()) {
				query.withSourceLocation(moduleReference.srcLocation.getNonNull());
			}
			if (moduleReference.dstLocation.isPresent()) {
				query.withDestinationLocation(moduleReference.dstLocation.getNonNull());
			}
			if (moduleReference.properties.isPresent()) {
				query.withProperties(moduleReference.properties.getNonNull());
			}
		});
	}

	@Override
	public Paged<ModuleRelationshipPojo> findRelationships(final Pagination paging, final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return moduleReferenceDao.find(paging, builder);
	}

	@Override
	public List<ModuleRelationshipPojo> findRelationship(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return moduleReferenceDao.find(builder);
	}

	@Override
	public Optional<ModuleRelationshipPojo> findAnyRelationship(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return moduleReferenceDao.findAnyRelationship(builder);
	}

	@Override
	public long countRelationships(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return moduleReferenceDao.count(builder);
	}

	@Cacheable(cacheNames = "aggregatedValues", cacheResolver = "cacheResolver", keyGenerator = "projectKeyGenerator")
	@Override
	/* project is required for caching */
	public List<AggregationResult<RelationshipFieldName>> getRelationshipAggregations(@ProjectIdArgument final EntityId project, final AggregationRequest<RelationshipFieldName> aggregationRequest) {
		return moduleReferenceDao.getRelationshipAggregations(q -> {
			aggregationRequest.getFilterObject().forEach((fieldName, filter) -> filter.forEach((operator, value) -> {
				switch (fieldName) {
				case ID:
					q.byId(operator, value);
					break;
				case RELATIONSHIP:
					q.withRelationship(operator, value);
					break;
				case DST_ID:
					q.withDstId(operator, value);
					break;
				case SRC_ID:
					q.withSrcId(operator, value);
					break;
				case DST_NAME:
					q.withDstName(operator, value);
					break;
				case SRC_NAME:
					q.withSrcName(operator, value);
					break;
				case DST_PROJECT_ID:
					q.withDstProjectId(operator, value);
					break;
				case SRC_PROJECT_ID:
					q.withSrcProjectId(operator, value);
					break;
				case DST_TECHNOLOGY:
					q.withDstTechnology(operator, value);
					break;
				case SRC_TECHNOLOGY:
					q.withSrcTechnology(operator, value);
					break;
				case DST_TYPE:
					q.withDstType(operator, value);
					break;
				case SRC_TYPE:
					q.withSrcType(operator, value);
					break;
				case PROPERTY_DB_ACCESS_TYPE:
					q.withPropertyDbType(operator, value);
					break;
				case PROPERTY_DB_ACCESS_OPERATION:
					q.withPropertyDbOperation(operator, value);
					break;
				case SRC_LINKHASH:
					q.withSrcLinkhash(operator, value);
					break;
				case DST_LINKHASH:
					q.withDstLinkhash(operator, value);
					break;
				case SRC_STORAGE:
					q.withSrcStorage(operator, value);
					break;
				case DST_STORAGE:
					q.withDstStorage(operator, value);
					break;
				case TAXONOMY_ID:
					q.withTaxonomy(operator, value);
					break;
				default:
					throw new UnsupportedOperationException(String.format("Relationship field %s not supported", fieldName));
				}
			}));
			aggregationRequest.getFields().forEach(q :: aggregate);
			aggregationRequest.getGroupBy().forEach(q :: groupBy);
			aggregationRequest.getOrderBy().forEach(q :: orderBy);
		}).map(rows -> AggregationResult.getResultFromTable(rows, aggregationRequest, RelationshipFieldName.class))
				.orElse(Collections.emptyList());
	}

	@Override
	public Optional<Table> getModuleRelationshipExport(final EntityId project, final boolean sorted) {
		return moduleReferenceDao.getModuleRelationshipExport(project, sorted);
	}

	@Override
	public List<Long> getSrcModuleIdsByRelationshipTypeAndDstId(final RelationshipType type, final UUID dstModuleId) {
		return moduleReferenceDao.getSrcModuleIdsByRelationshipTypeAndDstId(type, dstModuleId);
	}

	@Override
	public Optional<EntityId> createStatement(final StatementPojoPrototype statement, final boolean checkForDuplicates) {
		if (checkForDuplicates && statementDao.count(q -> q.ofModule(statement.module.getNonNull())
				.withType(statement.type.getNonNull())
				.withText(statement.text.getNonNull())) != 0) {
			return Optional.empty();
		}

		handleNullBytes(statement.text);
		return Optional.of(statementDao.create(statement));
	}

	@Override
	public void createStatements(final Collection<StatementPojoPrototype> statements) {
		if ( ! statements.isEmpty()) {
			statements.forEach(statement -> handleNullBytes(statement.text));
			statementDao.create(statements);
		}
	}

	@Override
	public int deleteStatement(final BuildingConsumer<StatementInquiryBuilder> builder) {
		return statementDao.delete(builder);
	}

	@Override
	public List<StatementPojo> findStatements(final BuildingConsumer<StatementInquiryBuilder> builder) {
		return statementDao.find(builder);
	}

	@Override
	public Paged<StatementPojo> findStatements(final Pagination paging, final BuildingConsumer<StatementInquiryBuilder> builder) {
		return statementDao.find(paging, builder);
	}

	@Cacheable(cacheNames = "aggregatedValues", cacheResolver = "cacheResolver", keyGenerator = "projectKeyGenerator")
	@Override
	/* project is required for caching */
	public List<AggregationResult<StatementFieldName>> getStatementAggregations(@ProjectIdArgument final EntityId project, final AggregationRequest<StatementFieldName> aggregationRequest) {
		return statementDao.getAggregations(q -> {
			aggregationRequest.getFilterObject().forEach((fieldName, filter) -> filter.forEach((operator, value) -> {
				switch (fieldName) {
				case ID:
					q.byId(operator, value);
					break;
				case MODULE_ID:
					q.ofModule(operator, value);
					break;
				case TAXONOMY_ID:
					q.withTaxonomy(operator, value);
					break;
				case STATEMENT_TYPE:
					q.withType(operator, value);
					break;
				case TEXT_LENGTH:
					q.withTextLength(operator, value);
					break;
				case TECHNOLOGY:
					q.withTechnology(operator, value);
					break;
				case TEXT:
					q.withText(operator, value);
					break;
				case PROJECT_ID:
					q.ofProject(operator, value);
					break;
				case CUSTOM_COMPLEXITY:
					q.withCustomComplexity(operator, value);
					break;
				case DISTINCT_TABLES:
					q.withDistinctTables(operator, value);
					break;
				case HALSTEAD_COMPLEXITY:
					q.withHalsteadComplexity(operator, value);
					break;
				case HALSTEAD_DIFFICULTY:
					q.withHalsteadDifficulty(operator, value);
					break;
				case SQL_LENGTH:
					q.withSqlLength(operator, value);
					break;
				case TABLES:
					q.withTables(operator, value);
					break;
				default:
					throw new UnsupportedOperationException(String.format("Statement field %s not supported", fieldName));
				}
			}));
			aggregationRequest.getFields().forEach(q :: aggregate);
			aggregationRequest.getGroupBy().forEach(q :: groupBy);
			aggregationRequest.getOrderBy().forEach(q :: orderBy);
		}).map(rows -> AggregationResult.getResultFromTable(rows, aggregationRequest, StatementFieldName.class))
				.orElse(Collections.emptyList());
	}

	@Override
	public long countStatements(final BuildingConsumer<StatementInquiryBuilder> builder) {
		return statementDao.count(builder);
	}

	@Override
	public Optional<Table> getModuleStatementExport(final EntityId project, final boolean isSql, final boolean sorted) {
		return statementDao.getModuleStatementExport(project, isSql, sorted);
	}

	@Override
	public void createDeadCode(final ModuleDeadCodePojoPrototype deadCode) {
		moduleDao.createDeadCode(deadCode);
	}

	@Override
	public void createDeadCodes(final Collection<ModuleDeadCodePojoPrototype> deadCodes) {
		if ( ! deadCodes.isEmpty()) {
			moduleDao.createDeadCodes(deadCodes);
		}
	}

	@Override
	public List<ModuleDeadCodePojo> findDeadCode(final BuildingConsumer<DeadCodeInquiryBuilder> builder) {
		return moduleDao.findDeadCode(builder);
	}

	@Override
	public long countDeadCode(final BuildingConsumer<DeadCodeInquiryBuilder> builder) {
		return moduleDao.countDeadCode(builder);
	}

	@Override
	public Optional<Table> findContainingAndRelatedModules(final Collection<UUID> moduleUids, final boolean collectContaining) {
		if (moduleUids.isEmpty()) {
			return Optional.empty();
		}

		return moduleDao.findContainingAndReferencingModules(moduleUids, collectContaining);
	}

	@Override
	public Optional<Table> getModuleDeadCode(final EntityId project, final boolean sorted) {
		return moduleDao.getModuleDeadCode(project, sorted);
	}

	@Override
	public Optional<Table> getModuleErrorMarkerExport(final EntityId project, final boolean sorted) {
		return errorMarkerDao.getModuleErrorMarkerExport(project, sorted);
	}

	@Override
	public void createErrorMarker(final ErrorMarkerPojoPrototype errorMarker) {
		/* Set default values like we did in ErrorMarker */
		if (errorMarker.severity.orElse(null) == null) {
			errorMarker.setSeverity(Severity.WARNING);
		}
		if (errorMarker.key.orElse(null) == null) {
			errorMarker.setKey(ErrorKey.MODULE_ABORT);
		}

		handleNullBytes(errorMarker.cause);
		errorMarkerDao.create(errorMarker);
	}

	@Override
	public void createErrorMarkers(final Collection<ErrorMarkerPojoPrototype> errorMarkers) {
		if ( ! errorMarkers.isEmpty()) {
			errorMarkers.forEach(errorMarker -> handleNullBytes(errorMarker.cause));
			errorMarkerDao.create(errorMarkers);
		}
	}

	@Override
	public List<ErrorMarkerPojo> findErrorMarkers(final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return errorMarkerDao.find(builder);
	}

	@Override
	public Paged<ErrorMarkerPojo> findErrorMarkers(final Pagination paging, final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return errorMarkerDao.find(paging, builder);
	}

	@Override
	public long countErrorMarkers(final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return errorMarkerDao.count(builder);
	}

	@Override
	public Map<UUID, Long> countErrorMarkersByModule(final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return errorMarkerDao.countByModule(builder);
	}

	@Override
	public int deleteErrorMarkers(final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return errorMarkerDao.deleteErrorMarkers(builder);
	}

	@Override
	public List<UUID> createDependencyDefinitions(final Collection<DependencyDefinitionPojoPrototype> dependencyDefinitions) {
		if (dependencyDefinitions.isEmpty()) {
			return Collections.emptyList();
		}

		return moduleDao.createDependencyDefinitions(dependencyDefinitions);
	}

	@Override
	public void setDependencyDefinitionResolved(final UUID id, final boolean resolved) {
		moduleDao.setDependencyDefinitionResolved(id, resolved);
	}

	@Override
	public int deleteDependencyDefinitions(final List<UUID> dependencyDefinitionIds) {
		return moduleDao.deleteDependencyDefinitions(dependencyDefinitionIds);
	}

	@Override
	public List<EntityId> findDependencyDefinitionModuleIds(final BuildingConsumer<DependencyDefinitionInquiryBuilder> builder) {
		return moduleDao.findDependencyDefinitionModuleIds(builder);
	}

	@Override
	public List<DependencyDefinitionPojo> findDependencyDefinitions(final BuildingConsumer<DependencyDefinitionInquiryBuilder> builder) {
		return moduleDao.findDependencyDefinitions(builder);
	}

	@Override
	public long countDependencyDefinitions(final BuildingConsumer<DependencyDefinitionInquiryBuilder> builder) {
		return moduleDao.countDependencyDefinitions(builder);
	}

	@Override
	public Long getModuleNid(final EntityId moduleId) {
		if ( ! moduleId.hasNid()) {
			final var modules = findModuleIds(b -> b.byId(moduleId).limit(1));
			if (modules.isEmpty()) {
				throw new MiningEntityNotFoundException("Module does not exists with uid: " + moduleId);
			}

			return modules.get(0).getNid();
		}

		return moduleId.getNid();
	}

	@Override
	public UUID getModuleUid(final EntityId moduleId) {
		if ( ! moduleId.hasUid()) {
			final var modules = findModuleIds(b -> b.byId(moduleId));
			if (modules.isEmpty()) {
				throw new MiningEntityNotFoundException("Module does not exists with nid: " + moduleId);
			}

			return modules.get(0).getUid();
		}

		return moduleId.getUid();
	}

	@Override
	public EntityId getModuleEntityId(final EntityId moduleId) {
		if (moduleId.hasUid() && moduleId.hasNid()) {
			return moduleId;
		}

		final List<EntityId> modules = findModuleIds(Pagination.FIRST, q -> q.byId(moduleId))
				.getContent();

		if (modules.isEmpty()) {
			throw new MiningEntityNotFoundException("Module does not exists with id: " + moduleId);
		}

		return modules.get(0);
	}

	@Override
	@Cacheable(cacheNames = "hotSpots", cacheResolver = "cacheResolver", keyGenerator = "projectKeyGenerator")
	public List<HotSpot> findHotSpots(@ProjectIdArgument final EntityId project, final FilterType type, @Nullable final Integer limit) {
		final var limitSet = limit == null ? 10 : limit;
		final BuildingConsumer<ModuleInquiryBuilder> builder = query -> { 
			query.ofProject(project)
				 .limit(limitSet);

			switch (type) {
				case CALLS:
				case CANDIDATE_RULE:
					break;

				case REFERENCES:
					query.withType(Type.PROGRAM);
					break;

				case DATA_SETS:
					query.withTechnology(Technology.RESOURCE)
						 .withType(Type.FILE)
						 .withOrigin(Origin.CUSTOM)
						 .notWithNameLike("&&%");
					break;

				case DATABASE_TABLES:
					query.withTechnology(Technology.SQL)
						 .withType(Type.TABLE)
						 .withOrigin(Origin.CUSTOM);
					break;

				default:
					throw new UnsupportedOperationException("FilterType '" + type + "' is not supported.");
			}
		};

		return findHotSpots(builder, type);
	}

	@Override
	public List<HotSpot> findHotSpots(final BuildingConsumer<ModuleInquiryBuilder> builder, final FilterType filterType) {
		return moduleDao.findHotSpots(builder, filterType);
	}

	@Override
	public DependencyGraph traverseDependencies(final EntityId projectId, final EntityId moduleId, final Long maxDepth, final Optional<Integer> maxGraphNodes,
			final List<NodeType> moduleNodeTypeFilter, final List<RelationshipType> relationshipTypeFilter, final boolean distinct, final boolean explorable) {
		final List<Tuple2<ModuleRelationshipPojo, Long>> relationshipsWithDepth = moduleReferenceDao.findRelationshipsWithDepthRecursive(moduleId,
				maxGraphNodes, maxDepth, distinct, explorable);

		final Set<EntityId> moduleIds = relationshipsWithDepth.stream()
				.flatMap(t -> {
					final var relation = t.e1;
					return Stream.of(relation.getSrcModule(), relation.getDstModule());
				})
				.distinct()
				.map(EntityId::of)
				.collect(Collectors.toSet());

		/* The initial Module is always fetched */
		moduleIds.add(moduleId);
		final List<ModulePojo> modules = moduleDao.findModules(q -> q.byIds(moduleIds));
		final List<ModuleRelationshipPojo> relationships = relationshipsWithDepth.stream()
				.map(t -> t.e1)
				.collect(Collectors.toList());

		final Long moduleNid = getModuleNid(moduleId);
		final DependencyGraph graph = new DependencyGraph(modules, relationships, Set.of(moduleNid));

		if ( ! moduleNodeTypeFilter.isEmpty() || ! relationshipTypeFilter.isEmpty()) {
			graph.filterGraph(moduleNodeTypeFilter::contains, relationshipTypeFilter::contains);
		}
		return graph;
	}

	@Override
	public List<SchemaInfoPojo> findSchemaInfos(final EntityId project) {
		return moduleDao.findSchemaInfos(project);
	}

	@Override
	@Cacheable(cacheNames = "moduleStatistics", cacheResolver = "cacheResolver")
	public ModuleStatisticsResponse calculateStatistics(@ProjectIdArgument final EntityId project) {
		final ModuleStatisticsResponse moduleStatisticsResponse = new ModuleStatisticsResponse();
		moduleStatisticsResponse.setWithErrorsCount(Long.valueOf(countModules(b -> b.ofProject(project).withErrors())));
		moduleStatisticsResponse.setCount(Long.valueOf(countModules(b -> b.ofProject(project))));
		moduleStatisticsResponse.setSourceCodeLineCount(countSourceMetricsCodeLines(b -> b.ofProject(project).withRepresentation(Representation.PHYSICAL)));
		moduleStatisticsResponse.setModuleTechnologyList(countSourceMetricsCodeLinesByTechnology(b -> b.ofProject(project)));
		moduleStatisticsResponse.setSourceFileCount(Long.valueOf(countModules(b -> b.ofProject(project).withRepresentation(Representation.PHYSICAL))));
		moduleStatisticsResponse.setMissingCount(Long.valueOf(countModules(b -> b.ofProject(project).withIdentified(false))));
		return moduleStatisticsResponse;
	}

	@Override
	public EntityId getProject(final EntityId moduleId) {
		return moduleDao.getProject(moduleId).orElseThrow(() -> new MiningEntityNotFoundException(ModulePojo.class, moduleId.toString()));
	}

	@Override
	public Map<UUID, ModulePojo> mapModules(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findModules(builder).stream().collect(Collectors.toMap(ModulePojo::getUid, Function.identity()));
	}

	private static void handleNullBytes(final Definable<String> def) {
		final String text = def.orElse(null);
		if (text != null) {
			def.set(text.replace('\0', ' '));
		}
	}

	@Override
	public void createUndiscovered(final ModuleUndiscoveredPojoPrototype moduleUndiscovered) {
		moduleDao.putUndiscovered(moduleUndiscovered);
	}

	@Override
	public List<ModuleUndiscoveredPojo> findUndiscovered(final BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder) {
		return moduleDao.findUndiscovered(builder);
	}

	@Override
	public Paged<ModuleUndiscoveredPojo> findUndiscovered(final Pagination paging, final BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder) {
		return moduleDao.findUndiscovered(paging, builder);
	}

	@Override
	public long countUndiscovered(final BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder) {
		return moduleDao.countUndiscovered(builder);
	}

	@Override
	public int deleteUndiscovered(final BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder) {
		return moduleDao.deleteUndiscovered(builder);
	}
	
	@Override
	public int setFromDeadCode(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder, final boolean fromDeadCode) {
		return this.moduleReferenceDao.setFromDeadCode(builder, fromDeadCode);
	}

	@Override
	public int updateRelationshipProperties(final Map<String, Object> properties, final String dependencyAttributes, final UUID id) {
		return moduleReferenceDao.updateProperties(properties, dependencyAttributes, id);
	}

	@Override
	public List<Pair<UUID, String>> findModuleUUIDsAndDependencyHashes(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return moduleDao.findModuleUidsAndDependencyHashes(builder);
	}
}
