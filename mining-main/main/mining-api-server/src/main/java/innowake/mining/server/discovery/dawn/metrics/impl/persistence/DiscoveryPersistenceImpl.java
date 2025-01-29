/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.persistence;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert.AssertionException;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.persistence.DiscoveryPersistence;
import innowake.mining.server.discovery.dawn.metrics.api.persistence.ImportResult;
import innowake.mining.shared.PatternConverter;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.ModuleLightInquiryBuilder;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.hashing.LinkHash;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.UncategorizedSQLException;
import org.springframework.stereotype.Service;

import javax.persistence.PersistenceException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static innowake.mining.server.discovery.dawn.metrics.impl.cache.DawnCacheConfiguration.FETCH_MODULES_CACHE;
import static innowake.mining.server.discovery.dawn.metrics.impl.cache.DawnCacheConfiguration.FETCH_MODULES_LIGHTWEIGHT_CACHE;
import static innowake.mining.server.discovery.dawn.metrics.impl.cache.DawnCacheConfiguration.FIND_MODULES_CACHE;

/**
 * DiscoveryPersistenceImpl is an implementation class of {@link DiscoveryPersistence}.
 */
@Service
@CacheConfig(cacheManager = "discoveryCacheManager")
public class DiscoveryPersistenceImpl implements DiscoveryPersistence {

	private static final EntityId DUMMY_RESULT = EntityId.of(0L);
	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryPersistenceImpl.class);

	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private SourceCachingService sourceService;

	private static final String ERROR_MSG = "Database Error occured while";
	private static final String ERROR_MSG_ON_PERSISTING = ERROR_MSG + " persisting %s %s";
	private static final String ERROR_MSG_ON_PERSISTING_MODULE_ID = ERROR_MSG_ON_PERSISTING + " for ModuleId %s";

	@Override
	@Cacheable(cacheNames = FIND_MODULES_CACHE)
	public List<EntityId> findModules(final DiscoveryContext context, final ModuleFilter filter, final ResolutionFlag... flags) {
		if (filter.getNames().isEmpty() && filter.getPaths().isEmpty() && filter.getTypes().isEmpty() && filter.getPathPatterns().isEmpty()
				&& filter.getModuleIds().isEmpty() &&  ! filter.getContainedIn().isPresent() && ! filter.getNot().isPresent()
				&& filter.getMetricsDate().isEmpty() && filter.getOrigin().isEmpty() && ! filter.isPhysical()) {
			return Collections.emptyList();
		}
		final var resolutionFlags = new HashSet<>(Arrays.asList(flags));
		
		final Set<EntityId> moduleIds = filter.getModuleIds();
		if ( ! (moduleIds.isEmpty() || filter.getNot().isPresent())) {
			return resolutionFlags.contains(ResolutionFlag.RESOLVE_TO_PARENT) ? findParentModuleIds(moduleIds) : new ArrayList<>(moduleIds);
		}

		final var ids = moduleService.findModuleIds(q -> {
			boolean caseInsensitiveNames = false;
			if (filter.getNames().isEmpty()) {
				caseInsensitiveNames = true;
			} else {
				for (final ResolutionFlag flag : flags) {
					if (flag == ResolutionFlag.RESOLVE_CASE_INSENSITIVE) {
						caseInsensitiveNames = true;
						break;
					}
				}
			}

			buildQuery(q, filter, context.getProjectId(), caseInsensitiveNames);

			filter.getContainedIn().ifPresent(moduleFilter -> q.withSourceRelationshipsFrom(p -> buildQuery(p, moduleFilter, context.getProjectId(), false),
																								RelationshipType.CONTAINS));

			filter.getNot().ifPresent(not -> q.notByIds(not.getModuleIds()));
		});

		return resolutionFlags.contains(ResolutionFlag.RESOLVE_TO_PARENT) ? findParentModuleIds(ids) : ids;
	}

	/**
	 * @param moduleIds child modules to find the parents for
	 * @return list of all parent modules for given list
	 */
	private List<EntityId> findParentModuleIds(final Collection<EntityId> moduleIds) {
		switch (moduleIds.size()) {
			case 0:
				return Collections.emptyList();
			case 1:
				return moduleService.findModuleIds(q -> q.withDestinationRelationshipsTo(moduleIds.iterator().next(), RelationshipType.CONTAINS));
			default:
				return moduleService.findModuleIds(q -> q.withDestinationRelationshipsTo(q2 -> q2.byIds(moduleIds), RelationshipType.CONTAINS));
		}
	}

	private void buildQuery(final ModuleLightInquiryBuilder<?> builder, final ModuleFilter filter, final EntityId projectId, final boolean caseInsensitiveNames) {
		builder.ofProject(projectId);

		final Set<EntityId> ids = filter.getModuleIds();
		if ( ! ids.isEmpty()) {
			builder.byIds(ids);
		} else {
			final Set<String> names = filter.getNames();
			if ( ! names.isEmpty()) {
				builder.withNames(names, caseInsensitiveNames);
			}

			final Set<ModuleType> types = filter.getTypes();
			if ( ! types.isEmpty()) {
				builder.withTechnologiesAndTypes(types.stream()
														.map(t -> new Tuple2<Technology, Type>(t.getTechnology(), t.getType()))
														.collect(Collectors.toList()));
			}

			final Set<String> paths = filter.getPaths();
			if ( ! paths.isEmpty()) {
				/* like in the IoDao.findByNamePathTechnologyAndType() we also have to include the path of the parent module when searching for path matches */
				builder.withPathsSelfOrContaining(projectId, paths);
			}

			final Set<String> pathPatterns = filter.getPathPatterns().stream().map(PatternConverter::convertAntToRegexPattern).collect(Collectors.toSet());
			if ( ! pathPatterns.isEmpty()) {
				/* like in the IoDao.findByNamePathTechnologyAndType() we also have to include the path of the parent module when searching for path matches */
				builder.withPathPatternsSelfOrContaining(projectId, pathPatterns);
			}

			if (filter.isPhysical()) {
				builder.withRepresentation(ModulePojo.Representation.PHYSICAL);
			}
			filter.getMetricsDate().ifPresent(builder::withMetricsDate);
			filter.getOrigin().ifPresent(builder::withOrigin);
			filter.getIdentification().ifPresent(identification -> builder.withIdentified(identification == Identification.IDENTIFIED));
		}
	}

	@Override
	@Cacheable(cacheNames = FETCH_MODULES_CACHE)
	public List<ModulePojo> fetchModules(final DiscoveryContext context, final List<EntityId> moduleIds) {
		return moduleService.findModules(b -> b.byIds(moduleIds));
	}

	@Override
	@Cacheable(cacheNames = FETCH_MODULES_LIGHTWEIGHT_CACHE)
	public List<ModuleLightweightPojo> fetchModulesLightWeight(final DiscoveryContext context, final List<EntityId> moduleIds) {
		return moduleService.findModulesLightweight(b -> b.byIds(moduleIds));
	}

	@Override
	public ImportResult<EntityId> persistModule(final DiscoveryContext context, final ModuleFilter moduleFilter, final ModulePojoPrototype moduleDefinition,
			final ResolutionFlag... flags) {
		try {
			final List<EntityId> moduleIds = findModules(context, moduleFilter, flags);
			final List<EntityId> containedInModule;

			if (moduleIds.size() > 1) {
				LOG.error(() -> String.format("Unable to persist module: Found more than one module having moduleIds: %s matching with the moduleFilter: %s", moduleIds, moduleFilter));
				return ImportResult.forAmbiguousMatch("More than one Module found for module filter: " + moduleFilter);
			}

			final Optional<ModuleFilter> filter = moduleFilter.getContainedIn();
			if (filter.isPresent()) {
				final ModuleFilter containedInFilter = filter.get();
				containedInModule = findModules(context, containedInFilter);
				if (containedInModule.isEmpty()) {
					LOG.error(() -> "Unable to persist module: No containedIn module found for the containedIn filter: " + containedInFilter);
					return ImportResult.forAmbiguousMatch("No containedIn Module found for containedIn filter: " + containedInFilter);
				}
				if (containedInModule.size() > 1) {
					LOG.error(() -> String.format("Unable to persist module: Found more than one containedIn module having moduleIds: %s with the containedIn filter: %s", containedInModule, containedInFilter));
					return ImportResult.forAmbiguousMatch("More than one containedIn Module found with Ids: " + containedInModule + " for containedIn filter: " + containedInFilter);
				}
			} else {
				containedInModule = Collections.emptyList();
			}

			/* Updates the module only if ModuleDefinition has any values to update */
			final EntityId moduleId;
			if ( ! moduleIds.isEmpty() && isAllFieldsNull(moduleDefinition)) {
				moduleId = moduleIds.get(0);
				if (moduleDefinition.sourceMetrics.isPresent()) {
					final SourceMetricsPojoPrototype sourceMetrics = moduleDefinition.sourceMetrics.getNonNull();
					sourceMetrics.setModule(moduleId);
					moduleService.putSourceMetrics(sourceMetrics);
				}
			} else {
				moduleId = storeModuleInDB(context, moduleDefinition, moduleIds, containedInModule);
			}


			/* Only create an containsModule edge if we are creating a new module and containedInModule exists */
			if ( ! containedInModule.isEmpty() && moduleIds.isEmpty()) {
				createContainsModuleEdge(moduleId, containedInModule.get(0), moduleDefinition.location.orElse(null));
			}

			return moduleIds.isEmpty() ? ImportResult.forSuccessfulCreation(moduleId) : ImportResult.forSuccessfulUpdate(moduleId);
		} catch (final Exception e ) {
			Throwable rootCause = ExceptionUtils.getRootCause(e);
			if (rootCause == null) {
				rootCause = e;
			}
			LOG.error(String.format(ERROR_MSG_ON_PERSISTING, "Module", moduleDefinition), rootCause);
			return ImportResult.forDbError(String.format(ERROR_MSG_ON_PERSISTING, "Module", moduleDefinition), rootCause);
		}
	}

	private static boolean isAllFieldsNull(final ModulePojoPrototype module) {
		return module.identification.orElse(null) == null &&
				module.location.orElse(null) == null &&
				module.technology.orElse(null) == null &&
				module.type.orElse(null) == null &&
				module.name.orElse(null) == null &&
				module.origin.orElse(null) == null &&
				module.path.orElse(null) == null &&
				module.representation.orElse(null) == null &&
				module.storage.orElse(null) == null;
	}

	@Override
	public List<ImportResult<EntityId>> persistStatements(final DiscoveryContext context, final EntityId moduleId, final ModulePojoPrototype module,
			final List<StatementPojoPrototype> statements) {
		if (statements.isEmpty()) {
			return Collections.emptyList();
		}
		final List<ImportResult<EntityId>> importResults = new ArrayList<>(statements.size());
		try {
			final List<StatementPojoPrototype> validateStatements = validateStatementAndSetTechnology(statements, module, moduleId, importResults);
			moduleService.createStatements(validateStatements);
			importResults.add(ImportResult.forSuccessfulCreation(DUMMY_RESULT));
		} catch (final Exception e) {
			final var msg = String.format(ERROR_MSG_ON_PERSISTING_MODULE_ID, "Statements", StringUtils.EMPTY, moduleId);
			LOG.error(msg, ExceptionUtils.getRootCause(e));
			importResults.add(ImportResult.forDbError(msg, ExceptionUtils.getRootCause(e)));
		}
		return importResults;
	}

	private List<StatementPojoPrototype> validateStatementAndSetTechnology(final List<StatementPojoPrototype> statements, final ModulePojoPrototype module,
			final EntityId moduleId, final List<ImportResult<EntityId>> importResults) {
		final List<StatementPojoPrototype> validatedStatements = new ArrayList<>(statements.size());
		for (final var statement : statements) {
			try {
				statement.setModule(moduleId);
				final var technology = statement.technology.orElse(null);
				if (technology == null) {
					setTechnologyToStatement(module, moduleId, statement);
				}
				validatedStatements.add(statement);
			} catch (final Exception e) {
				final var msg = String.format(ERROR_MSG_ON_PERSISTING_MODULE_ID, "Statement", statements, moduleId);
				LOG.error(msg, ExceptionUtils.getRootCause(e));
				importResults.add(ImportResult.forDbError(msg, ExceptionUtils.getRootCause(e)));
			}
		}

		return validatedStatements;
	}

	private void setTechnologyToStatement(final ModulePojoPrototype module, final EntityId moduleId, final StatementPojoPrototype statement) {
		Technology technology;
		final Map<String, Object> properties = statement.properties.orElse(null);

		if (properties != null && properties.keySet().stream().anyMatch(StatementPojo.SQL_PROPERTY_KEYS.keySet()::contains)) {
			for (final String key : StatementPojo.SQL_PROPERTY_KEYS.keySet()) {
				if (properties.get(key) == null) {
					throw new IllegalArgumentException(
							"The SQL property: " + key + " must not be null in SQL statement: " + statement.toString() + ", Module: " + moduleId);
				}
			}

			technology = Technology.SQL;
		} else {
			technology = module.technology.orElse(null);
		}
		/* When statements are created by anchor tasks (like in Batch), the module might not be loaded :( */
		if (technology == null) {
			technology = moduleService.getModuleLightweight(moduleId).getTechnology();
			module.setTechnology(technology);
		}
		statement.setTechnology(technology);
	}

	@Override
	public List<ImportResult<EntityId>> persistErrors(final DiscoveryContext context, final EntityId moduleId, final List<ErrorMarker> errors) {
		try {
			moduleService.createErrorMarkers(
					errors.stream().map(errorMarker -> errorMarker.convertToPojoPrototype().setModule(moduleId).setProject(context.getProjectId()))
							.collect(Collectors.toList()));
			return Collections.singletonList(ImportResult.forSuccessfulCreation(DUMMY_RESULT));
		} catch (final Exception e) {
			final var msg = String.format(ERROR_MSG_ON_PERSISTING_MODULE_ID, "Errors", StringUtils.EMPTY, moduleId);
			LOG.error(msg, ExceptionUtils.getRootCause(e));
			return Collections.singletonList(ImportResult.forDbError(msg, ExceptionUtils.getRootCause(e)));
		}
	}

	@Override
	public List<ImportResult<EntityId>> persistDeadCode(final DiscoveryContext context, final EntityId moduleId,
			final List<ModuleDeadCodePojoPrototype> deadCodes) {
		try {
			moduleService.createDeadCodes(deadCodes.stream().map(deadCode -> deadCode.setModule(moduleId)).collect(Collectors.toList()));
			return Collections.singletonList(ImportResult.forSuccessfulCreation(DUMMY_RESULT));
		} catch (final Exception e) {
			final var msg = String.format(ERROR_MSG_ON_PERSISTING_MODULE_ID, "deadCodes", StringUtils.EMPTY, moduleId);
			LOG.error(msg, ExceptionUtils.getRootCause(e));
			return Collections.singletonList(ImportResult.forDbError(msg, ExceptionUtils.getRootCause(e)));
		}
	}

	@Override
	public ImportResult<EntityId> createDependency(final DiscoveryContext context, final EntityId fromModuleId, final EntityId toModuleId,
			@Nullable final UUID dependencyDefinitionId, @Nullable final ModuleLocation location, final RelationshipType relationshipType,
			final Binding bindingType, final Map<String, Object> attributes, final List<ModuleFilter> reachedFromModules) {
		try {
			final var moduleReference = new ModuleRelationshipPojoPrototype()
					.setSrcModule(fromModuleId)
					.setDstModule(toModuleId)
					.setDependencyDefinition(dependencyDefinitionId)
					.setDependencyBinding(bindingType)
					.setDependencyAttributes(objectMapper.writeValueAsString(attributes))
					.setProperties(attributes)
					.setRelationship(relationshipType);

			if (location != null) {
				moduleReference.setSrcLocation(location);
			}
			
			if ( ! reachedFromModules.isEmpty()) {
				final var reachedFromModulesId = reachedFromModules.stream()
						.map(filter -> findModules(context, filter, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR))
						.flatMap(List::stream).collect(Collectors.toList());
				moduleReference.setValidIfReachedFrom(reachedFromModulesId);
			}

			moduleService.createRelationship(moduleReference);
			return ImportResult.forSuccessfulCreation(fromModuleId);
		} catch (final UncategorizedSQLException | EmptyResultDataAccessException e) {
			final String errorMsg = ERROR_MSG + " creating dependency from moduleId %s to moduleId %s";
			LOG.error(String.format(errorMsg, fromModuleId, toModuleId), ExceptionUtils.getRootCause(e));
			return ImportResult.forDbError(String.format(errorMsg, fromModuleId, toModuleId), ExceptionUtils.getRootCause(e));
		} catch (final JsonProcessingException e) {
			throw new IllegalArgumentException("While persisting dependency attributes to the database: the attributes cannot be represented as JSON", e);
		}
	}

	@Override
	public List<ImportResult<UUID>> persistDependencyDefinitions(final DiscoveryContext context, final EntityId moduleId,
			final List<DependencyDefinitionPojoPrototype> dependencyDefinitions) {

		dependencyDefinitions.forEach(dependencyDefinition -> dependencyDefinition.setModule(moduleId));

		try {
			return moduleService.createDependencyDefinitions(dependencyDefinitions).stream()
					.map(ImportResult::forSuccessfulCreation)
					.collect(Collectors.toList());
		} catch (final Exception e) {
			final var msg = String.format(ERROR_MSG_ON_PERSISTING_MODULE_ID, Integer.valueOf(dependencyDefinitions.size()), "dependency definitions", moduleId);
			LOG.error(msg, ExceptionUtils.getRootCause(e));
			return List.of(ImportResult.forDbError(msg, ExceptionUtils.getRootCause(e)));
		}
	}

	@Override
	public ImportResult<Long> markDependencyDefinitionResolved(final UUID dependencyDefinitionId) {
		try {
			moduleService.setDependencyDefinitionResolved(dependencyDefinitionId, true);
			return ImportResult.forSuccessfulUpdate(0L);
		} catch (final UncategorizedSQLException | AssertionException e) {
			LOG.error("Failed to update dependency definitions to resolved as true.", ExceptionUtils.getRootCause(e));
			return ImportResult.forDbError("Failed to update dependency definitions to resolved as true.", ExceptionUtils.getRootCause(e));
		}
	}

	@Override
	public List<EntityId> getModulesWithUnresolvedDependencies(final EntityId projectId) {
		return moduleService.findDependencyDefinitionModuleIds(q -> q.ofProject(projectId).withResolved(false));
	}

	@Override
	public List<DependencyDefinitionPojo> fetchUnresolvedDependencyDefinitions(final EntityId moduleId) {
		return moduleService.findDependencyDefinitions(q -> q.ofModule(moduleId).withResolved(false));
	}

	@Override
	public Set<EntityId> getResolvedTargetsForDependency(final UUID dependencyDefinitionId) {
		return moduleService.findRelationship(q -> q.ofDependencyDefinition(dependencyDefinitionId)).stream()
				.map(ModuleRelationshipPojo::getDstModule)
				.map(EntityId::of)
				.collect(Collectors.toSet());
	}

	@Override
	public List<EntityId> fetchModuleIdsWithMergeDuplicates(final EntityId projectId) {
		return moduleService.findDependencyDefinitionModuleIds(q -> q.ofProject(projectId).withResolved(false)
				.withFlag(ResolutionFlag.MERGE_DUPLICATES)
				.havingCountGreaterThan(1));
	}

	@Override
	public ImportResult<Integer> deleteDependencyDefinitions(final List<UUID> dependencyDefinitionIds) {
		final var deleted =  moduleService.deleteDependencyDefinitions(dependencyDefinitionIds);
		return ImportResult.forSuccessfulUpdate(deleted);
	}

	private EntityId storeModuleInDB(final DiscoveryContext context, final ModulePojoPrototype moduleDefinition,
			final List<EntityId> moduleIds, final List<EntityId> containedInModule) {
		final Long projectId = context.getProjectId().getNid();

		final var path = moduleDefinition.path.orElse(null);
		if (path != null) {
			try {
				final var sourceObject = sourceService.cachingByProjectPath(projectId, path);
				moduleDefinition.setSource(sourceObject.identity());
			} catch (final PersistenceException e) {
				/* ignore if not found */
			}
		}

		moduleDefinition.setProject(context.getProjectId());
		moduleDefinition.setMetricsDate(context.getModuleParameters().getMetricsDate());
		moduleDefinition.setModifiedDate(Instant.now());
		if ( ! containedInModule.isEmpty()) {
			moduleDefinition.setParent(containedInModule.get(0));
		}

		if (moduleIds.isEmpty()) {
			moduleDefinition.setCreator(Creator.DISCOVERY);
			moduleDefinition.setLinkHash(getLinkHashForModule(context, moduleDefinition, containedInModule));
			return moduleService.create(moduleDefinition);
		}

		moduleDefinition.setUid(moduleIds.get(0).getUid());
		moduleDefinition.setNid(moduleIds.get(0).getNid());
		
		return moduleService.update(moduleDefinition);
	}

	private String getLinkHashForModule(final DiscoveryContext context, final ModulePojoPrototype moduleDefinition, final List<EntityId> containedInModuleId) {
		final Optional<ModuleLightweightPojo> containedInModule = containedInModuleId.isEmpty() ? Optional.empty()
																		: fetchModulesLightWeight(context, containedInModuleId).stream().findFirst();
		if ( ! moduleDefinition.name.isPresent()) {
			throw new IllegalArgumentException("Name must be present");
		}
		if ( ! moduleDefinition.technology.isPresent()) {
			throw new IllegalArgumentException("Technology must be present");
		}
		if ( ! moduleDefinition.type.isPresent()) {
			throw new IllegalArgumentException("Type must be present");
		}

		final String name = moduleDefinition.name.getNonNull();
		final Technology technology = moduleDefinition.technology.getNonNull();
		final Type type = moduleDefinition.type.getNonNull();
		final String modulePath = moduleDefinition.path.orElse(null);
		final String containingModulePath = containedInModule.map(ModuleLightweightPojo::getPath).orElse(null);
		final String containingModuleLinkHash = containedInModule.map(ModuleLightweightPojo::getLinkHash).orElse(null);
		return LinkHash.calculateLinkHash(name, technology.toString(), type.toString(), modulePath, containingModulePath, containingModuleLinkHash);
	}
	
	private void createContainsModuleEdge(final EntityId moduleId, final EntityId containedInModuleId, @Nullable final ModuleLocation location) {
		final var relationship = new ModuleRelationshipPojoPrototype()
				.setSrcModule(containedInModuleId)
				.setDstModule(moduleId)
				.setRelationship(RelationshipType.CONTAINS);

		if (location != null) {
			relationship.setSrcLocation(location);
		}

		moduleService.createRelationship(relationship);
	}
}
