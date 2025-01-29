/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.ProfilingHelper.executeWithProfiling;

import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.jdbc.UncategorizedSQLException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Imports {@code module_relationship} entities from Discovery and sets parent relationships between {@code module} entities.
 */
public class DiscoveryReferenceImporter {

	private static final String PROFILING_CATEGORY = "discovery.referenceimporter";
	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);
	private static final Profiler PROFILER = ProfilingFactory.getProfilingSession().getProfiler(PROFILING_CATEGORY);

	private final ModuleService moduleService;
	private final Map<Long, EntityId> modulesNidToEid;
	private final Map<String, EntityId> missingModulesNameToId = new ConcurrentHashMap<>();
	@Nullable
	private final String jobId;
	@Nullable
	private final DiscoveryCache discoveryCache;
	@Nullable
	private final ModuleParameters moduleParameters;
	
	/**
	 * Public constructor.
	 * 
	 * @param moduleService the {@link ModuleService}
	 * @param modulesNidToEid the the map of module nid to module EntityId
	 * @param jobId the job Id
	 * @param discoveryCache the {@link DiscoveryCache}
	 * @param moduleParameters the additional module parameters
	 */
	public DiscoveryReferenceImporter(final ModuleService moduleService, final Map<Long, EntityId> modulesNidToEid, @Nullable final String jobId,
										@Nullable final DiscoveryCache discoveryCache, @Nullable final ModuleParameters moduleParameters) {
		this.moduleService = moduleService;
		this.modulesNidToEid = modulesNidToEid;
		this.jobId = jobId;
		this.discoveryCache = discoveryCache;
		this.moduleParameters = moduleParameters;
	}

	/**
	 * Imports an {@code module_relationship} entity from Discovery.
	 * 
	 * @param projectId the Project ID property
	 * @param source the source {@link ModelArtifact}
	 * @param target the target {@link ModelArtifact}
	 * @param targetName the target's Name property
	 * @param targetTechnology the target's Technology property
	 * @param targetType the target's Type property
	 * @param targetOrigin {@link Origin#ENVIRONMENT} for a <b>UTILITY</b>
	 * @param targetIdentification the {@link Identification} of the module
	 * @param binding the Binding property
	 * @param attributes the Attributes property
	 * @param relationship the type of {@link RelationshipType} to create
	 * @param fromLocation the from location of the dependency reference
	 * @param toLocation target's location
	 * @param checkForDuplicates {@code true} to check if the reference already exists. Otherwise {@code false}
	 * @return An {@link Optional} containing the UUID of the created {@code module_relationship} entity or an empty Optional if the reference could not be created
	 * or already existed
	 */
	public Optional<UUID> importReference(
			final EntityId projectId,
			final ModelArtifact source,
			final ModelArtifact target,
			final String targetName,
			final Technology targetTechnology,
			final Type targetType,
			final Origin targetOrigin,
			final Identification targetIdentification,
			final String binding,
			final String attributes,
			final RelationshipType relationship,
			@Nullable final ModuleLocation fromLocation,
			@Nullable final ModuleLocation toLocation,
			final boolean checkForDuplicates) {
		return importReference(projectId, source, target, targetName, targetTechnology, targetType, targetOrigin, targetIdentification, binding,
				attributes, relationship, fromLocation, toLocation, checkForDuplicates, null);

	}

	/**
	 * Imports an {@code module_relationship} entity from Discovery.
	 * 
	 * @param projectId the Project ID property
	 * @param source the source {@link ModelArtifact}
	 * @param target the target {@link ModelArtifact}
	 * @param targetName the target's Name property
	 * @param targetTechnology the target's Technology property
	 * @param targetType the target's Type property
	 * @param targetOrigin {@link Origin#ENVIRONMENT} for a <b>UTILITY</b>
	 * @param targetIdentification the {@link Identification} of the module
	 * @param binding the Binding property
	 * @param attributes the Attributes property
	 * @param relationship the type of {@link RelationshipType} to create
	 * @param fromLocation the from location of the dependency reference
	 * @param toLocation target's location
	 * @param checkForDuplicates {@code true} to check if the reference already exists. Otherwise {@code false}
	 * @param validIfReachedFrom conditional dependencies or null.
	 * @return An {@link Optional} containing the UUID of the created {@code module_relationship} entity or an empty Optional if the reference could not be created
	 * or already existed
	 */
	public Optional<UUID> importReference(
			final EntityId projectId, 
			final ModelArtifact source, 
			final ModelArtifact target, 
			final String targetName, 
			final Technology targetTechnology, 
			final Type targetType,
			final Origin targetOrigin,
			final Identification targetIdentification,
			final String binding, 
			final String attributes,
			final RelationshipType relationship, 
			@Nullable final ModuleLocation fromLocation,
			@Nullable final ModuleLocation toLocation,
			final boolean checkForDuplicates,
			@Nullable final List<EntityId> validIfReachedFrom) {

		PROFILER.start("importReference");
		try {
			final EntityId sourceModuleUid = source.getModuleId();
			if (sourceModuleUid == null) {
				LOG.error("Cannot determine Module uid for source module {}", source);
				throw new UnsupportedOperationException("Cannot determine Module uid for source module: " + source);
			}

			final Optional<EntityId> targetModuleUid = resolveTargetModuleId(projectId, -1l, target, targetName, target.getPath().orElse(null),
																		 targetTechnology, targetType, targetOrigin, targetIdentification);
			if ( ! targetModuleUid.isPresent()) {
				LOG.error("Cannot determine Module RID for target Module {}", targetName);
				return Optional.empty();
			}

			return executeWithProfiling(() -> createReference(sourceModuleUid, targetModuleUid.get(), relationship, binding, attributes, fromLocation, toLocation,
					checkForDuplicates, validIfReachedFrom), PROFILER, "createReference");
		} catch (final UncategorizedSQLException e) {
			throw new ConstraintViolationException("Reference", ExceptionUtils.getRootCauseMessage(e), e);
		} finally {
			PROFILER.stop();
		}
	}
	
	/**
	 * Imports an {@code module_relationship} entity from Discovery.
	 * 
	 * @param projectId the Project ID property
	 * @param sourceNid the source nid
	 * @param targetNid the target's nid
	 * @param targetName the target's Name property
	 * @param targetPath the target's Path property or null
	 * @param targetTechnology the target's Technology property
	 * @param targetType the target's Type property
	 * @param targetOrigin {@link Origin#ENVIRONMENT} for a <b>UTILITY</b>
	 * @param targetIdentification the {@link Identification} of the module
	 * @param binding the Binding property
	 * @param attributes the Attributes property
	 * @param relationship the type of {@link RelationshipType} to create
	 * @param fromLocation the from location of the dependency reference
	 * @param toLocation target's location
	 * @param checkForDuplicates {@code true} to check if the reference already exists. Otherwise {@code false}
	 * @param validIfReachedFrom conditional dependencies or null.
	 */
	public void importReference(
			final EntityId projectId, 
			final Long sourceNid, 
			final Long targetNid, 
			final String targetName,
			@Nullable final String targetPath,
			final Technology targetTechnology, 
			final Type targetType,
			final Origin targetOrigin,
			final Identification targetIdentification,
			final String binding, 
			final String attributes,
			final RelationshipType relationship, 
			@Nullable final ModuleLocation fromLocation,
			@Nullable final ModuleLocation toLocation,
			final boolean checkForDuplicates,
			final @Nullable List<EntityId> validIfReachedFrom) {

		PROFILER.start("importReference");
		try {
			final EntityId sourceModuleUid = modulesNidToEid.get(sourceNid);
			if (sourceModuleUid == null) {
				LOG.error("Cannot determine Module uid for source Module nid {}", sourceNid);
				return;
			}
			final Optional<EntityId> targetModuleUid = resolveTargetModuleId(projectId, targetNid, null, targetName, targetPath, targetTechnology, targetType,
					targetOrigin, targetIdentification);
			if ( ! targetModuleUid.isPresent()) {
				LOG.error("Cannot determine Module uid for target Module {}", targetName);
				return;
			}
			executeWithProfiling(() -> createReference(sourceModuleUid, targetModuleUid.get(), relationship, binding, attributes, fromLocation, toLocation,
														checkForDuplicates, validIfReachedFrom), PROFILER, "createReference");
		} catch (final UncategorizedSQLException e) {
			throw new ConstraintViolationException("Reference", ExceptionUtils.getRootCauseMessage(e), e);
		} finally {
			PROFILER.stop();
		}
	}

	/**
	 * Imports a Contains Module Edge from Discovery.
	 *
	 * @param projectId the id of the project 
	 * @param parentModule the module that contains the {@code module}
	 * @param module the module that is contained by the {@code parentModule}
	 */
	public void importContainsModule(final EntityId projectId, final ModelArtifact parentModule, final ModelArtifact module) {
		PROFILER.start("importContainsModule");
		try {
			final Optional<EntityId> moduleId = 
					resolveTargetModuleId(projectId, null, module, module.getName(), module.getPath().orElse(null),
							ResolveTargetHelper.toTechnology(module.getType()), ResolveTargetHelper.toType(module.getType()),
							assertNotNull(module.getOrigin()), module.getIdentification());
			if ( ! moduleId.isPresent()) {
				LOG.error("Cannot determine Module id for module {}", module.getName());
				return;
			}
			final EntityId parentModuleId = parentModule.getModuleId();
			if (parentModuleId == null) {
				LOG.error("Cannot determine parent module for module {}", moduleId);
			} else {
				importContainsModule(parentModuleId, moduleId.get());
			}
		} finally {
			PROFILER.stop();
		}
	}

	/**
	 * Imports a Contains Module Edge.
	 *
	 * @param parent the {@link EntityId} of the parent module to set
	 * @param module the {@link EntityId} of the module to update
	 */
	public void importContainsModule(final EntityId parent, EntityId module) {
		final var relationship = new ModuleRelationshipPojoPrototype()
				.setSrcModule(parent)
				.setDstModule(module)
				.setRelationship(RelationshipType.CONTAINS);

		moduleService.createRelationship(relationship);

		//Call update to recalculate the link hash after adding the link to the parent
		moduleService.update(new ModulePojoPrototype().withId(module));
	}

	private Optional<UUID> createReference(final EntityId srcModule, final EntityId dstModule, final RelationshipType relationship,  final String binding,
			final String attributes, @Nullable final ModuleLocation srcLocation, @Nullable final ModuleLocation dstLocation, final boolean checkForDuplicates,
			final @Nullable List<EntityId> validIfReachedFrom) {
		try {
			final Optional<Map<String, String>> propertiesMap = parseProperties(attributes);
			if ( ! propertiesMap.isPresent()) {
				return Optional.empty();
			}

			final var properties = Collections.<String, Object>unmodifiableMap(propertiesMap.get());
			final var reference = new ModuleRelationshipPojoPrototype();
			reference.setSrcModule(srcModule);
			reference.setDstModule(dstModule);
			reference.setRelationship(relationship);
			reference.setProperties(properties);
			if (srcLocation != null) {
				reference.setSrcLocation(srcLocation);
			}
			if (dstLocation != null) {
				reference.setDstLocation(dstLocation);
			}
			reference.setDependencyAttributes(attributes);
			reference.setDependencyBinding(Binding.fromName(binding));

			if (validIfReachedFrom != null && ! validIfReachedFrom.isEmpty()) {
				reference.setValidIfReachedFrom(validIfReachedFrom);
			}

			return moduleService.createRelationship(reference, checkForDuplicates);
		} catch (final UncategorizedSQLException e) {
			throw new ConstraintViolationException("Reference", ExceptionUtils.getRootCauseMessage(e), e);
		}
	}

	private Optional<EntityId> resolveTargetModuleId(final EntityId projectId, @Nullable final Long targetNid, @Nullable final ModelArtifact target, final String targetName,
			 @Nullable final String targetPath, final Technology targetTechnology, final Type targetType, final Origin origin, final Identification identification) {
		EntityId result = null;
		if (target != null && target.getModuleId() != null) {
			result = target.getModuleId();
		} else if (origin == Origin.ENVIRONMENT /* UTILITY */ || identification == Identification.MISSING) {
			try {
				result = executeWithProfiling(() -> calculateIdForMissingTargetModule(projectId, targetName, targetPath, targetTechnology, targetType, 
						origin, identification), PROFILER, "calculateIdForMissingTargetModule");
			} catch (final ConstraintViolationException | IllegalStateException e) {
				LOG.error(() -> "Error while creating custom utility for: " + targetName, e);
			}
		} else if (targetNid != null && modulesNidToEid.containsKey(targetNid)) {
			result = modulesNidToEid.get(targetNid);
		}
		return Optional.ofNullable(result);
	}
	
	private EntityId calculateIdForMissingTargetModule(final EntityId projectId, final String targetName, @Nullable final String targetPath,
			final Technology targetTechnology, final Type targetType, final Origin origin, final Identification identification) {
		/* WMIN-799: Use NOT ONLY the module name as lookup-key. */
		final var lookupKey = DiscoveryCache.createLockKeyForParameters(targetName, targetTechnology, targetType);
		final var computeCallable = getMissingModuleCallable(projectId, targetName, targetPath, targetTechnology, targetType, origin, identification);
		final var discoveryCacheLocal = discoveryCache;
		final var jobIdLocal = jobId;
		try {
			if (discoveryCacheLocal != null && jobIdLocal != null) {
				return (EntityId) discoveryCacheLocal.computeValueIfAbsent(jobIdLocal, lookupKey, computeCallable);
			} else {
				return missingModulesNameToId.computeIfAbsent(lookupKey, key -> {
					try {
						return computeCallable.call();
					} catch (final Exception e) {
						throw new IllegalStateException(e);
					}
				});
			}
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}
	
	private Callable<EntityId> getMissingModuleCallable(final EntityId projectId, final String targetName, @Nullable final String targetPath,
			final Technology targetTechnology, final Type targetType, final Origin origin, final Identification identification) {
		return () -> {
			if (origin == Origin.ENVIRONMENT /* UTILITY */) {
				LOG.info("Creating unknown utility with the name '{}'", targetName);
			} else if (identification == Identification.MISSING) {
				/* nothing to do */
			} else {
				throw new IllegalStateException(String.format("Could not determine uid for target. "
															+ "Name: %s, Path: %s, Technology: %s, Type: %s, Origin: %s, Identification: %s", 
															targetName, targetPath, targetTechnology, targetType, origin, identification));
			}

			if (identification == Identification.MISSING && targetTechnology == Technology.SQL && targetType == Type.TABLE) {
				return getOrCreateSqlTableModule(projectId, targetName);
			}

			final var module = new ModulePojoPrototype();
			module.setName(targetName);
			if (targetPath != null) {
				module.setPath(targetPath);
			}
			module.setProject(projectId);
			module.setTechnology(targetTechnology);
			module.setType(targetType);
			module.setStorage(Storage.from(targetTechnology, targetType));
			module.setIdentification(identification);
			module.setOrigin(origin);
			module.setCreator(Creator.DISCOVERY);
			if (moduleParameters != null) {
				final Instant metricsDate = moduleParameters.getMetricsDate();
				module.setMetricsDate(metricsDate);
				module.setModifiedDate(metricsDate);
			}
			if (moduleParameters != null) {
				final Instant metricsDate = moduleParameters.getMetricsDate();
				module.setMetricsDate(metricsDate); /* if null, the field is handled in dao layer */
				module.setModifiedDate(metricsDate);
			}

			if ("SQLCA".equals(targetName)) {
				module.setTechnology(Technology.COBOL);
				module.setType(Type.COPYBOOK);
				module.setStorage(Storage.from(Technology.COBOL, Type.COPYBOOK));

				final var sourceMetrics = new SourceMetricsPojoPrototype();
				sourceMetrics.setPhysicalLines(Integer.valueOf(0));
				sourceMetrics.setCodeLines(Integer.valueOf(0));
				sourceMetrics.setCommentLines(Integer.valueOf(0));
				sourceMetrics.setComplexityMcCabe(Integer.valueOf(0));
				sourceMetrics.setDeadCodeLines(Integer.valueOf(0));
				module.setSourceMetrics(sourceMetrics);
			}

			return moduleService.create(module);
		};
	}

	/**
	 * Special handling (workaround) for missing SQL_TABLEs: we check whether a VIEW with the same name exists
	 * If the VIEW exists then it is returned, otherwise the table module is newly created, i.e. tables are never marked as MISSING.
	 *
	 * @param projectId id of the project in which to create the table
	 * @param targetName the name of the table or view
	 * @return the existing view or newly created table
	 */
	private @Nullable EntityId getOrCreateSqlTableModule(final EntityId projectId, final String targetName) {
		final var views = moduleService.findModuleIds(b -> b.ofProject(projectId).withName(targetName).withTechnology(Technology.SQL).withType(Type.VIEW));
		if (views.size() == 1) {
			return views.get(0);
		} else {
			final var module = new ModulePojoPrototype();
			module.setName(targetName);
			module.setRepresentation(Representation.VIRTUAL);
			module.setProject(projectId);
			module.setTechnology(Technology.SQL);
			module.setType(Type.TABLE);
			module.setStorage(Storage.from(Technology.SQL, Type.TABLE));
			module.setIdentification(Identification.IDENTIFIED);
			module.setOrigin(Origin.CUSTOM);
			module.setCreator(Creator.DISCOVERY);
			if (moduleParameters != null) {
				final Instant metricsDate = moduleParameters.getMetricsDate();
				module.setMetricsDate(metricsDate);
				module.setModifiedDate(metricsDate);
			}
			return moduleService.create( module);
		}
	}

	private static Optional<Map<String, String>> parseProperties(final String attributes) {
		try {
			final Map<String, String> result = new HashMap<>();
			final var jsonNode = PojoMapper.jsonReader().readTree(attributes);
			new PropertyParser(jsonNode, result).parse();
			return Optional.ofNullable(result);
		} catch (final IllegalStateException | JsonProcessingException e) {
			LOG.error(e.getMessage(), e);
			return Optional.empty();
		}
	}

	private static class PropertyParser {
		
		private final JsonNode jsonNode;
		private final Map<String, String> result;
		
		private PropertyParser(final JsonNode jsonNode, final Map<String, String> result) {
			this.jsonNode = jsonNode;
			this.result = result;
		}
		
		private void parse() {
			final Iterator<Entry<String, JsonNode>> iter = jsonNode.fields();
			while (iter.hasNext()) {
				final Entry<String, JsonNode> entry = iter.next();
				final JsonNode value = entry.getValue();
				
				if (value.isObject()) {
					/* resolve object and discard key as required in WMIN-184 */
					new PropertyParser(value, result).parse();
				} else if (value.isArray()) {
					value.forEach(element -> {
						if (element.isArray()) {
							throw new IllegalStateException("Nested arrays in column Attributes not supported: " + value.toString());
						} else if (element.isObject()) {
							/* resolve object and discard key as required in WMIN-184 */
							new PropertyParser(element, result).parse();
						} else {
							result.merge(entry.getKey(), element.asText(), this::flatten);
						}
					});
				} else {
					result.put(entry.getKey(), value.asText());
				}
			}
		}
		
		private String flatten(final String first, final String second) {
			return String.join(",", first, second);
		}
	}
}
