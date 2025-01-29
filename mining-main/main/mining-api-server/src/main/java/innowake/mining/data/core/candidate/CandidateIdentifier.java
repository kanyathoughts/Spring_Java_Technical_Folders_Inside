/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.candidate;

import static innowake.mining.data.core.annotation.api.AnnotationIdentifier.getAnnotationIdentifier;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.CandidateIdentificationResult;
import innowake.mining.data.core.annotation.AnnotationCandidates;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.core.datadictionary.AstNodeToDataDictionaryEntryUtil;
import innowake.mining.data.core.datadictionary.impl.CicsDataDictionaryIdentifier;
import innowake.mining.data.core.datadictionary.impl.CobolDataDictionaryIdentifier;
import innowake.mining.data.core.datadictionary.impl.DataDictionaryIdentifier;
import innowake.mining.data.core.datadictionary.impl.NaturalDataDictionaryIdentifier;
import innowake.mining.data.core.datadictionary.impl.Pl1DataDictionaryIdentifier;
import innowake.mining.data.core.storeast.DefaultStoreAstExecutor;
import innowake.mining.data.core.storeast.api.StoreAstExecutor;
import innowake.mining.data.core.taxonomy.DependencyModuleDataFetcher;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;

/**
 * Provides entry points for candidate identification.
 */
public class CandidateIdentifier {

	private static final Logger LOG = LoggerFactory.getLogger(CandidateIdentifier.class);
	private static final Map<String, Long> ANNOTATION_CATEGORY_MAP = new HashMap<>();
	private static final EntityId DEFAULT_PROJECT = EntityId.of(0L);

	private final EntityId moduleId;
	private final StoreAstExecutor storeAstExecutor;
	private final Set<EntityId> updatedModuleIds = new HashSet<>();
	private final MiningDataCoreService core;
	private final String jobId;

	/**
	 * The Constructor.
	 * 
	 * @param moduleId the Id of the module for which candidates should be identified
	 * @param storeAstExecutor the {@link StoreAstExecutor} to create the AST if it's not already present in the database
	 * @param core the instance of {@linkplain MiningDataCoreService}
	 * @param jobId the Id of the job this instance was created for
	 */
	public CandidateIdentifier(final EntityId moduleId, final StoreAstExecutor storeAstExecutor,
			final MiningDataCoreService core, final String jobId) {
		this.moduleId = moduleId;
		this.storeAstExecutor = storeAstExecutor;
		this.core = core;
		this.jobId = jobId;
	}

	/**
	 * Starts the complete candidate identification for the given {@code moduleId} and returns the {@link CandidateIdentificationResult}.
	 *
	 * @param identifyOnlyDDE boolean to run identification of DDE only and skip annotation identification.
	 * @return the {@link CandidateIdentificationResult}
	 */
	public CandidateIdentificationResult identify(final boolean identifyOnlyDDE) {
		LOG.info("Starting candidate identification process for module with the ID: {}", moduleId);
		long overallIdentified = 0;
		long overallStored = 0;
		final ModuleLightweightPojo module = core.moduleService.findAnyModuleLightweight(q -> q.byId(moduleId)).orElseThrow(
				() -> new IllegalStateException("Unable to load Module for ID : " + moduleId));
		final AstNodePojo rootAstNode = getRootAstNode(module);
		try {
			if (rootAstNode != null) {
				LOG.debug("Starting data dictionary identification process for module with the ID {}, linkHash {}", moduleId, module.getLinkHash());
				final Tuple2<Long, Long> dataDictionaryEntryCounts = identifyDataDictionaryEntries(module, rootAstNode);
				overallIdentified += dataDictionaryEntryCounts.a.longValue();
				overallStored += dataDictionaryEntryCounts.b.longValue();
				LOG.debug("Identified {} data dictionaries for module with the ID: {}", dataDictionaryEntryCounts.a, moduleId);

				if (overallIdentified > 0) {
					LOG.debug("Starting business variable identification process for module with the ID {}, linkHash {}", moduleId, module.getLinkHash());
					final Tuple2<Long, Long> businessVariableCount = identifyBusinessVariables(module, rootAstNode);
					overallIdentified += businessVariableCount.a.longValue();
					overallStored += businessVariableCount.b.longValue();
					LOG.debug("Identified {} business variables for module with the ID: {}", businessVariableCount.a, moduleId);
				}

				if ( ! identifyOnlyDDE) {
					LOG.debug("Starting annotation candidate identification process for module with the ID {}, linkHash {}", moduleId, module.getLinkHash());
					final Tuple2<Long, Long> annotationCounts = identifyAnnotationCandidates(module, rootAstNode, AnnotationType.DATABASE, AnnotationType.RULE);
					overallIdentified += annotationCounts.a.longValue();
					overallStored += annotationCounts.b.longValue();
					LOG.debug("Identified {} annotation candidates for module with the ID {}, linkHash {}", annotationCounts.a, moduleId, module.getLinkHash());
				}

				LOG.debug("Starting business variable updating process for module with the ID {}, linkHash {}", moduleId, module.getLinkHash());
				final Tuple2<Long, Long> updatedBusinessVariableCount = updateBusinessVariables(module, rootAstNode);
				overallIdentified += updatedBusinessVariableCount.a.longValue();
				overallStored += updatedBusinessVariableCount.b.longValue();
				LOG.debug("Checked {} business variables for module with the ID: {}", updatedBusinessVariableCount.a, moduleId);
			}
		} catch (final IllegalArgumentException e) {
			final String message = e.getMessage();
			if (message.contains("supported")) {
				LOG.error(message, e);
			} else {
				throw e;
			}
		} finally {
			if ( ! updatedModuleIds.isEmpty()) {
				core.moduleService.updateModules(q -> q.byIds(updatedModuleIds), new ModulePojoPrototype().setModifiedDate(Instant.now()));
				LOG.debug(() -> String.format("Module IDs that has been updated : %s", updatedModuleIds));
			}
		}
		LOG.info("Identified {} candidates for module {} with the ID {}, linkHash {}", overallIdentified, module.getName(), moduleId, module.getLinkHash());
		return new CandidateIdentificationResult(overallIdentified, overallStored);
	}
	
	/**
	 * Returns the root AST node for the given {@code moduleId}
	 *
	 * @param module the module object
	 * @return the root AST node or {@code null}
	 */
	@Nullable
	private AstNodePojo getRootAstNode(final ModuleLightweightPojo module) {
		final Optional<AstNodePojo> rootNode = core.getAstRootOrCreateExceptInclusions(module.identity(), storeAstExecutor);
		if ( ! rootNode.isPresent()) {
			LOG.error("Could not determine AST for module {} with ID {} and linkHash {}", module.getName(), module.getId(), module.getLinkHash());
			return null;
		}
		return rootNode.orElse(null);
	}
	
	/**
	 * Identifies the annotation candidates for the given {@code moduleId} and annotation {@code types}.
	 * 
	 * @param module the module object
	 * @param rootAstNode the root AST node
	 * @param types the {@linkplain AnnotationType AnnotationTypes}
	 * @return {@link Tuple2} providing the identified and stored candidates
	 */
	private Tuple2<Long, Long> identifyAnnotationCandidates(final ModuleLightweightPojo module, final AstNodePojo rootAstNode, final AnnotationType... types) {
		final List<AnnotationPojoTemplate> annotationCandidates = new ArrayList<>();
		final AnnotationCandidates candidates = new AnnotationCandidates();
		final Technology technology = module.getTechnology();

		if (ANNOTATION_CATEGORY_MAP.isEmpty()) {
			for (var category : core.annotationService.findCategories(q -> q.ofProject(DEFAULT_PROJECT)
																			.withTypes(Arrays.asList(AnnotationType.RULE, AnnotationType.DATABASE)))) {
				ANNOTATION_CATEGORY_MAP.put(category.getName(), category.getId());
			}
		}

		final AstNodeToDataDictionaryEntryUtil dao = new AstNodeToDataDictionaryEntryUtil(new DefaultStoreAstExecutor(), core);
		final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> ddeAstNodeMap = new HashMap<>();
		dao.findConnections(rootAstNode.getModule()).forEach(
				conn -> ddeAstNodeMap.computeIfAbsent(conn.b.getIsBusiness().orElse(false), v -> new HashMap<>())
						.computeIfAbsent(conn.b.getName(), v -> new ArrayList<>()).add(conn));

		List<AnnotationPojoTemplate> annotations;
		for (final AnnotationType type : types) {
			switch (type) {
				case DATABASE:
					annotations = getAnnotationIdentifier(type)
										.identify(rootAstNode, ANNOTATION_CATEGORY_MAP, technology, Collections.emptyMap());
					annotationCandidates.addAll(annotations);
					break;
				case RULE:
					annotations = getAnnotationIdentifier(type)
										.identify(rootAstNode, ANNOTATION_CATEGORY_MAP, technology, ddeAstNodeMap);
					annotations = filterExcludedBrCandidates(core, annotations);
					annotationCandidates.addAll(annotations);
					break;
				default:
					break;
			}
		}

		final Long annotationStoreCount = candidates.store(module.identity(), annotationCandidates, core);
		updatedModuleIds.add(module.identity());
		return Tuple2.of(Long.valueOf(annotationCandidates.size()), annotationStoreCount);
	}
	
	/**
	 * Identifies business related {@linkplain DataDictionaryPojo DataDictionaryEntries} for given Module ID.
	 *
	 * @param root the root AST node of the Module
	 * @param module the module object
	 * @return {@link Tuple2} providing the identified and stored candidates
	 */
	private Tuple2<Long, Long> identifyBusinessVariables(final ModuleLightweightPojo module, final @Nullable AstNodePojo root) {
		final var includes = new ArrayList<>(core.moduleService.findRelatedModules(module.identity(), RelationshipType.INCLUDES, RelationshipDirection.OUT));
		includes.add(module.getUid());
		final Map<String, List<DataDictionaryPojo>> ddeNameMap = core.dataDictionaryService.find(q -> q.ofModuleUuids(includes)).stream()
				.collect(Collectors.toMap(
						DataDictionaryPojo::getName,
						dde -> { 
							final List<DataDictionaryPojo> list = new ArrayList<>();
							list.add(dde);
							return list;
						},
						(v1, v2) -> {
							v1.addAll(v2);
							return v1;
						}));

		final Set<EntityId> ids = new HashSet<>(getBusinessVariableIdentifier(
				module, root == null ? getRootAstNode(module) : root, ddeNameMap)
						.getBusinessDataDictionaryIds().values());
		core.dataDictionaryService.markAsBusinessVariables(ids);
		return Tuple2.of(Long.valueOf(ids.size()), Long.valueOf(ids.size()));
	}

	private Tuple2<Long, Long> updateBusinessVariables(final ModuleLightweightPojo module, final @Nullable AstNodePojo root) {
		final var includes = core.moduleService.findRelatedModules(module.identity(), RelationshipType.INCLUDES, RelationshipDirection.OUT)
						.stream().map(EntityId::of).collect(Collectors.toList());
		includes.add(moduleId);
		final Map<String, List<DataDictionaryPojo>> ddeNameMap = core.dataDictionaryService.find(q -> q.ofModules(includes)).stream()
				.collect(Collectors.toMap(
						DataDictionaryPojo::getName,
						dde -> {
							final List<DataDictionaryPojo> list = new ArrayList<>();
							list.add(dde);
							return list;
						},
						(v1, v2) -> {
							v1.addAll(v2);
							return v1;
						}));

		final Set<EntityId> ids = new HashSet<>(getBusinessVariableIdentifier(
				module, root == null ? getRootAstNode(module) : root, ddeNameMap).getBusinessDataDictionaryIds().values());

		final int updates = core.dataDictionaryService.update(q -> q.byIds(ids)
																	.withIsBusiness(true)
																	.notOfAnnotations(), 
															  new DataDictionaryPojoPrototype()
																	  .setUpdatedByUserId("admin")
																	  .setIsBusiness(false));
		
		return Tuple2.of(Long.valueOf(ids.size()), Long.valueOf(updates));
	}

	private List<AnnotationPojoTemplate> filterExcludedBrCandidates(final MiningDataCoreService core, final List<AnnotationPojoTemplate> annotations) {
		final Map<EntityId, List<AnnotationPojo>> exclusions = new HashMap<>();
		return annotations.stream()
				.filter(annotation -> {
					/* Get all EXCLUDE annotations. If there are none or if none overlaps with the current processed BR candidate annotation,
					 * then the annotation is valid and is not filtered */
					final List<AnnotationPojo> excludes = exclusions.computeIfAbsent(annotation.module.getNonNull(),
																					 module -> core.annotationService.find(q -> q.ofModule(module)
																																 .withType(AnnotationType.EXCLUDE)));
					if ( ! excludes.isEmpty()) {
						final ModuleLocation location = annotation.location.getNonNull();
						return excludes.stream()
										.map(excludeAnnotation -> excludeAnnotation.getLocation().orElse(null))
										.filter(Objects::nonNull)
										.noneMatch(location::overlapsWith);
					}
					return true;
				})
				.collect(Collectors.toList());
	}
	
	private Tuple2<Long, Long> identifyDataDictionaryEntries(final ModuleLightweightPojo module, final AstNodePojo rootNode) {
		final Optional<DependencyModule> dependencyModule = DependencyModuleDataFetcher.getModule(module.getProject(), module.identity(), core);
		final List<DataDictionaryPojoPrototype> identifiedEntries = getDataDictionaryIdentifier(module, module.getTechnology(), rootNode, dependencyModule)
																		.identify();
		final List<String> lockKeys = getLockKeys(identifiedEntries);

		try {
			/* Different modules include the same copybook modules. Therefore we must lock here, to ensure that there are never two separate tasks
			 * trying to create the same DDEs for copybook modules at the same time. Otherwise we get random duplicates. */
			core.discoveryCache.createLocks(jobId, lockKeys);

			final Long storedEntries = core.dataDictionaryService.createCandidates(identifiedEntries);
			return Tuple2.of(Long.valueOf(identifiedEntries.size()), storedEntries);
		} finally {
			core.discoveryCache.releaseLocks(jobId, lockKeys);
		}
	}

	private List<String> getLockKeys(final List<DataDictionaryPojoPrototype> identifiedEntries) {
		final var moduleIds = identifiedEntries.stream().map(dde -> dde.module.getNonNull()).distinct().collect(Collectors.toList());
		final List<UUID> uids = new ArrayList<>(moduleIds.size());
		final List<Long> nids = new ArrayList<>(moduleIds.size());
		/* Iterate moduleIds only once: Collect uids, if present. Otherwise collect nids so we can fetch the uids for them */
		for (final EntityId id : moduleIds) {
			if (id.hasUid()) {
				uids.add(id.getUid());
			} else {
				nids.add(id.getNid());
			}
		}
		if ( ! nids.isEmpty()) {
			core.moduleService.findModuleIds(q -> q.byNids(nids)).forEach(id -> uids.add(id.getUid()));
		}

		return uids.stream()
				.map(uuid -> "DDE_" + uuid.toString())
				.sorted()
				.collect(Collectors.toList());
	}
	
	private DataDictionaryIdentifier getDataDictionaryIdentifier(final ModuleLightweightPojo module, final Technology technology, final AstNodePojo rootNode,
			final Optional<DependencyModule> dependencyModule) {
		switch (technology) {
			case COBOL:
				return new CobolDataDictionaryIdentifier(core, module.identity(), rootNode, dependencyModule);
			case CICS:
				return new CicsDataDictionaryIdentifier(core, module.identity(), rootNode);
			case NATURAL:
				return new NaturalDataDictionaryIdentifier(core, module, rootNode);
			case PL1:
				return new Pl1DataDictionaryIdentifier(core, module, rootNode);
			default:
				throw new IllegalArgumentException("Unsupported Technology for DataDictionary Identification : " + technology);
		}
	}

	private BusinessVariableIdentifier getBusinessVariableIdentifier(final ModuleLightweightPojo module, final @Nullable AstNodePojo root,
			final Map<String, List<DataDictionaryPojo>> ddeNameMap) {
		switch (module.getTechnology()) {
			case COBOL:
			case NATURAL:
				return new BusinessVariableIdentifier(root, ddeNameMap);
			case PL1:
				return new Pl1BusinessVariableIdentifier(root, ddeNameMap);
			default:
				throw new IllegalArgumentException("Unsupported Technology for Business Variable Identification : " + module.getTechnology());
		}
	}
}
