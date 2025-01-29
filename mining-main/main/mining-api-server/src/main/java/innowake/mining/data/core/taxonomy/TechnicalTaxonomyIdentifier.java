/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.CandidateIdentificationResult;
import innowake.mining.data.core.OParseResult;
import innowake.mining.data.core.api.Model;
import innowake.mining.data.core.storeast.api.StoreAstExecutor;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.parsing.ast.AstModel;
import org.apache.commons.lang.time.StopWatch;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import groovy.lang.Tuple3;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Boxing.box;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.BATCH;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.LIBRARY;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.MQ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UI;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;
import static innowake.mining.shared.model.Type.MAINPROGRAM;
import static innowake.mining.shared.model.Type.PROGRAM;
import static innowake.mining.shared.model.Type.SUBPROGRAM;

/**
 * Identifies technical Taxonomies.
 */
@Component
public class TechnicalTaxonomyIdentifier {

	private static final Logger LOG = LoggerFactory.getLogger(TechnicalTaxonomyIdentifier.class);
	private static final Set<Name> NON_LIBRARY_PROGRAM_TAXONOMY_NAMES = Set.of(BATCH, UI, MQ);
	private static final Set<Type> PROGRAM_TYPES = EnumSet.of(PROGRAM, MAINPROGRAM, SUBPROGRAM);
	private static final Tuple2<Name, TypeName> LIBRARY_TAXONOMY = TechnicalTaxonomies.tuple(LIBRARY, PROGRAM_TYPE);
	private static final ConcurrentHashMap<EntityId, Long> PROJECT_TECHNICAL_TAXONOMY_ID_CACHE = new ConcurrentHashMap<>();
	private static final ConcurrentHashMap<Tuple3<EntityId, String, String>, EntityId> PROJECT_TAXONOMY_CACHE = new ConcurrentHashMap<>();
	private static final ConcurrentHashMap<Tuple2<EntityId, String>, UUID> PROJECT_TAXONOMY_TYPE_CACHE = new ConcurrentHashMap<>();

	@Autowired
	private ProjectService projectService;
	@Autowired(required = false)
	private final List<TaxonomyIdentifier<AstNodePojo>> astBasedIdentifiers = Collections.emptyList();
	@Autowired(required = false)
	private final List<TaxonomyIdentifier<AstModel>> astModelBasedIdentifiers = Collections.emptyList();
	@Autowired(required = false)
	private final List<TaxonomyIdentifier<DependencyModule>> dependencyBasedIdentifiers = Collections.emptyList();
	@Autowired(required = false)
	private final List<TaxonomyIdentifier<ModuleBasePojo>> moduleBasedIdentifiers = Collections.emptyList();

	/**
	 * Identifies Taxonomies for the given Module.
	 *
	 * @param moduleId the ID of the Module for which Taxonomies should be identified
	 * @param storeAstExecutor the {@link StoreAstExecutor} to create the AST if it's not already present in the database
	 * @param core the {@link MiningDataCoreService} to access entity services
	 * @return the identification result
	 */
	public CandidateIdentificationResult identify(final EntityId moduleId,
			final StoreAstExecutor storeAstExecutor, final MiningDataCoreService core) {
		final ModulePojo module = core.moduleService.getModule(moduleId);
		final EntityId projectId = module.getProject();
		final Optional<AstNodePojo> astNode = getAstNode(moduleId, storeAstExecutor, core);
		final Optional<DependencyModule> dependencyModule = DependencyModuleDataFetcher.getModule(projectId, moduleId, core);
		final Optional<AstModel> astModel = getAstModel(moduleId, core);
		
		final var overallWatch = new StopWatch();
		overallWatch.start();

		final var watch = new StopWatch();
		watch.start();
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = new ArrayList<>();
		if ( ! moduleBasedIdentifiers.isEmpty()) {
			/* Implementations of module based identifiers may be present in the extensions that are used by clients */
			final var lightweightPojo = new ModuleLightweightPojo(module);
			final List<Tuple2<Name, TypeName>> moduleBasedTaxonomies = moduleBasedIdentifiers.stream()
					.map(identifier -> identifier.identify(lightweightPojo))
					.flatMap(List::stream)
					.distinct()
					.collect(Collectors.toList());
			identifiedTaxonomies.addAll(moduleBasedTaxonomies);
		}
		identifiedTaxonomies.addAll(identify(dependencyModule, astNode, astModel));
		watch.stop();
		LOG.debug(() -> String.format("Identification of %d Taxonomies for Module %s took %s", box(identifiedTaxonomies.size()), moduleId, watch));

		if (identifiedTaxonomies.isEmpty()) {
			return new CandidateIdentificationResult(0L, 0L);
		}

		watch.reset();
		watch.start();
		final List<EntityId> taxonomyIds = new LinkedList<>();
		
		for (final Tuple2<Name, TypeName> identifiedTaxonomy : identifiedTaxonomies) {
			final String name = identifiedTaxonomy.e1.getDisplayName();
			final String typeName = identifiedTaxonomy.e2.getDisplayName();
			final EntityId taxonomyId = getOrCreateTaxonomy(core.taxonomyService, projectId, name, typeName);
			taxonomyIds.add(taxonomyId);
		}
		
		final long storeCount = core.taxonomyService.createModuleLinks(module.identity(), taxonomyIds);
		watch.stop();
		LOG.debug(() -> String.format("Storage of %d Taxonomies for Module %s took %s", box(identifiedTaxonomies.size()), moduleId, watch));
		LOG.info(() -> String.format("Identifying Taxonomies for Module %s took %s", moduleId, overallWatch));
		
		core.moduleService.updateModules(q -> q.byId(moduleId), new ModulePojoPrototype().setModifiedDate(Instant.now()));
		
		return new CandidateIdentificationResult((long) identifiedTaxonomies.size(), storeCount);
	}

	private EntityId getOrCreateTaxonomy(final TaxonomyService taxonomyService, final EntityId projectId, final String name, final String typeName) {
		final Tuple3<EntityId, String, String> key = new Tuple3<>(projectId, name, typeName);
		return PROJECT_TAXONOMY_CACHE.computeIfAbsent(key, k -> {
			final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName(typeName).withName(name));
			if (taxonomies.isEmpty()) {
				final UUID typeId = getOrCreateType(taxonomyService, projectId, typeName);
				return taxonomyService.create(new TaxonomyPojoPrototype()
						.setName(name)
						.setType(typeId)
						.setProject(projectId));
			} else {
				if (taxonomies.size() > 1) {
					LOG.warn(() -> String.format("More than one Taxonomy found for name: '%s' and type: '%s': %s", name, typeName, taxonomies));
				}
				return taxonomies.get(0).identity();
			}
		});
	}

	private UUID getOrCreateType(final TaxonomyService taxonomyService, final EntityId projectId, final String typeName) {
		final Tuple2<EntityId, String> key = new Tuple2<>(projectId, typeName);
		return PROJECT_TAXONOMY_TYPE_CACHE.computeIfAbsent(key, k -> {
			final List<TaxonomyTypePojo> types = taxonomyService.findTypes(q -> q.ofProject(projectId).withName(typeName));
			if (types.isEmpty()) {
				final var categoryId = PROJECT_TECHNICAL_TAXONOMY_ID_CACHE.computeIfAbsent(projectId,
						p -> projectService.get(projectId).getTechnicalTaxonomyCategoryId());
				return taxonomyService.createType(new TaxonomyTypePojoPrototype().setName(typeName).setProject(projectId).setCategoryId(categoryId));
			} else {
				if (types.size() > 1) {
					LOG.warn(() -> String.format("More than one TaxonomyType found for type: '%s': %s", typeName, types));
				}
				return types.get(0).getId();
			}
		});
	}

	/**
	 * Identifies technical Taxonomies based on the given {@link DependencyModule} and AST node.
	 *
	 * @param module the dependency module associated with the to be identified Module
	 * @param rootNode the AST root node of the to be identified Module, if no AST is available then {@link Optional#empty()}
	 * @param model the AST model associated with the to be identified Module
	 * @return a list of identified Taxonomies, not {@code null}
	 */
	public List<Tuple2<Name, TypeName>> identify(final Optional<DependencyModule> module, final Optional<AstNodePojo> rootNode, final Optional<AstModel> model) {
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = new ArrayList<>();
		
		module.ifPresent(dependencyModule -> {
			final List<Tuple2<Name, TypeName>> dependencyBasedTaxonomies = dependencyBasedIdentifiers.stream()
					.map(identifier -> identifier.identify(dependencyModule))
					.flatMap(List::stream)
					.distinct()
					.collect(Collectors.toList());
			identifiedTaxonomies.addAll(dependencyBasedTaxonomies);
		});
		
		rootNode.ifPresent(astNode -> {
			final List<Tuple2<Name, TypeName>> astBasedTaxonomies = astBasedIdentifiers.stream()
					.map(identifier -> identifier.identify(astNode))
					.flatMap(List::stream)
					.distinct()
					.collect(Collectors.toList());
			identifiedTaxonomies.addAll(astBasedTaxonomies);
		});
		
		model.ifPresent(astModel -> {
			final List<Tuple2<Name, TypeName>> astModelBasedTaxonomies = astModelBasedIdentifiers.stream()
					.map(identifier -> identifier.identify(astModel))
					.flatMap(List::stream)
					.distinct()
					.collect(Collectors.toList());
			identifiedTaxonomies.addAll(astModelBasedTaxonomies);
		});
		
		/* It's necessary that this check is only done after the other identifiers are run, as this works via exclusion of the other program types. */
		module.ifPresent(dependencyModule -> {
			if (listDoesNotContainOtherProgramTypes(identifiedTaxonomies) 
					&& PROGRAM_TYPES.contains(dependencyModule.getType())) {
				identifiedTaxonomies.add(LIBRARY_TAXONOMY);
			}
		});
		
		return identifiedTaxonomies;
	}
	
	private static Optional<AstModel> getAstModel(final EntityId moduleId, final MiningDataCoreService core) {
		return core.getParseResult(moduleId)
				.flatMap(OParseResult::getModel)
				.map(Model::getParseModel);
	}

	private static Optional<AstNodePojo> getAstNode(final EntityId moduleId, final StoreAstExecutor storeAstExecutor,
			final MiningDataCoreService core) {
		final Optional<AstNodePojo> rootNode = core.getAstRootOrCreateExceptInclusions(moduleId, storeAstExecutor);
		if (rootNode.isEmpty()) {
			LOG.error(() -> String.format("Could not determine AST for module %s", moduleId));
		}
		return rootNode;
	}
	
	private static boolean listDoesNotContainOtherProgramTypes(final List<Tuple2<Name, TypeName>> taxonomies) {
		return taxonomies.stream()
				.filter(taxonomy -> PROGRAM_TYPE.equals(taxonomy.e2))
				.noneMatch(taxonomy -> NON_LIBRARY_PROGRAM_TAXONOMY_NAMES.contains(taxonomy.e1));
	}

}
