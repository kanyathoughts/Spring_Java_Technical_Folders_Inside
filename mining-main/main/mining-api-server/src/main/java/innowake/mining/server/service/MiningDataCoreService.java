/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.MiningContentProvider;
import innowake.mining.data.core.ModelProvider;
import innowake.mining.data.core.OParseResult;
import innowake.mining.data.core.api.Model;
import innowake.mining.data.core.storeast.api.StoreAstExecutor;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.lang.HashCache;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;

/**
 * Collective Service for Mining Data Core
 */
@Service
public class MiningDataCoreService {
	
	public final ModuleService moduleService;
	public final SourceService sourceService;
	public final AnnotationService annotationService;
	public final DataDictionaryService dataDictionaryService;
	public final TaxonomyService taxonomyService;
	public final AstService astService;
	public final FF4j ff4j;
	public final DiscoveryCache discoveryCache;
	
	@Autowired
	public MiningDataCoreService(
			final ModuleService moduleService, final SourceService sourceService, final AnnotationService annotationService, 
			final DataDictionaryService dataDictionaryService, final TaxonomyService taxonomyService, final AstService astService,
			final FF4j ff4j, final DiscoveryCache discoveryCache) {
		this.moduleService = moduleService;
		this.sourceService = sourceService;
		this.annotationService = annotationService;
		this.dataDictionaryService = dataDictionaryService;
		this.taxonomyService = taxonomyService;
		this.astService = astService;
		this.ff4j = ff4j;
		this.discoveryCache = discoveryCache;
	}
	
	private static final Logger LOG = LoggerFactory.getLogger(MiningDataCoreService.class);
	private static final String MODULE_NOT_FOUND_MESSAGE_PATTERN = "Module with id %s could not be found.";
	private static final ModelProvider MODEL_PROVIDER = new ModelProvider();
	
	/**
	 * Checks whether a feature toggle is set to active by running an OrientDB query.
	 *
	 * @param featureToggleId id of the feature toggle
	 * @return true if feature toggle exists and is active; false if not
	 */
	public final boolean isFeatureToggleActive(final String featureToggleId) {
		return ff4j.getFeature(featureToggleId).isEnable();
	}
	
	/**
	 * Returns the root AST node of non-included modules.
	 * <p>
	 * If no AST is available, it is tried to create an AST.
	 * 
	 * @param moduleId the ID of the module
	 * @param storeAstExecutor the {@link StoreAstExecutor} to create the AST if it's not already present in the database
	 * @return the root AST node, or {@link Optional#empty()} if no AST could be created
	 */
	public final Optional<AstNodePojo> getAstRootOrCreateExceptInclusions(final EntityId moduleId, final StoreAstExecutor storeAstExecutor) {
		return getAstRootOrCreateExceptInclusions(moduleId, storeAstExecutor, new HashCache<>());
	}
	
	/**
	 * Returns the root AST node of non-included modules.
	 * <p>
	 * If no AST is available, it is tried to create an AST.
	 * 
	 * @param moduleId the ID of the module
	 * @param storeAstExecutor the {@link StoreAstExecutor} to create the AST if it's not already present in the database
	 * @param astCache AST cache containing all loaded {@link AstNodePojo} entities
	 * @return the root AST node, or {@link Optional#empty()} if no AST could be created
	 */
	public final Optional<AstNodePojo> getAstRootOrCreateExceptInclusions(final EntityId moduleId, final StoreAstExecutor storeAstExecutor, final HashCache<UUID, AstNodePojo> astCache) {
		final Optional<ModuleLightweightPojo> module = moduleService.findAnyModuleLightweight(q -> q.byId(moduleId));
		if ( ! module.isPresent() || (isInclusion(module.get().identity()) && module.get().getTechnology() != Technology.CICS)) {
			return Optional.empty();
		}

		return getAstRootOrCreate(moduleId, storeAstExecutor, astCache);
	}
	
	/**
	 * Executes the parsing for a certain module and returns the parse result.
	 * 
	 * @param moduleId the id of the module
	 * @return the parse result
	 * 
	 */
	public Optional<OParseResult> getParseResult(final EntityId moduleId) {
		final long tick = System.currentTimeMillis();
		
		final Optional<ModulePojo> module = moduleService.findAnyModule(q -> q.byId(moduleId).includeContent(true));
		if (module.isPresent()) {
			final ModulePojo modulePojo = module.get();
			if (StringUtils.isEmpty(modulePojo.getContent().orElse(null))) {
				return Optional.empty();
			}

			final Model model = MODEL_PROVIDER.get(modulePojo, new MiningContentProvider(this, modulePojo));
			if (model != null) {
				final long took = System.currentTimeMillis() - tick;
				LOG.info(() -> String.format("Parsing of %s (%s) took %d ms.", modulePojo.getName(), modulePojo.identity(), Long.valueOf(took)));
			} else {
				LOG.warn(() -> String.format("Parsing of %s (%s) did not succeed.", modulePojo.getName(), modulePojo.identity()));
			}
			return Optional.of(new OParseResult(moduleId, model, module.get().getTechnology()));
			
		} else {
			LOG.warn(() -> String.format(MODULE_NOT_FOUND_MESSAGE_PATTERN, moduleId));
		}
		
		return Optional.empty();
	}

	/**
	 * Checks whether the given module is an inclusion module.
	 *
	 * @param moduleId ID of a Module}
	 * @return true if the given module is an inclusion module; false if not
	 */
	public boolean isInclusion(final EntityId moduleId) {
		return moduleService.countRelationships(q -> q.ofModuleInDirection(moduleId, RelationshipDirection.IN).withType(RelationshipType.INCLUDES)) > 0;
	}

	private Optional<AstNodePojo> getAstRootOrCreate(final EntityId moduleId, final StoreAstExecutor storeAstExecutor, final HashCache<UUID, AstNodePojo> cache) {
		var rootNodeId = astService.findIds(q -> q.withRelationshipToModule(moduleId, AstModuleRelationshipType.ROOT)).stream().findAny();
		
		if (rootNodeId.isEmpty()) {
			rootNodeId = storeAstExecutor.execute(moduleId, this);
			if (rootNodeId.isEmpty()) {
				LOG.warn(() -> String.format("Ast for module with id %s not available.", moduleId.toString())); 
			}
		}
		
		if (rootNodeId.isPresent()) {
			cache.putAll(astService.find(q -> q.ofModule(moduleId).usingCache(cache)), AstNodePojo::getId);
		}
		
		return rootNodeId.map(cache::get);
	}

}
