/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast;

import static innowake.lib.core.lang.Assert.assertEqual;
import static innowake.lib.core.lang.Assert.assertInstanceOf;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.StopWatch;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.OParseResult;
import innowake.mining.data.core.api.Model;
import innowake.mining.data.core.api.MultiPartModel;
import innowake.mining.data.core.api.MultiPartModel.ModelPart;
import innowake.mining.data.core.storeast.api.AstModelToMiningAst;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.data.core.storeast.api.StoreAstExecutor;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.job.NullParsingSummarizer;
import innowake.mining.shared.model.job.ParsingSummarizer;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ast.AdvancedLocationProvider;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.error.ParserError;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Default implementation of the {@link StoreAstExecutor} that contains the core logic for the storeAst operation
 * independent of the caller location.
 */
public class DefaultStoreAstExecutor implements StoreAstExecutor {
	
	private final ParsingSummarizer summarizer;
	
	/**
	 * The name of the storeAst operation.
	 */
	public static final String NAME = "storeAst";
	
	private static final Logger LOG = LoggerFactory.getLogger(DefaultStoreAstExecutor.class);
	
	/**
	 * This logger logs in CSV format:
	 * moduleId, 
	 * number of edges deleted, 
	 * number of vertices deleted, 
	 * time for deleting, 
	 * time for creating AST nodes,
	 * time for creating edges from AST nodes to modules, 
	 * number of lines the assembled content of the module has
	 */
	private static final Logger DEBUG_LOG = LoggerFactory.getLogger(DefaultStoreAstExecutor.class);
	private static final String MESSAGE = NAME + ": Error while checking arguments for module with record id %s.";
	
	/**
	 * Creates an instance of the {@link DefaultStoreAstExecutor} that does not summarize the results.
	 */
	public DefaultStoreAstExecutor() {
		this(NullParsingSummarizer.INSTANCE);
	}
	
	/**
	 * Creates an instance of the {@link DefaultStoreAstExecutor} that will allow for summarizing the results.
	 * @param summarizer the instance that will record the AST operations for summarization.
	 */
	public DefaultStoreAstExecutor(final ParsingSummarizer summarizer) {
		this.summarizer = summarizer;
	}
	
	/**
	 * Executes storeAst for the provided {@code moduleId}.
	 * 
	 * @param moduleId the Id of the module for which the description should be identified
	 * @return the root AST node
	 */
	@Override
	public Optional<UUID> execute(final EntityId moduleId, final MiningDataCoreService core) {
		final Optional<OParseResult> parseResultOptional;
		try {
			parseResultOptional = core.getParseResult(moduleId);
		} catch (final IllegalArgumentException e) {
			LOG.error(() -> String.format("%s: Error while parsing module %s.", NAME, moduleId.toString()), e);
			/* do not throw an exception to support iterative function calls */
			return Optional.empty();
		}
		
		if ( ! parseResultOptional.isPresent()) {
			LOG.error(() -> String.format("%s: No parse result for module %s was obtained.", NAME, moduleId.toString()));
			return Optional.empty();
		}
		
		final OParseResult parseResult = parseResultOptional.get();

		final Optional<ModulePojo> modulePojo = core.moduleService.findAnyModule(q -> q.byId(parseResult.getModuleId()));
		if (modulePojo.isEmpty()) {
			summarizer.error(moduleId);
			LOG.error(() -> String.format(MESSAGE + " Module does not exist.", parseResult.getModuleId()));
			/* do not throw an exception to support iterative function calls */
			return Optional.empty();
		}

		final Technology technology = parseResult.getTechnology();
		if (technology == Technology.UNKNOWN || technology == Technology.NONE) {
			summarizer.unsupported(moduleId);
			LOG.error(() -> String.format(MESSAGE + " Unsupported parse result technology %s.", parseResult.getModuleId(), technology));
			/* do not throw an exception to support iterative function calls */
			return Optional.empty();
		}

		final Optional<Model> modelOptional = parseResult.getModel();
		if ( ! modelOptional.isPresent()) {
			summarizer.error(moduleId);
			LOG.error(() -> String.format(MESSAGE + " Function must not be called with a null parse model.", parseResult.getModuleId()));
			/* do not throw an exception to support iterative function calls */
			return Optional.empty();
		}
		
		final Model model = modelOptional.get();
		if ( ! model.getParseModel().getRoot().isPresent()) {
			summarizer.error(moduleId);
			LOG.error(() -> String.format(MESSAGE + " AstModel requires a valid root node.", parseResult.getModuleId()));
			/* do not throw an exception to support iterative function calls */
			return Optional.empty();
		}
		
		final AstModel astModel = model.getParseModel();

		final List<ParserError> parserErrors = astModel.getParserErrors();
		if ( ! parserErrors.isEmpty()) {
			summarizer.error(moduleId);
			if (LOG.isErrorEnabled()) {
				LOG.error(MESSAGE + ": the following parse errors occurred:", moduleId);
				parserErrors.stream()
						.map(ParserError::toString)
						.forEach(LOG::error);
			}
			/* do not throw an exception to support iterative function calls */
			return Optional.empty();
		}
		final Object assembling = model.getAssembling();
		return Optional.of(storeAst(core, modulePojo.get(), model, assembling, technology));
	}

	@Nullable
	private UUID storeAst(final MiningDataCoreService core, final ModulePojo module, final Model model, final Object assembling, final Technology technology) {
		final EntityId moduleId = module.identity();
		final StopWatch watch = new StopWatch();
		watch.start();
		final StringBuilder debugMsg = new StringBuilder();
		final StopWatch debugWatch = new StopWatch();
		if (DEBUG_LOG.isDebugEnabled()) {
			debugWatch.start();
			debugMsg.append(moduleId).append(",");
		}
		
		try {
			/* Delete old and add new AstNode vertices. */
			debugLogDeleted(debugMsg, core.astService.delete(q -> q.ofModule(moduleId)));
			debugLogTime(debugWatch, debugMsg, true);
			
			final var document = getDocument(core, module.identity());
			
			final StoreAstPrototype rootNode;
			if (model instanceof MultiPartModel) {
				rootNode = storeAstMultiPart(moduleId, model, technology, document);
			} else {
				final AstModel astModel = model.getParseModel();
				@SuppressWarnings("unchecked")
				final RetracingProvider<ModulePojo> retracingProvider = RetracingProvider.createInstance((IAssembling<ModulePojo>) assembling);
				final ModuleProvider<ModulePojo> moduleProvider = ModuleProvider.createInstance(retracingProvider);
				final AdvancedLocationProvider<ModulePojo> locationProvider = astModel.getAdvancedLocationProvider(ModulePojo.class);
				final String source = astModel.getSource();
				final AstNodeToMiningNode astNodeToMiningNode = AstNodeToMiningNode.createInstance(source, moduleProvider, locationProvider,
						retracingProvider, technology, document);
				rootNode = AstModelToMiningAst.createInstance(astNodeToMiningNode).traverse(astModel.getRoot().orElseThrow());
			}
			
			final AstPersistation astToOrient = new AstPersistation(core);
			final UUID storedRootNode = astToOrient.traverse(rootNode);
			astToOrient.createEdges();
			
			debugLogTime(debugWatch, debugMsg, false);
			
			if (DEBUG_LOG.isDebugEnabled()) {
				final Document doc = new Document(String.valueOf(model.getAssembledContent()));
				debugMsg.append(doc.numberOfLines());
				DEBUG_LOG.debug(debugMsg::toString);
			}
			watch.stop();
			LOG.info(() -> String.format("%s: Storing AST for module ID %s took %s.", NAME, moduleId, watch.toString()));
			return storedRootNode;
		} catch (final Exception e) {
			final String message = String.format("%s: Failed to store AST for module ID %s", NAME, moduleId);
			LOG.error(() -> message, e);
			// database.rollback();
			throw new IllegalStateException(message, e);
		}
	}

	private Document getDocument(final MiningDataCoreService core, final EntityId moduleId) {
		final Optional<String> moduleSource = core.sourceService.findAnyContent(q -> q.withModule(moduleId)).map(s -> s.getContent().toString());
		return new Document(moduleSource.orElse(StringUtils.EMPTY));
	}
	
	private StoreAstPrototype storeAstMultiPart(final EntityId moduleId, final Model model, final Technology technology, final Document document) {
		final MultiPartModel multipartModel = assertInstanceOf(model, MultiPartModel.class);
		final List<ModelPart> modelParts = multipartModel.modelParts;
		
		StoreAstPrototype result = null;
		for (final ModelPart modelPart : modelParts) {
			if ( ! modelPart.model.getRoot().isPresent()) {
				throw new IllegalStateException(String.format(MESSAGE + " Every part of a MultiPartModel requires a valid root node.", moduleId));
			}
			
			final RetracingProvider<ModulePojo> retracingProvider = RetracingProvider.createInstance(modelPart.assembling);
			final ModuleProvider<ModulePojo> moduleProvider = ModuleProvider.createInstance(retracingProvider);
			final AdvancedLocationProvider<ModulePojo> locationProvider = modelPart.model.getAdvancedLocationProvider(ModulePojo.class);
			final String source = modelPart.model.getSource();
			final AstNodeToMiningNode astNodeToMiningNode = AstNodeToMiningNode.createInstance(source, moduleProvider, locationProvider,
					retracingProvider, technology, document);
			final StoreAstPrototype storeAstNode = AstModelToMiningAst.createInstance(astNodeToMiningNode).traverse(
					modelPart.model.getRoot().orElseThrow(() -> new IllegalArgumentException("Root node not present")));
			
			if (result == null) {
				/* the first one must always be the root model part. */
				assertEqual(ModelPart.TargetPartPosition.ROOT, modelPart.partPosition);
				result = storeAstNode;
			} else {
				/* merge additional models as child into the root model part. */
				if (storeAstNode.hasChildren()) {
					final List<StoreAstPrototype> existingChildren = result.children(); /* unmodifiable list */
					final List<StoreAstPrototype> newChildren = new ArrayList<>();
					final List<StoreAstPrototype> childrenToAdd = storeAstNode.children();
					
					switch (modelPart.partPosition) {
						case FIRST_CHILD:
							for (final StoreAstPrototype childToAdd : childrenToAdd) {
								childToAdd.setParent(result.id.getNonNull());
								newChildren.add(childToAdd);
							}
							newChildren.addAll(existingChildren);
							break;
						case LAST_CHILD:
							newChildren.addAll(existingChildren);
							for (final StoreAstPrototype childToAdd : childrenToAdd) {
								childToAdd.setParent(result.id.getNonNull());
								newChildren.add(childToAdd);
							}
							break;
						default:
							throw new IllegalStateException("Unsupported part position: " + modelPart.partPosition);
					}
					result.resetChildren(newChildren);
				}
			}
		}
		
		return assertNotNull(result);
	}

	private void debugLogTime(final StopWatch debugWatch, final StringBuilder debugMsg, final boolean restart) {
		if (DEBUG_LOG.isDebugEnabled()) {
			debugWatch.stop();
			debugMsg.append(debugWatch.toString())
					.append(",");
			if (restart) {
				debugWatch.reset();
				debugWatch.start();
			}
		}
	}

	private void debugLogDeleted(final StringBuilder debugMsg, final int count) {
		if (DEBUG_LOG.isDebugEnabled()) {
			debugMsg.append(count).append(",");
		}
	}
}
