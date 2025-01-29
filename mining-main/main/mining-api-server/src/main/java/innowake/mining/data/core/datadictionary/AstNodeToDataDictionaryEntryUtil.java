/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.datadictionary;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.*;
import java.util.stream.Collectors;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.storeast.api.StoreAstExecutor;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;

/**
 * Utility to find AST nodes for {@linkplain DataDictionaryPojo DataDictionaryEntries}.
 */
public class AstNodeToDataDictionaryEntryUtil {

	private static final Logger LOG = LoggerFactory.getLogger(AstNodeToDataDictionaryEntryUtil.class);
	private final StoreAstExecutor storeAstExecutor;
	private final MiningDataCoreService core;
	
	/**
	 * Creates an instance of this DAO.
	 * 
	 * @param storeAstExecutor the {@linkplain StoreAstExecutor} instance
	 * @param core the instance of {@linkplain MiningDataCoreService} 
	 */
	public AstNodeToDataDictionaryEntryUtil(final StoreAstExecutor storeAstExecutor, final MiningDataCoreService core) {
		this.storeAstExecutor = storeAstExecutor;
		this.core = core;
	}
	
	/**
	 * Returns the AST node for the provided {@linkplain DataDictionaryPojo}.
	 * <p>Consider using <b>findConnections</b> instead of calling this method multiple times for a single module.</p>
	 * 
	 * @param dde the {@linkplain DataDictionaryPojo}
	 * @return the List of related AST nodes
	 */
	public List<AstNodePojo> findAstNode(final DataDictionaryPojo dde) {
		final String dataElementName = dde.getName();
		final EntityId moduleId = dde.getModule();
		final Integer offset = dde.getLocation().orElseThrow(() -> new NoSuchElementException("Location is required")).getOffset();
		
		if (DefinedLocation.PROGRAM == dde.getDefinedLocation().orElse(null)) {
			if (core.getAstRootOrCreateExceptInclusions(moduleId, storeAstExecutor).isPresent()) {
				var node = core.astService.findAny(q -> q.ofModule(moduleId)
						.withSuperTypes("FieldDefinition").withLabelContaining(dataElementName)
						.withRetracedOffset(Comperator.LESSER_OR_EQUAL, offset).withRetracedEndOffset(Comperator.GREATER_OR_EQUAL, offset));
				if (node.isPresent()) {
					return Collections.singletonList(node.get());
				}
			} else {
				throw new IllegalStateException(String.format("Could not create AstNodes for Module with ID %s.", moduleId));
			}
		} else {
			final List<EntityId> programs = findProgramIdsFromCopybook(moduleId);
			final Map<Boolean, List<EntityId>> hasAst = programs.stream().collect(
					Collectors.partitioningBy(id -> core.getAstRootOrCreateExceptInclusions(id, storeAstExecutor).isPresent()));

			hasAst.get(Boolean.FALSE).forEach(id -> LOG.error("Could not create AstNodes for Module with IDs {}.", id));

			final List<AstNodePojo> nodes = new ArrayList<>();
			nodes.addAll(core.astService.find(q -> q.ofModules(EntityId.allUids(hasAst.get(Boolean.TRUE)))
					.withSuperTypes("FieldDefinition").withLabelContaining(dataElementName)));
			if ( ! nodes.isEmpty()) {
				return nodes;
			}
		}
		throw new IllegalStateException(String.format("Error retrieving AstNodes for DataDictionaryEntry with name %s.", dataElementName));
	}

	/**
	 * Returns the {@linkplain DataDictionaryPojo} for the provided AST node.
	 * <p>Consider using <b>findConnections</b> instead of calling this method multiple times for a single module.</p>
	 * 
	 * @param astNode the AST node
	 * @return the linked {@linkplain DataDictionaryPojo}
	 */
	public DataDictionaryPojo findDatatDictionaryEntry(final AstNodePojo astNode) {
		final String name = assertNotNull(astNode.getProperties().get("name"), "AstNode should have name property set with data field name").toString();
		final EntityId moduleId = astNode.getModule();
		final ModuleLocation location = astNode.getLocation().convertToSharedModuleLocation();
		final List<DataDictionaryPojo> dde = core.dataDictionaryService.find(q -> q.ofModule(moduleId).withName(name).withLocation(location, true));

		if (dde.isEmpty()) {
			/* when empty, try to resolve via children */
			final List<EntityId> incudedModuleIds = core.moduleService.findRelatedModules(moduleId, RelationshipType.INCLUDES, RelationshipDirection.OUT)
					.stream().map(EntityId::of)
					.collect(Collectors.toList());
			final List<DataDictionaryPojo> includedDataDictionaryEntries = core.dataDictionaryService.find(q -> q.ofModules(incudedModuleIds).withName(name).withLocation(location, true));

			if (includedDataDictionaryEntries.isEmpty()) {
				throw new IllegalStateException(String.format("Could not find DataDictionaryEntry for AstNode for module %s with label %s, location %s.",
						moduleId, astNode.getLabel(), astNode.getLocation()));
			}
			return includedDataDictionaryEntries.get(0);
		} else {
			return dde.get(0);
		}
	}

	/**
	 * Returns all pairs of AST nodes and {@linkplain DataDictionaryPojo DatatDictionaryEntries} for the specified module.
	 * <p>The result is intended to be used to construct a lookup table or a similar data structure, as required.
	 * Use this method to avoid multiple lookups per module.</p>
	 * 
	 * @param moduleId the {@linkplain Module} id
	 * @return {@linkplain List} of DDEs and AstNodes
	 */
	public List<Tuple2<AstNodePojo, DataDictionaryPojo>> findConnections(final EntityId moduleId) {
		final List<AstNodePojo> astNodes = new LinkedList<>();
		if (core.getAstRootOrCreateExceptInclusions(moduleId, storeAstExecutor).isPresent()) {
			astNodes.addAll(core.astService.find(q -> q.ofModule(moduleId).withSuperTypes("FieldDefinition")));
			return getDdeToAstNodes(moduleId, astNodes);
		}
		throw new IllegalStateException(String.format("Could not create AstNodes for Module with ID %s.", moduleId));
	}

	private List<Tuple2<AstNodePojo, DataDictionaryPojo>> getDdeToAstNodes(final EntityId moduleId, final List<AstNodePojo> astNodes) {
		final List<Tuple2<AstNodePojo, DataDictionaryPojo>> ddeToAstNodeTuples = new ArrayList<>();
		final var includes = core.moduleService.findRelatedModules(moduleId, RelationshipType.INCLUDES, RelationshipDirection.OUT)
													.stream().map(EntityId::of).collect(Collectors.toList());
		includes.add(moduleId);
		core.dataDictionaryService.find(q -> q.ofModules(includes)).forEach(dde -> {
			boolean match = false;
			final var location = dde.getLocation().orElseThrow(() -> new NoSuchElementException("Location is required"));
			for (final AstNodePojo node : astNodes) {
				if ((location.isWithin(node.getLocation().convertToSharedModuleLocation())
						&& AstNodeUtils.getFieldDefinitionNameFromReference(node).equals(dde.getName()))
						&& (node.getIncludedModule().isEmpty() || dde.getModule().equals(node.getIncludedModule().get()))) {
					ddeToAstNodeTuples.add(new Tuple2<>(node, dde));
					match = true;
					break;
				}
			}

			if ( ! match) {
				LOG.error("Could not find AstNode for DataDictionaryEntry for field {}. (id : {}, offset : {}) in Module having id: {}",
						dde.getName(), location, dde.getId(), moduleId);
			}
		});
		return ddeToAstNodeTuples;
	}
	
	public List<EntityId> findProgramIdsFromCopybook(final EntityId module) {
		final var inIncludes = core.moduleService.findRelatedModules(module, RelationshipType.INCLUDES, RelationshipDirection.IN);
		return core.moduleService.findModuleIds(q -> q.byUids(inIncludes).withType(Type.PROGRAM));
	}
}
