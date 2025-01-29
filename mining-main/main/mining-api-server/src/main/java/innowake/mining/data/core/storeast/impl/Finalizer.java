/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static innowake.mining.data.core.api.AstNodeUtils.INCLUSION_NODE;
import static innowake.mining.data.core.api.AstNodeUtils.STATEMENT;

import java.util.ArrayList;
import java.util.List;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.ndt.naturalparser.model.NaturalDataAreaNode;
import innowake.ndt.naturalparser.model.NaturalIncludeNode;

/**
 * Reassigns nodes to artificial parent nodes according to the nodes's inclusion state.<p>
 * Assigns parent, children and siblings of a node.
 */
final class Finalizer {

	private static final String COBOL_COPYBOOK_INCLUSION_ROOT_TYPE = "CobolCopyStmt";
	private static final String NATURAL_COPYCODE_INCLUSION_ROOT_TYPE = "IncludeStmt";
	private static final String NATURAL_LDA_INCLUSION_ROOT_TYPE = "LocalDataArea";
	private static final String NATURAL_PDA_INCLUSION_ROOT_TYPE = "ParameterDataArea";
	private static final String NATURAL_GDA_INCLUSION_ROOT_TYPE = "GlobalDataArea";
	private static final String NATURAL_AIV_INCLUSION_ROOT_TYPE = "AivDataArea";
	private static final String NATURAL_UNKNOWN_INCLUSION_ROOT_TYPE = "UnknownDataArea";
	
	private static final String NATURAL_INCLUDE_NODE = NaturalIncludeNode.class.getSimpleName();
	private static final String NATURAL_DATAAREA_NODE = NaturalDataAreaNode.class.getSimpleName();
	private static final String NATURAL_NODE_START = "Natural";

	private Finalizer() {}
	
	/**
	 * Reassigns nodes to artificial parent nodes according to the nodes's inclusion state.<p>
	 * Assigns parent, children and siblings of a node.
	 *
	 * @param node the parent node
	 * @param children the child nodes
	 * @return the parent node
	 */
	static StoreAstPrototype execute(final StoreAstPrototype node, final List<StoreAstPrototype> children) {
		final RootInserter rootInserter = new RootInserter(node);
		final InclusionReassigner reassigner = new InclusionReassigner();

		List<StoreAstPrototype> reassignedChildren = rootInserter.execute(children);
		for (int level = rootInserter.getMaxLevel(); level > 0; level--) {
			reassignedChildren = reassigner.execute(reassignedChildren, level);
		}
		
		return node.resetChildren(reassignedChildren);
	}
	
	private static final class RootInserter {

		private StoreAstPrototype previousNode;
		private int maxLevel = 0;
		
		private RootInserter(final StoreAstPrototype node) {
			previousNode = node;
		}

		private List<StoreAstPrototype> execute(final List<StoreAstPrototype> nodes) {
			final List<StoreAstPrototype> childrenWithInclusionRoot = new ArrayList<>();
			for (final StoreAstPrototype child : nodes) {
				if (isInclusionStart(child)) {
					childrenWithInclusionRoot.add(createInclusionRoot(child));
				}
				childrenWithInclusionRoot.add(child);
				maxLevel = Math.max(child.getInclusionLevel(), maxLevel);
			}
			return childrenWithInclusionRoot;
		}
		
		private int getMaxLevel() {
			return maxLevel;
		}

		private boolean isInclusionStart(final StoreAstPrototype node) {
			final boolean result = node.getInclusionLevel() > previousNode.getInclusionLevel() ||
					node.getInclusionLevel() == previousNode.getInclusionLevel() &&
					node.getInclusionOrdinal() > previousNode.getInclusionOrdinal();
			previousNode = node;
			return result;
		}
		
		private StoreAstPrototype createInclusionRoot(final StoreAstPrototype node) {
			final EntityId inclusionCalleeModule = node.includedModule.get();
			final StoreAstPrototype result = new StoreAstPrototype(getInclusionRootType(node),
					/* This is an artificial node, which does not actually have source code 
					 * in this case we will just add the module information as label if present */
					inclusionCalleeModule != null ? inclusionCalleeModule.toString() : "",
					node.module.getNonNull(), 
					node.location.getNonNull(),
					node.getInclusionLevel(), 
					node.getInclusionOrdinal(), 
					inclusionCalleeModule);
			result.addSuperType(INCLUSION_NODE);
			result.addSuperType(STATEMENT);
			
			/* inclusionCalleeModule can be null if data provider cannot resolve object -> WNDT-2677 */
			if (inclusionCalleeModule != null) {
				result.addModuleRelationship(inclusionCalleeModule, AstModuleRelationshipType.ROOT);
			}
			return result;
		}
		
		private static String getInclusionRootType(final StoreAstPrototype node) {
			final String nodeType = node.type.getNonNull();
			if (NATURAL_INCLUDE_NODE.equals(nodeType)) {
				/* if the copycode hasn't been successfully assembled, we still have the INCLUDE statement in the AST. */
				return NATURAL_COPYCODE_INCLUSION_ROOT_TYPE;
			} else if (NATURAL_DATAAREA_NODE.equals(nodeType)) {
				/* assembled and non assembled data areas always have this node. */
				final String dataAreaType = (String) node.properties.getNonNull().get(NaturalDataAreaNode.PROPERTY_DATA_AREA_TYPE);
				switch (NaturalDataAreaNode.DataAreaType.valueOf(dataAreaType)) {
					case LOCAL:
						return NATURAL_LDA_INCLUSION_ROOT_TYPE;
					case PARAMETER:
						return NATURAL_PDA_INCLUSION_ROOT_TYPE;
					case GLOBAL:
						return NATURAL_GDA_INCLUSION_ROOT_TYPE;
					case AIV:
						return NATURAL_AIV_INCLUSION_ROOT_TYPE;
					default:
						return NATURAL_UNKNOWN_INCLUSION_ROOT_TYPE;
				}
			}
			/* already assembled natural INCLUDE statements are a normal AST node */
			return nodeType.startsWith(NATURAL_NODE_START) ? NATURAL_COPYCODE_INCLUSION_ROOT_TYPE : COBOL_COPYBOOK_INCLUSION_ROOT_TYPE;
		}
	}
	
	private static final class InclusionReassigner {
		
		private List<StoreAstPrototype> cuttingNodes = new ArrayList<>();
		@Nullable private StoreAstPrototype root = null;
		
		private List<StoreAstPrototype> execute(final List<StoreAstPrototype> nodes, final int level) {
			final List<StoreAstPrototype> remainingNodes = new ArrayList<>();
			reset();
			
			for (final StoreAstPrototype node : nodes) {
				if (node.getInclusionLevel() < level) {
					reassign();
					remainingNodes.add(node);
				} else if (node.getInclusionLevel() == level && isInclusionRootType(node.type.getNonNull())) {
					reassign();
					remainingNodes.add(node);
					root = node;
				} else if (root != null) {
					cuttingNodes.add(node);
				} else {
					remainingNodes.add(node);
				}
			}
			
			reassign();
			
			return remainingNodes;
		}
		
		private void reassign() {
			if (root != null) {
				root.resetChildren(cuttingNodes);
				reset();
			}
		}
		
		private void reset() {
			root = null;
			cuttingNodes = new ArrayList<>();
		}
		
		private static boolean isInclusionRootType(final String type) {
			return COBOL_COPYBOOK_INCLUSION_ROOT_TYPE.equals(type) || NATURAL_COPYCODE_INCLUSION_ROOT_TYPE.equals(type)
					|| NATURAL_LDA_INCLUSION_ROOT_TYPE.equals(type) || NATURAL_PDA_INCLUSION_ROOT_TYPE.equals(type)
					|| NATURAL_GDA_INCLUSION_ROOT_TYPE.equals(type) || NATURAL_AIV_INCLUSION_ROOT_TYPE.equals(type)
					|| NATURAL_UNKNOWN_INCLUSION_ROOT_TYPE.equals(type);
		}
	}
}
