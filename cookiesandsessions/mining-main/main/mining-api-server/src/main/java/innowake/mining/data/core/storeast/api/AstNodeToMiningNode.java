/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.api;

import java.util.List;
import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.storeast.impl.AstNodeToMiningNodeFunctionImpl;
import innowake.mining.data.core.storeast.impl.CobolNodeToMiningNodeFunctionImpl;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.Technology;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.parsing.ast.AdvancedLocationProvider;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.statement.BindingNode;
import innowake.ndt.core.parsing.spi.Document;

/**
 * A function for nodes of type {@link innowake.ndt.core.parsing.ast.AstNode} returning nodes of type {@link StoreAstPrototype}.
 */
public interface AstNodeToMiningNode {

	/**
	 * Creates a new instance.
	 * 
	 * @param source the assembled source code of the Module to be transformed
	 * @param moduleProvider the {@link ModuleProvider}
	 * @param locationProvider the {@link AdvancedLocationProvider}
	 * @param retracingProvider the {@link RetracingProvider}
	 * @param technology the {@link Technology}
	 * @param <T> the concrete type of {@link Retracing}
	 * @param document {@link Document} with unassembled content 
	 * @return a new instance
	 */
	public static <T> AstNodeToMiningNode createInstance(
			final String source,
			final ModuleProvider<T> moduleProvider, 
			@Nullable
			final AdvancedLocationProvider<T> locationProvider,
			final RetracingProvider<T> retracingProvider,
			final Technology technology,
			final Document document) {
		if (technology == Technology.COBOL) {
			return new CobolNodeToMiningNodeFunctionImpl<>(source, moduleProvider, locationProvider, retracingProvider, document);
		} else {
			return new AstNodeToMiningNodeFunctionImpl<>(source, moduleProvider, locationProvider, retracingProvider, document);
		}
	}
	
	/**
	 * Returns a node of type {@link StoreAstPrototype} for a node of type {@link innowake.ndt.core.parsing.ast.AstNode}
	 *
	 * @param node the {@link innowake.ndt.core.parsing.ast.AstNode}
	 * @return the {@link StoreAstPrototype}
	 */
	public StoreAstPrototype apply(final AstNode node);
	
	/**
	 * Returns a Map from {@linkplain AstNode AST Nodes} to {@linkplain StoreAstPrototype}s which map the AstNodes returned after parsing to 
	 * their representation in the stored AST
	 * 
	 * @return a map from {@linkplain AstNode AST Nodes} to {@linkplain StoreAstPrototype}s
	 */
	public Map<AstNode, StoreAstPrototype> getParserToStoreNodeMap();

	/**
	 * Returns a list of Binding nodes.
	 *
	 * @return a list of {@linkplain BindingNode Binding Nodes}
	 */
	public List<AstNode> getBindingNodes();
}
