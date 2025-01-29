/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static java.util.Collections.emptyList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AbstractTraverser;
import innowake.mining.data.core.storeast.api.AstModelToMiningAst;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.statement.BindingNode;

/**
 * A traverser for transforming {@link AstNode} ASTs to {@link StoreAstPrototype}.
 */
public final class AstModelToMiningAstImpl implements AstModelToMiningAst {

	private final AstTraverser traverser;

	/**
	 * Constructor.
	 * 
	 * @param visitFunction the {@link AstNodeToMiningNode}
	 */
	public AstModelToMiningAstImpl(final AstNodeToMiningNode visitFunction) {
		this(visitFunction, null);
	}

	/**
	 * Constructor.
	 * 
	 * <p>
	 * The {@code traverseChildrenPredicate} is used to test, if the child elements of the current visited element should be traversed too. When the predicate
	 * returns {@code false} the child elements are not traversed.
	 * </p>
	 * <p>
	 * By default all child elements are traversed.
	 * </p>
	 * 
	 * @param visitFunction the {@link AstNodeToMiningNode}
	 * @param traverseChildrenPredicate the {@link Predicate}
	 */
	public AstModelToMiningAstImpl(final AstNodeToMiningNode visitFunction,
			@Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> traverseChildrenPredicate) {
		this.traverser = new AstTraverser(visitFunction, AstNode::getChildren, traverseChildrenPredicate);
	}

	@Override
	public StoreAstPrototype traverse(final AstNode node) {
		final StoreAstPrototype root = traverser.traverse(node);
		addBindings(traverser.getVisitFunction().getBindingNodes(),
				traverser.getVisitFunction().getParserToStoreNodeMap());
		return root;
	}

	private void addBindings(final List<AstNode> bindingNodes, final Map<AstNode, StoreAstPrototype> parserToStoreNodeMap) {
		for (final AstNode astNode : bindingNodes) {
			final StoreAstPrototype storeBindingNode = parserToStoreNodeMap.get(astNode);
			final BindingNode<?> bindingNode = astNode.getGenericType(BindingNode.class);
			if (bindingNode != null) {
				for (final Map.Entry<String, List<AstNode>> bindings : bindingNode.getBindings().entrySet()) {
					final String label = bindings.getKey();
					for (final AstNode node : bindings.getValue()) {
						final StoreAstPrototype storeTargetNode = parserToStoreNodeMap.get(node);
						storeBindingNode.addRelationshipTo(storeTargetNode, AstRelationshipType.BINDING).setLabel(label);
					}
				}
			}
		}
	}

	private static class AstTraverser extends AbstractTraverser<AstNode, StoreAstPrototype> {

		private final AstNodeToMiningNode visitFunction;

		@Nullable
		private final Predicate<AstNode> traverseChildrenPredicate;

		protected AstTraverser(final AstNodeToMiningNode visitFunction,
				final Function<AstNode, List<AstNode>> reproductionFunction,
				@Nullable final Predicate<AstNode> traverseChildrenPredicate) {
			super(reproductionFunction);
			this.visitFunction = visitFunction;
			this.traverseChildrenPredicate = traverseChildrenPredicate;
		}

		@Override
		public StoreAstPrototype traverse(final innowake.ndt.core.parsing.ast.AstNode element) {
			final Predicate<innowake.ndt.core.parsing.ast.AstNode> predicate = this.traverseChildrenPredicate;
			if (predicate == null || predicate.test(element)) {
				return super.traverse(element);
			} else {
				return traversed(visit(element), emptyList());
			}
		}

		@Override
		protected StoreAstPrototype visit(final innowake.ndt.core.parsing.ast.AstNode node) {
			return visitFunction.apply(node);
		}

		@Override
		protected StoreAstPrototype traversed(final StoreAstPrototype node, final List<StoreAstPrototype> children) {
			/* reassigning is called from bottom to top */
			return Finalizer.execute(node, children);
		}
		
		protected AstNodeToMiningNode getVisitFunction() {
			return visitFunction;
		}
	}

}
