/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.api;

import java.util.function.Predicate;
import innowake.mining.data.core.storeast.impl.AstModelToMiningAstImpl;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * A traverser for transforming {@link AstNode} ASTs to {@link AstNodePojoPrototype}s.
 */
public interface AstModelToMiningAst {

	/**
	 * Creates a new instance.
	 * 
	 * @param function the {@link AstNodeToMiningNode}
	 * @return a new instance
	 */
	public static AstModelToMiningAst createInstance(final AstNodeToMiningNode function) {
		return new AstModelToMiningAstImpl(function);
	}
	
    /**
	 * Creates a new instance.
	 * 
 	 * <p>The {@given traverseChildrenPredicate} is used to test, if the child elements of the current visited element should be traversed too. When the
	 * predicate returns {@code false} the child elements are not traversed.</p>
 	 * <p>By default all child elements are traversed.</p>
 	 * 
	 * @param function the {@link AstNodeToMiningNode}
	 * @param traverseChildrenPredicate the {@link Predicate}
	 * @return a new instance
	 */
	public static AstModelToMiningAst createInstance(final AstNodeToMiningNode function, final Predicate<AstNode> traverseChildrenPredicate) {
		return new AstModelToMiningAstImpl(function, traverseChildrenPredicate);
	}

    /**
     * Traverses a tree of {@link AstNode} and returns an element of type {@code StoreAstNode}.
     *
     * @param node the node to traverse
     * @return a result element
     */
	public StoreAstPrototype traverse(final AstNode node);	
}
