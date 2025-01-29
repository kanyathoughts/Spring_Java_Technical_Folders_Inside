/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

import innowake.mining.shared.entities.ast.AstNodePojo;


/**
 * Collects AST nodes based on a given predicate. 
 */
public final class AstNodeCollector {
	
	private final Predicate<AstNodePojo> collectorPredicate;

	/**
	 * Creates a new collector defined by a predicate.
	 * 
	 * @param collectorPredicate the predicate which determines which nodes should be collected
	 */
	public AstNodeCollector(final Predicate<AstNodePojo> collectorPredicate) {
		this.collectorPredicate = collectorPredicate;
	}
	
	/**
	 * Collects all nodes based on the predicate with a children depth of {@code 1}. 
	 *
	 * @param root the root node from which the collection will start
	 * @return the list of collected nodes
	 */
	public List<AstNodePojo> all(final AstNodePojo root) {
		final AstNodeCollectingTraverser traverser = new AstNodeCollectingTraverser(collectorPredicate, 1);
		traverser.traverse(root);
		return traverser.collect();
	}
	
	/**
	 * Collects all nodes based on the predicate with a children depth of {@code Integer#MAX_VALUE}. 
	 *
	 * @param root the root node from which the collection will start
	 * @return the list of collected nodes
	 */
	public List<AstNodePojo> allDeep(final AstNodePojo root) {
		final AstNodeCollectingTraverser traverser = new AstNodeCollectingTraverser(collectorPredicate);
		traverser.traverse(root);
		return traverser.collect();
	}
	
	/**
	 * Collects the first node based on the predicate with a children depth of {@code 1}. 
	 *
	 * @param root the root node from which the collection will start
	 * @return the collected node
	 */
	public Optional<AstNodePojo> first(final AstNodePojo root) {
		final AstNodeCollectingFirstTraverser traverser = new AstNodeCollectingFirstTraverser(collectorPredicate, 1);
		traverser.traverse(root);
		return traverser.first();
	}
	
	/**
	 * Collects the first node based on the predicate with a children depth of {@code Integer#MAX_VALUE}. 
	 *
	 * @param root the root node from which the collection will start
	 * @return the collected node
	 */
	public Optional<AstNodePojo> firstDeep(final AstNodePojo root) {
		final AstNodeCollectingFirstTraverser traverser = new AstNodeCollectingFirstTraverser(collectorPredicate);
		traverser.traverse(root);
		return traverser.first();
	}
	
	/**
	 * Collects the last node based on the predicate with a children depth of {@code 1}.
	 *
	 * @param root the root node from which the collection will start
	 * @return the collected node
	 */
	public Optional<AstNodePojo> last(final AstNodePojo root) {
		final AstNodeCollectingTraverser traverser = new AstNodeCollectingTraverser(collectorPredicate, 1);
		traverser.traverse(root);
		final List<AstNodePojo> collection = traverser.collect();
		return collection.isEmpty() ? Optional.empty() : Optional.of(collection.get(collection.size() - 1));
	}
	
	/**
	 * Collects the last node based on the predicate with a children depth of {@code Integer#MAX_VALUE}.
	 *
	 * @param root the root node from which the collection will start
	 * @return the collected node
	 */
	public Optional<AstNodePojo> lastDeep(final AstNodePojo root) {
		final AstNodeCollectingTraverser traverser = new AstNodeCollectingTraverser(collectorPredicate);
		traverser.traverse(root);
		final List<AstNodePojo> collection = traverser.collect();
		return collection.isEmpty() ? Optional.empty() : Optional.of(collection.get(collection.size() - 1));
	}
}
