/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Result containing all the created fields, statements and proxy containers
 */
public class DataLineageResult {

	private final Collection<DataFlowNodePrototype> dataFlowNodes;
	private final Collection<ProxyContainerPrototype> proxyContainers;

	//TODO: should maybe use a different type here
	private final Collection<DataFlowNodePrototype> statements;

	/**
	 * Constructor.
	 * @param dataFlowNodes {@link DataFlowNodePrototype} fields collection
	 * @param proxyContainers {@link ProxyContainerPrototype} collection
	 * @param statements {@link DataFlowNodePrototype} statements collection
	 */
	private DataLineageResult(final Collection<DataFlowNodePrototype> dataFlowNodes, final Collection<ProxyContainerPrototype> proxyContainers,
							 final Collection<DataFlowNodePrototype> statements) {
		this.dataFlowNodes = dataFlowNodes;
		this.proxyContainers = proxyContainers;
		this.statements = statements;
	}

	/**
	 * Data lineage result for collection of input {@link DataFlowNodePrototype} fields
	 *
	 * @param dataFlowNodes collection of Field {@link DataFlowNodePrototype}
	 * @return {@link DataLineageResult}
	 */
	public static DataLineageResult ofNodes(final Collection<DataFlowNodePrototype> dataFlowNodes) {
		return new DataLineageResult(dataFlowNodes, Collections.emptyList(), Collections.emptyList());
	}

	/**
	 * Data lineage result along with a collection of input {@link DataFlowNodePrototype} fields
	 *
	 * @param dataFlowNodes collection of Field {@link DataFlowNodePrototype}
	 * @return {@link DataLineageResult}
	 */
	public DataLineageResult withNodes(final Collection<DataFlowNodePrototype> dataFlowNodes) {
		return new DataLineageResult(dataFlowNodes, proxyContainers, statements);
	}

	/**
	 * Create a Data lineage result for collection of {@link ProxyContainerPrototype}
	 *
	 * @param proxyContainers collection of {@link ProxyContainerPrototype}
	 * @return {@link DataLineageResult}
	 */
	public static DataLineageResult ofProxyContainers(final Collection<ProxyContainerPrototype> proxyContainers) {
		return new DataLineageResult(Collections.emptyList(), proxyContainers, Collections.emptyList());
	}

	/**
	 * Create a Data lineage result along with a collection of {@link ProxyContainerPrototype}
	 *
	 * @param proxyContainers collection of {@link ProxyContainerPrototype}
	 * @return {@link DataLineageResult}
	 */
	public DataLineageResult withProxyContainers(final Collection<ProxyContainerPrototype> proxyContainers) {
		return new DataLineageResult(dataFlowNodes, proxyContainers, statements);
	}

	/**
	 * Create a Data lineage result for collection of {@link ProxyContainerPrototype}
	 *
	 * @param statements collection of statements {@link DataFlowNodePrototype}
	 * @return {@link DataLineageResult}
	 */
	public static DataLineageResult ofStatements(final Collection<DataFlowNodePrototype> statements) {
		return new DataLineageResult(Collections.emptyList(), Collections.emptyList(), statements);
	}

	/**
	 * Create a Data lineage result along with a collection of {@link ProxyContainerPrototype}
	 *
	 * @param statements collection of statements {@link DataFlowNodePrototype}
	 * @return {@link DataLineageResult}
	 */
	public DataLineageResult withStatements(final Collection<DataFlowNodePrototype> statements) {
		return new DataLineageResult(dataFlowNodes, proxyContainers, statements);
	}

	public DataLineageResult combinedWith(final DataLineageResult ... results) {
		return combinedWith(List.of(results));
	}

	/**
	 * Combines the current {@link DataLineageResult} with the collection of results
	 * @param results collection of {@link DataLineageResult}
	 * @return {@link DataLineageResult}
	 */
	public DataLineageResult combinedWith(final Collection<DataLineageResult> results) {
		final List<DataLineageResult> combined = new ArrayList<>(results.size() + 1);
		combined.add(this);
		combined.addAll(results);
		return combine(combined);
	}

	/**
	 * Creates an empty {@link DataLineageResult}
	 *
	 * @return {@link DataLineageResult}
	 */
	public static DataLineageResult empty() {
		return new DataLineageResult(Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
	}

	public static DataLineageResult combine(final DataLineageResult ... results) {
		return combine(List.of(results));
	}

	
	/**
	 * Combine the fields, statements and proxy containers of the result collection into a single {@link DataLineageResult}
	 *
	 * @param results collection of {@link DataLineageResult}
	 * @return finalized {@link DataLineageResult}
	 */
	public static DataLineageResult combine(final Collection<DataLineageResult> results) {
		final Collection<DataFlowNodePrototype> dataFlowNodes = new ArrayList<>();
		final Collection<ProxyContainerPrototype> proxyContainers = new ArrayList<>();
		final Collection<DataFlowNodePrototype> statements = new ArrayList<>();

		for (final DataLineageResult result : results) {
			dataFlowNodes.addAll(result.getDataFlowNodes());
			proxyContainers.addAll(result.getProxyContainers());
			statements.addAll(result.getStatements());
		}

		return new DataLineageResult(dataFlowNodes, proxyContainers, statements);
	}

	/**
	 * Returns the collection of fields {@link DataFlowNodePrototype} 
	 *
	 * @return the collection of fields {@link DataFlowNodePrototype}
	 */
	public Collection<DataFlowNodePrototype> getDataFlowNodes() {
		return dataFlowNodes;
	}

	/**
	 * Returns the collection of proxy containers {@link ProxyContainerPrototype} 
	 *
	 * @return the collection of proxy containers {@link ProxyContainerPrototype}
	 */
	public Collection<ProxyContainerPrototype> getProxyContainers() {
		return proxyContainers;
	}

	/**
	 * Returns the collection of statements {@link DataFlowNodePrototype} 
	 *
	 * @return the collection of statements {@link DataFlowNodePrototype}
	 */
	public Collection<DataFlowNodePrototype> getStatements() {
		return statements;
	}
}
