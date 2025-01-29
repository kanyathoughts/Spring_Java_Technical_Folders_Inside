/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;

/**
 * HTTP REST service for getting the dependencies of a specific module of a given project.
 */
public class TraverseDependencies extends ModuleIdService <TraverseDependencies, DependencyGraph> {
	
	/**
	 * Base endpoint URL.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/graph/dependencies" ;
	
	@Nullable
	private Long maxDepth = Long.valueOf(1);

	@Nullable
	private Boolean distinct;
	
	private Optional<Integer> maxGraphNodes = Optional.empty();

	private Optional<String> graphFilterOptions = Optional.empty();
		
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public TraverseDependencies(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Getter method for maxDepth.
	 * 
	 * @return the depth of the traversal
	 */
	@Nullable
	public Long getMaxDepth() {
		return maxDepth;
	}

	
	/**
	 * Setter Method for maxDepth.
	 *
	 * @param maxDepth depth of traversal
	 * @return the instance of {@code this}
	 */
	public TraverseDependencies setMaxDepth(final Long maxDepth) {
		this.maxDepth = maxDepth;
		return this;
	}

	/**
	 * Returns the {@code distinct} option that, if {@code Boolean.TRUE}, removes duplicate dependencies from the graph.
	 * 
	 * @return The distinct option; can be {@code null}
	 */
	@Nullable
	public Boolean getDistinct() {
		return distinct;
	}

	/**
	 * Sets the {@code distinct} option that, if {@code Boolean.TRUE}, removes duplicate dependencies from the graph.
	 * 
	 * @param distinct The distinct option; can be {@code null}
	 * @return The instance of {@code this}, for method chaining; not {@code null}
	 */
	public TraverseDependencies setDistinct(final Boolean distinct) {
		this.distinct = distinct;
		return this;
	}

	/**
	 * Getter method for maxGraphNodes.
	 * 
	 * @return the maximum graph nodes
	 */
	public Optional<Integer> getMaxGraphNodes() {
		return maxGraphNodes;
	}

	/**
	 * Setter Method for maxGraphNodes.
	 *
	 * @param maxGraphNodes maximum graph nodes to be returned
	 * @return the instance of {@code this}
	 */
	public TraverseDependencies setMaxGraphNodes(@Nullable final Integer maxGraphNodes) {
		this.maxGraphNodes = Optional.ofNullable(maxGraphNodes);
		return this;
	}

	/**
	 * Setter method for filter query.
	 * An example of such a filter would be:
	 * "modules=in=(JCL_PGM, COBOL_PROGRAM) and relationships=in=(CALLS, ACCESSES)"
	 *
	 * @param graphFilterOptions to filter the graph
	 * @return the instance of {@code this}
	 */
	public TraverseDependencies setGraphFilterOptions(@Nullable final String graphFilterOptions) {
		this.graphFilterOptions = Optional.ofNullable(graphFilterOptions);
		return this;
	}

	/**
	 * Generate and return a {@link DependencyGraph} for a given Module
	 * by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <p>
	 * The service may throw an {@link IllegalArgumentException}
	 * if the filter query provided was not of appropriate format.
	 * 
	 * @return {@link DependencyGraph} of specified depth that may be filtered
	 */
	@Override
	public Result<DependencyGraph> execute() throws IOException {
		validate();
		setServiceUrl(createUrl());
		return execute(httpGet(), new TypeReference<DependencyGraph>() {});
	}
	
	@Override
	protected void validate() {
		super.validate() ;
		final Long currentMaxDepth = maxDepth; 
		if (currentMaxDepth == null || currentMaxDepth.longValue() < 1) {
			throw new IllegalStateException(String.format("The value of maxDepth must not be null and a positive integer, was %s", currentMaxDepth));
		}
	} 
	
	private String createUrl() {
		final URIBuilder uri;
		try {
			uri = new URIBuilder(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		if (maxDepth != null) {
			uri.addParameter("maxDepth", maxDepth.toString());
		}
		if (distinct != null) {
			uri.addParameter("distinct", distinct.toString());
		}
		if (maxGraphNodes.isPresent()) {
			uri.addParameter("maxGraphNodes", maxGraphNodes.get().toString());
		}
		graphFilterOptions.ifPresent(filter -> uri.addParameter("query", filter));
		return uri.toString();
	}
}
