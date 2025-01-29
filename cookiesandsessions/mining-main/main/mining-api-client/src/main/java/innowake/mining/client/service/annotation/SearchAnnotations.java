/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.model.AnnotationSearch;
import innowake.mining.shared.model.WorkingState;

/**
 * HTTP REST service for search for annotations and displaying the result in the Annotation Search View.
 */
public class SearchAnnotations extends ProjectIdService<SearchAnnotations, AnnotationSearch> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotation-search";
	
	private Optional<String> moduleNamePattern = Optional.empty();
	private Optional<WorkingState[]> states = Optional.empty();
	private Optional<Long[]> categoryIds = Optional.empty();
	private Optional<String> modulePath = Optional.empty();
	private Optional<Long> size = Optional.empty();

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	SearchAnnotations(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all annotations with the given naming pattern, states and categoryIds by sending a HTTP GET request to the specified {@value #ENDPOINT} with the filter parameters:
	 * <li>modulename
	 * <li>modulepath
	 * <li>states
	 * <li>categoryIds
	 * <li>size
	 * These filters can be omitted or empty to obtain all annotations.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the {@linkplain AnnotationSearch search result} if the call was successful
	 */
	@Override
	public Result<AnnotationSearch> execute() throws IOException {
		validate();
		try {
			final URIBuilder uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
			moduleNamePattern.ifPresent(value -> uri.addParameter("modulename", value));
			modulePath.ifPresent(path -> uri.addParameter("modulepath", path));
			states.ifPresent(value -> {
				try {
					uri.addParameter("states", PojoMapper.jsonWriter().writeValueAsString(value));
				} catch (final JsonProcessingException e) {
					throw new IllegalStateException(e);
				}
			});
			categoryIds.ifPresent(value -> {
				try {
					uri.addParameter("categoryIds", PojoMapper.jsonWriter().writeValueAsString(value));
				} catch (final JsonProcessingException e) {
					throw new IllegalStateException(e);
				}
			});
			size.ifPresent(value -> uri.addParameter("size", String.valueOf(value)));
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		
		return execute(httpGet(), new TypeReference<AnnotationSearch>() {});
	}


	/**
	 * Sets the pattern for the module name.
	 * The pattern can contain * for any character sequence and ? for any single character.
	 *
	 * @param moduleNamePattern the pattern for the module name
	 * @return {@code this}
	 */
	public SearchAnnotations setModuleNamePattern(final String moduleNamePattern) {
		this.moduleNamePattern = Optional.of(moduleNamePattern);
		return getThis();
	}

	/**
	 * Sets the states.
	 *
	 * @param states the states.
	 * @return {@code this}
	 */
	public SearchAnnotations setStates(final WorkingState[] states) {
		this.states = Optional.of(states);
		return getThis();
	}

	/**
	 * Sets the annotation category IDs.
	 *
	 * @param categoryIds the annotation category IDs
	 * @return {@code this}
	 */
	public SearchAnnotations setCategoryIds(final Long[] categoryIds) {
		this.categoryIds = Optional.of(categoryIds);
		return getThis();
	}

	/**
	 * Sets the path of the module to filter on.
	 *
	 * @param path the path of the module 
	 * @return {@code this}
	 */
	public SearchAnnotations setModulePath(final String path) {
		this.modulePath = Optional.of(path);
		return getThis();
	}
	
	/**
	 * Sets the size of the search result.
	 *
	 * @param size the size of the search result, or {@code null} to reset the value
	 * @return {@code this}
	 */
	public SearchAnnotations setSize(@Nullable final Long size) {
		/* This is nullable in order to reset the size limit */
		this.size = Optional.ofNullable(size);
		return getThis();
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (moduleNamePattern.isPresent() && modulePath.isPresent()) {
			throw new IllegalStateException("Please only provide a path or a module name pattern, but not both.");
		}
	}
}
