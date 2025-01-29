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
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;


/**
 * HTTP REST service for deleting all modules of a project.
 */
public class DeleteAllModules extends ProjectIdService<DeleteAllModules, Void> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules";

	private Optional<Boolean> deleteSourceObjects = Optional.empty();

	DeleteAllModules(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the value {@code true} if the source objects should be deleted.
	 *
	 * @param deleteSourceObjects The boolean value for deleteSourceObjects.
	 * @return {@code this} instance.
	 */
	public DeleteAllModules setDeleteSourceObjects(@Nullable final Boolean deleteSourceObjects) {
		this.deleteSourceObjects = Optional.of(deleteSourceObjects);
		return this;
	}

	/**
	 * Deletes all modules of a particular project by sending a HTTP DELETE request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status code:
	 * <li><strong>204</strong>: regardless if there are modules present or not 
	 * 
	 * @return a result holding only the status code of the response
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(createUrl());
		return execute(httpDelete(), new TypeReference<Void>() {});
	}
	
	private String createUrl() {
		final URIBuilder uri;
		try {
			uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		uri.addParameter("deleteSourceObjects", deleteSourceObjects.orElse(Boolean.TRUE).toString());
		return uri.toString();
	}
}
