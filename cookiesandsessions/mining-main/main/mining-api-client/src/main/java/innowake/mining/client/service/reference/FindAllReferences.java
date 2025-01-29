/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.reference;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;

/**
 * HTTP REST service for getting all references.
 */
public class FindAllReferences extends ProjectIdService<FindAllReferences, ModuleRelationshipPojo[]> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/references";
	
	private Optional<RelationshipType> relationship = Optional.empty();
	private RelationshipDirection direction = RelationshipDirection.BOTH;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllReferences(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the {@link RelationshipType} to use.
	 *
	 * @param relationship the {@link RelationshipType}
	 * @return {@code this}
	 */
	public FindAllReferences setRelationship(final RelationshipType relationship) {
		this.relationship = Optional.of(relationship);
		return this;
	}
	
	/**
	 * Sets the {@link RelationshipDirection} to use.
	 *
	 * @param direction the {@link RelationshipDirection}
	 * @return {@code this}
	 */
	public FindAllReferences setDirection(final RelationshipDirection direction) {
		this.direction = direction;
		return this;
	}
	
	/**
	 * Finds all references by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * If {@link #setRelationship(RelationshipType)} was called, a parameter with the same name is added to the request in order to filter the references.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all {@linkplain ModuleRelationshipPojo ModuleRelationshipPojos} on success
	 */
	@Override
	public Result<ModuleRelationshipPojo[]> execute() throws IOException {
		validate();
		try {
			final var uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
			uri.addParameter("direction", direction.toString());
			relationship.ifPresent(value -> uri.addParameter("relationship", value.toString()));
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		return execute(httpGet(), new TypeReference<ModuleRelationshipPojo[]>() {});
	}
}
