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
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;

/**
 * HTTP REST service for getting all references for a given module.
 */
public class FindAllReferencesForModule extends ModuleIdService<FindAllReferencesForModule, ModuleRelationshipPojo[]> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/references";

	private Optional<RelationshipType> relationship = Optional.empty();
	private RelationshipDirection direction = RelationshipDirection.BOTH;
	private boolean distinct;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllReferencesForModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the {@link RelationshipType} to use.
	 *
	 * @param relationship the {@link RelationshipType}
	 * @return {@code this}
	 */
	public FindAllReferencesForModule setRelationship(final RelationshipType relationship) {
		this.relationship = Optional.of(relationship);
		return this;
	}
	

	/**
	 * Sets the {@link RelationshipDirection} to use.
	 *
	 * @param direction the {@link RelationshipDirection}
	 * @return {@code this}
	 */
	public FindAllReferencesForModule setDirection(final RelationshipDirection direction) {
		this.direction = direction;
		return this;
	}

	/**
	 * Returns the {@code distinct} option that, if {@code true}, removes duplicate dependencies.
	 * 
	 * @return The distinct option
	 */
	public boolean getDistinct() {
		return distinct;
	}

	/**
	 * Sets the {@code distinct} option that, if {@code true}, removes duplicate dependencies.
	 * 
	 * @param distinct The distinct option
	 * @return The instance of {@code this}, for method chaining; not {@code null}
	 */
	public FindAllReferencesForModule setDistinct(final boolean distinct) {
		this.distinct = distinct;
		return this;
	}
	/**
	 * Finds all references of a given module by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * If {@link #setRelationship(RelationshipType)} was called, a parameter with the same name is added to the request in order to filter the references.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given module or project does not exist
	 * 
	 * @return a result holding all {@linkplain ModuleRelationshipPojo ModuleRelationshipPojos} on success
	 */
	@Override
	public Result<ModuleRelationshipPojo[]> execute() throws IOException {
		validate();
		try {
			final var uri = new URIBuilder(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
			uri.addParameter("direction", direction.toString());
			uri.addParameter("distinct", Boolean.toString(distinct));
			
			relationship.ifPresent(value -> uri.addParameter("relationship", value.toString()));
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		return execute(httpGet(), new TypeReference<ModuleRelationshipPojo[]>() {});
	}
}
