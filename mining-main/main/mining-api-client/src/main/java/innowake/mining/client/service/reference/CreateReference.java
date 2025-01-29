/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.reference;

import java.io.IOException;

import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;


/**
 * HTTP REST service for creating a reference.
 */
public class CreateReference extends ModuleIdService<CreateReference, ModuleRelationshipPojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/references";
	
	@Nullable
	private ModuleRelationshipPojoPrototype reference;

	CreateReference(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the reference to create.
	 *
	 * @param reference the reference to create
	 * @return {@code this}
	 */
	public CreateReference setReference(final ModuleRelationshipPojoPrototype reference) {
		this.reference = reference;
		return this;
	}

	/**
	 * Creates a new reference by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>201</strong>: on success
	 * <li><strong>400</strong>: if the given reference is not valid
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding the newly created {@link ModuleRelationshipPojo} if the call was successful
	 */
	@Override
	public Result<ModuleRelationshipPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		final var post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(reference), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<ModuleRelationshipPojo>() {});
	}

	@Override
	protected void validate() {
		super.validate();
		if (reference == null) {
			throw new IllegalStateException("Reference must be set.");
		}
	}
}
