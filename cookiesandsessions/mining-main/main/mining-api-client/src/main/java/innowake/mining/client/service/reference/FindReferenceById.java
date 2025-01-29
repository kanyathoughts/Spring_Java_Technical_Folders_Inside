/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.reference;

import java.io.IOException;
import java.util.UUID;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ModuleRelationshipPojo;


/**
 * HTTP REST service for finding a single reference given an ID.
 */
public class FindReferenceById extends ModuleIdService<FindReferenceById, ModuleRelationshipPojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/references/%s";
	
	@Nullable
	private UUID referenceId;

	FindReferenceById(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the ID of the reference to delete.
	 *
	 * @param referenceId the ID of the reference to delete
	 * @return {@code this}
	 */
	public FindReferenceById setReferenceId(final UUID referenceId) {
		this.referenceId = referenceId;
		return getThis();
	}

	/**
	 * Finds a reference given its ID by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project, module or reference does not exist
	 * 
	 * @return a result holding the found {@link ModuleRelationshipPojo} if the call was successful
	 */
	@Override
	public Result<ModuleRelationshipPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId), referenceId));
		return execute(httpGet(), new TypeReference<ModuleRelationshipPojo>() {});
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (referenceId == null) {
			throw new IllegalStateException("Reference ID must be set.");
		}
	}

}
