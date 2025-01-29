/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.reference;

import java.io.IOException;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;


/**
 * HTTP REST service for deleting a reference.
 */
public class DeleteReference extends ModuleIdService<DeleteReference, Void> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/references/%s";

	@Nullable
	private UUID referenceId;

	DeleteReference(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the ID of the reference to delete.
	 *
	 * @param referenceId the ID of the reference to delete
	 * @return {@code this}
	 */
	public DeleteReference setReferenceId(final UUID referenceId) {
		this.referenceId = referenceId;
		return this;
	}

	/**
	 * Deletes a reference by sending a HTTP DELETE request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status code:
	 * <li><strong>204</strong>: regardless if the reference exists or not 
	 * 
	 * @return a result holding only the status code of the response
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId), referenceId));
		return execute(httpDelete());
	}

	@Override
	protected void validate() {
		super.validate();
		if (referenceId == null) {
			throw new IllegalStateException("Reference ID must be set.");
		}
	}
}
