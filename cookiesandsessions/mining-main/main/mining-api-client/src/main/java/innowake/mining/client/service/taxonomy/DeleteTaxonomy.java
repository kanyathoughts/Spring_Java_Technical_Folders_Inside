/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.access.EntityId;

/**
 * HTTP REST service for delete taxonomy.
 */
public class DeleteTaxonomy extends ProjectIdService<DeleteTaxonomy, Void> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies/%s";
	
	@Nullable
	private EntityId taxonomyId;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	DeleteTaxonomy(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Deletes a taxonomy by sending a HTTP DELETE request to {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status code:
	 * <li><strong>204</strong>: regardless if the taxonomy exists or not 
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding only the status code of the response
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(taxonomyId)));
		return execute(httpDelete(), new TypeReference<Void>() {});
	}

	/**
	 * Sets the taxonomy id.
	 *
	 * @param taxonomyId the id of the taxonomy.
	 * @return {@code this}
	 */
	public DeleteTaxonomy setId(final EntityId taxonomyId) {
		this.taxonomyId = taxonomyId;
		return this;
	}
	
	@Override
	protected void validate() {
		if (taxonomyId == null) {
			throw new IllegalStateException("Taxonomy id must be set.");
		}
		if ( ! assertNotNull(taxonomyId).hasNid() && ! assertNotNull(taxonomyId).hasUid()) {
			throw new IllegalStateException("Taxonomy numeric or unique id must be set.");
		}
		super.validate();
	}
}
