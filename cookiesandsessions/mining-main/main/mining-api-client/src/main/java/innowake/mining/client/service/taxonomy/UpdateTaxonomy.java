/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * HTTP REST service for update taxonomy.
 */
public class UpdateTaxonomy extends TaxonomyService<UpdateTaxonomy> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies/%s";
	
	@Nullable
	private EntityId taxonomyId;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	UpdateTaxonomy(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Updates a taxonomy by sending a HTTP PUT request to {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>400</strong>: if the given {@link TaxonomyPojo} is not valid
	 * <li><strong>404</strong>: if the given {@link TaxonomyPojo} does not exist
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the updated {@link TaxonomyPojo} if the call was successful
	 */
	@Override
	public Result<TaxonomyPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(taxonomyId)));
		final var put = httpPut();
		put.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(taxonomy), ContentType.APPLICATION_JSON));
		return execute(put, new TypeReference<TaxonomyPojo>() {});
	}

	/**
	 * Sets the id of the taxonomy.
	 *
	 * @param taxonomyId the id of the taxonomy
	 * @return {@code this}
	 */
	public UpdateTaxonomy setId(final EntityId taxonomyId) {
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
