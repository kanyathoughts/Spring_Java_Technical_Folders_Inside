/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import java.io.IOException;

import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * HTTP REST service for create new taxonomy.
 */
public class CreateTaxonomy extends TaxonomyService<CreateTaxonomy> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	CreateTaxonomy(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Creates a new taxonomy by sending a HTTP POST request to {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>201</strong>: on success
	 * <li><strong>400</strong>: if the given {@link TaxonomyPojo} is not valid
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the newly created {@link TaxonomyPojo} if the call was successful
	 */
	@Override
	public Result<TaxonomyPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(taxonomy), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<TaxonomyPojo>() {});
	}
}
