/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * HTTP REST service for find all taxonomies.
 */
public class FindAllTaxonomies  extends ProjectIdService<FindAllTaxonomies, TaxonomyPojo[]> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies";
	
	private Optional<String> taxonomyType = Optional.empty();
	
	private Optional<Long> countOnlyModulesWithTaxonomyId = Optional.empty();
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	FindAllTaxonomies(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all taxonomies by sending a HTTP GET request to {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all found {@linkplain TaxonomyPojo taxonomies} if the call was successful
	 */
	@Override
	public Result<TaxonomyPojo[]> execute() throws IOException {
		validate();
		try {
			final URIBuilder uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
			taxonomyType.ifPresent(value -> uri.addParameter("type", value));
			countOnlyModulesWithTaxonomyId.ifPresent(value -> uri.addParameter("countOnlyModulesWithTaxonomyId", String.valueOf(value)));
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		return execute(httpGet(), new TypeReference<TaxonomyPojo[]>() {});
	}

	/**
	 * Sets the taxonomy type.
	 *
	 * @param taxonomyType the taxonomy type
	 * @return {@code this}
	 */
	public FindAllTaxonomies setTaxonomyType(final String taxonomyType) {
		this.taxonomyType = Optional.of(taxonomyType);
		return this;
	}
	
	/**
	 * Sets the modules count with Taxonomy.
	 *
	 * @param countOnlyModulesWithTaxonomyId the ID of the taxonomy to consider for the module count
	 * @return {@code this}
	 */
	public FindAllTaxonomies setCountOnlyModulesWithTaxonomyId(final Long countOnlyModulesWithTaxonomyId) {
		this.countOnlyModulesWithTaxonomyId = Optional.of(countOnlyModulesWithTaxonomyId);
		return this;
		
	}
}
