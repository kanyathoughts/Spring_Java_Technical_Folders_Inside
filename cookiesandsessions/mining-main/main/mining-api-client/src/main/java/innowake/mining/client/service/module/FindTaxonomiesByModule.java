package innowake.mining.client.service.module;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * HTTP REST service for find all taxonomies by module.
 */
public class FindTaxonomiesByModule extends ModuleIdService<FindTaxonomiesByModule, TaxonomyPojo[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/taxonomies";
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	FindTaxonomiesByModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all taxonomies of a given module by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding all {@linkplain TaxonomyPojo taxonomies} on success
	 */
	@Override
	public Result<TaxonomyPojo[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		return execute(httpGet(), new TypeReference<TaxonomyPojo[]>() {});
	}

}
