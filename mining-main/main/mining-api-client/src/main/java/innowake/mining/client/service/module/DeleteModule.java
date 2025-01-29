package innowake.mining.client.service.module;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for deleting a module.
 */
public class DeleteModule extends ModuleIdService<DeleteModule, Void> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s";

	DeleteModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Deletes a module by sending a HTTP DELETE request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status code:
	 * <li><strong>204</strong>: regardless if the module exists or not 
	 * 
	 * @return a result holding only the status code of the response
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		return execute(httpDelete(), new TypeReference<Void>() {});
	}
}
