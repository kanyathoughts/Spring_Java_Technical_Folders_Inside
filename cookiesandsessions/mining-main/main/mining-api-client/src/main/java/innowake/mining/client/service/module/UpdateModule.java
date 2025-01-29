package innowake.mining.client.service.module;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;

/**
 * HTTP REST service for updating a module.
 */
public class UpdateModule extends ProjectIdService<UpdateModule, ModulePojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s";
	
	@Nullable
	private ModulePojoPrototype module;

	UpdateModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the module to update.
	 *
	 * @param module the module to update
	 * @return {@code this}
	 */
	public UpdateModule setModule(final ModulePojoPrototype module) {
		this.module = module;
		return this;
	}
	
	/**
	 * Updates a module by sending a HTTP PUT request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>400</strong>: if the given module is not valid
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding the updated {@link ModulePojo} if the call was successful
	 */
	@Override
	public Result<ModulePojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(assertNotNull(module).identityProvisional())));
		final HttpPut put = httpPut();
		put.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(module), ContentType.APPLICATION_JSON));
		return execute(put, new TypeReference<ModulePojo>() {});
	}

	@Override
	protected void validate() {
		super.validate();
		if (module == null) {
			throw new IllegalStateException("Module must be set.");
		}

		if ( ! assertNotNull(module).uid.isPresent() && ! assertNotNull(module).nid.isPresent()) {
			throw new IllegalStateException("Module numeric or unique id must be set.");
		}
	}
}
