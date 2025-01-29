/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;

import org.apache.http.client.methods.HttpPost;
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
 * HTTP REST service for creating a new module.
 */
public class CreateModule extends ProjectIdService<CreateModule, ModulePojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/";
	
	@Nullable
	private ModulePojoPrototype module;

	CreateModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the module to use.
	 *
	 * @param module the module
	 * @return {@code this}
	 */
	public CreateModule setModule(final ModulePojoPrototype module) {
		this.module = module;
		return this;
	}

	/**
	 * Creates a new module by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>201</strong>: on success
	 * <li><strong>400</strong>: if the given module is not valid
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the newly created {@link ModulePojo} if the call was successful
	 */
	@Override
	public Result<ModulePojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(module), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<ModulePojo>() {});
	}

	@Override
	protected void validate() {
		super.validate();
		if (module == null) {
			throw new IllegalStateException("Module must be set.");
		}
	}
}
