/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service to submit a job provided by a job extension.
 */
public class SubmitJobExtension extends ProjectIdService<SubmitJobExtension, String> {
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected SubmitJobExtension(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v2/projects/%s/job-extensions/%s";
	
	@Nullable
	private String extensionId;

	@Override
	public Result<String> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), extensionId));
		return execute(httpPostForMultiPart(), new TypeReference<String>() {});
	}

	/**
	 * Sets the id of the extension that provides the job to be submitted.
	 *
	 * @param extensionId the id of the job extension
	 * @return {@code this}
	 */
	public SubmitJobExtension setExtensionId(final String extensionId) {
		this.extensionId = extensionId;
		return this;
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (extensionId == null) {
			throw new IllegalStateException("Extension id must be set.");
		}
	}
}
