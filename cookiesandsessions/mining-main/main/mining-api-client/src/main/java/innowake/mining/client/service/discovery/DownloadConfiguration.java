/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.*/
package innowake.mining.client.service.discovery;

import java.io.IOException;

import org.apache.commons.io.IOUtils;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service to download configurations for a project.
 */
public class DownloadConfiguration extends ProjectIdService<DownloadConfiguration, byte[]> {

	/**
	 * The endpoint for running Download Config.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/discovery/config";

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	DownloadConfiguration(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Executes Download Config by sending a HTTP GET request to the specified
	 * {@value #ENDPOINT}.
	 * <p>
	 * Returns The following status codes:
	 * <li><strong>204</strong>: On success
	 * <li><strong>404</strong>: If the given project does not exist
	 * 
	 * @return A result holding the zip files which contains the discovery configuration files.
	 * @throws IOException In case of an error
	 */
	@Override
	public Result<byte[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		try {
			return execute(httpGet(), new TypeReference<byte[]>() {
			}, response -> {
				try {
					return IOUtils.toByteArray(response.getEntity().getContent());
				} catch (final UnsupportedOperationException | IOException e) {
					throw new IllegalStateException(e);
				}
			});
		} catch (final IllegalStateException e) {
			throw new IOException(e);
		}

	}
}
