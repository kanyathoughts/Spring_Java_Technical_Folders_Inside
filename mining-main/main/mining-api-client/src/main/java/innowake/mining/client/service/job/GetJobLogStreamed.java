/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpEntity;

/**
 * HTTP REST service to request the job log of a single specific job using outlet Streams
 */
public class GetJobLogStreamed extends JobIdService<byte[], GetJobLogStreamed> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v2/jobs/%s/log";

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	GetJobLogStreamed(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Request the job log(s) by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if there is no job with the provided Id
	 *
	 * @return a result holding the job log(s) if the call was successful
	 */
	@Override
	public Result<byte[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, jobId));
		try {
			return execute(httpGet(), new TypeReference<byte[]>() {},
					response -> {
						final byte[] buffer;
						try {
							final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
							final HttpEntity re = response.getEntity();
							buffer = new byte[IOUtils.DEFAULT_BUFFER_SIZE];
							int numOfBytesRead;
							final InputStream in = re.getContent();
							while ((numOfBytesRead = in.read(buffer, 0, buffer.length)) != -1) {
								outputStream.write(buffer, 0, numOfBytesRead);
							}
							return outputStream.toByteArray();
						} catch (final UnsupportedOperationException | IOException e) {
							throw new IllegalStateException("Error while trying to retrieve the job log: ", e);
						}
					});
		} catch (final IllegalStateException e) {
			throw new IOException("Error while trying to retrieve the job log: ", e);
		}
	}
}
