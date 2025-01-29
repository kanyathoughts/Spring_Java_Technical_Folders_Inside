/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.OptionalLong;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for exporting source objects.
 */
public class ExportSourceObjects extends ProjectIdService<ExportSourceObjects, byte[]> {

	/**
	 * Receive progress updates during transfer.
	 */
	public interface ProgressCallback {
		/**
		 * Called repeatedly as transfer progresses.
		 *
		 * @param current The number of bytes transfered so far.
		 * @param total The total number of bytes to transfer.
		 */
		public void report(long current, long total);
	}
	
	/**
	 * The endpoint for running Export Source Objects.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/source-objects";
	
	private OptionalLong baseRevision = OptionalLong.empty(); 
	@Nullable
	private ProgressCallback progress = null;

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	ExportSourceObjects(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Setter for baseRevision.
	 *
	 * @param baseRevision The value of baseRevision.
	 * @return {@code this}
	 */
	public ExportSourceObjects setBaseRevision(final OptionalLong baseRevision) {
		this.baseRevision = baseRevision;
		return this;
	}

	/**
	 * Sets an optional handler for receiving progress updates.
	 *
	 * @param handler A routine to be called every buffer cycle with the current progress.
	 * @return {@code this}
	 */
	public ExportSourceObjects progressUpdate(@Nullable final ProgressCallback handler) {
		this.progress = handler;
		return this;
	}

	/**
	 * Executes Export source objects by sending a HTTP POST request to the specified
	 * {@value #ENDPOINT}.
	 * <p>
	 * Returns The following status codes:
	 * <li><strong>204</strong>: On success
	 * <li><strong>404</strong>: If the given project does not exist
	 * 
	 * @return A result holding the zip files which contains .mining-file-index and SourceObjects.
	 * @throws IOException In case of an error
	 */
	@Override
	public Result<byte[]> execute() throws IOException {
		validate();
		
		final URIBuilder uri;
		try {
			uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
		} catch (URISyntaxException e1) {
			throw new IllegalStateException(e1);
		}
		baseRevision.ifPresent(value -> uri.addParameter("baseRevision", String.valueOf(value)));
		setServiceUrl(uri.toString());
		try {
			return execute(httpGet(), new TypeReference<byte[]>() {
			}, response -> {
				final byte[] buffer;
				try {
					final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
					final HttpEntity re = response.getEntity();
					buffer = new byte[IOUtils.DEFAULT_BUFFER_SIZE];
					if (progress != null) {
						progress.report(0, buffer.length);
					}
					int pos = 0;
					int numOfBytesRead;
					final InputStream in = re.getContent();
					while ((numOfBytesRead = in.read(buffer, 0, buffer.length)) != -1) {
						outputStream.write(buffer, 0, numOfBytesRead);
						pos += numOfBytesRead;
						if (progress != null) {
							progress.report(pos, pos);
						}
					}
					return outputStream.toByteArray();
				} catch (final UnsupportedOperationException | IOException e) {
					throw new IllegalStateException(e);
				}
			});
		} catch (final IllegalStateException e) {
			throw new IOException(e);
		}

	}
}