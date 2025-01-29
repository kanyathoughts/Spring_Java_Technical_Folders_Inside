/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import java.io.IOException;
import java.util.Optional;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.net.HttpHeaders;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;

/**
 * HTTP REST service for exporting a Discovery result file.
 *
 * @param <S> the type of the actual service
 */
public abstract class AbstractExportService<S extends AbstractExportService<S>> extends ProjectIdService<S, Tuple2<String, byte[]>> {

	/**
	 * Constructor.
	 *
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	AbstractExportService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Exports an Discovery result file by sending a HTTP get request to the specified end point.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 *
	 * @return a result holding the file name and content
	 * @throws IOException in case of an error
	 */
	@Override
	public Result<Tuple2<String, byte[]>> execute() throws IOException {
		validate();

		try {
			return execute(httpGet(), new TypeReference<Tuple2<String, byte[]>>() {}, response -> {

				final Optional<String> fileName = Stream.of(response.getAllHeaders())
						.filter(header1 -> header1.getName().equals(HttpHeaders.CONTENT_DISPOSITION))
						.findFirst()
						.map(header2 -> header2.getValue().split("=")[1]);

				try {
					final byte[] bytes = IOUtils.toByteArray(response.getEntity().getContent());
					return new Tuple2<>(fileName.orElse("<unavailable>"), bytes);
				} catch (final UnsupportedOperationException | IOException e) {
					throw new IllegalStateException(e);
				}
			});
		} catch (final IllegalStateException e) {
			throw new IOException(e);
		}
	}
}
