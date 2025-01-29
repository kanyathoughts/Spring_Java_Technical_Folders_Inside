/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.client.service.io;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.collect.Lists;
import com.google.common.net.HttpHeaders;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for exporting mining data to a given format
 */
public class ExportToFormat extends AbstractExportService<ExportToFormat> {

	/**
	 * The end-point for exporting a To Format.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/export/%s";

	private final Map<String, List<String>> parameters = new HashMap<>();

	private String exportFormat = StringUtils.EMPTY;

	/**
	 * Setter for exportFormat.
	 *
	 * @param exportFormat The value of exportFormat.
	 * @return {@code this}
	 */
	public ExportToFormat setExportFormat(final String exportFormat) {
		this.exportFormat = exportFormat;
		return this;
	}

	/**
	 * Constructor.
	 *
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	ExportToFormat(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public Result<Tuple2<String, byte[]>> execute() throws IOException {
		validate();

		final URIBuilder uri;
		try {
			uri = new URIBuilder(String.format(ENDPOINT, encode(projectId), exportFormat));
		} catch (final URISyntaxException e1) {
			throw new IllegalStateException(e1);
		}
		parameters.forEach((param, values) -> values.forEach(value -> uri.addParameter(param, value)));
		setServiceUrl(uri.toString());
		try {
			return execute(httpGet(), new TypeReference<Tuple2<String, byte[]>>() {
			}, response -> {
				final Optional<String> fileName = Stream.of(response.getAllHeaders())
						.filter(header1 -> header1.getName().equals(HttpHeaders.CONTENT_DISPOSITION)).findFirst()
						.map(header2 -> header2.getValue().split("=")[1]);

				try {
					return new Tuple2<>(fileName.orElse("<unavailable>"), IOUtils.toByteArray(response.getEntity().getContent()));
				} catch (final UnsupportedOperationException | IOException e) {
					throw new IllegalStateException(e);
				}
			});
		} catch (final IllegalStateException e) {
			throw new IOException(e);
		}
	}

	/**
	 * Adds the given value to the list of parameter values for param
	 *
	 * @param param URI param key
	 * @param value URI param value
	 * @return {@code this}
	 */
	public ExportToFormat addParameter(final String param, final String value) {
		if (parameters.containsKey(param)) {
			parameters.get(param).add(value);
		} else {
			parameters.put(param, Lists.newArrayList(value));
		}
		return this;
	}

	/**
	 * Sets the given parameter to the given values
	 *
	 * @param param URI param key
	 * @param values URI param values
	 * @return {@code this}
	 */
	public ExportToFormat setParameter(final String param, final List<String> values) {
		parameters.put(param, values);
		return this;
	}
	
	/**
	 * Clears the URI parameters
	 */
	public void clearParameters() {
		parameters.clear();
	}
}
