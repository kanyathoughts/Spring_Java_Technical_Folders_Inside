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
import innowake.mining.client.service.RouteConfiguration;


/**
 * HTTP REST service for exporting a Effort Summary Excel workbook.
 */
public class ExportEffortSummaryExcel extends ProjectIdService<ExportEffortSummaryExcel, Tuple2<String, byte[]>> {

	/**
	 * The endpoint for exporting a Effort Summary Excel workbook.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/effort-summary-excel";

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	protected ExportEffortSummaryExcel(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public Result<Tuple2<String, byte[]>> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		
		try {
			return execute(httpGet(), new TypeReference<Tuple2<String, byte[]>>() {}, response -> {
				
				final Optional<String> fileName = Stream.of(response.getAllHeaders())
					.filter(header1 -> header1.getName().equals(HttpHeaders.CONTENT_DISPOSITION))
					.findFirst()
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

}
