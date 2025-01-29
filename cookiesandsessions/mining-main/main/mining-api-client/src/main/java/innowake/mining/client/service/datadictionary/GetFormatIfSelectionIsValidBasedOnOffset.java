/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import java.io.IOException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.DataFieldFormat;

/**
 * HTTP REST Service to test if the offset that has been selected is related to valid Data Dictionary entry or not and return the format.
 */
public class GetFormatIfSelectionIsValidBasedOnOffset extends ModuleIdService<GetFormatIfSelectionIsValidBasedOnOffset, DataFieldFormat>{

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/data-dictionary/offset/%s";
	
	@Nullable
	private Long offset;

	/**
	 * Creates a new instance of the service
	 * @param connectionInfo the connection info to use
	 */
	public GetFormatIfSelectionIsValidBasedOnOffset(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the offset to get the DataFieldFormat if selection is valid.
	 *
	 * @param offset the offset to get DataFieldFormat
	 * @return {@code this}
	 */
	public GetFormatIfSelectionIsValidBasedOnOffset setOffset(final Long offset) {
		this.offset = offset;
		return this;
	}

	@Override
	public Result<DataFieldFormat> execute() throws IOException {
		validate();
		final URIBuilder uri = new URIBuilder();
		uri.setPath(String.format(ENDPOINT, encode(projectId), encode(moduleId), offset));
		setServiceUrl(uri.toString());
		return execute(httpGet(), new TypeReference<DataFieldFormat>() {});
	}

	@Override
	protected void validate() {
		super.validate();
		if (offset == null) {
			throw new IllegalStateException("Offset must be set");
		}
	}
}

