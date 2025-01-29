/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.DataDictionaryPojo;


/**
 * HTTP REST service for searching for data dictionary entries based on their description and data element name.
 * 
 * The search is done case-insensitive and if both attributes are given, both must match.
 */
public class SearchDataDictionaryEntry extends ProjectIdService<SearchDataDictionaryEntry, DataDictionaryPojo[]> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/data-dictionary/search";

	@Nullable
	private String description;

	@Nullable
	private String dataElementName;

	SearchDataDictionaryEntry(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Searches for data dictionary entries given a description AND/OR a data element name.
	 * <p>
	 * This is done by sending a HTTP GET request to {@value #ENDPOINT} with the parameters:
	 * <li>description
	 * <li>dataElementName
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 */
	@Override
	public Result<DataDictionaryPojo[]> execute() throws IOException {
		validate();
		setServiceUrl(createUrl());
		return execute(httpGet(), new TypeReference<DataDictionaryPojo[]>() {});
	}

	/**
	 * Sets the description to search for.
	 *
	 * @param description the description to search for
	 * @return {@code this}
	 */
	public SearchDataDictionaryEntry setDescription(final String description) {
		this.description = description;
		return this;
	}
	
	/**
	 * Sets the data element name to search for.
	 *
	 * @param dataElementName the data element name to search for
	 * @return {@code this}
	 */
	public SearchDataDictionaryEntry setDataElementName(final String dataElementName) {
		this.dataElementName = dataElementName;
		return this;
	}

	@Override
	protected void validate() {
		super.validate();
		if (description == null && dataElementName == null) {
			throw new IllegalStateException("Description AND/OR data element name must be set");
		}
	}

	private String createUrl() {
		final URIBuilder uri;
		try {
			uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		if (description != null) {
			uri.addParameter("description", description);
		}
		if (dataElementName != null) {
			uri.addParameter("dataElementName", dataElementName);
		}
		return uri.toString();
	}

}
