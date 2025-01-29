/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.lang.Assert;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.DataDictionaryPojo;


/**
 * HTTP REST service for updating a {@link DataDictionaryPojo}.
 */
public class UpdateDataDictionaryEntry extends DataDictionaryEntryService<UpdateDataDictionaryEntry> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/data-dictionary/%s";

	UpdateDataDictionaryEntry(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Updates a data dictionary entry by sending a HTTP PUT request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>400</strong>: if the given data dictionary entry is not valid
	 * <li><strong>404</strong>: if the given project, module or data dictionary does not exist
	 * 
	 * @return a result holding the updated {@link DataDictionaryPojo} if the call was successful
	 */
	@Override
	public Result<DataDictionaryPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId), encode(Assert.assertNotNull(dataDictionaryEntry).identityProvisional())));
		final HttpPut put = httpPut();
		put.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(dataDictionaryEntry), ContentType.APPLICATION_JSON));
		return execute(put, new TypeReference<DataDictionaryPojo>() {});
	}

	@Override
	protected void validate() {
		super.validate();

		if ( ! assertNotNull(dataDictionaryEntry).uid.isPresent() && ! assertNotNull(dataDictionaryEntry).nid.isPresent()) {
			throw new IllegalStateException("DataDictionary numeric or unique id must be set.");
		}
	}
}
