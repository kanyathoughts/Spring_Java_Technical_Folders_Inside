/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import java.io.IOException;

import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.DataDictionaryPojo;


/**
 * HTTP REST service for creating a new data dictionary entry.
 */
public class CreateDataDictionaryEntry extends DataDictionaryEntryService<CreateDataDictionaryEntry> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/data-dictionary";

	CreateDataDictionaryEntry(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Creates a new data dictionary entry by sending a HTTP POST request to {@value #ENDPOINT}.
	 * <p>
	 * <li><strong>201</strong>: on success
	 * <li><strong>400</strong>: if the given data dictionary entry is not valid
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding the newly created {@link DataDictionaryPojo} if the call was successful
	 */
	@Override
	public Result<DataDictionaryPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(dataDictionaryEntry), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<DataDictionaryPojo>() {});
	}

}
