/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.metamodel;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.CustomPropertyMetadata;


/**
 * HTTP REST service for getting the metamodel of a given class.
 */
public class FindMetamodel extends MetamodelService<FindMetamodel, CustomPropertyMetadata[]> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/metamodel/%s";

	FindMetamodel(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds the metamodel of a given class by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given class does not exist
	 * 
	 * @return a result holding all {@linkplain CustomPropertyMetadata metadata information} on success
	 */
	@Override
	public Result<CustomPropertyMetadata[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, className));
		return execute(httpGet(), new TypeReference<CustomPropertyMetadata[]>() {});
	}
}
