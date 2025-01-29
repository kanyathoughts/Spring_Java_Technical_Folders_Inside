/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.metamodel;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.CustomPropertyMetadata;


/**
 * HTTP REST service for getting the metadata of a given custom property of a class.
 */
public class FindPropertyMetadata extends MetamodelService<FindPropertyMetadata, CustomPropertyMetadata> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/metamodel/%s/%s";
	
	@Nullable
	private String propertyName;

	FindPropertyMetadata(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds metadata information of a given custom property by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given class or custom property does not exist
	 * 
	 * @return a result holding the found {@link CustomPropertyMetadata} if the call was successful
	 */
	@Override
	public Result<CustomPropertyMetadata> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, className, propertyName));
		return execute(httpGet(), new TypeReference<CustomPropertyMetadata>() {});
	}

	/**
	 * Sets the name of the property.
	 *
	 * @param propertyName the name of the property
	 * @return the service instance for chaining
	 */
	public FindPropertyMetadata setPropertyName(final String propertyName) {
		this.propertyName = propertyName;
		return this;
	}

	@Override
	protected void validate() {
		super.validate();
		if (propertyName == null || (propertyName != null && propertyName.isEmpty())) {
			throw new IllegalStateException("Property name must be set");
		}
	}
}
