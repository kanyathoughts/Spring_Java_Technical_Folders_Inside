/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotationcategory;

import java.io.IOException;

import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.lang.Assert;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.LegacyJsonUtil;
import innowake.mining.shared.model.AnnotationCategory;

/**
 * HTTP REST service for update existing annotation category.
 */
public class UpdateAnnotationCategory extends AnnotationCategoryService {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotation-categories/%s";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	UpdateAnnotationCategory(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Updates an annotation category by sending a HTTP PUT request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>400</strong>: if the given annotation category is not valid
	 * <li><strong>404</strong>: if the given annotation category does not exist
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the updated {@link AnnotationCategory} if the call was successful
	 */
	@Override
	public Result<AnnotationCategory> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), Assert.assertNotNull(annotationCategory).getId()));
		final HttpPut put = httpPut();
		put.setEntity(new StringEntity(LegacyJsonUtil.getMapper().writeValueAsString(annotationCategory), ContentType.APPLICATION_JSON));
		return execute(put, new TypeReference<AnnotationCategory>() {});
	}
}
