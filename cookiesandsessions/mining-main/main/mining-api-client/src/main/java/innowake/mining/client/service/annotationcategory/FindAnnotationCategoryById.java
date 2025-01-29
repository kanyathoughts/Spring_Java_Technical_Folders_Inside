/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotationcategory;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.AnnotationCategory;

/**
 * HTTP REST service for find single annotation category.
 */
public class FindAnnotationCategoryById extends ProjectIdService<FindAnnotationCategoryById, AnnotationCategory> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotation-categories/%s";
	
	@Nullable
	private Long annotationCategoryId;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAnnotationCategoryById(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the annotation category id to use.
	 *
	 * @param annotationCategoryId the annotation category id
	 * @return {@code this}
	 */
	public FindAnnotationCategoryById setAnnotationCategoryId(final Long annotationCategoryId) {
		this.annotationCategoryId = annotationCategoryId;
		return this;
	}

	/**
	 * Finds an annotation category by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given annotation category id does not exist
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the found {@link AnnotationCategory} if the call was successful
	 */
	@Override
	public Result<AnnotationCategory> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), annotationCategoryId));
		return execute(httpGet(), new TypeReference<AnnotationCategory>() {});
	}

	@Override
	protected void validate() {
		super.validate();
		if (annotationCategoryId == null) {
			throw new IllegalStateException("Annotation category id must be set.");
		}
	}
}
