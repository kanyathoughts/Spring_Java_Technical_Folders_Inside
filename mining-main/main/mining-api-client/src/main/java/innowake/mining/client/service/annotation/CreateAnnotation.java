/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import java.io.IOException;

import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * HTTP REST service for create new annotation with a reference to a module.
 */
public class CreateAnnotation extends AnnotationService<CreateAnnotation> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/annotations";

	@Nullable
	protected EntityId moduleId;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	CreateAnnotation(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the module id to use.
	 *
	 * @param moduleId the module id
	 * @return {@code this}
	 */
	public CreateAnnotation setModuleId(final EntityId moduleId) {
		this.moduleId = moduleId;
		return this;
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (moduleId == null) {
			throw new IllegalStateException("Module ID must be set.");
		}
	}

	/**
	 * Creates a new annotation by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>201</strong>: on success
	 * <li><strong>400</strong>: if the given annotation is not valid
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding the newly created {@link AnnotationPojo} if the call was successful
	 */
	@Override
	public Result<AnnotationPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(annotation), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<AnnotationPojo>() {});
	}
}
