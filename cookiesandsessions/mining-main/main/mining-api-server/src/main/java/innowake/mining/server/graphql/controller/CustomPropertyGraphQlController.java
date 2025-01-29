/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.stereotype.Controller;

import graphql.execution.DataFetcherResult;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.service.UserRoleService;

/**
 * Controller for the {@linkplain CustomPropertyMetadata annotations} GraphQl Query.
 */
@Controller
public class CustomPropertyGraphQlController implements MiningDataPointSource {
	
	private static final String CUSTOM_PROPERTY_TYPE_NAME = "CustomProperty";
	
	@Autowired
	private CustomPropertiesService customPropertiesService;
	
	@Autowired
	private UserRoleService userRoleService;
	
	/**
	 * Query for {@linkplain CustomPropertyMetadata} of a module.
	 * 
	 * @param projectId the ID of the project that contains the annotation
	 * @param entityName the name of the Entity
	 * @return the list of statements contained in the annotation
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public DataFetcherResult<List<CustomPropertyMetadata>> customProperties(@Argument final Long projectId, @Argument final String entityName) {
		final List<CustomPropertyMetadata> result = customPropertiesService.findPropertyDefinitions(q -> q.withParentEntity(EntityId.of(projectId), entityName));
		
		return DataFetcherResult.<List<CustomPropertyMetadata>>newResult()
				.data(result)
				.localContext(new ControllerLocalContext(projectId, userRoleService.getProjectIds(), userRoleService.getClientAdminIds(),
						userRoleService.isAdmin()))
				.build();
	}

	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		/* add CustomPropertyMetadata to schema */
		builder.defineType(CUSTOM_PROPERTY_TYPE_NAME)
			.representedBy(CustomPropertyMetadata.class)
			.withDefaultProperties()
			.add();
		builder.defineDataPointsFromSchemaMappingAnnotations(this);
	}
}
