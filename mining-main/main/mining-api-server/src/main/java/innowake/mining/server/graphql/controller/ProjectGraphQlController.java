/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.DISCOVERY_LIGHT;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * Controller for the "project" GraphQl Query.
 */
@Controller
public class ProjectGraphQlController implements MiningDataPointSource {
	
	@Autowired
	private ProjectService projectService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private CustomPropertiesService customPropertiesService;
	
	@MiningQueryMapping
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({VIEWER})
	public ProjectPojo project(@Argument final EntityId projectId) {
		return projectService.get(projectId);
	}

	@SchemaMapping(typeName = "Project")
	@MiningDataPoint(scalarType = ScalarType.JSON)
	public List<SearchOrder> searchOrders(final ProjectPojo project) {
		return project.getSearchOrders();
	}

	@SchemaMapping(typeName = "Project")
	public ClientPojo client(final ProjectPojo project) {
		return clientService.get(project.getClient(), false);
	}

	@SchemaMapping(typeName = "Project")
	@MiningDataPoint(scalarType = ScalarType.JSON)
	public Map<String, Set<String>> customPropertyClasses(final ProjectPojo project) {
		return project.getCustomPropertyClasses();
	}

	@SchemaMapping(typeName = "Project")
	@MiningDataPoint(scalarType = ScalarType.JSON)
	public Map<String, Set<String>> autoCompletionMap(final ProjectPojo project) {
		return customPropertiesService.getEnumsAndValues(project.identity());
	}
	
	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		/* add ProjectPojo to schema */
		builder.defineType("Project")
			.representedBy(ProjectPojo.class)
			.withDefaultProperties()
			.add();
		builder.defineDataPointsFromSchemaMappingAnnotations(this);
	}
}
