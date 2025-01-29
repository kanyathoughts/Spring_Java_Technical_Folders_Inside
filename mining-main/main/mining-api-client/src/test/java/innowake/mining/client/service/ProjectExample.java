/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;

/**
 * Command line runner to test project services.
 */
public class ProjectExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", "");
		final ProjectServiceProvider service = MiningApiClient.projectService(connectionInfo);

		LOG.info("find all projects" + MARK);
		LOG.info("Status message: " + service.findAllProjects().execute().getStatusMessage());
		
		LOG.info("find project with id=1" + MARK);
		final Result<ProjectPojo> foundProjectResult = service.findProjectById().setProjectId(EntityId.of(1l)).execute();
		final ProjectPojo foundProject = foundProjectResult.getValue().orElse(null);
		LOG.info("Status message: " + foundProjectResult.getStatusMessage());
		
		LOG.info("find non existent project" + MARK);
		LOG.info("Status message: " + service.findProjectById().setProjectId(EntityId.of(-1l)).execute().getStatusMessage());
		
		LOG.info("create new project" + MARK);
		final ProjectPojoPrototype newProject = new ProjectPojoPrototype();
		newProject.setName("A new Project 3");
		newProject.setClient(EntityId.of(1L));
		LOG.info("Status message: " + service.createProject().setProject(newProject).execute().getStatusMessage());
		
		LOG.info("create new project with empty name" + MARK);
		final ProjectPojoPrototype newProject2 = new ProjectPojoPrototype();
		LOG.info("Status message: " + service.createProject().setProject(newProject2).execute().getStatusMessage());
		
		LOG.info("update an existing project with a new name" + MARK);
		LOG.info("Status message: " + service.updateProject().setProject(p -> p.withId(foundProject.identity()).setName("A new name for the project"))
				.execute().getStatusMessage());
		
		LOG.info("update an existing project with an existing name" + MARK);
		LOG.info("Status message: " + service.updateProject().setProject(p -> p.withId(foundProject.identity()).setName("Demo project 2"))
				.execute().getStatusMessage());
		
		LOG.info("update an existing project with an empty name" + MARK);
		final ProjectPojoPrototype newProject3 = new ProjectPojoPrototype();
		newProject3.setNid(1l);
		LOG.info("Status message: " + service.updateProject().setProject(newProject3).execute().getStatusMessage());
		
		LOG.info("update a non existing project" + MARK);
		final ProjectPojoPrototype newProject4 = new ProjectPojoPrototype();
		newProject4.setNid(99l);
		newProject4.setName("project does not exist");
		LOG.info("Status message: " + service.updateProject().setProject(newProject4).execute().getStatusMessage());
		
		LOG.info("create new project with non existent client" + MARK);
		final ProjectPojoPrototype newProject5 = new ProjectPojoPrototype();
		newProject5.setName("A new Project 5");
		newProject5.setClient(EntityId.of(-1L));
		LOG.info("Status message: " + service.createProject().setProject(newProject5).execute().getStatusMessage());
	}
}
