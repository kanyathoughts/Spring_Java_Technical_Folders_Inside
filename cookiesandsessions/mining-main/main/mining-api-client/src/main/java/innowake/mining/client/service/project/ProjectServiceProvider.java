/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.project;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to project services.
 */
public class ProjectServiceProvider extends ServiceProvider<ProjectServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public ProjectServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllProjects}.
	 *
	 * @return the service instance
	 */
	public FindAllProjects findAllProjects() {
		return new FindAllProjects(connectionInfo);
	}
	
	/**
	 * Access to {@link FindProjectById}.
	 *
	 * @return the service instance
	 */
	public FindProjectById findProjectById() {
		return new FindProjectById(connectionInfo);
	}
	
	/**
	 * Access to {@link CreateProject}.
	 *
	 * @return the service instance
	 */
	public CreateProject createProject() {
		return new CreateProject(connectionInfo);
	}
	
	/**
	 * Access to {@link UpdateProject}.
	 *
	 * @return the service instance
	 */
	public UpdateProject updateProject() {
		return new UpdateProject(connectionInfo);
	}
	
	/**
	 * Access to {@link ResetProjectConfiguration}.
	 *
	 * @return the service instance
	 */
	public ResetProjectConfiguration resetProjectConfiguration() {
		return new ResetProjectConfiguration(connectionInfo);
	}

}