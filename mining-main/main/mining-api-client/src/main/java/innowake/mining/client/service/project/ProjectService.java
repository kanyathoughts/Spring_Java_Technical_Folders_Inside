/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.project;

import java.util.function.Consumer;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;

/**
 * Base service for projects endpoints.
 */
public abstract class ProjectService extends RestService<ProjectPojo> {

	@Nullable
	protected ProjectPojoPrototype project;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected ProjectService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 * @param serviceUrl the endpoint to use
	 */
	protected ProjectService(final ConnectionInfo connectionInfo, final String serviceUrl) {
		super(connectionInfo, serviceUrl);
	}

	/**
	 * Sets the project.
	 *
	 * @param project the project
	 * @return {@code this}
	 */
	public ProjectService setProject(final ProjectPojoPrototype project) {
		this.project = project;
		return this;
	}
	
	/**
	 * Sets the project.
	 *
	 * @param projectBuilder Builder for the project prototype.
	 * @return {@code this}
	 */
	public ProjectService setProject(final Consumer<ProjectPojoPrototype> projectBuilder) {
		this.project = new ProjectPojoPrototype();
		projectBuilder.accept(project);
		return this;
	}

	@Override
	protected void validate() {
		if (project == null) {
			throw new IllegalStateException("Project must be set.");
		}
	}
}
