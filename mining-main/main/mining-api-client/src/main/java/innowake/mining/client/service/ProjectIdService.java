/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.shared.access.EntityId;


/**
 * Base class for all REST services that include a project id.
 * 
 * @param <T> the type of the service result 
 * @param <S> the type of the actual service 
 */
public abstract class ProjectIdService<S extends ProjectIdService<S, T>, T> extends RestService<T> {
	
	@Nullable
	protected EntityId projectId;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public ProjectIdService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the project id to use.
	 *
	 * @param projectId the project id
	 * @return {@code this}
	 */
	public S setProjectId(final EntityId projectId) {
		this.projectId = projectId;
		return getThis();
	}
	
	/**
	 * Sets the project id to use.
	 *
	 * @param projectId the project id
	 * @return {@code this}
	 */
	public S setProjectId(final Long projectId) {
		this.projectId = EntityId.of(projectId);
		return getThis();
	}

	@Override
	protected void validate() {
		if (projectId == null) {
			throw new IllegalStateException("Project id must be set.");
		}
	}
	
	/**
	 * Returns this object for method chaining.
	 *
	 * @return this object
	 */
	@SuppressWarnings("unchecked")
	protected S getThis() {
		return (S) this;
	}
}
