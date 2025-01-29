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
public abstract class ModuleIdService<S extends ModuleIdService<S, T>, T> extends ProjectIdService<S, T> {

	@Nullable
	protected EntityId moduleId;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public ModuleIdService(ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the module id to use.
	 *
	 * @param moduleId the module id
	 * @return {@code this}
	 */
	public S setModuleId(final EntityId moduleId) {
		this.moduleId = moduleId;
		return getThis();
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (moduleId == null) {
			throw new IllegalStateException("Module ID must be set.");
		}
	}

}
