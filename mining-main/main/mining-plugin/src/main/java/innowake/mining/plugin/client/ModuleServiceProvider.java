/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.module.CreateModule;
import innowake.mining.client.service.module.DeleteAllModules;
import innowake.mining.client.service.module.DeleteModule;
import innowake.mining.client.service.module.FindAllLinkedModules;
import innowake.mining.client.service.module.FindAllModules;
import innowake.mining.client.service.module.FindAnnotationsByModule;
import innowake.mining.client.service.module.FindModuleById;
import innowake.mining.client.service.module.FindModuleByName;
import innowake.mining.client.service.module.FindModuleByPath;
import innowake.mining.client.service.module.GetModuleCount;
import innowake.mining.client.service.module.IdentifyModuleDescriptions;
import innowake.mining.client.service.module.UpdateModule;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides module services with project id already set. 
 */
public class ModuleServiceProvider extends innowake.mining.client.service.module.ModuleServiceProvider {

	@Nullable private final ProjectData projectData;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	ModuleServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	ModuleServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
		projectData = null;
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllModules findAllModules() {
		return init(super.findAllModules());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindModuleByPath findModuleByPath() {
		return init(super.findModuleByPath());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public GetModuleCount getModuleCount() {
		return init(super.getModuleCount());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public CreateModule createModule() {
		return init(super.createModule());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public UpdateModule updateModule() {
		return init(super.updateModule());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DeleteModule deleteModule() {
		return init(super.deleteModule());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAnnotationsByModule findAnnotationsByModule() {
		return init(super.findAnnotationsByModule());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindModuleByName findModuleByName() {
		return init(super.findModuleByName()); 
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DeleteAllModules deleteAllModules() {
		return init(super.deleteAllModules());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public FindModuleById findModuleById() {
		return init(super.findModuleById());
	}
	
	@Override
	public FindAllLinkedModules findAllLinkedModules() {
		return init(super.findAllLinkedModules());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public IdentifyModuleDescriptions identifyModuleDescriptions() {
		return init(super.identifyModuleDescriptions());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		if (projectData != null) {
			return (T) service.setProjectId(projectData.getProjectId());
		}
		return (T) service;
	}
}
