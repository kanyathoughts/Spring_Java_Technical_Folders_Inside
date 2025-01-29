/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing.service;

import innowake.mining.server.datalineage.context.DataLineageContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.mining.server.datalineage.fieldtracing.tracedmodule.TracedModuleProvider;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.ndt.fieldtracing.model.TracedModule;

/**
 * Utility class for providing a traced module when given a projectId and a moduleId
 */
@Service
public class TracedModuleProviderService {

	@Autowired
	private ProjectService projectService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private SourceCachingService sourceService;
	
	/**
	 * Provides a {@link TracedModule} instance based on the module with the specified id and projectId.
	 *
	 * @param context the context of the data lineage operation in which the field tracer is used
	 * @param moduleId id of the traced module
	 * @return traced module
	 */
	public TracedModule<ModuleLightweightPojo> provideTracedModule(final DataLineageContext context, final EntityId moduleId) {
		if ( ! projectService.isValid(context.getProjectId())) {
			throw new IllegalArgumentException("While obtaining a TracedModuleProvider for Project " + context.getProjectId() + ": the Project does not exist");
		}
		return new TracedModuleProvider(context, moduleService, sourceService)
				.provideTracedModuleFromModule(moduleId);
	}
	
}

