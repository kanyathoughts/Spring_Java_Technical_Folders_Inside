/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing.tracedmodule;

import java.util.EnumSet;
import java.util.Set;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.ndt.fieldtracing.cobol.parser.CobolParserActions;
import innowake.ndt.fieldtracing.model.TracedModule;

/**
 * This class provides the functionality to retrieve a TracedModule, either by moduleId or sourceObjectId.
 * Depending on which technology is used it provides the corresponding module
 */
public class TracedModuleProvider {

	private static final Set<Technology> SUPPORTED_TECHNOLOGIES = EnumSet.of(Technology.COBOL, Technology.NATURAL);

	private final DataLineageContext context;

	private final ModuleService moduleService;
	private final SourceCachingService sourceService;
	
	private final CobolParserActions<ModuleLightweightPojo> parserActions;
	
	public TracedModuleProvider(final DataLineageContext context, final ModuleService moduleService, final SourceCachingService sourceService) {
		this.context = context;
		this.moduleService = moduleService;
		this.sourceService = sourceService;
		parserActions = new CobolParserActions<>(context.getCachingFunctionForFieldTracer());
	}
	
	/**
	 * This method provides the TracedModule from a given moduleId
	 * @param moduleId The moduleId of which to return the tracedModule
	 * @return Returns the TracedModule
	 */
	public TracedModule<ModuleLightweightPojo> provideTracedModuleFromModule(final EntityId moduleId) {
        context.getProgressMonitor().checkCanceled();		/* Finds the module by id */
        final ModuleLightweightPojo module = moduleService.getModuleLightweight(moduleId);
		final String path = module.getPath();
		if (path == null) {
			throw new IllegalArgumentException("While attempting to perform field tracing on Module " + moduleId 
					+ ": Only 'main modules' representing an entire source file can be traced."
					+ " This Module has no path and can therefore not be traced.");
		}
		final SourcePojo sourceObject = sourceService.cachingByProjectPath(context.getProjectId().getNid(), path);
		
		return provideTracedModule(module, sourceObject);
	}

	/**
	 * This method provides the TracedModule from a given moduleId
	 * @param sourceObjectId The sourceObjectId of which to return the tracedModule
	 * @return Returns the TracedModule
	 */
	public TracedModule<ModuleLightweightPojo> provideTracedModuleFromSourceObject(final EntityId sourceObjectId) {
		context.getProgressMonitor().checkCanceled();
		final SourcePojo sourceObject = sourceService.findAny(q -> q.byId(sourceObjectId))
				.orElseThrow(() -> new IllegalArgumentException("While attempting to perform field tracing on SourcePojo " + sourceObjectId + ": SourcePojo not found"));
		final EntityId projectId = context.getProjectId();
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(q -> q.ofProject(projectId).withPath(sourceObject.getPath()))
				.orElseThrow(() -> new MiningEntityNotFoundException("Module not found for project: " + projectId + " and path: " + sourceObject.getPath()));
		return provideTracedModule(module, sourceObject);
	}
	
	private TracedModule<ModuleLightweightPojo> provideTracedModule(final ModuleLightweightPojo module, final SourcePojo sourceObject) {
		final Technology technology = module.getTechnology();
		switch(technology) {
			case COBOL:
				return new CobolTracedModule(context, module, sourceObject, this, moduleService, parserActions);
			case NATURAL:
			default: 
				throw new IllegalArgumentException("Tracing a field for " + technology.toString() + " is not supported.");
		}
	}
	

	/**
	 * Performs a number of checks to check whether a module can be traced:
	 * <li>Does the module exist?</li>
	 * <li>Does the module have a path?</li>
	 * <li>Is the technology of the module supported?</li>
	 *
	 * @param projectId id of the project containing the module
	 * @param moduleId id of the module
	 * @return true if checks all evaluate to true; false otherwise
	 */
	public boolean canTraceModule(final EntityId projectId, final EntityId moduleId) {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(b -> b.ofProject(projectId).byId(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId + " in project: " + projectId));

		if (module.getPath() == null) {
			return false;
		}

		return SUPPORTED_TECHNOLOGIES.contains(module.getTechnology());
	}
}

