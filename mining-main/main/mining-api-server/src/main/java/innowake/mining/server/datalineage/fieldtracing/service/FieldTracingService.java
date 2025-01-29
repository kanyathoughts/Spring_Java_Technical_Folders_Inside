/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing.service;

import innowake.mining.server.datalineage.context.DataLineageContext;
import java.util.Optional;

import innowake.mining.shared.access.EntityId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.fieldtracing.FieldTracerUtils;
import innowake.ndt.fieldtracing.TextSelection;
import innowake.ndt.fieldtracing.legacy.tracer.IFieldTracer;
import innowake.ndt.fieldtracing.model.Model;
import innowake.ndt.fieldtracing.model.TracedModule;

/**
 * This service collects the correct Tracer and tracedModule and executes the tracing
 */
@Service
public class FieldTracingService {
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private FieldTracerProviderService fieldTracerProviderService;
	@Autowired
	private TracedModuleProviderService tracedModuleProviderService;
	
	/**
	 * Traces a field in the specified module at the specified location.
	 *
	 * @param context the context of the data lineage operation in which the field tracer is used
	 * @param moduleId id of module containing the field that's supposed to be traced
	 * @param location location of the field in the module
	 * @return field tracing result
	 * @throws IllegalArgumentException when module technology is not supported
	 */
	public Model<ModuleLightweightPojo> traceField(final DataLineageContext context, final EntityId moduleId, final ModuleLocation location) throws IllegalArgumentException {
		final Optional<ModuleLightweightPojo> module = moduleService.findAnyModuleLightweight(q -> q.byId(moduleId));
		if (module.isEmpty()) {
			throw new IllegalArgumentException("While attempting to perform field tracing on Module " + moduleId + ": Module not found");
		}
		final IFieldTracer<ModuleLightweightPojo> fieldTracer = fieldTracerProviderService.provideFieldTracer(context, module.get().getTechnology());
		final TracedModule<ModuleLightweightPojo> tracedModule = tracedModuleProviderService.provideTracedModule(context, moduleId);
		
		final TextSelection tSelection = FieldTracerUtils.createSelection(location.getOffset().intValue(), location.getLength().intValue(), "PLACEHOLDER");
		return fieldTracer.trace(tracedModule, tSelection);
	}
}

