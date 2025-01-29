/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.base;

import java.util.List;

import com.google.common.collect.ImmutableList;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.controller.cfg.ControlFlowSupport;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Base implementation of a task working on the program module.
 */
public abstract class GenericProgramModulesTask extends ModuleTask {
	
	protected static final List<Tuple2<Technology, Type>> SUPPORTED;
	
	static {
		SUPPORTED = ControlFlowSupport.getActuallySupportedTypes().stream()
				.map(nodeType -> Tuple2.of(nodeType.getTechnology(), nodeType.getType()))
				.collect(ImmutableList.toImmutableList());
	}
	
	/**
	 * Constructor.
	 * 
	 * @param progressMonitor the progress monitor to use
	 * @param jobId the Id of the job this task belongs to
	 * @param projectId the Id of the project
	 * @param moduleId the id of the module to process
	 */
	public GenericProgramModulesTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId, projectId, moduleId);
	}
}
