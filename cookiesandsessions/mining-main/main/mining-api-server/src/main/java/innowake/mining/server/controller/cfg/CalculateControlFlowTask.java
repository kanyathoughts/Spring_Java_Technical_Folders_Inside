/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.cfg;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.job.base.ModuleTask;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * {@link Task} implementation that will calculate the control flow for a single module.
 */
public class CalculateControlFlowTask extends ModuleTask {

	@Autowired
	private transient CalculateControlFlowService calculateControlFlowService;

	private static final List<Tuple2<Technology, Type>> SUPPORTED = new ArrayList<>();

	static {
		/* this is a temporary solution until the central feature/function support configuration of WMIN-1573 is implemented */
		ControlFlowSupport.getActuallySupportedTypes().forEach(nodeType -> SUPPORTED.add(Tuple2.of(nodeType.getTechnology(), nodeType.getType())));
	}

	/**
	 * Constructor.
	 *
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param jobId the Id of the job that started this task
	 * @param projectId the Id of the project
	 * @param moduleId the id of the module to process
	 * @see {@link #CalculateControlFlowTask(ProgressMonitor, String, Long)} when directly working with a resolved module Id
	 */
	CalculateControlFlowTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId, projectId, moduleId);
	}

	/**
	 * Constructor.
	 *
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param jobId the Id of the job that started this task
	 * @param moduleId the Id of the module to process
	 * @see {@link #CalculateControlFlowTask(ProgressMonitor, String, Long, Long)} when working with a module path
	 */
	CalculateControlFlowTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId moduleId) {
		super(progressMonitor, jobId, EntityId.VOID, moduleId);
	}


	@Override
	protected void run(final EntityId moduleId) {
		if (isSourceAvailable(moduleId)) {
			calculateControlFlowService.calculateControlFlowGraph(moduleId);
		}
	}
}
