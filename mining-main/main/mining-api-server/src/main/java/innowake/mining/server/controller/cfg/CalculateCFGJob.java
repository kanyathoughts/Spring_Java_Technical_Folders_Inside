/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.cfg;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.mining.server.job.base.ModuleTask;
import innowake.mining.server.job.base.ModulesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * {@link Job} implementation that calculates Control Flow Graph for one or multiple modules.
 */
public class CalculateCFGJob extends ModulesJob {
	
	@Nullable
	private final EntityId moduleId;
	private static final String JOB_DESCRIPTION_PATTERN = "Control flow calculation for the project %s";
	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN =
			"Identified %d Module(s) in selection which are supported by the control flow calculation.";


	private static final List<Tuple2<Technology, Type>> SUPPORTED = new ArrayList<>();

	static {
		/* this is a temporary solution until the central feature/function support configuration of WMIN-1573 is implemented */
		ControlFlowSupport.getActuallySupportedTypes().forEach(nodeType -> SUPPORTED.add(Tuple2.of(nodeType.getTechnology(), nodeType.getType())));
	}

	/**
	 * Constructor.
	 *
	 * @param moduleId the Id of the module
	 */
	public CalculateCFGJob(final EntityId projectId, final EntityId moduleId) {
		super(projectId, new ModuleMatcher(List.of(moduleId), Collections.emptyList()));
		this.moduleId = moduleId;
	}

	public CalculateCFGJob(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		super(projectId, moduleMatcher);
		this.moduleId = null;
	}

	@Override
	protected String getJobDescriptionPattern() {
		return JOB_DESCRIPTION_PATTERN;
	}

	@Override
	protected String getIdentifiedMessagePattern() {
		return IDENTIFIED_MODULES_MESSAGE_PATTERN;
	}

	@Override
	protected List<Tuple2<Technology, Type>> getSupportedModuleTypes() {
		return SUPPORTED;
	}

	@Override
	protected ModuleTask createModuleTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		return new CalculateControlFlowTask(progressMonitor, getJobId(), projectId, moduleId);
	}


	/**
	 * Runs the control flow calculation job on remote machine.
	 */
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final EntityId modId = moduleId;
		if (modId != null) {
			final ResultConsumer<Serializable> resultConsumer = new ResultConsumer<Serializable>(new ReportMessageExceptionHandler<>(assertNotNull(jobMonitor))) {

				@Override
				protected void handleResult(final String taskId, final Result<Serializable> result) {
					/* No additional result processing required. */
				}
			};
			try {
				progressMonitor.setJobDescription("Control Flow calculation for module with Id " + modId);
				forkTask(createTaskProcessor(), new CalculateControlFlowTask(progressMonitor.subMonitor(1), getJobId(), modId), resultConsumer);
			} catch (final Exception e) {
				return new Result<>(new Status(e));
			}
			return new Result<>(new Status(resultConsumer.getHighestSeverity()));
		} else {
			return super.run(progressMonitor);
		}
	}


	@Override
	public String getJobName() {
		return "Calculate Control Flow Graph";
	}

}
