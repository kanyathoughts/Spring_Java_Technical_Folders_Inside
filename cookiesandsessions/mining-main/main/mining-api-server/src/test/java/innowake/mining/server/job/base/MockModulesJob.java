/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.base;

import java.io.Serializable;
import java.util.List;

import com.google.common.collect.ImmutableList;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.controller.cfg.ControlFlowSupport;
import innowake.mining.shared.access.EntityId;

import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * A mock implementation of a {@link ModulesJob}.
 */
public class MockModulesJob extends ModulesJob {

	/**
	 * Constructs an instance of the mock job.
	 * @param projectId the project id to work on.
	 * @param moduleMatcher the module matcher to use.
	 */
	public MockModulesJob(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		super(projectId, moduleMatcher);
	}

	@Override
	protected Task<Serializable> createModuleTask(final ProgressMonitor subMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		return new MockModuleTask(subMonitor, jobId, projectId, moduleId);
	}

	@Override
	protected String getJobDescriptionPattern() {
		return "Fake Modules Job";
	}

	@Override
	protected String getIdentifiedMessagePattern() {
		return "Fake Modules Job";
	}

	@Override
	protected List<Tuple2<Technology, Type>> getSupportedModuleTypes() {
		return ControlFlowSupport.getActuallySupportedTypes().stream()
				.map(nodeType -> Tuple2.of(nodeType.getTechnology(), nodeType.getType()))
				.collect(ImmutableList.toImmutableList());
	}	
}