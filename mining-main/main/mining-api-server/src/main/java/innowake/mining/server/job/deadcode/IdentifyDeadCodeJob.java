/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.deadcode;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.google.common.collect.ImmutableList;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.controller.cfg.ControlFlowSupport;
import innowake.mining.server.job.base.ModulesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * {@link Job} implementation that identifies all dead code for the supported modules. Every module will be handled by a separate task.
 */
public class IdentifyDeadCodeJob extends ModulesJob {
	
	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN = "Identified %d Module(s) in selection which are supported by the dead code identification,"
			+ " currently supported Cobol, Natural and Pl1 Program modules.";
	
	private static final String JOB_DESCRIPTION_PATTERN = "Dead code identification for the project %s ";
	
	public static final List<Tuple2<Technology, Type>> SUPPORTED_TECHNOLOGY_TYPE = new ArrayList<>();
	static {
		SUPPORTED_TECHNOLOGY_TYPE.addAll(ControlFlowSupport.getActuallySupportedTypes().stream()
				.map(nodeType -> Tuple2.of(nodeType.getTechnology(), nodeType.getType()))
				.collect(ImmutableList.toImmutableList()));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.CICS, Type.BMS_MAPSET));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.CICS, Type.BMS_MAP));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.PL1, Type.MAINPROGRAM));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.PL1, Type.PROGRAM));
		SUPPORTED_TECHNOLOGY_TYPE.remove(Tuple2.of(Technology.C, Type.PROGRAM));
		SUPPORTED_TECHNOLOGY_TYPE.remove(Tuple2.of(Technology.JCL, Type.JOB));
		SUPPORTED_TECHNOLOGY_TYPE.remove(Tuple2.of(Technology.JAVA, Type.COMPILATION_UNIT));
	}
	
	public IdentifyDeadCodeJob(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		super(projectId, moduleMatcher);
	}

	@Override
	protected Task<Serializable> createModuleTask(final ProgressMonitor subMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		return new IdentifyDeadCodeTask(subMonitor, jobId, projectId, moduleId);
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
		return SUPPORTED_TECHNOLOGY_TYPE;
	}
}
