/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.collect.ImmutableList;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.controller.cfg.ControlFlowSupport;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.server.job.base.ModulesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * {@link Job} implementation that generate {@link ModuleBlockGeneration}.
 */
public class ModuleBlockGenerationJob extends ModulesJob {
	
	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN =
			"Identified %d Module(s) in selection which are supported by the functional block generation.";

	private static final String JOB_DESCRIPTION_PATTERN = "Functional block Generation for the project %s ";
	
	private static final List<Tuple2<Technology, Type>> SUPPORTED_TECHNOLOGY_TYPE = new ArrayList<>();

	static {
		SUPPORTED_TECHNOLOGY_TYPE.addAll(ControlFlowSupport.getActuallySupportedTypes().stream()
				.map(nodeType -> Tuple2.of(nodeType.getTechnology(), nodeType.getType()))
				.collect(ImmutableList.toImmutableList()));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.CICS, Type.BMS_MAPSET));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.CICS, Type.BMS_MAP));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.PL1, Type.MAINPROGRAM));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.PL1, Type.PROGRAM));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.C, Type.PROGRAM));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.JCL, Type.JOB));
		SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.JAVA, Type.COMPILATION_UNIT));
	}
	
	/**
	 * Constructor.
	 *
	 * @param projectId the Id of the project
	 * @param moduleMatcher the module matcher of all modules to handle
	 */
	public ModuleBlockGenerationJob(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		super(projectId, moduleMatcher);
	}

	@Override
	protected Task<Serializable> createModuleTask(final ProgressMonitor subMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		return new ModuleBlockGenerationTask(subMonitor, getJobId(), projectId, moduleId);
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

	@Override
	protected Set<EntityId> filterExecutableModuleIds() {
		return new HashSet<>(moduleMatcher.getIds());
	}

	@Override
	public String getJobName() {
		return "Functional Block Generation";
	}
}
