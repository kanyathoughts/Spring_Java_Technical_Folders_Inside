/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.identification;

import com.google.common.collect.ImmutableList;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.controller.cfg.ControlFlowSupport;
import innowake.mining.server.job.base.ModuleTask;
import innowake.mining.server.job.base.ModulesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

import java.util.ArrayList;
import java.util.List;


/**
 * {@link Job} implementation that identifies Module descriptions for the provided modules.
 * <p>
 * Every module will be handled by a separate task.
 */
public class IdentifyModuleDescriptionsJob extends ModulesJob {

	private static final String JOB_DESCRIPTION_PATTERN = "Module description identification for the project %s";
	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN =
			"Identified %d Module(s) in selection which are supported by the Module description identification, i.e. COBOL Program and BMS Mapset Modules.";

	private static final List<Tuple2<Technology, Type>> SUPPORTED_TECHNOLOGY_TYPE = new ArrayList<>();

	static {
			SUPPORTED_TECHNOLOGY_TYPE.addAll(ControlFlowSupport.getActuallySupportedTypes().stream()
					.map(nodeType -> Tuple2.of(nodeType.getTechnology(), nodeType.getType()))
					.collect(ImmutableList.toImmutableList()));
			SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.COBOL, Type.PROGRAM));
			SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.CICS, Type.BMS_MAPSET));
			SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.JCL, Type.JOB));
			SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.NATURAL, Type.PROGRAM));
			SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.NATURAL, Type.SUBPROGRAM));
			SUPPORTED_TECHNOLOGY_TYPE.add(Tuple2.of(Technology.NATURAL, Type.SUBROUTINE));
	}

	/**
	 * Creates a new job instance for identifying Module descriptions.
	 *
	 * @param projectId the Id of the project
	 * @param moduleMatcher the module matcher of all modules to handle
	 */
	public IdentifyModuleDescriptionsJob(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		super(projectId, moduleMatcher);
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
	protected ModuleTask createModuleTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		return new IdentifyModuleDescriptionTask(progressMonitor, getJobId(), projectId, moduleId);
	}

	@Override
	public String getJobName() {
		return "Identify Module Descriptions";
	}
}
