/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.identification;

import com.google.common.collect.ImmutableList;
import innowake.lib.core.util.tuple.Tuple2;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.controller.cfg.ControlFlowSupport;
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.server.event.DataDictionariesModifiedEvent;
import innowake.mining.server.job.base.ModulesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

import javax.annotation.PostConstruct;

import static innowake.mining.shared.model.FeatureId.IDENTIFY_DDE_ONLY;

/**
 * {@link Job} implementation that identifies all candidates for the supported modules. Every module will be handled by a separate task.
 */
public class IdentifyCandidatesJob extends ModulesJob {

	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN =
			"Identified %d Module(s) in selection which are supported by the Candidate identification, i.e. Cobol and Natural Program modules.";

	private static final String JOB_DESCRIPTION_PATTERN = "Candidate identification for the project %s ";

	private boolean identifyOnlyDDE;

	@Autowired
	private transient FF4j ff4j;

	@Autowired
	private transient ApplicationEventPublisher eventPublisher;
	
	private static final List<Tuple2<Technology, Type>> SUPPORTED_TECHNOLOGY_TYPE = new ArrayList<>();

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

	/**
	 * Constructor.
	 *
	 * @param projectId the Id of the project
	 * @param moduleMatcher the module matcher of all modules to handle
	 */
	public IdentifyCandidatesJob(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		super(projectId, moduleMatcher);
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
	protected String getJobDescriptionPattern() {
		return JOB_DESCRIPTION_PATTERN;
	}

	@PostConstruct
	private void postContruct() {
		identifyOnlyDDE = ff4j.getFeature(IDENTIFY_DDE_ONLY.getId()).isEnable();
	}

	@Override
	protected Task<Serializable> createModuleTask(final ProgressMonitor subMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		return new IdentifyCandidatesTask(subMonitor, getJobId(), projectId, moduleId, identifyOnlyDDE);
	}
	
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final Result<Serializable> rslt = super.run(progressMonitor);
		eventPublisher.publishEvent(new AnnotationEvent(projectId));
		eventPublisher.publishEvent(new DataDictionariesModifiedEvent(projectId));
		return rslt;
	}

	@Override
	public String getJobName() {
		return "Identify Candidates";
	}
}
