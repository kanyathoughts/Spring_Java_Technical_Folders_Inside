/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.identification;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.job.base.ModulesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * {@link Job} implementation that identifies all Technical Taxonomies for the provided modules.
 * <p>
 * Every module will be handled by a separate task.
 */
public class IdentifyTechnicalTaxonomiesJob extends ModulesJob {

	private static final Logger LOG = LoggerFactory.getLogger(IdentifyTechnicalTaxonomiesJob.class);
	private static final String JOB_DESCRIPTION_PATTERN = "Technical Taxonomy identification for the project %s";
	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN =
			"Identified %d Module(s) in selection which are supported by the Technical Taxonomy identification";
	private static final List<Tuple2<Technology, Type>> SUPPORTED_TYPES = new ArrayList<>(Arrays.asList(
			Tuple2.of(Technology.COBOL, Type.PROGRAM),
			Tuple2.of(Technology.NATURAL, Type.PROGRAM),
			Tuple2.of(Technology.NATURAL, Type.SUBPROGRAM),
			Tuple2.of(Technology.NATURAL, Type.SUBROUTINE),
			Tuple2.of(Technology.PL1, Type.PROGRAM),
			Tuple2.of(Technology.PL1, Type.MAINPROGRAM),
			Tuple2.of(Technology.PL1, Type.SUBROUTINE),
			Tuple2.of(Technology.PL1, Type.FUNCTION)
	));

	@Autowired
	private transient ApplicationEventPublisher eventPublisher;
	@Autowired
	private transient CallChainService callChainService;

	/**
	 * Creates a new job instance for identifying Technical Taxonomies.
	 *
	 * @param projectId the Id of the project
	 * @param moduleMatcher the module mather of all modules to handle
	 */
	public IdentifyTechnicalTaxonomiesJob(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		super(projectId, moduleMatcher);
	}

	@Override
	protected String getJobDescriptionPattern() {
		return JOB_DESCRIPTION_PATTERN;
	}

	@Override
	protected Task<Serializable> createModuleTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		return new IdentifyTechnicalTaxonomyTask(progressMonitor, getJobId(), projectId, moduleId);
	}

	@Override
	protected String getIdentifiedMessagePattern() {
		return IDENTIFIED_MODULES_MESSAGE_PATTERN;
	}

	@Override
	protected List<Tuple2<Technology, Type>> getSupportedModuleTypes() {
		return SUPPORTED_TYPES;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final Result<Serializable> rslt = super.run(progressMonitor);
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
		return rslt;
	}

	@Override
	public String getJobName() {
		return "Identify Technical Taxonomies";
	}


	@Override
	protected Set<EntityId> filterExecutableModuleIds() {
		final Set<EntityId> moduleIds = new HashSet<>();
		moduleMatcher.getIds().forEach(moduleId -> {
			final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).byId(moduleId))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId + " in project: " + projectId));
			if (getSupportedModuleTypes().contains(Tuple2.of(module.getTechnology(), module.getTechnology()))) {
				moduleIds.add(moduleId);
			} else {
				if (module.getTechnology().equals(Technology.BASIC) && module.getType().equals(Type.OBJECT)) {
					moduleIds.add(moduleId);
					/* find dependent modules for moduleId that have no physical file storage, mean storage = Storage.FILE_SECTION */
					moduleIds.addAll(moduleService.findModuleIds(b -> b.ofProject(projectId)
																		.withSourceRelationshipsFrom(moduleId, RelationshipType.DEPENDENCY_TYPES_ARR)
																		.withStorage(Storage.FILE_SECTION)));
				} else if (module.getTechnology() == Technology.JCL && (module.getType() == Type.JOB || module.getType() == Type.PROC)) {
					addJclStepModuleIds(moduleId, moduleIds);
				}
			}
		});
		return moduleIds;
	}

	/**
	 * Adds all moduleIds of the EXEC_PGM modules that are part of the JCL module to the moduleIds set.
	 *
	 * @param moduleId the JCL module id
	 * @param moduleIds the set of moduleIds to add the EXEC_PGM moduleIds to
	 */
	private void addJclStepModuleIds(final EntityId moduleId, final Set<EntityId> moduleIds) {
		final var graphs = getJclCallChainGraphs(moduleId);
		graphs.stream().map(CallChainGraph::getTargetMap).forEach(m -> m.entries().forEach(e -> {
			final var key = e.getKey();
			final var value = e.getValue();
			if (key.getType() == Type.EXEC_PGM || key.getType() == Type.EXEC || key.getType() == Type.PROC) {
				moduleIds.add(key.identity());
			}
			if (value.getType() == Type.EXEC_PGM || value.getType() == Type.EXEC || value.getType() == Type.PROC) {
				moduleIds.add(value.identity());
			}
		}));
	}

	private List<CallChainGraph> getJclCallChainGraphs(final EntityId moduleId) {
		final Parameters parameters = new Parameters.Builder().setProjectId(projectId)
				.setDirections(List.of(CallChain.CallChainDirection.OUT))
				.setStartModuleIds(List.of(moduleId))
				.setCallTypes(Set.of(
						RelationshipType.CALLS, RelationshipType.INCLUDES, RelationshipType.REFERENCES, RelationshipType.ACCESSES, RelationshipType.CONTAINS))
				.setParallel(1)
				.setEndModuleTypes(Set.of(Type.EXEC_PGM)) // Don't include Type.EXEC here as it would stop the call chain at the PROC Step
				.build();
		return callChainService.createCallChainGraphs(new NullProgressMonitor(), parameters).orElse(List.of());
	}

	@Override
	protected Set<EntityId> resolveModulePaths() {
		final Set<EntityId> moduleIds = new HashSet<>(moduleMatcher.getIds());
		moduleMatcher.getPathPatterns().forEach(path -> {
			try {
				var result = moduleService.findModuleIds(b -> b.ofProject(projectId)
																.withPath(path)
																.withTechnologiesAndTypes(getSupportedModuleTypes()));
				if (result.isEmpty()) {
					final var basicModuleId = moduleService.findAnyModuleId(b -> b.ofProject(projectId)
																			.withPath(path)
																			.withTechnology(Technology.BASIC)
																			.withType(Type.OBJECT));

					basicModuleId.ifPresentOrElse(moduleId -> {
						moduleIds.add(moduleId);
						/* find dependent modules for moduleId that have no physical file storage, mean storage = Storage.FILE_SECTION */
						moduleIds.addAll(moduleService.findModuleIds(b -> b.ofProject(projectId)
																			.withSourceRelationshipsFrom(moduleId, RelationshipType.DEPENDENCY_TYPES_ARR)
																			.withStorage(Storage.FILE_SECTION)));
					}, () -> {
						final var jclModuleId = moduleService.findAnyModuleId(b -> b.ofProject(projectId)
																		.withPath(path)
																		.withTechnology(Technology.JCL)
																		.withTypes(Set.of(Type.JOB, Type.PROC)));
						final var moduleId = jclModuleId.orElseThrow(
								() -> new MiningEntityNotFoundException("Unable to find supported module for given path : " + path));
						addJclStepModuleIds(moduleId, moduleIds);
					});
				} else {
					moduleIds.add(result.get(0));
				}
			} catch (final MiningEntityNotFoundException e) {
				LOG.trace(e::getLocalizedMessage, e);
			}
		});
		return moduleIds;
	}
}
