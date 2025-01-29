/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.job;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.data.core.datadictionary.AstNodeToDataDictionaryEntryUtil;
import innowake.mining.data.core.storeast.DefaultStoreAstExecutor;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.server.util.BranchStatementUtility;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;

/**
 * Job for Linking Annotation to Data Dictionaries.
 */
public class LinkAnnotationToDataDictionaryJob extends MiningJob<Serializable> {

	@Autowired
	private transient AstService astService;
	@Autowired
	private transient BranchStatementUtility branchStatementUtility;
	@Autowired
	private transient MiningDataCoreService core;
	@Autowired
	protected transient ModuleService moduleService;

	protected final EntityId annotationId;
	protected final ModuleLocation location;
	protected final EntityId moduleId;

	/**
	 * Creates a new LinkAnnotationToDataDictionaryJob instance.
	 * 
	 * @param annotationId ID of the {@link AnnotationPojo}
	 * @param location location of the Annotation
	 * @param projectId the ID of the {@code project}
	 * @param moduleId the ID of the {@code module}
	 */
	public LinkAnnotationToDataDictionaryJob(final EntityId annotationId, final ModuleLocation location, final EntityId projectId, final EntityId moduleId) {
		super(projectId);
		this.annotationId = annotationId;
		this.location = location;
		this.moduleId = moduleId;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setStepDescription("Started Linking Annotation to Data Dictionaries");
		final Map<UUID, DataDictionaryPojo> foundDdes = new HashMap<>();
		final Optional<ModuleLightweightPojo> module = moduleService.findAnyModuleLightweight(q -> q.byId(moduleId));

		if (module.isPresent()) {
			if (core.isInclusion(module.get().identity())) {
				final AstNodeToDataDictionaryEntryUtil dao = new AstNodeToDataDictionaryEntryUtil(new DefaultStoreAstExecutor(), core);
				final List<EntityId> programIds = dao.findProgramIdsFromCopybook(moduleId);
				final List<Tuple2<AstNodePojo, DataDictionaryPojo>> nodesAndDdsOfPrograms = new ArrayList<>();
				for (final EntityId id : programIds) {
					nodesAndDdsOfPrograms.addAll(dao.findConnections(id));
				}
	
				nodesAndDdsOfPrograms.stream()
					.filter(tup -> {
						final long retracedOffset = tup.a.getLocation().getRetracedOffset().orElseThrow();
						final long retracedLength = tup.a.getLocation().getRetracedLength().orElseThrow();
						final long offset = location.getOffset().longValue();
						final long length = location.getLength().longValue();
						final var inclusionCalleeModuleId = tup.a.getIncludedModule();
						return retracedOffset <= offset + length && retracedOffset + retracedLength >= offset && inclusionCalleeModuleId.isPresent()
								&& inclusionCalleeModuleId.get().equals(moduleId)
								&& (tup.a.getSuperTypes().contains("FieldReference") || tup.a.getSuperTypes().contains("FieldDefinition"));
					}).forEach(tup -> foundDdes.put(tup.b.getUid(), tup.b));
			} else {
				core.getAstRootOrCreateExceptInclusions(moduleId, new DefaultStoreAstExecutor());
				final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(moduleId)
																		 .withSuperTypes("FieldReference", "FieldDefinition")
																		 .withRetracedOffset(Comperator.LESSER_OR_EQUAL, location.getOffset() + location.getLength())
																		 .withRetracedEndOffset(Comperator.GREATER_OR_EQUAL, location.getOffset())
																		 .withIncludedModule(null));
				getDataDictionaryEntries(foundDdes, moduleId, module.get().getTechnology(), astNodes);
			}
		}
		progressMonitor.setStepDescription(foundDdes + " Data dictionaries has been identified for linking");

		//Performance: do batchwise
		for (final DataDictionaryPojo dde : foundDdes.values()) {
			core.dataDictionaryService.linkAnnotations(dde.identity(), annotationId);
		}

		progressMonitor.setStepDescription("Linking Annotation and Data Dictionary ended");
		return new Result<>(Status.OK);
	}

	private void getDataDictionaryEntries(final Map<UUID, DataDictionaryPojo> entries, final EntityId moduleId, final Technology technology, final List<AstNodePojo> astNodes) {
		for (final AstNodePojo conditionNode : astNodes) {
			final AstNodeLocation advancedModuleLocationV2 = conditionNode.getLocation();
			if (advancedModuleLocationV2 != null) {
				final var retracedLength = advancedModuleLocationV2.getRetracedLength();
				final var retracedOffset = advancedModuleLocationV2.getRetracedOffset();
				if (retracedLength.isPresent() && retracedOffset.isPresent()) {
					if (technology == Technology.PL1) {
						branchStatementUtility.getReferencedDDEntriesForPL1(moduleId, new ModuleLocation(retracedOffset.get(), retracedLength.get())).stream()
												.forEach(dde -> entries.put(dde.getUid(), dde));
					} else {
						branchStatementUtility.getReferencedDDEntries(moduleId, new ModuleLocation(retracedOffset.get(), retracedLength.get())).stream()
												.forEach(dde -> entries.put(dde.getUid(), dde));
					}
				}
			}
		}
	}
}
