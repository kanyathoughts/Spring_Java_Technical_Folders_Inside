/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.job;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.data.core.datadictionary.AstNodeToDataDictionaryEntryUtil;
import innowake.mining.data.core.storeast.DefaultStoreAstExecutor;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Job for Linking Data Dictionary to Annotations.
 */
public class LinkDataDictionaryToAnnotationJob extends MiningJob<Serializable> {

	@Autowired
	private transient MiningDataCoreService core;
	@Autowired
	protected transient ModuleService moduleService;
	@Autowired
	protected transient DataDictionaryService dataDictionaryService;


	protected final EntityId ddeId;
	protected final EntityId moduleId;
	/**
	 * Creates a new LinkDataDictionaryToAnnotationJob instance.
	 * 
	 * @param ddeId the ID of the {@link DataDictionaryPojo} to fetch and work on
	 * @param projectId the id of the {@code project}
	 * @param moduleId the id of the {@code module}
	 */
	public LinkDataDictionaryToAnnotationJob(final EntityId ddeId, final EntityId projectId, final EntityId moduleId) {
		super(projectId);
		this.ddeId = ddeId;
		this.moduleId = moduleId;
	}

	@Override
	public Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final DataDictionaryPojo dde = dataDictionaryService.get(q -> q.byId(ddeId));
		progressMonitor.setStepDescription("Started Linking Data Dictionary to Annotations");
		final Optional<ModuleLightweightPojo> module = moduleService.findModulesLightweight(q -> q.byId(moduleId)).stream().findAny();

		final Set<MiningPojo> foundAnnotations = new HashSet<>();
		if (module.isPresent() && core.isInclusion(module.get().identity())) {
			final AstNodeToDataDictionaryEntryUtil dao = new AstNodeToDataDictionaryEntryUtil(new DefaultStoreAstExecutor(), core);
			final List<EntityId> programIds = dao.findProgramIdsFromCopybook(moduleId);
			getAnnotationsForPrograms(dde, programIds, dao, foundAnnotations);
		} else {
			core.getAstRootOrCreateExceptInclusions(moduleId, new DefaultStoreAstExecutor());
			final AstNodeToDataDictionaryEntryUtil dao = new AstNodeToDataDictionaryEntryUtil(new DefaultStoreAstExecutor(), core);
			getAnnotationsForPrograms(dde, Collections.singletonList(moduleId), dao, foundAnnotations);
		}
		progressMonitor.setStepDescription(foundAnnotations.size() + "Annotations has been identified for linking");

		//Performance: do batchwise
		for (final MiningPojo annotation : foundAnnotations) {
			core.dataDictionaryService.linkAnnotations(dde.identity(), annotation.identity());
		}

		progressMonitor.setStepDescription("Linking Annotation and Data Dictionary ended");
		return new Result<>(Status.OK);
	}

	private void getAnnotationsForPrograms(final DataDictionaryPojo dataDictionaryEntry, final List<EntityId> programIds,
			final AstNodeToDataDictionaryEntryUtil dao, final Set<MiningPojo> foundAnnotations) {
		for (final EntityId id : programIds) {
			core.getAstRootOrCreateExceptInclusions(id, new DefaultStoreAstExecutor());
			final List<AstNodePojo> astNodes = dao.findAstNode(dataDictionaryEntry);
			if (astNodes.get(0) != null) {
				final List<AstNodePojo> fieldReferences = astNodes.get(0).getIncomingRelations().stream()
																		 .filter(r -> r.getType() == AstRelationshipType.BINDING)
																		 .map(edge -> edge.getSrcNode())
																		 .collect(Collectors.toList());
				final List<AnnotationPojo> annotations = core.annotationService.find(q -> q.ofModule(id));
				foundAnnotations.addAll(getAnnotations(fieldReferences, annotations));
			}
		}
	}

	private Set<AnnotationPojo> getAnnotations(final List<AstNodePojo> fieldReferences, final List<AnnotationPojo> annotations) {
		final Set<AnnotationPojo> foundAnnotations = new HashSet<>();
		for (final AnnotationPojo annotation : annotations) {
			final ModuleLocation moduleLocation = annotation.getLocation().orElse(null);
			if (moduleLocation != null) {
				for (final AstNodePojo fieldReference : fieldReferences) {
					if (includesOffset(fieldReference.getLocation().getRetracedOffset().orElseThrow(), moduleLocation)) {
						foundAnnotations.add(annotation);
					}
				}
			}
		}
		return foundAnnotations;
	}
	
	private boolean includesOffset(final Integer offset, final ModuleLocation location) {
		return offset >= location.getOffset() && offset < location.getOffset() + location.getLength();
	}
}
