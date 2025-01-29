/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.job;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.Lists;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.AnnotationType;

/**
 * Job for deleting a functional block and its children. This job is used for deleting functional block of type FunctionalGroup that are generated
 * automatically by the {@link innowake.mining.server.functionalblocks.generation.datalineagefunctionalblock.DataLineageFunctionalBlockGeneration}.
 * The job deletes the functional block and its children with type FunctionalUnit and FunctionalCondition.
 * It also deletes the annotations of type "FUNCTIONAL" that are associated with the functional unit and no other parent then deleted functional block.
 */
public class AutomaticGeneratedFunctionalBlockDeletionJob extends MiningJob<Serializable> {

	private static final Logger LOG = LoggerFactory.getLogger(AutomaticGeneratedFunctionalBlockDeletionJob.class);
	private static final int BATCH_SIZE = 1000;

	@Autowired
	private transient FunctionalBlockService functionalBlockService;
	@Autowired
	private transient AnnotationService annotationService;

	private final UUID functionalBlockUid;

	public AutomaticGeneratedFunctionalBlockDeletionJob(final EntityId projectId, final UUID functionalBlockUid) {
		super(projectId);
		this.functionalBlockUid = functionalBlockUid;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		LOG.debug("Starting Automatic Generated Functional Block Deletion Job for functional block with uid: {}", functionalBlockUid);
		progressMonitor.setJobDescription("Automatic Generated Functional Block Deletion Job for functional block with uid: " + functionalBlockUid);
		final Result<Serializable> deletedResult = deleteFunctionalBlock(progressMonitor);
		LOG.debug("Automated Functional Block Deletion Job Finished for functional block with uid: {}", functionalBlockUid);
		return deletedResult;
	}

	private Result<Serializable> deleteFunctionalBlock(final ProgressMonitor progressMonitor) {
		try {
			final FunctionalBlockPojo functionalBlock = functionalBlockService.find(functionalBlockUid)
					.orElseThrow(() -> new MiningEntityNotFoundException(FunctionalBlockPojo.class, functionalBlockUid.toString()));
			if ( functionalBlock == null ) {
				throw new IllegalArgumentException("Provided functional block uid " + functionalBlockUid + " does not exist");
			}

			final List<FunctionalBlockPojo> children = functionalBlockService.find(b -> b.ofProject(projectId)
					.withTypes(List.of(FunctionalBlockType.FUNCTIONAL_UNIT, FunctionalBlockType.FUNCTIONAL_CONDITION,
							FunctionalBlockType.FUNCTIONAL_STATEMENT))
					.withParent(p -> p.byUid(functionalBlockUid)));
			final List<UUID> deleteBlocks = new ArrayList<>();
			final Map<EntityId, UUID> functionalTypeBlocks = functionalBlockService.getSingleParentFunctionalUnitsByAnnotationType(projectId,
					functionalBlockUid, "FUNCTIONAL");
			deleteBlocks.add(functionalBlockUid);

			if ( ! children.isEmpty() ) {
				for (final FunctionalBlockPojo child : children) {
					if ( ( (List<?>) child.getFlags().get(FunctionalBlockFlag.TYPE.name()) ).contains(FunctionalBlockType.FUNCTIONAL_CONDITION.name()) ) {
						deleteBlocks.add(child.getUid());
					}
				}
			}
			deleteBlocks.addAll(new ArrayList<>(functionalTypeBlocks.values()));
			progressMonitor.setStepDescription("Automatic Generated Functional Block Deletion Job Started");
			final var batchesOfIdsToBeDeleted = Lists.partition(deleteBlocks, BATCH_SIZE);
			batchesOfIdsToBeDeleted.forEach(idBatch -> {
				functionalBlockService.delete(idBatch);
			});

			if ( ! functionalTypeBlocks.isEmpty() ) {
				annotationService.delete(query -> query.ofProject(projectId).byIds(functionalTypeBlocks.keySet()).withType(AnnotationType.FUNCTIONAL));
			}
			progressMonitor.setStepDescription("Automatic Generated Functional Block Deletion Job Finished");
		} catch (final Exception e) {
			return new Result<>(new Status(e));
		}

		return new Result<>(Status.OK);
	}

}
