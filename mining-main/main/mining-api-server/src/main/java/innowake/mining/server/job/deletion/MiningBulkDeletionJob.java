/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.job.deletion;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import com.google.common.collect.Lists;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.event.AnnotationDeletedEvent;
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.server.event.DataDictionariesModifiedEvent;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;

/**
* Job for deleting list of annotations or data dictionaries .
*/
public class MiningBulkDeletionJob extends MiningJob<Serializable> {

	private static final Logger LOG = LoggerFactory.getLogger(MiningBulkDeletionJob.class);
	private static final int BATCH_SIZE = 1000;

	@Autowired
	private transient AnnotationService annotationService;

	@Autowired
	private transient DataDictionaryService dataDictionaryService;
	
	@Autowired
	private transient FunctionalBlockService functionalBlockService;
	
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	private final String jobName;
	private final List<EntityId> idsToBeDeleted;

	/**
	 * Constructor.
	 * @param projectId the project id
	 * @param idsToBeDeleted the ids to be deleted
	 * @param jobName the job type either annotation or data dictionary
	 * 
	 */
	public MiningBulkDeletionJob(final EntityId projectId, final List<EntityId> idsToBeDeleted, final String jobName) {
		super(projectId);
		this.idsToBeDeleted = idsToBeDeleted;
		this.jobName = jobName;
	}

	@Override
	public String getJobName() {
		return jobName;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		LOG.debug("Starting " + jobName + " Job");
		progressMonitor.setJobDescription(jobName);
		try {
			final var batchesOfIdsToBeDeleted = Lists.partition(idsToBeDeleted, BATCH_SIZE);
			if (jobName.equals("Bulk Annotation Delete")) {
				batchesOfIdsToBeDeleted.forEach(idBatch -> {
					annotationService.delete(query -> query.ofProject(projectId).byIds(idBatch));
					functionalBlockService.deleteGeneratedFromAnnotations(idBatch);
				});
				eventPublisher.publishEvent(new AnnotationDeletedEvent(projectId));
			} else if (jobName.equals("Bulk Data Dictionary Delete")) {
				batchesOfIdsToBeDeleted.forEach(idBatch -> dataDictionaryService.delete(query -> query.byIds(idBatch)));
				eventPublisher.publishEvent(new DataDictionariesModifiedEvent(projectId));
			} else {
				throw new IllegalArgumentException("Unsupported job: " + jobName);
			}
			progressMonitor.setStepDescription(jobName + " Finished");
			return new Result<>(Status.OK);
		} catch (final Exception e) {
			return new Result<>(new Status(e));
		}
	}

}
