/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import java.io.Serializable;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.event.SavedSearchModifiedEvent;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.TaxonomyTypePojo;

/**
 * Job for Taxonomy Type deletion.
 */
public class TaxonomyTypeDeletionJob extends MiningJob<Serializable> {
	
	private static final Logger LOG = LoggerFactory.getLogger(TaxonomyTypeDeletionJob.class);

	private final String taxonomyTypeName;

	@Autowired
	private transient ApplicationEventPublisher eventPublisher;
	
	@Autowired
	private transient TaxonomyService taxonomyService;
	
	/**
	 * Creates a new TaxonomyTypeDeletionJob instance.
	 * 
	 * @param projectId the project ID of the {@linkplain TaxonomyTypePojo} to delete.
	 * @param taxonomyTypeName the name of the {@linkplain TaxonomyTypePojo} to delete.
	 */
	public TaxonomyTypeDeletionJob(final EntityId projectId, final String taxonomyTypeName) {
		super(projectId);
		this.taxonomyTypeName = taxonomyTypeName;
	}
	
	@Override
	public String getJobName() {
		return "Taxonomy Type Deletion";
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		
		LOG.debug("Starting Taxonomy Type Deletion");
		progressMonitor.setJobDescription("Taxonomy Type Deletion");
				
		final Result<Serializable> deletedResult = deleteTaxonomyType(progressMonitor);
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
		eventPublisher.publishEvent(new SavedSearchModifiedEvent(projectId));
		LOG.debug("Taxonomy Type Deletion finished");
		
		return deletedResult;
	}

	private Result<Serializable> deleteTaxonomyType(final ProgressMonitor progressMonitor) {
		try {
			progressMonitor.setStepDescription("Starting Taxonomy Type Deletion: " + taxonomyTypeName);
			taxonomyService.deleteType(q -> q.ofProject(projectId).withName(taxonomyTypeName));
			progressMonitor.setStepDescription("Taxonomy Type Deletion finished");
		} catch (final Exception e) {
			return new Result<>(new Status(e));
		}

		return new Result<>(Status.OK);
	}
}
