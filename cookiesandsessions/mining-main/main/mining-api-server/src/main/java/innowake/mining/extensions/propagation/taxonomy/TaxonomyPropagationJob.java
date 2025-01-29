/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.propagation.taxonomy;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.MimeResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.model.taxonomy.assignment.PropagationData;

/**
 * Job which handles {@link TaxonomyPropagation}
 */
public class TaxonomyPropagationJob extends MiningJob<MimeResult> {

	private static final Logger LOG = LoggerFactory.getLogger(TaxonomyPropagationJob.class);

	private final List<PropagationData> propagationData;
	
	@Autowired
	private transient TaxonomyService taxonomyService;
	
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;
	
	
	/**
	 * Constructor takes project id and {@link PropagationData}
	 * @param projectId ID of the Project the job is run for.
	 * @param propagationData {@link PropagationData} for the project
	 */
	public TaxonomyPropagationJob(final EntityId projectId, final List<PropagationData> propagationData) {
		super(projectId);
		this.projectId = projectId;
		this.propagationData = propagationData;
	}
	
	@Override
	public String getJobName() {
		return "Taxonomy Propagation";
	}

	@Override
	protected Result<MimeResult> run(final ProgressMonitor progressMonitor) {
		LOG.info("Started TaxonomyPropagationJob");
		
		progressMonitor.setJobDescription("Taxonomy Propagation");
		progressMonitor.begin(propagationData.size());
		
		/* Assigning taxonomy for propagated data */
		propagationData.forEach(d -> taxonomyService.createModuleLinks(EntityId.of(d.getModuleId()), d.getTaxonomies()));
		
		progressMonitor.setStepDescription("Taxonomies Assignment completed");
		progressMonitor.worked(propagationData.size());
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));

		LOG.info("TaxonomyPropagationJob finished");
		return new Result<>(Status.OK);
	}

}
