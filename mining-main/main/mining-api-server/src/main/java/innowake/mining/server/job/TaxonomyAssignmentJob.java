/* Copyright (c) 2022 Deloitte. All rights reserved.*/
package innowake.mining.server.job;

import java.io.Serializable;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.service.TaxonomyModelService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;

/**
 * Job for handling BulkTaxonomyAssignment
 */
public class TaxonomyAssignmentJob extends MiningJob<Serializable> {

	private final TaxonomyAssignmentsSetRequest taxonomyAssignmentRequest;

	@Autowired
	private transient TaxonomyModelService taxonomyModelService;
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	/**
	 * Create a new job instance of bulk taxonomy assignment
	 *
	 * @param projectId id of the Project.
	 * @param taxonomyAssignmentRequest {@link TaxonomyAssignmentsSetRequest} for the project.
	 */
	@SuppressWarnings("null")
	public TaxonomyAssignmentJob(final EntityId projectId, final TaxonomyAssignmentsSetRequest taxonomyAssignmentRequest) {
		super(projectId,
				taxonomyAssignmentRequest.getModules() != null && taxonomyAssignmentRequest.getModules().getIds().size() == 1
						? taxonomyAssignmentRequest.getModules().getIds().get(0) : null);
		this.taxonomyAssignmentRequest = taxonomyAssignmentRequest;
	}

	/**
	 * Runs the Taxonomy Assignment job
	 *
	 * @param progressMonitor {@link ProgressMonitor} Tracks progress of a job
	 */
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Starting Assigning Taxonomies to modules");
		taxonomyModelService.updateAssignments(projectId, taxonomyAssignmentRequest);
		progressMonitor.setJobDescription("Taxonomy Assignment completed");
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
		return new Result<>(Status.OK);
	}

	/**
	 * Method to return the job name.
	 *
	 * @return name of the Job
	 */
	@Override
	public String getJobName() {
		return "Taxonomy Assignment";
	}
}
