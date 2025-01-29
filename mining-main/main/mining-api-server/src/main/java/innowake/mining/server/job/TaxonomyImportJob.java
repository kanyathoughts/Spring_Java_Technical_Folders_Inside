/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import innowake.lib.job.internal.hazelcast.HzJobMonitor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.error.TaxonomyImportException;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.service.TaxonomyImportService;
import innowake.mining.shared.access.EntityId;

/**
 * Job for handling Taxonomy Assignment import from CSV
 */
public class TaxonomyImportJob extends MiningJob<Serializable> {
	
	@Autowired
	private TaxonomyImportService importService;
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	private final List<Map<String, String>> importLines;

	/**
	 * The constructor.
	 *
	 * @param projectId the project ID
	 * @param importLines the data from the uploaded CSV file
	 */
	public TaxonomyImportJob(final EntityId projectId, final List<Map<String, String>> importLines) {
		super(projectId);
		this.projectId = projectId;
		this.importLines = importLines;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(importLines.size());
		try {
			importService.importTaxonomies(projectId, importLines, progressMonitor);
			eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
			return new Result<>(Status.OK);
		} catch (final TaxonomyImportException e) {
			//Send the modified event if partial work was done.
			if (progressMonitor instanceof HzJobMonitor) {
				final var jobInformation = ((HzJobMonitor) progressMonitor).getJobInformation();
				if (jobInformation != null && jobInformation.getProcessedWorkUnits() > 0) {
					eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
				}
			}
			return new Result<>(new Status(e));
		}
	}

	@Override
	public String getJobName() {
		return "Taxonomy Import";
	}
}
