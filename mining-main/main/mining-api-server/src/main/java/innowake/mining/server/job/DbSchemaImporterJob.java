/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job;

import java.io.Serializable;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import com.innowake.innovationlab.commons.model.LegacyDatabase;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.event.ModulesModifiedEvent;
import innowake.mining.server.service.DataSchemaImportService;
import innowake.mining.shared.access.EntityId;

/**
 * Job for handling Database schema import
 */
public class DbSchemaImporterJob extends MiningJob<Serializable> {
	
	private static final long serialVersionUID = 1L;

	@Autowired
	private transient DataSchemaImportService dataSchemaImportService;
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	private final LegacyDatabase  dbData;
	
	/**
	 * The constructor.
	 *
	 * @param projectId the project ID
	 * @param dbData the database definition data
	 */
	public DbSchemaImporterJob(final EntityId projectId, final LegacyDatabase dbData) {
		super(projectId);
		this.dbData = dbData;
	}
	
	/**
	 * Runs the Database Schema import job
	 * 
	 * @param progressMonitor {@link ProgressMonitor} Tracks progress of a job
	 */
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Starting Database Schema Import");
		dataSchemaImportService.importSchema(projectId, dbData);
		progressMonitor.setJobDescription("Database Schema Import completed");
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.empty()));
		return new Result<>(Status.OK);
	}
	
	/**
	 * Method to return the job name.
	 * 
	 * @return name of the Job
	 */	
	@Override
	public String getJobName() {
		return "DB Schema Importer";
	}
}
