/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import com.google.gson.Gson;
import com.google.gson.stream.JsonWriter;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.service.TaxonomyImportService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.model.TaxonomyImportValidationResult;

/**
 * Job for handling Taxonomy Import Validation
 */
public class TaxonomyValidationJob extends MiningJob<FileSystemResult> {
	
	@Autowired
	private transient Gson gson;
	@Autowired
	private transient TaxonomyImportService importService;
	@Autowired
	private transient ProjectService projectService;

	private final List<Map<String, String>> importLines;

	/**
	 * The constructor.
	 *
	 * @param projectId the project ID
	 * @param importLines the data from the uploaded CSV file
	 */
	public TaxonomyValidationJob(final EntityId projectId, final List<Map<String, String>> importLines) {
		super(projectId);
		this.importLines = importLines;
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(importLines.size());
		final TaxonomyImportValidationResult validationResult = importService.validate(projectId, importLines, progressMonitor);

		final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime());
		final String fileName = String.format("taxonomy_validation_%d_%s.json", projectService.getNid(projectId), timestamp);

		try (final JsonWriter writer = new JsonWriter(new OutputStreamWriter(createResultFile(), StandardCharsets.UTF_8))) {
			gson.toJson(validationResult, TaxonomyImportValidationResult.class, writer);
		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		return new Result<>(new FileSystemResult("text/json", fileName));
	}

	@Override
	public String getJobName() {
		return "Taxonomy Validation";
	}
}
