/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import com.google.gson.Gson;
import com.google.gson.stream.JsonWriter;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.server.service.AnnotationImportService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.AnnotationImportJobResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Job that imports annotations from a CSV file.
 */
public class AnnotationCsvImportJob extends MiningJob<FileSystemResult> {

	private static final long serialVersionUID = 1L;

	private final List<Map<String, String>> csvData;
	private final String requestingUser;

	@Autowired
	private transient AnnotationImportService annotationImportService;
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	/**
	 * The constructor.
	 *
	 * @param projectId the project ID
	 * @param csvData the csv data
	 * @param requestingUser the requesting user ID
	 */
	public AnnotationCsvImportJob(final EntityId projectId, final List<Map<String, String>> csvData, final String requestingUser) {
		super(projectId);
		this.csvData = csvData;
		this.requestingUser = requestingUser;
	}

	/**
	 * Runs the Annotation CSV import job
	 *
	 * @param progressMonitor {@link ProgressMonitor} Tracks progress of a job
	 */
	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Annotation CSV Import");
		progressMonitor.setStepDescription("Importing annotations");
		progressMonitor.begin(csvData.size());

		final var result = annotationImportService.importAnnotations(Optional.of(progressMonitor), projectId, csvData, requestingUser);

		final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime());
		final String fileName = String.format("annotation_import_%d_%s.json", projectId.getNid(), timestamp);

		try (final JsonWriter writer = new JsonWriter(new OutputStreamWriter(createResultFile(), StandardCharsets.UTF_8))) {
			new Gson().toJson(result, AnnotationImportJobResult.class, writer);
		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		progressMonitor.setStepDescription("Import completed");
		eventPublisher.publishEvent(new AnnotationEvent(projectId));
		return new Result<>(new FileSystemResult("text/json", fileName));
	}
}
