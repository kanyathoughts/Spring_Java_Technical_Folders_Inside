/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.discovery;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.data.io.DiscoveryCsvExportService;
import innowake.mining.data.io.DiscoveryExportOptions;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.Calendar;

/**
 * Job for exporting CSV. This job can be serialized.
 */
public class DiscoveryCsvExporterJob extends MiningJob<FileSystemResult> {

	public static final String PARAMETER_LEGACY_COMPAT = "legacyCompat";
	public static final String CONTENT_TYPE_CSV = "text/csv";

	@Autowired
	private transient DiscoveryCsvExportService csvExportService;
	@Autowired
	private transient ProjectService projectService;


	public DiscoveryCsvExporterJob(final EntityId projectId) {
		super(projectId);
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Export Discovery Data in CSV Format");
		try (final OutputStream out = new BufferedOutputStream(createResultFile())) {
			csvExportService.exportCsv(projectId,
					out,
					new DiscoveryExportOptions());
		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime());
		final String fileName = String.format("discovery_%d_%s.csv", projectService.getNid(projectId), timestamp);

		return new Result<>(new FileSystemResult(CONTENT_TYPE_CSV, fileName));
	}

	@Override
	public String getJobName() {
		return "Discovery CSV Exporter";
	}
}
