/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.discovery;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.data.io.DiscoveryExcelExportService;
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
 * Job for exporting Excel. This job can be serialized.
 */
public class DiscoveryExcelExporterJob extends MiningJob<FileSystemResult> {

	public static final String CONTENT_TYPE_EXCEL = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";

	@Autowired
	private transient DiscoveryExcelExportService excelExportService;
	@Autowired
	private transient ProjectService projectService;

	public DiscoveryExcelExporterJob(final EntityId projectId) {
		super(projectId);
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Export Discovery Data in Excel Format");
		try (final OutputStream out = new BufferedOutputStream(createResultFile())) {
			excelExportService.exportExcel(projectId,
					out,
					new DiscoveryExportOptions());
		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime());
		final String fileName = String.format("discovery_%d_%s.xlsx", projectService.getNid(projectId), timestamp);

		return new Result<>(new FileSystemResult(CONTENT_TYPE_EXCEL, fileName));
	}

	@Override
	public String getJobName() {
		return "Discovery Excel Exporter";
	}
}
