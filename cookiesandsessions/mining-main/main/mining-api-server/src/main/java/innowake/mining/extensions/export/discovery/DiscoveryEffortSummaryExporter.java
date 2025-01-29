/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.discovery;

import innowake.mining.data.io.DiscoveryExcelExportService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

/**
 * Implementation of effort summary exporter
 */
@Service
public class DiscoveryEffortSummaryExporter implements MiningExportExtension {

	@Autowired
	private DiscoveryExcelExportService excelExportService;
	@Autowired
	private ProjectService projectService;

	@Override
	public NatureType getRequiredNature() {
		return NatureType.DISCOVERY;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.VIEWER;
	}

	@Override
	public String getFormatIdentifier() {
		return "discovery-effort-summary";
	}

	@Override
	public String getDescription() {
		return "Download Effort Summary Excel";
	}

	@Override
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "Discovery", getDescription());
	}

	@Override
	public ExportValue export(final EntityId projectId, final Map<String, List<String>> parameters) throws ExportException {
		final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime());
		final String workbookId = String.format("effort-summary_%d_%s.xlsx", projectService.getNid(projectId), timestamp);
		final byte[] workbook;
		try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			excelExportService.exportEffortSummaryExcel(projectId, out);
			workbook = out.toByteArray();
		} catch (final IOException e) {
			throw new ExportException("Failed to export effort summary", e);
		}
		return new ExportValue() {
			@Override
			public String getFileName() {
				return workbookId;
			}

			@Override
			public String getContentType() {
				return "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
			}

			@Override
			public InputStream getInputStream() {
				return new ByteArrayInputStream(workbook);
			}
		};
	}
}