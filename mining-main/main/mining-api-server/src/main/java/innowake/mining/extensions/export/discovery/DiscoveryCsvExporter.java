/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.discovery;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Allows to execute Discovery CSV export as a Job.
 */
@Service
public class DiscoveryCsvExporter implements MiningJobExtension<FileSystemResult> {

	@Override
	public NatureType getRequiredNature() {
		return NatureType.DISCOVERY;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.VIEWER;
	}
	
	@Override
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "Discovery", "Download CSV");
	}

	@Override
	public String getIdentifier() {
		return "discovery-csv";
	}

	@Override
	public String getDescription() {
		return "Export Discovery Data in CSV Format";
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new DiscoveryCsvExporterJob(projectId);
	}
}
