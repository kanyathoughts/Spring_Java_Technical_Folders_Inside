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
 * Allows to execute Discovery Excel export as a Job.
 */
@Service
public class DiscoveryExcelExporter implements MiningJobExtension<FileSystemResult> {

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
		return new ShowOnExportPage(true, "Discovery", "Download Excel");
	}

	@Override
	public String getIdentifier() {
		return "discovery-excel";
	}

	@Override
	public String getDescription() {
		return "Export Discovery Data in Excel Format";
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new DiscoveryExcelExporterJob(projectId);
	}
}
