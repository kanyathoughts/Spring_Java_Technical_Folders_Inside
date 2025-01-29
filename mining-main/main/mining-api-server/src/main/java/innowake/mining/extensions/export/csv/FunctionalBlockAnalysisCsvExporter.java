/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.csv;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * This job generates the CSV file for the tree view of functional block analysis.
 */
@Service
public class FunctionalBlockAnalysisCsvExporter implements MiningJobExtension<FileSystemResult> {
	
	@Override
	public String getIdentifier() {
		return "functional-block-analysis-csv";
	}

	@Override
	public String getDescription() {
		return "Functional Block Analysis CSV Data";
	}
	
	@Override
	public NatureType getRequiredNature() {
		return MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return VIEWER;
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new FunctionalBlockAnalysisCsvExporterJob(projectId, parameters, SecurityContextHolder.getContext());
	}

}