/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.discovery.featurereport;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 */
@Component
public class DiscoveryFeatureReportExtension implements MiningJobExtension<FileSystemResult> {
	
	@Override
	public Job<FileSystemResult> createJob(Long projectId, Map<String, List<String>> parameters, HttpEntity<byte[]> inputData) {
		return createJob(projectId, parameters);
	}
	
	@Override
	public Job<FileSystemResult> createJob(Long projectId, Map<String, List<String>> parameters) {
		return new DiscoveryFeatureReportJob();
	}
	
	@Override
	public NatureType getRequiredNature() {
		return NatureType.DISCOVERY;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.ADMIN;
	}

	@Override
	public String getIdentifier() {
		return "discovery-feature-report";
	}

	@Override
	public String getDescription() {
		return "Overview over Discovery features";
	}
}
