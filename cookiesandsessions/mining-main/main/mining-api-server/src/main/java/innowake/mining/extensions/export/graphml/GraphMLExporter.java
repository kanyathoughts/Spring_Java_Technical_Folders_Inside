/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.graphml;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;

import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Implementation of GraphML exporter for exporting projects in mining UI as GraphML.
 */
@Component
public class GraphMLExporter implements MiningJobExtension<FileSystemResult> {
	
	@Autowired
	private FF4j ff4j;

	@Override
	public String getDescription() {
		return "Download";
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
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "GraphML", getDescription());
	}

	@Override
	public String getIdentifier() {
		return "graphml";
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new GraphMLExportJob(projectId, parameters, ff4j.getFeature(FeatureId.DETAILED_TAXONOMY_GRAPHML_EXPORT.getId()).isEnable());
	}
}
