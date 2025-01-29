/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.datalineage;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Extension for generating a .gml-file from a data flow graph for yEd.
 */
@Component
public class DataLineageGMLExtension implements MiningJobExtension<FileSystemResult>{

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.VIEWER;
	}

	@Override
	public String getIdentifier() {
		return "datalineage-gml";
	}

	@Override
	public String getDescription() {
		return "Exports a Data Flow Graph in GML format (for yEd)";
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new DataLineageGMLJob(projectId, parameters);
	}
}
