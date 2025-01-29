/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.datalineage;

import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

/**
 * Extension allowing to reset Data Lineage data for a Module or Project.
 */
@Service
public class DataLineageResetExtension implements MiningJobExtension<Boolean> {
	@Override
	public String getIdentifier() {
		return "datalineage-reset";
	}

	@Override
	public String getDescription() {
		return "Reset Data Lineage data for a Module or Project";
	}

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.MANAGER;
	}

	@Override
	public Job<Boolean> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		final List<String> moduleIdParam = parameters.get("moduleId");
		final EntityId moduleId;
		if (moduleIdParam == null || moduleIdParam.isEmpty()) {
			moduleId = null;
		} else {
			moduleId = EntityId.of(moduleIdParam.get(0));
		}
		return new DataLineageResetJob(projectId, moduleId);
	}
}
