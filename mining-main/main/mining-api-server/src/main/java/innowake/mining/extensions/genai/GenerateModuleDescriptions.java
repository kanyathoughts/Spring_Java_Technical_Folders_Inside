/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.genai;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.MANAGER;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import innowake.mining.shared.access.EntityId;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.server.job.genai.GenerateModuleDescriptionsJob;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Job Extension that utilizes GenAI to bulk-generate module descriptions for a list of modules.
 */
@Component
public class GenerateModuleDescriptions implements MiningJobExtension<Serializable> {
	
	@Override
	public NatureType getRequiredNature() {
		return MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return MANAGER;
	}

	@Override
	public String getIdentifier() {
		return "generate-module-descriptions";
	}

	@Override
	public String getDescription() {
		return "Generate Module Descriptions";
	}

	@Override
	public Job<Serializable> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new GenerateModuleDescriptionsJob(projectId, parameters);
	}
}
