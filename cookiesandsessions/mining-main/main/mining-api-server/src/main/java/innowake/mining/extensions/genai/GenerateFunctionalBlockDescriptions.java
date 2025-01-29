/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.genai;

import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.server.job.genai.GenerateFunctionalBlockDescriptionsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import static innowake.mining.shared.security.NatureType.MINING;
import innowake.mining.shared.security.RoleType;
import static innowake.mining.shared.security.RoleType.MANAGER;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Job Extension that utilizes GenAI to bulk-generate descriptions for functional blocks.
 */
@Component
public class GenerateFunctionalBlockDescriptions implements MiningJobExtension<Serializable> {

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
		return "generate-functional-block-descriptions";
	}

	@Override
	public String getDescription() {
		return "Generate Descriptions for Functional Blocks";
	}

	@Override
	public Job<Serializable> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new GenerateFunctionalBlockDescriptionsJob(projectId, parameters);
	}

}
