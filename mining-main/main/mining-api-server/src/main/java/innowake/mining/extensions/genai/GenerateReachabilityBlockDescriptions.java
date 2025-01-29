/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.genai;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.MANAGER;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.server.job.genai.GenerateReachabilityBlockDescriptionsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Job Extension that utilizes GenAI to bulk-generate descriptions for reachability blocks.
 */
@Component
public class GenerateReachabilityBlockDescriptions implements MiningJobExtension<Serializable> {

	public static String IDENTIFIER = "generate-reachability-block-descriptions";
	
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
		return IDENTIFIER;
	}

	@Override
	public String getDescription() {
		return "Generate Descriptions for Reachability Blocks";
	}

	@Override
	public Job<Serializable> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new GenerateReachabilityBlockDescriptionsJob(projectId, parameters);
	}

}
