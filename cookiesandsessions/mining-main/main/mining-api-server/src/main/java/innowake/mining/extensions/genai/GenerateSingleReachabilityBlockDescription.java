/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.genai;

import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.server.job.genai.GenerateSingleReachabilityBlockDescriptionJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;

/**
 * Job Extension that utilizes GenAI to generate a description for a single reachability block.
 */
@Component
public class GenerateSingleReachabilityBlockDescription implements MiningJobExtension<Serializable> {

    @Override
    public NatureType getRequiredNature() {
        return MINING;
    }

    @Override
    public RoleType getRequiredRole() {
        return EDITOR;
    }

    @Override
    public String getIdentifier() {
        return "generate-single-reachability-block-description";
    }

    @Override
    public String getDescription() {
        return "Generate Description for a Reachability Block";
    }

    @Override
    public Job<Serializable> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
        return new GenerateSingleReachabilityBlockDescriptionJob(projectId, parameters);
    }
}
