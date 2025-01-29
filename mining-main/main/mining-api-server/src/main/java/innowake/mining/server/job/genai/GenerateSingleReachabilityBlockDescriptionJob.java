/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.mining.shared.model.job.Message;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.mining.extensions.genai.GenerateReachabilityBlockDescriptions;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Job that utilizes GenAI to generate a description for a single reachability block. The job doesn't persist the description.
 * The description is returned in the job result with the following JSON structure:
 * {
 *     "description": "The generated description",
 *     "uid": "The UID of the reachability block"
 * }
 */
public class GenerateSingleReachabilityBlockDescriptionJob extends GenerateReachabilityBlockDescriptionsJob {

    /**
     * Creates a new GenerateSingleReachabilityBlockDescriptionJob.
     *
     * @param projectId  the id of the project which contains the reachability block we want to generate a descriptions for
     * @param parameters the export parameters - they should contain a list with the reachability block UID at key "uids" and a flag "overwrite" for
     *                   overwriting existing descriptions
     */
    public GenerateSingleReachabilityBlockDescriptionJob(final EntityId projectId, final Map<String, List<String>> parameters) {
        super(projectId, parameters);
    }
    
    @Override
    protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
    	if (parameters.get("uids").size() > 1) {
			return new Result<>(new Status(Severity.ERROR),
					"More than one uid was passed. For generating multiple reachability block descriptions please use the job with the identifier \""
							+ GenerateReachabilityBlockDescriptions.IDENTIFIER + "\".");
    	}
    	return super.run(progressMonitor);
    }

    @Override
    public String getJobName() {
        return "Generate Reachability Block Description";
    }
    
    @Override
    protected boolean shouldGenerateDescription(final FunctionalBlockPojo block) {
    	return true;
    }

    @Override
    protected void handleDescription(final String description, final String uid) {
        final Map<String, String> resultMap = new HashMap<>();
        resultMap.put("description", description);
        resultMap.put("uid", uid);
		try {
			result = new ObjectMapper().writeValueAsString(resultMap);
		} catch (final JsonProcessingException e) {
            errorOccurredDuringGeneration = true;
            jobMonitor.addMessage(new Message(Message.Severity.ERROR,
                    "Error occurred while writing result JSON for the reachability block with the following UID: " + uid + " Cause: " + e.getMessage()));
		}
	}
}
