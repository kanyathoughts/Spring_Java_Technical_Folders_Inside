/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.datalineage;

import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

/**
 * Job extension for exporting {@link DataFlowGraph}.
 * <p>
 * The following parameters can be given (parameters marked as "multi" can be specified multiple times):
 * <ul>
 *  <li> "moduleId" (required, multi) id of the module where the start field is located
 *  <li> "fieldOffset" (optional, multi, see below) offset (in characters) of the start field within its module
 *  <li> "detailLevel" (optional) either "MODULE", "FIELD" or "STATEMENT". Default: "MODULE"
 *  <li> "queryDirection" (optional) either "UPSTREAM", "DOWNSTREAM" or "BOTH". Default: "BOTH"
 *  <li> "maxModuleDistance" (optional) maximum distance for the trace, counting modules from the start module. Passing 0 will produce an empty result.
 *       Values < 0 mean there's no limit. Default: -1 (no limit)
 * </ul>
 * Note: specifying "fieldOffset" is optional. If absent, then all fields in the given module(s) will be traced. When giving multiple moduleIds you must either
 * provide an equal number of fieldOffsets or no fieldOffsets at all!
 */
@Service
public class DataFlowGraphQueryExtension implements MiningJobExtension<DataFlowGraph>{
	
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
		return "datalineage";
	}

	@Override
	public String getDescription() {
		return "Data Lineage Graph";
	}

	@Override
	public Job<DataFlowGraph> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		final Parameters queryParameters = new Parameters.Builder().fromMap(parameters).setProjectId(projectId).build();

		return new DataFlowGraphQueryJob(queryParameters);
	}
}
