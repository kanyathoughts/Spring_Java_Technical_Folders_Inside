/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.export.callchain;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;
import static java.util.Collections.emptyMap;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Customizable Call Chain Exporter with provision to run as a Job.
 * <p>
 * The following parameters can be given. Parameters with 'multi' support multiple values:
 * <ul>
 *  <li> "startModuleId" (optional, multi) id of the start module where the call chain should begin (or end in case "IN" direction); can be given
 *          multiple times to export call chains for multiple start modules at once. Special case: if neither this parameter nor "startModuleType" is provided
 *          then the start modules are determined automatically by searching for "unreferenced" Modules (Modules without incoming resp. outgoing edges
 *          depending on whether "direction" is OUT or IN).
 *  <li> "startModuleType" (optional, multi) types of Modules that are used as start modules. Can be used to export call chains for all Modules of a certain type.
 *          This parameter can be used as an alternative to or in combination with "startModuleId".
 *  <li> "endModuleName" (optional, multi) names of modules where to stop continuing the call chain;
 *          when specified, only call chains that actually reach one of the end modules are included in the final result
 *  <li> "endModuleType" (optional, multi) types of modules where to stop continuing the call chain;
 *          when specified, only call chains that actually reach one of the end modules are included in the final result
 *  <li> "direction" (optional, multi) either "IN" or "OUT" or both. The default is: "OUT".
 *  <li> "callType" (optional, multi) the types of Edges that the call chain will follow. when not specified the call chain will follow all Edges
 *  <li> "filteredName" (optional, multi) names of modules that are hidden from the result
 *  <li> "filteredType" (optional, multi) types of modules that are hidden from the result
 *  <li> "ignoredTaxonomy" (optional, multi) call chain stops when encountering a Module that has one of the given Taxonomies
 *  <li> "dataAccessBased" (optional) when set to "true", the call chain will take the FILE_ACCESS_TYPE or DB_ACCESS_TYPE into account
 *          when following "ReadsWrites" relationships; from the perspective of a file or database table, a write access will be treated as
 *          "incoming" relationship and a read access will be treated as "outgoing", regardless of the actual direction of the "ReadsWrites" relationship.
 *          The default is: "false".
 *  <li> "depth" (optional) maximum length of the call chain; the actual length of the call chain will be depth + 1 as the start module is always added;
 *          a value less than 0 means infinite depth, which is also the default
 *  <li> "edgePropertyFilter" (optional) a JSON object that describes a filter for Edge properties. Only Edges that match the filter (i.e. Edges that have
 *        matching properties) will be followed when building the call chain. Use this to exclude certain edges or include a specific type of edge only.
 *        Multiple conditions can be joined with "_or" or "_and". Conditions can be negated with "_not" (see examples). For now, only the "eq" (equals) operator
 *        is available. You can use <pre>{eq: null}</pre> to check for <i>absence</i> of a property.
 *        <p>
 *        Examples:
 *        <p> Only follow EXEC CICS SEND MAP edges:
 *        <pre>{"CALL_TYPE": {"eq": "MAP"}}</pre>
 *        <p> Do NOT follow EXEC CICS SEND MAP edges:
 *        <pre>{"_not": {"CALL_TYPE": {"eq": "MAP"}}}</pre>
 *        <p> Follow either COBOL CALL or EXEC CICS SEND MAP edges:
 *        <pre>{"_or": [{"CALL_TYPE": {"eq": "CALL"}}, {"CALL_TYPE": {"eq": "MAP"}}]}</pre>
 *  <li> "exportFormat" (optional) the format of the call chain export, either "CSV" or "GraphML". The default export format is {@code CSV}.
 *  <li> "compressed" (optional) {@code true} to return exported files as ZIP. {@code false} for plain exported files (less performant)
 * </ul>
 * <p>
 * The result is returned in CSV format. Each row in the CSV is one complete call chain from (or to) the start module. A call chain ends when either
 * <ul>
 * <li> the end of the chain has been reached (i.e. there are no more outgoing (resp. incoming) edges on the current node that we can follow)
 * <li> one of the configured "end modules" has been reached
 * <li> the maximum depth has been reached
 * <li> a loop has been detected. In this case the call chains stops after encountering the same module twice.
 * <li> a Module that has one of the Taxonomies given in the {@code ignoredTaxonomy} parameter is reached
 * </ul>
 * <p>
 * Each column of each row in the CSV has the following format:
 * <pre>ModuleName(ModuleType)[CallType]</pre>
 * Where {@code [CallType]} indicates how the current module calls the next module in the chain.
 * <p>
 * Example for a program that writes a file:
 * <pre>"SomePrg(PROGRAM)[ReadsWrites]","SomeFile(FILE)[]"</pre>
 * Note that the last entry in the chain naturally does not contain a CallType.
 * <p>The computation and export of call chains can be resource intensive for call chains containing modules that have a lot of incoming or outgoing
 * references. By default the maximum number of parallel running computation threads is limited to 8 per call chain export. The maximum number of
 * exported CSV call chain lines is limit to 5,000,000. Both parameters can be configured in the {@code application.yaml} file.
 * <pre>
 * configuration:
 *     # maximum number of task threads to be used for the computation of call chains
 *     # A value less than 0 means that all available processors are used which can significantly slow down other running jobs and cause that the
 *     # UI is unresponsive while call chains are computed
 *     call-chain-maximum-export-threads: 8
 *     # maximum number of CSV lines that can be exported by each single call chain job
 *     # A value less than 1 means that no limit is used for the CSV export
 *     call-chain-maximum-csv-export-lines: 5000000
 */
@Service
public class CallChainExporter implements MiningJobExtension<FileSystemResult> {

	private static final Logger LOG = LoggerFactory.getLogger(CallChainExporter.class);

	private final ObjectMapper objectMapper;

	/**
	 * Constructor.
	 * 
	 * @param objectMapper the {@link ObjectMapper} for parsing JSON parameters such as {@code edgePropertyFilter}
	 */
	@Autowired
	public CallChainExporter(final ObjectMapper objectMapper) {
		this.objectMapper = objectMapper;
	}

	@Override
	public String getIdentifier() {
		return "callchain";
	}

	@Override
	public String getDescription() {
		return "CallChain Exporter";
	}
	
	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new CallChainExporterJob(buildParameters(projectId, parameters));
	}

	private Parameters buildParameters(final EntityId projectId, final Map<String, List<String>> parameters) {
		return new Parameters.Builder()
				.fromMap(parameters)
				.setProjectId(projectId)
				.setEdgePropertyFilter(getEdgePropertyFilter(parameters))
				.build();
	}

	private Map<String, Serializable> getEdgePropertyFilter(final Map<String, List<String>> parameters) {
		return Optional.ofNullable(parameters.get(Parameters.PARAMETER_EDGE_PROPERTY_FILTER))
				.map(prop -> {
					if (prop.isEmpty()) {
						return Collections.<String, Serializable>emptyMap();
					} else {
						try {
							@SuppressWarnings("unchecked")
							final Map<String, Serializable> filterObject = objectMapper.readValue(prop.get(0), Map.class);
							return filterObject;
						} catch (final JsonProcessingException exc) {
							LOG.warn("Invalid value for parameter 'edgePropertyFilter': must be a JSON object", exc);
							throw new IllegalArgumentException("Invalid value for parameter 'edgePropertyFilter': must be a JSON object", exc);
						}
					}
				})
				.orElse(emptyMap());
	}

	@Override
	public NatureType getRequiredNature() {
		return MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return VIEWER;
	}
}
