/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.datalineage;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.compress.utils.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.mining.extensions.export.datalineage.DataFlowGraphQueryExtension;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;

/**
 * Job for transforming a data flow graph represented as JSON-file into a GML-file.
 */
public class DataLineageGMLJob extends MiningJob<FileSystemResult> {

	@Autowired
	private transient DataFlowGraphQueryExtension dataFlowGraphQueryExtension;
	
	@Autowired
	private transient MiningJobService jobResultService;
	
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient ProjectService projectService;
	
	private final Map<String, List<String>> parameters;
	
	/**
	 * Constructor to create data lineage gml job instance.
	 * @param projectId Unique identifier.
	 * @param parameters Arguments for invoking the job.
	 */
	public DataLineageGMLJob(final EntityId projectId, final Map<String, List<String>> parameters) {
		super(projectId);
		this.parameters = parameters;
	}

	@Override
	public String getJobName() {
		return "Data Lineage GML Exporter";
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		/* submit the Job that generate the DataFlowGraph as JSON */
		final Job<DataFlowGraph> dataFlowGraphJob = dataFlowGraphQueryExtension.createJob(projectId, parameters);
		final Status dataFlowGraphStatus = jobManager.submitFromJobAndWait(dataFlowGraphJob, assertNotNull(jobMonitor));
		
		/* check whether job was successful */
		if (dataFlowGraphStatus.getSeverity() != Severity.OK) {
			return new Result<>(dataFlowGraphStatus);
		}
		
		/* retrieve the result for the DataFlowGraph job */
		final Optional<MiningJobService.JobResult> jobResult = jobResultService.getJobResult(dataFlowGraphJob.getJobId());
		if ( ! jobResult.isPresent()) {
			throw new IllegalStateException("Failed to generate Data Flow Graph");
		}
		
		try {
			/* obtain the DataFlowGraph in JSON format (as String) and convert it to GML */
			final String json = new String(IOUtils.toByteArray(jobResult.get().getContent()), StandardCharsets.UTF_8);
			final DataLineageGmlUtilWithSeparateCopybooks dataLineage = new DataLineageGmlUtilWithSeparateCopybooks(json);
			final String gmlContent = dataLineage.getGml();
			final List<String> moduleIds = parameters.get("moduleId");
			final String fileName = (moduleIds.size() == 1
										? moduleService.getModuleLightweight(EntityId.of(moduleIds.get(0))).getName()
										: projectService.get(projectId).getName())
									+ ".gml";
			try(final OutputStreamWriter writer = new OutputStreamWriter(new BufferedOutputStream(createResultFile()), StandardCharsets.UTF_8)) {
				writer.write(gmlContent);
				return new Result<>(new FileSystemResult("application/gml+xml", fileName));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}		
	}
}
