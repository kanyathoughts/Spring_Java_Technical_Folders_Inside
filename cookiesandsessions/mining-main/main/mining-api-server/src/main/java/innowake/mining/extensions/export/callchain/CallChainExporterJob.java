/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.export.callchain;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.annotation.PostConstruct;

import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLWriter;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.extensions.export.callchain.csv.CallChainCSVWriter;
import innowake.mining.extensions.export.callchain.csv.CsvExportAbortException;
import innowake.mining.extensions.export.callchain.csv.CsvExportWriterThread;
import innowake.mining.extensions.export.callchain.csv.ExportAbortReason;
import innowake.mining.extensions.export.callchain.dependency.DependencyGraphExportUtil;
import innowake.mining.extensions.export.callchain.graphml.GraphML;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.util.ExporterUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.entities.ModuleLightweightPojo;

/**
 * Job which handles the Call Chain exporting functionality
 */
public class CallChainExporterJob extends MiningJob<FileSystemResult> {
	
	private static final String STRING_TYPE = "string";
	private static final String CALL_CHAIN_FOR = "Call Chain for %s.%s";

	/**
	 * List of all relationship types to be used when exporting files.
	 */
	/* sorted only because we expect a consistent order in our unit tests, otherwise the order is not relevant */
	public static final List<String> ALL_RELATIONSHIP_TYPES = ExporterUtil.VALID_RELATIONSHIP_TYPES.stream().sorted().collect(Collectors.toList());

	private static final String NO_RESULTS_MESSAGE = "(no results)";

	private static final Logger LOG = LoggerFactory.getLogger(CallChainExporterJob.class);
	private static final String TYPE = "type";
	private static final String TECHNOLOGY = "technology";
	public static final String TAXONOMY_PREFIX = "tax_";

	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient ProjectService projectService;
	@Autowired
	private transient CallChainService callChainService;
	@Autowired
	private transient GenericConfiguration configProperties;
	@Autowired
	private transient TaxonomyService taxonomyService;

	private int maxLines;

	private final Parameters parameters;

	/**
	 * Constructor.
	 *
	 * @param parameters the export parameters
	 */
	public CallChainExporterJob(final Parameters parameters) {
		super(assertNotNull(parameters.getProjectId(), "Project must not be null"));
		this.parameters = parameters;
	}

	@PostConstruct
	private void setup() {
		maxLines = configProperties.getCallChainMaximumCsvExportLines();
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Export Call Chain");

		if (progressMonitor.isCanceled()) {
			return new Result<>(new Status(Severity.CANCELED));
		}

		progressMonitor.setStepDescription("Creating Call Chains");
		final Optional<List<CallChainGraph>> callChains = callChainService.createCallChainGraphs(progressMonitor, parameters);

		if (progressMonitor.isCanceled()) {
			return new Result<>(new Status(Severity.CANCELED));
		}

		final String outputFileName = (parameters.getStartModuleIds().size() == 1
				? moduleService.getModule(parameters.getStartModuleIds().get(0)).getName()
				: projectService.get(parameters.getProjectId()).getName());

		progressMonitor.setStepDescription("Starting Call Chain export");

		final ExportFormat exportFormat = parameters.getExportFormat();

		try (final OutputStream out = new BufferedOutputStream(createResultFile())) {
			if (parameters.isCompressed()) {
				try (final ZipOutputStream zipOut = new ZipOutputStream(out, StandardCharsets.UTF_8)) {
					zipOut.putNextEntry(new ZipEntry(String.format(CALL_CHAIN_FOR, outputFileName, exportFormat.fileExtension)));
					exportCallChains(progressMonitor, callChains, zipOut);
				}
			} else {
				exportCallChains(progressMonitor, callChains, out);
			}
			progressMonitor.setStepDescription("Call Chain export is successful");
		} catch (final IOException e) {
			LOG.error("Error while writing CallChain to output stream", e);
			return new Result<>(new Status(e));
		}

		if (progressMonitor.isCanceled()) {
			return new Result<>(new Status(Severity.CANCELED));
		}

		if (parameters.isCompressed()) {
			return new Result<>(new FileSystemResult("application/zip", String.format(CALL_CHAIN_FOR, outputFileName, "zip")));
		} else {
			return new Result<>(new FileSystemResult(exportFormat.contentType, String.format(CALL_CHAIN_FOR, outputFileName, exportFormat.fileExtension)));
		}
	}

	@Override
	public String getJobName() {
		return "Call Chain Exporter";
	}

	private void exportCallChains(final ProgressMonitor progressMonitor, final Optional<List<CallChainGraph>> callChainGraphs, final OutputStream out)
			throws IOException {
		if (parameters.getExportFormat() == ExportFormat.DEPENDENCY_GRAPH) {
			var projectId = parameters.getProjectId();
			if ( ! projectId.hasUid() || ! projectId.hasNid()) {
				//Project ID need to be complete when creating a Pojo of it.
				projectId = projectService.get(projectId).identity();
			}
			DependencyGraphExportUtil.exportDependencyGraph(callChainGraphs, out, projectId, moduleService);
		} else if (parameters.getExportFormat() == ExportFormat.GRAPHML) {
			final Map<String, String> edgeKeyTypes = new HashMap<>();
			edgeKeyTypes.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), STRING_TYPE);
			edgeKeyTypes.put(ModelAttributeKey.DB_ACCESS_TYPE.name(), STRING_TYPE);

			final Map<String, String> vertexKeyTypes = new HashMap<>();

			final Map<EntityId, List<Tuple2<String, String>>> taxonomiesPerModules = callChainGraphs.map(this::getTaxonomiesPerModules).orElse(Collections.emptyMap());
			taxonomiesPerModules.values().stream()
						.flatMap(l -> l.stream())
						.map(tuple -> tuple.a)
						.distinct()
						.forEach(tax -> vertexKeyTypes.put(tax, STRING_TYPE));

			vertexKeyTypes.put(TECHNOLOGY, STRING_TYPE);
			vertexKeyTypes.put(TYPE, STRING_TYPE);
			
			GraphMLWriter.build()
				.vertexKeyTypes(vertexKeyTypes)
				.edgeKeyTypes(edgeKeyTypes)
				.create()
				.writeGraph(out, new GraphML(taxonomiesPerModules, callChainGraphs.map(graphs -> graphs.toArray(new CallChainGraph[0])).orElse(new CallChainGraph[0])));
		} else {
			exportCsv(progressMonitor, callChainGraphs, out);
		}
	}
	
	private final Map<EntityId, List<Tuple2<String, String>>> getTaxonomiesPerModules(final List<CallChainGraph> callChainGraphs) {
		final List<EntityId> moduleIds = callChainGraphs.stream()
									.flatMap(graph -> Stream.concat(Stream.of(graph.getRoot()), graph.getTargetMap().keySet().stream()))
									.map(ModuleLightweightPojo::identity)
									.distinct() /* same module can be present in different CallChainGraphs */
									.toList();

		final Map<EntityId, List<Tuple2<String, String>>> mapping = new HashMap<>(moduleIds.size());
		
		/* bulk fetch taxonomies but with partitions as callchain graphs can get huge */
		ListUtils.partition(moduleIds, 1000).forEach(
				partition -> taxonomyService.findTaxonomiesPerModule(q -> q.ofModules(partition)).entrySet().forEach(
						entry -> mapping.put(entry.getKey(), entry.getValue().stream().map(t -> Tuple2.of(TAXONOMY_PREFIX + t.getType().getName(), t.getName())).collect(Collectors.toList()))));
		return mapping;
	}

	private void exportCsv(final ProgressMonitor progressMonitor, final Optional<List<CallChainGraph>> callChainGraphs, final OutputStream out)
			throws IOException {
			try (final CallChainCSVWriter writer = new CallChainCSVWriter(new OutputStreamWriter(out, StandardCharsets.UTF_8))) {
				final StopWatch stopWatch = new StopWatch();
				LOG.info("Begin CallChain CSV export ...");
				stopWatch.start();

				writer.writeNext(new String[] { parameters.toString() });
				writer.writeNext(new String[0]); /* print a blank line after parameters */

				final int[] lineCount = { 0 };
				if (callChainGraphs.isPresent()) {
					final CsvExportWriterThread writerThread = new CsvExportWriterThread(writer);
					writerThread.start();
					final int maxExportLines = maxLines - 1;

					final Consumer<CallChain> callChainConsumer = callChain -> {
						if (callChain.getCallChainEntries().size() < 2) {
							/* don't export "call chains" that are empty or contain only the start module - at least 2 modules are required */
							return;
						}

						if (lineCount[0] >= maxExportLines) {
							throw new CsvExportAbortException(ExportAbortReason.EXPORT_LIMIT_REACHED);
						}

						if (lineCount[0] % 10_000 == 0 && progressMonitor.isCanceled()) {
							throw new CsvExportAbortException(ExportAbortReason.JOB_CANCELLED);
						}

						lineCount[0]++;
						if (lineCount[0] % 100_000 == 0) {
							LOG.info(() -> String.format("CallChain CSV exported %d", Integer.valueOf(lineCount[0])));
						}

						writerThread.put(callChain);
					};

					try {
						final Set<String> processedCallChains = new HashSet<>();
						callChainGraphs.get().forEach(graph -> callChainService.traverseGraph(graph, parameters, callChainConsumer, processedCallChains));
                    } catch (final CsvExportAbortException exc) {
						if (exc.getReason() == ExportAbortReason.EXPORT_LIMIT_REACHED) {
							LOG.warn(() -> String.format("Call Chain CSV export reached export limit and was aborted after %d lines", Integer.valueOf(maxLines)));
						}
					} finally {
						writerThread.put(CsvExportWriterThread.STOP_SIGNAL);
						if ( ! writerThread.waitForFinish()) {
							LOG.error("CsvExportWriterThread got interrupted or did not shut down successfully within 5 minutes.");
						}
					}
				}

				if ( ! progressMonitor.isCanceled()) {
					/* print "(no results)" if the file would otherwise be empty */
					if (lineCount[0] == 0) {
						writer.writeNext(new String[] { NO_RESULTS_MESSAGE });
					} else if (lineCount[0] >= maxLines) {
						writer.writeNext(new String[0]); /* print a blank line */
						writer.writeNext(new String[] { String.format("Warning: Call Chain CSV export was aborted after %d lines!", Integer.valueOf(maxLines)) });
					}
				}

				stopWatch.stop();
				LOG.info(() -> String.format("CallChain CSV export of %d rows took %s (H:mm:ss.SSS)", Integer.valueOf(lineCount[0]), stopWatch));
			}
		}

	/**
	 * The supported export formats.
	 */
	public enum ExportFormat {
		DEPENDENCY_GRAPH("json", "application/json"),
		CSV("csv", "text/csv"),
		GRAPHML("graphml", "text/xml");

		private final String fileExtension;
		private final String contentType;

		private ExportFormat(final String fileExtension, final String contentType) {
			this.fileExtension = fileExtension;
			this.contentType = contentType;
		}
	}

}
