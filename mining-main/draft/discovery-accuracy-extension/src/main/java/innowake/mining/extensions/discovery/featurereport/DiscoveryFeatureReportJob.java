/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.discovery.featurereport;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.opencsv.CSVWriter;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.ResultOrderTaskProcessor;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskProcessor;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.extensions.discovery.featurereport.model.DiscoveryFeatures;
import innowake.mining.extensions.discovery.featurereport.model.DiscoveryFeaturesModel;
import innowake.mining.extensions.discovery.featurereport.model.FeatureMatrix;
import innowake.mining.extensions.discovery.featurereport.model.FeatureMatrixRoot;
import innowake.mining.extensions.discovery.featurereport.model.MiningFeatures;
import innowake.mining.extensions.discovery.featurereport.model.Supported;
import innowake.mining.extensions.discovery.featurereport.model.TechnologyAndType;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ProjectPojo;

/**
 *
 */
public class DiscoveryFeatureReportJob extends Job<FileSystemResult> {

	private static final int PAGE_SIZE = 1000;
	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryFeatureReportJob.class);
	private static final Set<String> ATTRIBUTES_TO_BE_FILTER = new HashSet<>(Arrays.asList("ID_NAME", "DCB", "SPACE", "DATACLAS", "AMP", "UNIT", "DSORG",
			"RETPD", "OUTBOUND_TARGETS", "FILE_ALIAS", "STATEMENT", "OUTBOUND", "IMS_PCB_SENSEG", "IMS_PCB_TYPE", "IMS_PCB_PROCSEQ", "IMS_PCB_PROCOPT",
			"IMS_DBD_NAME", "IMS_DBD_SEGMENT_PARENT", "ID_NAME", "RETPD", "DISP", "STORCLAS", "AMP", "MGMTCLAS", "BLKSIZE", "VOL", "RECFM", "AVGREC",
			"LABEL", "LRECL", "INBOUND", "DSN"));

	@Autowired
	private transient ProjectService projectDao;

	@Autowired
	private transient ModuleService moduleService;

	private transient DiscoveryFeaturesModel model;

	@PostConstruct
	public void init() {
		model = new DiscoveryFeaturesModel();
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Discovery Feature Report Collection");
		progressMonitor.setStepDescription("Setting up");
		final TaskProcessor<DiscoveryFeatures> taskProcessor = new ResultOrderTaskProcessor<>(jobManager, assertNotNull(jobMonitor));
		final TaskSource<DiscoveryFeatures> taskSource = createTaskSource(progressMonitor);
		final ResultConsumer<DiscoveryFeatures> resultConsumer = createResultConsumer(progressMonitor);

		forkTasks(taskProcessor, taskSource, resultConsumer);

		progressMonitor.setStepDescription("Exporting result");

		final String dateString = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date());

		try (final ZipOutputStream zipOut = new ZipOutputStream(new BufferedOutputStream(createResultFile()))) {
			zipOut.putNextEntry(new ZipEntry(String.format("discoveryFeatureReport_%s.csv", dateString)));
			final ByteArrayOutputStream baos = new ByteArrayOutputStream();
			try (final CSVWriter writer = new CSVWriter(new OutputStreamWriter(baos, StandardCharsets.UTF_8))) {
				writer.writeNext(DiscoveryFeatures.getCsvHeaders());
				model.getFeatures().values().forEach(features -> writer.writeNext(features.toCsv()));
			}
			zipOut.write(baos.toByteArray());

			zipOut.putNextEntry(new ZipEntry(String.format("discoveryFeatureReport_%s.json", dateString)));
			final ObjectMapper objectMapper = new ObjectMapper();
			objectMapper.writeValue(zipOut, prepareJsonModel());
		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		return new Result<>(new FileSystemResult("application/zip", String.format("discoveryFeatureReport_%s.zip", dateString)));
	}

	private FeatureMatrixRoot prepareJsonModel() {
		final Set<FeatureMatrix> featureMatrixs = new HashSet<>();
		final FeatureMatrixRoot matrixRoot = new FeatureMatrixRoot();
		final List<String> allSupportedFeatures = Stream.of(MiningFeatures.values()).map(MiningFeatures::getSupportedFeature).collect(Collectors.toList());
		matrixRoot.setFeatures(new LinkedHashSet<>(allSupportedFeatures));
		model.getFeatures().values().forEach(feature -> {
			try {
				final FeatureMatrix featureMatrix = new FeatureMatrix();
				featureMatrix.setModuleType(feature.getTechnology() + "_" + feature.getType());
				featureMatrix.setTechnology(feature.getTechnology().toString());
				featureMatrix.setType(feature.getType().toString());
				featureMatrix.setFullySupportedFeatures(mapSupportedFeature(feature));
				featureMatrix.setContains(feature.getContains().stream().sorted().map(TechnologyAndType::toString).collect(Collectors.toSet()));
				featureMatrix.setRepresentation(getReprsentations(feature));
				filterAttributes(feature);
				featureMatrix.setDependencies(feature.getPossibleRelationships());
				featureMatrixs.add(featureMatrix);
			} catch (final Exception ex) {
				LOG.info("Getting error during mapping of model object to json" + ex);
			}
		});
		matrixRoot.setFeatureMatrixs(featureMatrixs);
		return matrixRoot;
	}

	private void filterAttributes(final DiscoveryFeatures feature) {
		feature.getPossibleRelationships().forEach(relationshipTo -> relationshipTo.getPossibleAttributes().forEach((key, value) -> {
			if (ATTRIBUTES_TO_BE_FILTER.contains(key)) {
				relationshipTo.getPossibleAttributes().put(key, new HashSet<>(Arrays.asList("<filtered>")));
			}
		}));
	}

	/**
	 * Added for collecting supported feature for module
	 *
	 * @param feature
	 * @return list of supported features
	 */
	private Set<String> mapSupportedFeature(final DiscoveryFeatures feature) {
		final Set<String> supportedFeatures = new LinkedHashSet<>();
		if (feature.getSupportsPhysicalLines() == Supported.YES || feature.getSupportsPhysicalLines() == Supported.SOMETIMES) {
			supportedFeatures.add(MiningFeatures.PHY_LINES_COUNT.getSupportedFeature());
		}
		if (feature.getSupportsCodeLines() == Supported.YES || feature.getSupportsCodeLines() == Supported.SOMETIMES) {
			supportedFeatures.add(MiningFeatures.CODE_LINES_COUNT.getSupportedFeature());
		}
		if (feature.getSupportsCommentLines() == Supported.YES || feature.getSupportsCommentLines() == Supported.SOMETIMES) {
			supportedFeatures.add(MiningFeatures.COMMENT_LINES_COUNT.getSupportedFeature());
		}
		if (feature.getSupportsLinesOfDeadCode() == Supported.YES || feature.getSupportsLinesOfDeadCode() == Supported.SOMETIMES) {
			supportedFeatures.add(MiningFeatures.DEAD_CODE_LINES_COUNT.getSupportedFeature());
		}
		if (feature.getSupportsComplexity() == Supported.YES || feature.getSupportsComplexity() == Supported.SOMETIMES) {
			supportedFeatures.add(MiningFeatures.COMPLEXITY_MCCABE.getSupportedFeature());
		}
		if (feature.getSupportsSqlStatements() == Supported.YES || feature.getSupportsSqlStatements() == Supported.SOMETIMES) {
			supportedFeatures.add(MiningFeatures.SQL_STATEMENTS.getSupportedFeature());
		}
		if (feature.getSupportsStatements() == Supported.YES || feature.getSupportsStatements() == Supported.SOMETIMES) {
			supportedFeatures.add(MiningFeatures.STATEMENTS.getSupportedFeature());
		}
		return supportedFeatures;
	}

	private Set<String> getReprsentations(final DiscoveryFeatures feature) {
		final Set<String> representations = new HashSet<>();
		if (feature.getHasPath() == Supported.YES) {
			representations.add(ContributorResult.Type.ROOT_MODULE.name());
		}
		if (feature.getHasContentsPath() == Supported.YES) {
			representations.add(ContributorResult.Type.SUB_MODULE.name());
		}
		if (feature.getHasPath() == Supported.NO && feature.getHasContentsPath() == Supported.NO) {
			representations.add(ContributorResult.Type.EXTERNAL_MODULE.name());
		}
		return representations;
	}

	private TaskSource<DiscoveryFeatures> createTaskSource(final ProgressMonitor progressMonitor) {
		final List<EntityId> projectIds = projectDao.find(q -> { /* findAll */ }).stream().map(ProjectPojo::identity).collect(Collectors.toList());
		final Long totalNumberOfModules = moduleService.countModules(q -> q.withIdentified(true));

		progressMonitor.setStepDescription(String.format("Collecting information from %d Modules", totalNumberOfModules));
		progressMonitor.begin(totalNumberOfModules.intValue());

		return new TaskSource<DiscoveryFeatures>() {

			private int projectIndex = 0;
			private int pageIndex = 0;
			private int rowIndex = 0;
			private List<EntityId> currentPage = null;
			private boolean hasNextPage = true;

			@Override
			public boolean hasNextTask() {
				if (projectIds.isEmpty()) {
					/* no projects, no tasks */
					return false;
				}

				if (currentPage == null) {
					/* get first page */
					getNextPage();
				}

				if (rowIndex == currentPage.size()) {
					/* done with current page, see if we can fetch more */
					if (hasNextPage) {
						pageIndex++;
					} else if (projectIndex < projectIds.size() - 1) {
						pageIndex = 0;
						projectIndex++;
						LOG.info(() -> String.format("Fetching the information from project: %s", projectIds.get(projectIndex)));
					} else {
						return false;
					}
					getNextPage();
				}

				while ( ! (hasNextPage || rowIndex < currentPage.size()) && projectIndex < projectIds.size() - 1) {
					/* when the current project does not have any modules then read through next project */
					pageIndex = 0;
					projectIndex++;
					getNextPage();
				}
				return hasNextPage || rowIndex < currentPage.size();
			}

			@Override
			public Task<DiscoveryFeatures> nextTask() {
				return new DiscoveryFeatureReportTask(progressMonitor, jobId, currentPage.get(rowIndex++));
			}

			private void getNextPage() {
				final var nextPage = moduleService.findModuleIds(Pagination.at(pageIndex, PAGE_SIZE), q -> q.ofProject(projectIds.get(projectIndex)));
				hasNextPage = nextPage.getLastElement() < nextPage.getTotalElements();
				rowIndex = 0;
				currentPage = nextPage.getContent();
			}
		};
	}

	private ResultConsumer<DiscoveryFeatures> createResultConsumer(final ProgressMonitor progressMonitor) {
		return new ResultConsumer<DiscoveryFeatures>(new ReportMessageExceptionHandler<>(assertNotNull(jobMonitor))) {

			@Override
			protected void handleResult(final String taskId, final Result<DiscoveryFeatures> result) {
				model.merge(result.value);
				progressMonitor.worked(1);
			}
		};
	}
}
