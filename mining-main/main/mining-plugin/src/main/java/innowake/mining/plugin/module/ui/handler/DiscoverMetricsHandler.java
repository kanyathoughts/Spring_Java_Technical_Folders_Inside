/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.plugin.base.JobUtil.isJobNotRunning;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.CustomResponseHandler;
import innowake.mining.client.service.RestService;
import innowake.mining.plugin.Features;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.JobUtil;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.ResultContainer;

/**
 * Executes Discovery Metrics on the Mining server.
 */
public class DiscoverMetricsHandler extends AbstractBaseDiscoverHandler {

	@Override
	protected String jobName() {
		return "Discover Metrics";
	}

	@Override
	protected RestService<String> serviceToExecute(final IProject project) throws CoreException, StorageException {
		return ApiClient.discoveryService(project).discoverMetrics().setIncremental( ! forceFullScan);
	}
	
	@Override
	protected String getLogPrefix() {
		return "discover-metrics";
	}

	@Override
	protected boolean shouldJobBlocking() {
		return false;
	}

	@Override
	protected boolean preProcess(final IProject project, final IProgressMonitor monitor, @Nullable final ExecutionEvent event) {
		try {
			if (preventSourceObjectUpload(event)) {
				Logging.info("Skipping SourceObject upload");
			} else if ( ! uploadFiles(assertNotNull(shell), project)) {
				return false;
			}
			new UploadConfigurationHandler().upload(project);
			final ConnectionInfo connectionInfo = MiningPreferences.getConnectionInfo(project).orElseThrow(() ->
			new ValidationException("Configuration error",
					String.format("API server for project '%s' not configured",
							project.getName())));
			final Long projectId = MiningPreferences.getApiProject(project)
					.orElseThrow(() -> new ValidationException("Project data not found for %s ", project.getName()))
					.getProjectId();
			final Long moduleCount = MiningServiceExecutor
					.create(() -> ApiClient.moduleService(connectionInfo).getModuleCount().setProjectId(projectId))
					.execute()
					.orElse(Long.valueOf(-1));

			if (moduleCount.equals(Long.valueOf(-1))) {
				return false;
			}

			if (moduleCount.equals(Long.valueOf(0))) {
				return true;
			}
			
			if (Features.INSTANCE.isEnabled(project, FeatureId.INCREMENTAL_SCAN)) {
				return true;
			}

			return isOverridingExistingMetricsResults(HandlerUtil.getActiveShell(event), moduleCount);

		} catch (final ValidationException | ExecutionException e) {
			throw new IllegalStateException(e);
		}
	}

	@Override
	protected void postProcess(final IProject project, final IProgressMonitor monitor, final JobInformation jobInfo, @Nullable final ExecutionEvent event) {
		super.postProcess(project, monitor, jobInfo, event);
		try {
			/* if Discover Metrics was successful, download the result file as Excel or CSV */
			if (jobInfo.getStatus() == JobStatus.SUCCESS) {
				boolean outputExcel = false;
				boolean outputCsv = false;
				final IFile file = project.getFile(ConfigResources.DISCOVERY_CONFIG.getResourceName());
				if (file.exists()) {
					final Config config = Config.loadConfig(new InputStreamReader(file.getContents(), file.getCharset()));
					outputExcel = config.isOutputExcelFormat();
					outputCsv = config.isOutputCsvFormat();
				}

				if (outputExcel) {
					final Optional<Tuple2<String, byte[]>> excelOpt = submitJobAndGetResult("discovery-excel", project, monitor);
					createFile(project, excelOpt);
				}

				if (outputCsv) {
					final Optional<Tuple2<String, byte[]>> csvOpt = submitJobAndGetResult("discovery-csv", project, monitor);
					createFile(project, csvOpt);
				}

				final Optional<Tuple2<String, byte[]>> effortSummaryOpt = MiningServiceExecutor
						.create(() -> ApiClient.ioService(project).exportEffortSummaryExcel())
						.execute();
				createFile(project, effortSummaryOpt);
			}

		} catch (final CoreException | UnsupportedEncodingException | DiscoveryException e) {
			Logging.error(e.getLocalizedMessage(), e);
		}
	}

	private void createFile(final IProject project, final Optional<Tuple2<String, byte[]>> fileOpt) throws CoreException {
		if (fileOpt.isPresent()) {
			final String fileName = fileOpt.get().a;
			final byte[] content = fileOpt.get().b;
			project.getFile(fileName).create(new ByteArrayInputStream(content), true, null);
		}
	}

	private boolean uploadFiles(final Shell shell, final IProject project) throws ExecutionException {
		final IResource srcFolder = project.getFolder(IdentificationMapper.SRC_ROOT);
		if (srcFolder.exists()) {
			return new UploadSourceObjectsHandler().process(shell, project, Arrays.asList(srcFolder), true, forceFullScan);
		}
		return UIHandlerUtils.getUserResponseFunction(shell)
				.apply("Do you want to perform Discover metrics for the modules (if any) in the server?",
						"\"src\" folder is not present locally")
				.booleanValue();
	}

	private static boolean isOverridingExistingMetricsResults(final Shell shell, final Long value) {
		/* if user answers "No" then process is aborted and no Job is scheduled */
		return UIHandlerUtils.getUserResponseFunction(shell)
				.apply(String.format(
						"Caution: %d Modules were already discovered in a previous run. Continuing will overwrite the previous results. Ok?",
						value), "Override existing Discover Metrics results?")
				.booleanValue();
	}
	
	private Optional<Tuple2<String, byte[]>> submitJobAndGetResult(final String extensionId, final IProject project, final IProgressMonitor monitor)  {
		Long projectId;
		try {
			projectId = MiningPreferences.getApiProject(project)
					.orElseThrow(() -> new ValidationException("Project data not found for %s ", project.getName()))
					.getProjectId();
		} catch (final ValidationException exception) {
			throw new IllegalStateException(exception);
		}
		
		final Optional<String> jobId = MiningServiceExecutor.create(() -> ApiClient.jobService(project).submitJobExtension()
				.setProjectId(projectId)
				.setExtensionId(extensionId))
				.setInvalidResultConsumer(
						invalidResult -> Logging.error("Failed to schedule " + extensionId + " Job. Response was: " + invalidResult.getExtendedStatusMessage()))
				.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception))
				.execute();

		if ( ! jobId.isPresent()) {
			return Optional.empty();
		}
		JobUtil.submittedRemoteJob(project, jobId.get());

		while ( ! monitor.isCanceled()) {
			try {
				Thread.sleep(getPollIntervalSeconds() * 1000L);
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
			}

			final Optional<JobInformation> jobInfo = MiningServiceExecutor.create(() -> ApiClient.jobService(project).getJobInfo().setJobId(jobId.get()))
					.setInvalidResultConsumer(invalidResult -> Logging
							.error("Failed to retrieve " + extensionId + " Job information. Response was: " + invalidResult.getExtendedStatusMessage()))
					.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception)).execute();

			if ( ! jobInfo.isPresent()) {
				return Optional.empty();
			}
			/* when job is no longer running ... */
			if (isJobNotRunning(jobInfo.get().getStatus())) {
				final AtomicReference<Tuple2<String, byte[]>> jobResult = new AtomicReference<>();
				final CustomResponseHandler<ResultContainer> responseHandler = new CustomResponseHandler<ResultContainer>() {

					@Override
					@Nullable
					public ResultContainer handleResponse(final CloseableHttpResponse response) {
						final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
						try {
							IOUtils.copy(response.getEntity().getContent(), outputStream);
						} catch (final UnsupportedOperationException | IOException e) {
							throw new IllegalStateException(e);
						}
						
						final Optional<String> fileName = Stream.of(response.getAllHeaders())
								.filter(header -> header.getName().equals("Content-Disposition"))
								.findFirst()
								.map(contentHeader -> contentHeader.getValue().split("=")[1]);
						
						final Tuple2<String, byte[]> result = new Tuple2<>(fileName.orElseThrow(() -> 
										new IllegalStateException("Unable to determine file name or downloaded file.")), 
								outputStream.toByteArray());
						
						jobResult.set(result);
						
						return null;
					}
				};
				MiningServiceExecutor.create(() -> ApiClient.jobService(project).getJobResult()
						.setJobId(jobId.get())
						.setCustomResponseHandler(responseHandler))
				.execute();
				
				return Optional.ofNullable(jobResult.get());
			}
		}

		/* IProgressMonitor was cancelled */
		MiningServiceExecutor.create(() -> ApiClient.jobService(project).cancelJob().setJobId(jobId.get()))
				.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception))
				.execute();
		
		return Optional.empty();
	}
}
