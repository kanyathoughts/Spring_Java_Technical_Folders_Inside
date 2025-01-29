/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.model.job.JobStatus.CANCELED;
import static innowake.mining.shared.model.job.JobStatus.CANCEL_REQUESTED;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;

import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.util.MultiValueMap;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ByteArrayResult;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.IllegalJobStateException;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobInfoService.JobInfoInquiryBuilder;
import innowake.lib.job.api.MimeResult;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.config.properties.ClusterProperties.ClusterMode;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.config.properties.LogProperties;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.JobInfo;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.server.config.security.AuthenticationFacade;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.MiningJobInfoService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.MiningJobInfoPojo;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.ResultContainer;
import innowake.mining.shared.model.job.ResultStatus;
import innowake.mining.shared.model.job.ResultStatus.Severity;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import innowake.mining.shared.service.UserRoleService;

/**
 * Handles all job related operations like submission, deletion and listing out the jobs by connecting with DB.
 * It also check the authorization of jobs with respective to the login user and submit the job.
 */
@Service
public class MiningJobService {

	private static final Logger LOG = LoggerFactory.getLogger(MiningJobService.class);

	@Autowired
	private JobManager jobManager;

	@Autowired
	private MiningJobInfoService miningJobInfoService;
	
	@Autowired
	private UserRoleService userRoleService;
	
	@Autowired
	private UserNameUtil userNameUtil;
	
	@Autowired
	protected AuthenticationFacade authentication;
	
	@Autowired(required = false)
	private List<MiningJobExtension<?>> jobExtensions = Collections.emptyList();

	@Autowired
	private JobConfigurationProperties jobConfigProperties;
	@Autowired
	@Qualifier(JobConfiguration.NODE_NAME)
	private String nodeName;
	@Autowired
	private ObjectMapper objectMapper;
	
	/**
	 * Cleans up the log directory on server startup, so that we don't accumulate log file indefinitely.
	 * <p>
	 * We do this on startup rather than shutdown, so it is possible to investigate log files after stopping
	 * the server.
	 */
	@PostConstruct
	public void cleanupLogDirectory() {
		final LogProperties properties = jobConfigProperties.getLog();
		final ClusterMode clusterMode = jobConfigProperties.getCluster().getMode();
		final String logFolder = properties.getLogFolder() + (clusterMode != ClusterMode.STANDALONE ? "/" + nodeName : ""); 
		final String glob = "glob:" + logFolder + "/" + properties.getLogFilePrefix() + "*.log";
		collectLogFiles(logFolder, glob).forEach(File::delete);
	}

	/**
	 * Returns all log files for the given {@code jobId} from all cluster nodes.
	 *
	 * @param jobId the Id of the job to get all logs for
	 * @return a Map containing a mapping from cluster node Id to log file content
	 * @throws IOException if an error occurred fetching the logs
	 */
	public Map<String, String> getJobLog(final String jobId) throws IOException {
		return getJobLogFile(jobId).entrySet().stream()
				.collect(HashMap::new, (m, e) -> {
					try {
						m.put(e.getKey(), FileUtils.readFileToString(e.getValue(), StandardCharsets.UTF_8));
					} catch (final IOException ex) {
						LOG.error(() -> "Encountered error while reading log " + e.getValue().getAbsolutePath(), ex);
						throw new RuntimeException(ex);
					}
				}, HashMap::putAll);
	}

	private Tuple2<String, List<File>> getAllLogsForJobId(final String jobId) {
		final LogProperties properties = jobConfigProperties.getLog();
		final ClusterMode clusterMode = jobConfigProperties.getCluster().getMode();
		final String logFolder = properties.getLogFolder();
		final String glob = "glob:" + logFolder + (clusterMode != ClusterMode.STANDALONE ? "/*/" : "/") + properties.getLogFilePrefix() + jobId + ".log";
		final List<File> logFiles = collectLogFiles(logFolder, glob);
		return Tuple2.of(glob, logFiles);
	}

	/**
	 * Returns all log files as a String, File Map for the given {@code jobId} from all cluster nodes.
	 *
	 * @param jobId the Id of the job to get all logs for
	 * @return a Map containing a mapping from cluster node Id to File Object of log file
	 */
	public Map<String, File> getJobLogFile(final String jobId) {
		final Tuple2<String, List<File>> logFiles = getAllLogsForJobId(jobId);
		if (logFiles.b.isEmpty()) {
			LOG.info(() -> "No log files could be found with the pattern: " + logFiles.a);
			return Collections.emptyMap();
		}
		final Map<String, File> result = new HashMap<>();
		final LogProperties properties = jobConfigProperties.getLog();
		final Pattern nodeIdPattern = Pattern.compile(".*" + properties.getLogFolder() + "[/\\\\](.*)[/\\\\]" + properties.getLogFilePrefix() + jobId + ".*");
		for (final File file : logFiles.b) {
			final Matcher matcher = nodeIdPattern.matcher(file.getAbsolutePath());
			if (matcher.matches()) {
				final String nodeId = matcher.group(1);
				result.put(nodeId,file);
			} else {
				/* when there's no node Id in the path, then we are in standalone mode with only one possible log file from the current instance.*/
				result.put(nodeName, file);
				break;
			}
		}
		return result;
	}

	private List<File> collectLogFiles(final String logFolder, final String glob) {
		final List<File> logFiles = new ArrayList<>();
		final PathMatcher pathMatcher = FileSystems.getDefault().getPathMatcher(glob);
		
		final Path jobLogFolder = Paths.get(logFolder);
		if (jobLogFolder.toFile().exists()) {
			try (final Stream<Path> files = Files.walk(jobLogFolder)) {
				files.forEach(path -> {
					if (pathMatcher.matches(path)) {
						final File file = path.toFile();
						if (file.isFile() && file.exists()) {
							logFiles.add(file);
						}
					}
				});
			} catch (final IOException e) {
				LOG.error("Unable to collect log files.", e);
			}
		}
		return logFiles;
	}

	@Deprecated
	public Optional<JobResult> getJobResult(final String jobId) {
		return getJobResult(UUID.fromString(jobId));
	}
	
	/**
	 * Returns the {@link JobResult} for the given {@link Job} ID, if available.
	 * 
	 * @param jobId the ID of the job to retrieve the result from
	 * @return a {@link JobResult} if available
	 */
	public Optional<JobResult> getJobResult(final UUID jobId) {
		final var jobInfos = jobManager.getJobs(q -> q.byId(jobId));
		if (jobInfos.isEmpty()) {
			LOG.debug(() -> "No job information available for the ID " + jobId); 
			return Optional.empty();
		}
		final var jobInfo = jobInfos.get(0);
		final Result<Serializable> result = jobInfo.getResult();
		if (result == null) {
			LOG.debug(() -> "No result available for the Job with ID " + jobId); 
			return Optional.empty();
		}
		
		final Serializable value = result.value;
		try {
			if (value instanceof ByteArrayResult byteArrayResult) {
				return Optional.of(JobResult.of(new ByteArrayInputStream(byteArrayResult.getData()), byteArrayResult.getContentType(), byteArrayResult.getFileName()));
			} else if (value instanceof FileSystemResult fileSystemResult) {
				final File resultFile = Paths.get(jobConfigProperties.getJobResultFolder(), jobId.toString()).toFile();
				if ( ! resultFile.exists()) {
					LOG.debug(() -> "Result file " + resultFile.getAbsolutePath() + " does not exist for the Job with ID " + jobId); 
					return Optional.empty();
				}
				return Optional.of(JobResult.of(new FileInputStream(resultFile), fileSystemResult.getContentType(), fileSystemResult.getFileName()));
			} else if (value != null) {
				/* Serialize as JSON to the response output stream. */
				final byte[] serializedObject = objectMapper.writeValueAsBytes(new ResultContainer(value));
				return Optional.of(JobResult.of(new ByteArrayInputStream(serializedObject), MediaType.APPLICATION_JSON_VALUE));
			} else {
				LOG.debug(() -> "Unsupported result value " + value + " for Job with ID " + jobId);
			}
		} catch (final IOException e) {
			LOG.error(e::getLocalizedMessage, e);
		}
		return Optional.empty();
	}

	/**
	 * Submits a job contributed by a job extension.
	 *
	 * @param projectId id of the project for which the job is submitted
	 * @param extensionId the id of the extension that contributes the job
	 * @param parameters map of parameters for the job
	 * @return the id of the submitted job
	 */
	public char[] submitJob(final EntityId projectId, final String extensionId, final Map<String, List<String>> parameters) {
		final MiningJobExtension<?> extension = getJobExtension(projectId.getNid(), extensionId);
		final Job<?> job = extension.createJob(projectId, parameters);
		return jobManager.submit(job).getJobId().toCharArray();
	}
	
	/**
	 * Submits a job contributed by a job extension. In this "V2" variant the parameters are passed via the query string,
	 * allowing additional (binary) input data to be passed to the job.
	 *
	 * @param projectId id of the project for which the job is submitted
	 * @param extensionId the id of the extension that contributes the job
	 * @param parameters map of parameters for the job
	 * @param file input data for the job passed in the request body
	 * @return the id of the submitted job
	 * @throws IOException when reading the uploaded data fails
	 */
	public char[] submitJobV2(final EntityId projectId, final String extensionId, final MultiValueMap<String, String> parameters, @Nullable final MultipartFile file)
			throws IOException {
		final MiningJobExtension<?> extension = getJobExtension(projectId.getNid(), extensionId);
		@SuppressWarnings("unchecked")
		final Job<?> job = extension.createJob(projectId, parameters, file == null ? (HttpEntity<byte[]>) HttpEntity.EMPTY : new HttpEntity<>(file.getBytes()));
		return jobManager.submit(job).getJobId().toCharArray();
	}
	
	/**
	 * Deletes {@link JobInfo} and {@link MiningJobInfoPojo} for the given {@code jobId}.
	 *
	 * @param jobId the ID of the {@link JobInfo}
	 * @return the number of deleted jobs
	 */
	public int deleteByJobId(final UUID jobId) {
		try {
			miningJobInfoService.delete(q -> q.byJobId(jobId));
			return jobManager.delete(jobId.toString());
		} catch (final IllegalJobStateException e) {
			throw new ResponseStatusException(HttpStatus.BAD_REQUEST, null, e);
		}
	}

	/**
	 * Deletes all {@link JobInfo}s and {@link MiningJobInfoPojo}s
	 *
	 * @return the number of deleted jobs
	 */
	public int deleteAll() {
		miningJobInfoService.delete(q -> {});
		return jobManager.delete(q -> {});
	}
	
	/**
	 * Deletes {@link JobInfo} and {@link MiningJobInfoPojo} for the given user name.
	 *
	 * @return the number of deleted jobs
	 */
	public int deleteByUserName() {
		final String userName = authentication.getUserId();
		miningJobInfoService.deleteByUserName(userName);
		return jobManager.delete(q -> q.withCreatedByUserId(userName));
	}
	
	/**
	 * Returns a {@link Paged} of {@link JobInformation JobInformations} that match with the filters in the given {@code builder}. The returned list can also
	 * contain jobs that are currently executed but not yet persisted.
	 * 
	 * @param pageable {@link Pagination} instance
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria.
	 * @return a page of matching {@link JobInformation JobInformations}
	 */
	public Paged<JobInformation> getAll(final Pagination pageable, final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		final var jobInfos = jobManager.getJobs(pageable, builder);

		/* Client and server use mining specific DTOs, therefore we have to map the objects. */
		return jobInfos.map(this::mapJobInfo);
	}
	
	/**
	 * Gets a {@link JobInformation} for the given job id.
	 *
	 * @param jobId the job id
	 * @return the {@link JobInformation}
	 * @throws ResponseStatusException if no job available with given job id
	 */
	public innowake.lib.job.api.management.JobInformation getJobInfoForId(final UUID jobId) {
		final List<innowake.lib.job.api.management.JobInformation> jobInfos = jobManager.getJobs(q -> q.byId(jobId));
		if (jobInfos.isEmpty()) {
			throw new ResponseStatusException(HttpStatus.NOT_FOUND);
		}
		return jobInfos.get(0);
	}
	
	/**
	 * Cancel a {@link JobInformation} based on the given job id and status.
	 *
	 * @param jobId the job id
	 * @param jobStatus the job status
	 * @throws ResponseStatusException if job is not active or job not found with given job id
	 */
	public void cancelJob(final UUID jobId, final JobStatus jobStatus) {
		if (jobStatus == CANCEL_REQUESTED || jobStatus == CANCELED) {
			return;
		} else if ( ! JobStatus.isActive(jobStatus)) {
			throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Job is not active and cannot be cancelled");
		}

		final JobMonitor jobMonitor = assertNotNull(jobManager).getJobMonitor(jobId.toString());
		if (jobMonitor == null) {
			throw new ResponseStatusException(HttpStatus.NOT_FOUND);
		} else {
			jobMonitor.cancel();
		}
	}
	
	/**
	 * Maps the internal JobInformation object of the job-api to the mining specific JobInformation DTO that is shared
	 * between the server and client implementation.
	 * 
	 * @param jobInfo the job info from job-api
	 * @return the mining specific {@link JobInformation}
	 */
	public JobInformation mapJobInfo(final innowake.lib.job.api.management.JobInformation jobInfo) {
		final JobInformation.Builder builder = new JobInformation.Builder()
				.setJobId(jobInfo.getJobId())
				.setJobName(jobInfo.getJobName())
				.setDescription(jobInfo.getJobDescription())
				.setEta(jobInfo.getEta())
				.setFinishTime(jobInfo.getFinishTime())
				.setScheduledStartTime(jobInfo.getScheduledStartTime())
				.setStartTime(jobInfo.getStartTime())
				.setStepDescription(jobInfo.getStepDescription())
				.setSubmitTime(jobInfo.getSubmitTime())
				.setTotalWorkUnits(jobInfo.getTotalWorkUnits())
				.setUserName(userNameUtil.getUserName(jobInfo.getUserName()))
				.setWorked(jobInfo.getProcessedWorkUnits())
				.setStatus(jobInfo.getStatus())
				.setMessages(jobInfo.getMessages());

		final Result<Serializable> result = jobInfo.getResult();
		if (result != null) {
			final boolean internalResult = result.value instanceof FileSystemResult && ((FileSystemResult) result.value).isAppInternal();
			final boolean hasCollectableResult = result.value instanceof MimeResult;
			final ResultStatus resultStatus = new ResultStatus(mapJobResultSeverity(result.status.getSeverity()), result.status.getMessage(),
					result.status.getStackTrace(), hasCollectableResult, internalResult);
			builder.setResultStatus(resultStatus);
		}

		final Optional<MiningJobInfoPojo> miningJobInfo = miningJobInfoService.findAny(q -> q.byJobId(UUID.fromString(jobInfo.getJobId())));
		if (miningJobInfo.isPresent()) {
			builder.setProjectId(miningJobInfo.get().getProject().map(EntityId::getNid).orElse(null));
			builder.setModuleId(miningJobInfo.get().getModule().map(EntityId::getNid).orElse(null));
		} else {
			LOG.error("No mining job info exists for the filter.");
		}

		return builder.build();
	}
	
	/**
	 * Deletes {@link JobInfo}s and {@link MiningJobInfoPojo}s for given projectId
	 * 
	 * @param projectId project Id
	 */	
	public void deleteByProjectId(final EntityId projectId) {
		final List<UUID> jobIds = findJobIdForProject(projectId);
		if ( ! jobIds.isEmpty()) {
			jobManager.delete(q -> q.byIds(jobIds));
		}
		miningJobInfoService.delete(q -> q.ofProject(projectId));
	}
	
	/**
	 * Deletes {@link JobInfo}s and {@link MiningJobInfoPojo}s for given list of moduleIds
	 * 
	 * @param moduleIds the IDs of the modules
	 */	
	public void deleteByModuleId(final List<EntityId> moduleIds) {
		final List<UUID> jobIds = findJobIdForModuleIds(moduleIds);
		if ( ! jobIds.isEmpty()) {
			jobManager.delete(q -> q.byIds(jobIds));
		}
		miningJobInfoService.delete(q -> q.ofModules(moduleIds.stream().map(EntityId :: getUid).collect(Collectors.toList())));
	}
	
	/**
	 * Find jobIds for given projectId
	 * 
	 * @param projectId the projectId
	 * @return  {@linkplain List} of JobIds. 
	 */
	public List<UUID> findJobIdForProject(final EntityId projectId) {
		return miningJobInfoService.findJobId(q -> q.ofProject(projectId));
	}
	
	/**
	 * Find jobIds for given moduleIds
	 * 
	 * @param moduleIds the IDs of the modules
	 * @return  {@linkplain List} of JobIds. 
	 */
	public List<UUID> findJobIdForModuleIds(final List<EntityId> moduleIds) {
		return miningJobInfoService.findJobId(q -> q.ofModules(moduleIds.stream().map(EntityId :: getUid).collect(Collectors.toList())));
	}
	
	/*
	 * Maps the internal Severity object of the job-api to the mining specific Severity DTO that is shared
	 * between the server and client implementation.
	 */
	private Severity mapJobResultSeverity(final innowake.lib.job.api.Severity severity) {
		switch (severity) {
			case CANCELED:
				return Severity.CANCELED;
			case ERROR:
				return Severity.ERROR;
			case OK:
				return Severity.OK;
			case UNDEFINED:
				return Severity.UNDEFINED;
			case WARNING:
				return Severity.WARNING;
			default:
				throw new IllegalArgumentException("Unsupported result severity: " + severity.name());
		}
	}

	private MiningJobExtension<?> getJobExtension(final Long projectId, final String extensionId) {
		final Optional<MiningJobExtension<?>> extension = jobExtensions.stream().filter(ext -> ext.getIdentifier().equals(extensionId)).findFirst();

		if ( ! extension.isPresent()) {
			throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Job extension " + extensionId + " not registered");
		}

		final NatureType requiredNature = extension.get().getRequiredNature();
		final RoleType requiredRole = extension.get().getRequiredRole();

		if ( ! userRoleService.isAdmin()
				&& ( ! userRoleService.getNaturesOnProject(projectId.longValue()).contains(requiredNature) || ! userRoleService.hasRequiredRole(projectId, requiredRole))
				&& ! userRoleService.isClientAdmin(projectId)) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN, "This action requires Nature " + requiredNature + " and Role" + requiredRole);
		}

		return extension.get();
	}

	/**
	 * Wrapper for the necessary information to send a job result to the client.
	 */
	public static class JobResult {
		private final InputStream content;
		private final String contentType;
		private final Optional<String> fileName;
		
		private JobResult(final InputStream content, final String contentType, @Nullable final String fileName) {
			this.content = content;
			this.contentType = contentType;
			this.fileName = Optional.ofNullable(fileName);
		}
		
		private static JobResult of(final InputStream content, final String contentType) {
			return new JobResult(content, contentType, null);
		}
		
		private static JobResult of(final InputStream content, final String contentType, final String fileName) {
			return new JobResult(content, contentType, fileName);
		}

		/**
		 * @return the content of the result as {@link InputStream}
		 */
		public InputStream getContent() {
			return content;
		}
		
		/**
		 * @return the content type of the result
		 */
		public String getContentType() {
			return contentType;
		}
		
		/**
		 * @return the file name of the result if available
		 */
		public Optional<String> getFileName() {
			return fileName;
		}
	}
}
