/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize;

import static innowake.mining.shared.discovery.categorize.Identification.notIdentified;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import com.google.common.io.Files;

import brave.Tracer;
import brave.Tracer.SpanInScope;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.CategorizerTask.Parameters.CategorizingMode;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob.DetectionType;
import innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase;
import innowake.mining.server.discovery.categorize.c.CFileTypeDetection;
import innowake.mining.server.discovery.categorize.cobol.CobolFileTypeDetection;
import innowake.mining.server.discovery.categorize.easytrieve.EasytrieveFileTypeDetection;
import innowake.mining.server.discovery.categorize.jcl.JclFileTypeDetection;
import innowake.mining.server.discovery.categorize.pl1.Pl1FileTypeDetection;
import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.server.discovery.monitor.ProgressMonitorText;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;
import innowake.mining.shared.discovery.config.core.Mapping.MappingType;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Used to walk thru the selected files and execute the different file type detections.
 * Does the file system move operation if a the file type detection was successful.
 * Identification is done in multiple steps.
 *
 * <li>1. Find the main object types for a language
 * <li>2. Find outgoing dependencies of the main types
 * <li>3. Identify the language by content (tokens)
 *
 * <p>Every file identify files from the selection and remove it from the pending detection file list.
 */
public class Categorizer {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CATEGORIZE_CATEGORIZER);

	private final SourceService sourceService;

	private Set<DetectionType> allActiveDetectionTypes = new HashSet<>();
	private Set<ResolveTarget> detectionLanguagesPreparePhase = new HashSet<>();
	private Set<ResolveTarget> detectionLanguagesMainPhase = new HashSet<>();
	private final Set<ResolveTarget> identifiedLanguagesMainTypeAndToken = ConcurrentHashMap.newKeySet();
	private final Set<ResolveTarget> identifiedLanguagesDependency = ConcurrentHashMap.newKeySet();
	private final IdentificationMapper mapper;

	/* Map of identifications that are a "maybe", the key is the full path to the file, the value is a list of maybe identifications for that file */
	private final Map<String, List<Identification>> maybeIdentifications = new ConcurrentHashMap<>();

	private final Map<Long, SourcePojo> unidentifiedResources;
	private final Map<Long, SourcePojo> allResources;
	private final Set<String> existingPaths = ConcurrentHashMap.newKeySet();
	private final ExecutorService updateExecutor;
	private final Set<Future<?>> updateTasks = ConcurrentHashMap.newKeySet();

	private final EntityId projectId;
	private final Statistic statistic = new Statistic();
	private final TaskHandler taskHandler;
	private final DiscoveryJobCache discoveryCache;
	private final String jobId;
	private final Tracer tracer;
	private final SearchOrders searchOrders; 
	private final Map<FeatureId, Boolean> featureMap;

	/**
	 * Constructs the Categorizer with the identification mapping and list of files to categorize.
	 *
	 * @param projectId the project id
	 * @param mapper The identification configuration for mapping
	 * @param sourceObjects The list of files to categorize and move
	 * @param sourceService class for accessing source object
	 * @param taskHandler the {@link TaskHandler} to fork additional tasks
	 * @param discoveryCache the {@link DiscoveryJobCache} that will hold intermediate results
	 * @param tracer the {@link Tracer}
	 * @param configProperties the {@link GenericConfigProperties}
	 * @param existingSourceObject the source object
	 * @param searchOrders the project searchOrders
	 * @param featureMap the map containing feature settings.
	 */
	public Categorizer(final EntityId projectId, final IdentificationMapper mapper, final Map<Long, SourcePojo> sourceObjects,
			final SourceService sourceService, final TaskHandler taskHandler, final DiscoveryJobCache discoveryCache, final Tracer tracer,
			final GenericConfigProperties configProperties,
			final List<SourcePojo> existingSourceObject, final SearchOrders searchOrders, final Map<FeatureId, Boolean> featureMap) {
		this.projectId = projectId;
		this.mapper = mapper;
		this.unidentifiedResources = sourceObjects;
		this.allResources = new ConcurrentHashMap<>(unidentifiedResources);
		this.sourceService = sourceService;
		this.taskHandler = taskHandler;
		this.discoveryCache = discoveryCache;
		this.jobId = taskHandler.getJobId();
		this.tracer = tracer;
		this.searchOrders = searchOrders;
		populateExistingDiscoverPath(existingSourceObject);
		LOG.debug(() -> "Configured discover code DB statement executor with " + configProperties.getDiscoverCodeDbThreads() + " threads");
		this.updateExecutor = Executors.newFixedThreadPool(configProperties.getDiscoverCodeDbThreads());
		this.featureMap = featureMap;
	}

	/**
	 * Prepares the Categorizer for performing detection(s).
	 *
	 * Clears the current statistics and sets the current list of source objects for the statistics.
	 */
	public void prepareBeforeDetection() {
		statistic.clear();
		statistic.setSelected(unidentifiedResources.size());
	}

	/**
	 * Performs file detection using the main types process.
	 *
	 * @param detectMonitor The detection monitor for monitoring progress.
	 * @param detections The list of file type detections to consider during the detection process.
	 */
	public void detectByMainType(final ProgressMonitor detectMonitor, final Set<DetectionType> detections) {
		setDetectionTypes(detections);

		if ( ! unidentifiedResources.isEmpty()) {
			detectMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_MAINTYPE);
			detectMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_MAINTYPE);
			detectMainTypes(detectMonitor.subMonitor(50));
			if ( ! unidentifiedResources.isEmpty()) {
				detectMonitor.setJobDescription(
						ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_MAINTYPE_ALL_AT_ONCE);
				detectMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_MAINTYPE_ALL_AT_ONCE);
				detectMainTypesMultiPhase(detectMonitor.subMonitor(50));
			} 
		}
	}

	/**
	 * Performs file detection using the token identification process.
	 *
	 * @param detectMonitor The detection monitor for monitoring progress.
	 * @param detections The list of file type detections to consider during the detection process.
	 */
	public void detectByTokens(final ProgressMonitor detectMonitor, final Set<DetectionType> detections) {
		setDetectionTypes(detections);
		if ( ! unidentifiedResources.isEmpty()) {
			detectMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_TOKEN);
			detectMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_TOKEN);
			detectByTokens(detectMonitor.subMonitor(100));
		}
	}

	/**
	 * Performs file detection using the dependency identification process.
	 *
	 * @param detectMonitor The detection monitor for monitoring progress.
	 * @param detections The list of file type detections to consider during the detection process.
	 */
	public void detectByDependencies(final ProgressMonitor detectMonitor, final Set<DetectionType> detections) {
		setDetectionTypes(detections);
		if ( ! unidentifiedResources.isEmpty()) {
			detectMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_DEPENDENCY);
			detectMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DEPENDENCY);
			detectDependencies(detectMonitor.subMonitor(100));
		}
	}

	/**
	 * Performs file detection using the file extension.
	 *
	 * @param detectMonitor The detection monitor for monitoring progress.
	 * @param detections The list of file type detections to consider during the detection process.
	 */
	public void detectByExtension(final ProgressMonitor detectMonitor, final Set<DetectionType> detections) {
		setDetectionTypes(detections);
		if ( ! unidentifiedResources.isEmpty()) {
			detectMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_EXTENSION);
			detectMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_EXTENSION);
			detectByExtension(detectMonitor.subMonitor(100));
		}
	}

	/**
	 * Detects any remaining unidentified files. This method is intended to be invoked only after the other detection processes have been run.
	 *
	 * This will also handle any lingering files that have not yet been identified as a definite YES (Identification.ID.YES), meaning it will handle
	 * files identified as maybe as well as no identification.
	 *
	 * @param detectMonitor The detection monitor for monitoring progress.
	 */
	public void detectRemainingUnidentified(final ProgressMonitor detectMonitor) {
		if ( ! unidentifiedResources.isEmpty()) {
			detectMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_UNIDENTIFIED);
			detectMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_UNIDENTIFIED);
			handleMaybes();
			detectUnidentified(detectMonitor.subMonitor(100));
		}
	}

	/**
	 * Wraps up the detection process.
	 * 
	 * @param jobMonitor the job monitor
	 */
	public void wrapUpDetection(final JobMonitor jobMonitor) {
		updateExecutor.shutdownNow();
		statistic.setUnidentified(unidentifiedResources.size());
	}

	/**
	 * Returns the statistic summary.
	 * 
	 * @return the statistic summary
	 */
	public Statistic getStatistic() {
		return statistic;
	}
	
	private void detectDependencies(final ProgressMonitor detectMonitor) {
		detectDependenciesPl1();

		/* Only Cobol, JCL and PL/I support dependency detection, with the latter one already handled above.
		 * Dependency detection requires the main resource objects to already be fully identified. Therefore we select those from
		 * the database to be processed. */
		final boolean detectCobol = detectionLanguagesMainPhase.contains(ResolveTarget.COBOL);
		final boolean detectJcl = detectionLanguagesMainPhase.contains(ResolveTarget.JCL);
		if (detectCobol || detectJcl) {
			final Map<Long, SourcePojo> relevantSourceObjects = new ConcurrentHashMap<>();
			sourceService.find(q -> {
					q.ofProject(projectId);
					if (detectCobol && detectJcl) {
						q.withTechnology(List.of(Technology.COBOL, Technology.JCL));
					} else if (detectJcl) {
						q.withTechnology(Technology.JCL);
					} else {
						q.withTechnology(Technology.COBOL);
					}
				}).parallelStream().forEach(sourceObject -> relevantSourceObjects.put(sourceObject.getId(), sourceObject));

			while ( ! unidentifiedResources.isEmpty()) {
				detectMonitor.checkCanceled();
				final int pendingFiles = unidentifiedResources.size();
				LOG.debug(() -> "categorize by dependencies. Pending files " + Integer.valueOf(pendingFiles));

				forkTasks(relevantSourceObjects, CategorizingMode.DEPENDENCY, DetectionPhase.MAIN, detectionLanguagesMainPhase, detectMonitor);

				final Map<Long, Identification> identifications = new ConcurrentHashMap<>();
				collectCobolDependencyIdentifications(identifications);
				collectJclDependencyIdentifications(identifications);

				relevantSourceObjects.clear();
				identifications.values().forEach(identification -> {
					if (identification.getId() == Identification.ID.MAYBE) {
						addOneMaybe(identification);
					} else if (identification.getId() == Identification.ID.YES) {
						move(identification);
						final SourcePojo removedObject = unidentifiedResources.remove(identification.resourceId);
						if (removedObject != null) {
							/* dependency detection needs to be executed again for every identified dependency,
							 * to make sure that we also get the dependencies of those. */
							relevantSourceObjects.put(removedObject.getId(), removedObject);
						}
						identifiedLanguagesDependency.add(identification.getTarget().getLanguage());
					}
				});
				waitForUpdateTasks();

				if (unidentifiedResources.size() == pendingFiles) {
					LOG.debug(() -> "No more files could be identified by dependencies.");
					break;
				}
			}
		}
	}

	private void detectDependenciesPl1() {
		if (detectionLanguagesMainPhase.contains(ResolveTarget.PL1)) {
			/* All necessary information for the PL1 dependency detection has already been put into the cache while categorizing by main type.
			 * Therefore we can simply iterate over the cache values. This achieves a much better performance instead of forking many tasks
			 * that would just check a single value in the distributed cache, as no parsing or any other heavyweight operation is being executed. */
			@SuppressWarnings("unchecked")
			final Set<String> includeTargets = (Set<String>) (Set<?>) discoveryCache.getMultiValue(jobId, Pl1FileTypeDetection.CACHE_KEY);
			if ( ! includeTargets.isEmpty()) {
				final List<Identification> pl1Identifications = unidentifiedResources.values().parallelStream()
						.filter(file -> includeTargets.contains(FilenameUtils.getBaseName(file.getName()).toLowerCase()))
						.map(file -> new Identification(ID.YES, file.getId(), ResolveTarget.PL1_COPYBOOK, ResolveTarget.PL1))
						.collect(Collectors.toList());
				pl1Identifications.stream().forEach(identification -> {
					move(identification);
					unidentifiedResources.remove(identification.resourceId);
					identifiedLanguagesDependency.add(identification.getTarget().getLanguage());
				});
			}
			waitForUpdateTasks();
		}
	}

	private void collectCobolDependencyIdentifications(final Map<Long, Identification> identifications) {
		if (detectionLanguagesMainPhase.contains(ResolveTarget.COBOL)) {
			@SuppressWarnings("unchecked")
			final Set<String> copyTargetNames = (Set<String>) (Set<?>) discoveryCache.getMultiValue(jobId, CobolFileTypeDetection.CACHE_KEY);
			if ( ! copyTargetNames.isEmpty()) {
				/* identified sources can have the same name and different path */
				unidentifiedResources.values().parallelStream()
				.filter(sourceObject -> copyTargetNames.parallelStream()
						.anyMatch(targetName -> Files.getNameWithoutExtension(sourceObject.getName()).equalsIgnoreCase(targetName)))
				.forEach(sourceObject ->
					identifications.put(sourceObject.getId(), new Identification(ID.YES, sourceObject.getId(), ResolveTarget.COBOL_COPYBOOK, ResolveTarget.COBOL)));
			}
		}
	}

	private void collectJclDependencyIdentifications(final Map<Long, Identification> identifications) {
		if (detectionLanguagesMainPhase.contains(ResolveTarget.JCL)) {
			@SuppressWarnings("unchecked")
			final Set<String> controlCardNames = (Set<String>) (Set<?>) discoveryCache.getMultiValue(jobId, JclFileTypeDetection.CONTROL_CARDS_CACHE_KEY);
			@SuppressWarnings("unchecked")
			final Set<String> potentialProcNames = (Set<String>) (Set<?>) discoveryCache.getMultiValue(jobId, JclFileTypeDetection.PROCS_CACHE_KEY);
			@SuppressWarnings("unchecked")
			final Set<String> includeGroups = (Set<String>) (Set<?>) discoveryCache.getMultiValue(jobId, JclFileTypeDetection.INCLUDES_CACHE_KEY);

			unidentifiedResources.values().parallelStream()
			/* map control card and/or proc names to identifications */
			.map(sourceObject -> {
				final String name;
				final Set<String> jclExtensions = ResolveTargetHelper.getExtensionsByLanguage(ResolveTargetHelper.fromTechnology(Technology.JCL));
				if (jclExtensions.contains(Files.getFileExtension(sourceObject.getName()))) {
					name = Files.getNameWithoutExtension(sourceObject.getName());
				} else {
					name = sourceObject.getName();
				}
				if (controlCardNames.contains(name)) {
					/* sry for this ugly hack! We are changing the name of the source here but you can not set the new name on the SourcePojo and
					 * I did not want to introduce another map for holding the SourcePojoPrototype for a SourcePojo with the changed values */
					return new Identification(ID.YES, sourceObject.getId(), ResolveTarget.JCL_CONTROLCARD, ResolveTarget.JCL, name.concat(".CRD"));
				} else if (potentialProcNames.contains(name)) {
					return new Identification(ID.YES, sourceObject.getId(), ResolveTarget.JCL_PROC, ResolveTarget.JCL);
				} else if (includeGroups.contains(name)) {
					return new Identification(ID.YES, sourceObject.getId(), ResolveTarget.JCL_INCLUDE, ResolveTarget.JCL);
				} else {
					return null;
				}
			})
			.filter(Objects::nonNull)
			.forEach(identification -> {
				/* If the sourceObject has already identified as YES for another language, then we have to change
				   the identification as MAYBE  */
				if (identifications.containsKey(identification.getResourceId())){
					identification.setId(ID.MAYBE);
				}
				identifications.put(identification.getResourceId(), identification);
			});
		}
	}

	private void detectByTokens(final ProgressMonitor detectMonitor) {
		final Map<Long, List<Identification>> identifications = forkTasks(unidentifiedResources, CategorizingMode.TOKEN,
				DetectionPhase.MAIN, detectionLanguagesMainPhase, detectMonitor);
		processIdentifications(identifications);
	}

	private void detectByExtension(final ProgressMonitor detectMonitor) {
		final Map<Long, SourcePojo> relevantUnidentifiedResources = unidentifiedResources.entrySet().parallelStream()
				.filter(entry -> StringUtils.isNotBlank(FilenameUtils.getExtension(entry.getValue().getPath())))
				.collect(Collectors.toMap(Entry<Long, SourcePojo>::getKey, Entry<Long, SourcePojo>::getValue));
		final Map<Long, List<Identification>> identifications = forkTasks(relevantUnidentifiedResources, CategorizingMode.EXTENSION,
				DetectionPhase.MAIN, detectionLanguagesMainPhase, detectMonitor);
		processIdentifications(identifications);
	}

	private void detectMainTypes(final ProgressMonitor detectMonitor) {
		detectMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_MAINTYPE);
		detectMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_MAINTYPE);

		final Map<Long, List<Identification>> identifications = forkTasks(unidentifiedResources, CategorizingMode.MAIN,
				DetectionPhase.MAIN, detectionLanguagesMainPhase, detectMonitor);
		processIdentifications(identifications);
	}

	private void detectMainTypesMultiPhase(final ProgressMonitor detectMonitor) {
		LOG.debug(() -> String.format("Categorize by content all remaining files (%s) ", Integer.valueOf(unidentifiedResources.size())));
		detectMonitor.checkCanceled();

		final Map<Long, List<Identification>> identifications = forkTasks(unidentifiedResources, CategorizingMode.MAIN_MULTI_PHASE,
				DetectionPhase.PREPARE, detectionLanguagesPreparePhase, detectMonitor);

		forkTasks(unidentifiedResources, CategorizingMode.MAIN_MULTI_PHASE, DetectionPhase.MAIN, detectionLanguagesMainPhase, detectMonitor)
		.forEach((key, value) -> identifications.computeIfAbsent(key, k -> new ArrayList<>()).addAll(value));

		collectCMainTypeIdentifications(identifications);
		collectEztMainTypeIdentifications(identifications);

		identifications.values().stream().forEach(nested -> {
			final List<Identification> yes = nested.stream().filter(i -> i.getId() == ID.YES).collect(Collectors.toList());
			if ( ! yes.isEmpty()) {
				final Identification identification = resolveId(yes);
				if (identification != null) {
					move(identification);
					unidentifiedResources.remove(identification.resourceId);
					identifiedLanguagesMainTypeAndToken.add(identification.getTarget().getLanguage());
				}
			}
		});
		waitForUpdateTasks();
	}

	private void collectCMainTypeIdentifications(final Map<Long, List<Identification>> identifications) {
		/* The C detection will put all headers found during the detection into the cache as parser currently doesn't
		 * distinguish between program and header sources. We then iterate over all identified C resources and check
		 * if they're contained in the list of headers and set them to be identified as a header. */
		if (detectionLanguagesMainPhase.contains(ResolveTarget.C)) {
			/* Contains the name of all headers encountered during the detection. */
			final Set<Object> cHeaders = discoveryCache.getMultiValue(jobId, CFileTypeDetection.CACHE_KEY);
			identifications.values().parallelStream()
			.flatMap(List<Identification>::stream)
			.filter(i -> i.getCategorizerLanguage() == ResolveTarget.C) /* only process entries created by the C detection */
			.filter(i -> i.getTarget() == ResolveTarget.C_PROGRAM && i.getId() != ID.NO)
			.filter(i -> {
				final String sourceFileName = Files.getNameWithoutExtension(allResources.get(i.resourceId).getName().toUpperCase()).trim();
				final String extension = FilenameUtils.getExtension(allResources.get(i.resourceId).getPath());
				return (cHeaders.contains(sourceFileName) || cHeaders.contains(sourceFileName + ".h") || cHeaders.contains(sourceFileName + ".H") &&
						! extension.equalsIgnoreCase("C"));
			})
			.forEach(i -> {
				i.setTarget(ResolveTarget.C_HEADER);
				i.setId(ID.YES);
			});
		}
	}

	private void collectEztMainTypeIdentifications(final Map<Long, List<Identification>> identifications) {
		/* The Easytrieve detection initially marks all macros as "maybe" identified during the preparation phase, as the actual
		 * identification is only known when the programs had been successfully parsed during the main phase. The main phase then
		 * puts all properly identified macros into the cache. We iterate over this list and switch the "maybe" identifications to "yes". */
		if (detectionLanguagesMainPhase.contains(ResolveTarget.EASYTRIEVE)) {
			/* The "maybe" macros identified during the preparation phase. */
			final Set<Identification> macroIdentifications = identifications.values().parallelStream()
					.flatMap(List<Identification>::stream)
					.filter(i -> i.getCategorizerLanguage() == ResolveTarget.EASYTRIEVE)
					.filter(i -> i.getTarget() == ResolveTarget.EASYTRIEVE_MACRO_FILE)
					.collect(Collectors.toSet());

			/* Contains the names of all properly identified macros. */
			@SuppressWarnings("unchecked")
			final Set<String> identifiedMacroCalls = (Set<String>) (Set<?>) discoveryCache.getMultiValue(jobId, EasytrieveFileTypeDetection.CACHE_KEY);

			/* Change to a definitive "yes" any maybe macros that are in the macro call list */
			macroIdentifications.parallelStream()
			.filter(i -> i.getId() != ID.NO)
			.filter(i -> identifiedMacroCalls.parallelStream()
					.anyMatch(m -> m.equalsIgnoreCase(Files.getNameWithoutExtension(unidentifiedResources.get(i.resourceId).getName()))))
			.forEach(i -> i.setId(ID.YES));
		}
	}


	/*
	 * Returns a Set of languages that are eligible for the provided phase.
	 */
	@SuppressWarnings("unchecked")
	private Set<ResolveTarget> getDetectionLanguagesForPhase(final DetectionPhase phase) {
		final Set<ResolveTarget> languages = new HashSet<>();
		for (final DetectionType detectionType : allActiveDetectionTypes) {
			try {
				final Method method = detectionType.detectionClass.getMethod("getDetectionPhases");
				final Set<DetectionPhase> supportedPhases = ((Set<DetectionPhase>) method.invoke(null));
				if (supportedPhases.contains(phase)) {
					languages.add(detectionType.language);
				}
			} catch (final NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
				throw new IllegalStateException("Every FileTypeDetection requires a public static method 'getDetectionPhases' returning the supported identification phases", e);
			}
		}
		return languages;
	}

	private Map<Long, List<Identification>> forkTasks(final Map<Long, SourcePojo> resources, final CategorizingMode mode,
			final DetectionPhase phase, final Set<ResolveTarget> detectionLanguages, final ProgressMonitor monitor) {
		final Map<Long, List<Identification>> identifications = new HashMap<>();

		if ( ! detectionLanguages.isEmpty()) {
			final int total = resources.size();
			final Iterator<Entry<Long, SourcePojo>> fileIter = resources.entrySet().iterator();

			final TaskSource<Identification[]> taskSource = new TaskSource<Identification[]>() {
				private int submitCount;

				@Override
				public boolean hasNextTask() {
					return fileIter.hasNext();
				}

				@Override
				public Task<Identification[]> nextTask() {
					LOG.trace(() -> String.format("Submitting discover code task for mode '%s' and phase '%s' (%d/%d)", mode.name(), phase.name(),
							Integer.valueOf(submitCount++), Integer.valueOf(total)));
					final CategorizerTask.Parameters parameters = new CategorizerTask.Parameters(projectId, detectionLanguages, fileIter.next().getValue().getId(),
							mode, phase, searchOrders, featureMap);
					return new CategorizerTask(monitor.subMonitor(1), jobId, parameters);
				}
			};

			final JobMonitor jobMonitor = taskHandler.getJobMonitor();
			final ResultConsumer<Identification[]> resultConsumer = new ResultConsumer<Identification[]>(new ReportMessageExceptionHandler<>(jobMonitor)) {
				private int resultCount = 1;

				@Override
				protected void handleResult(final String taskId, final Result<Identification[]> result) {
					final String message = String.format("Receiving discover code task result for mode '%s' and phase '%s' (%d/%d)", mode.name(), phase.name(),
							Integer.valueOf(resultCount++), Integer.valueOf(total));
					LOG.trace(() -> message);
					ProgressMonitorThrottle.throttleStepDescription(message, monitor);
					final Identification[] values = result.value;
					if (values != null) {
						final List<Identification> ids = Arrays.asList(values);
						if ( ! ids.isEmpty()) {
							identifications.computeIfAbsent(ids.get(0).resourceId, key -> new ArrayList<>()).addAll(ids);
						}
					}
				}
			};

			taskHandler.forkJobTasks(taskSource, resultConsumer);
		}

		return identifications;
	}

	/**
	 * Detects all files that haven't been identified and moves them to their own folder (determined by a config property).
	 *
	 * @param monitor The progress monitor.
	 */
	private void detectUnidentified(final ProgressMonitor detectMonitor) {
		final Iterator<SourcePojo> fileIter = unidentifiedResources.values().iterator();
		while (fileIter.hasNext()) {
			detectMonitor.checkCanceled();
			final SourcePojo file = fileIter.next();
			LOG.trace(() -> "categorize by unidentified " + file);
			final Identification identification = notIdentified(file, ResolveTarget.NONE);

			move(identification);
		}
		waitForUpdateTasks();
	}

	/**
	 * Inspects a given list of {@link Identification} results and resolves it accordingly.
	 *
	 * <li> There can only be one or none {@link ID#YES}
	 * <li> Only if there is one {@link ID#MAYBE} it will be resolved
	 *
	 * @param identifications a list of {@link Identification}
	 * @return Identification the resolved {@link Identification} from the list of Identifications.
	 */
	@Nullable
	private Identification resolveId(final List<Identification> identifications) {
		if (identifications.isEmpty()) {
			return null;
		}

		final SourcePojo sourceObject = Assert.assertNotNull(allResources.get(identifications.get(0).resourceId));

		final Map<ID, List<Identification>> groupedIds = identifications.stream().collect(Collectors.groupingBy(Identification::getId));
		final List<Identification> yes = groupedIds.computeIfAbsent(ID.YES, id -> Collections.emptyList());
		final List<Identification> maybe = groupedIds.computeIfAbsent(ID.MAYBE, id -> Collections.emptyList());

		if (yes.size() == 1) {
			return yes.get(0);
		} else if (yes.size() > 1) {
			final List<Identification> exceptBinaryType = yes.stream().filter(identification -> identification.getTarget() != ResolveTarget.BINARY)
					.collect(Collectors.toList());
			if (exceptBinaryType.size() == 1) {
				return exceptBinaryType.get(0);
			}
			LOG.error(() -> String.format("File %s got multiple identifications %s", sourceObject.getName(), yes));
			return notIdentified(sourceObject, ResolveTarget.NONE);
		} else if ( ! maybe.isEmpty()) {
			/* Accumulate list of maybes to handle at the end of the detection process */
			addListMaybe(sourceObject, maybe);
			return notIdentified(sourceObject, ResolveTarget.NONE);
		} else {
			return notIdentified(sourceObject, ResolveTarget.NONE);
		}
	}

	/**
	 * Helper method to add one maybe to the accumulated map of maybe identifications.
	 *
	 * @param id The identification to add to the map of maybe identifications.
	 */
	private void addOneMaybe(final Identification id) {
		if (allResources.containsKey(id.resourceId)) {
			final String path = allResources.get(id.resourceId).getPath();
			maybeIdentifications.computeIfAbsent(path, key -> new ArrayList<>()).add(id);
		}
	}

	/**
	 * Helper method to add a list of maybe identifications to the accumulated map of maybe identifications.
	 *
	 * @param file The file to add to the map of maybe identifications.
	 * @param maybe The list of maybe identifications.
	 */
	private void addListMaybe(final SourcePojo sourceObject, final List<Identification> maybe) {
		maybeIdentifications.computeIfAbsent(sourceObject.getPath(), key -> new ArrayList<>()).addAll(maybe);
	}

	/**
	 * Handles any files that have been identified as maybe AND haven't been identified as a YES.
	 *
	 * During the detection processes maybes are accumulated into the maybeIdentifications map. If they haven't been identified
	 * as a YES then they are handled here.
	 */
	private void handleMaybes() {
		/* Collect only those files that haven't yet been identified */
		final Set<String> resourcePaths = unidentifiedResources.values().parallelStream()
				.map(SourcePojo::getPath)
				.collect(Collectors.toSet());
		final Map<String, List<Identification>> map = maybeIdentifications.entrySet().parallelStream()
				.filter(maybeEntry -> resourcePaths.contains(maybeEntry.getKey()))
				.collect(Collectors.toMap(Entry::getKey, Entry::getValue));

		map.entrySet().stream().forEach(maybeEntry -> {
			final List<Identification> maybeIdentifications = maybeEntry.getValue();
			final SourcePojo sourceObject = allResources.get(maybeIdentifications.get(0).resourceId);

			/* If there is only one maybe identification, assume that is correct and move the file */
			if (maybeIdentifications.size() == 1) {
				move(maybeIdentifications.get(0));
				unidentifiedResources.remove(maybeIdentifications.get(0).resourceId);
			} else {
				final List<Identification> maybeIdentificationsExceptBinary = maybeIdentifications.stream()
						.filter(identification -> identification.getTarget() != ResolveTarget.BINARY).collect(Collectors.toList());

				/* If there is only one non binary maybe identification, assume that is correct and move the file */
				if (maybeIdentificationsExceptBinary.size() == 1) {
					move(maybeIdentificationsExceptBinary.get(0));
					unidentifiedResources.remove(maybeIdentificationsExceptBinary.get(0).resourceId);
				} else {
					LOG.info(() -> String.format("Could not identify %s. But it might be one of %s", sourceObject, maybeIdentifications));
				}
			}
		});
		waitForUpdateTasks();
	}

	private void move(final Identification identification) {
		final Tuple2<String, MappingType> mappingResult = mapper.map(identification, allResources);
		final String target = mappingResult.e1;
		final SourcePojo sourceObject = allResources.get(identification.resourceId);
		if (sourceObject != null) {
			final Tuple2<String, Boolean> pathToSet = getPath(target, identification);
			if (pathToSet.e2 == Boolean.TRUE) {
				final String resourceName = sourceObject.getName();
				if (identification.getId() != Identification.ID.NO && LOG.isErrorEnabled()) {
					LOG.error("[{}] A resource was already identified and moved to {}", resourceName, sourceObject);
					LOG.error("[{}] Move: {} to: {} instead", resourceName, target, pathToSet.e1);
					LOG.error("[{}] Changing the discovery config might prevent this error.", resourceName);
				} else if (identification.getId() == Identification.ID.NO && LOG.isDebugEnabled()) {
					LOG.debug("[{}] A resource was already identified and moved to {}", resourceName, sourceObject);
					LOG.debug("[{}] Move: {} to: {} instead", resourceName, target, pathToSet.e1);
				}
				statistic.addDuplicate(identification);
			} else {
				if (LOG.isDebugEnabled()) {
					LOG.debug("move: {} to: {}", sourceObject, target);
				}
				if (identification.getId() != Identification.ID.NO) {
					if (mappingResult.e2 == MappingType.SOLID) {
						statistic.addDiscovered(identification);
					} else {
						statistic.addMaybe(identification);
					}
				}
			}

			final String name = identification.getNewName() != null ? identification.getNewName() : sourceObject.getName();
			updateTasks.add(updateExecutor.submit(() -> {
				try (final SpanInScope parentScope = tracer.withSpanInScope(tracer.currentSpan())) {
					sourceService.update(new SourcePojoPrototype().withId(sourceObject.identity())
						.setName(FilenameUtils.getBaseName(name))
						.setPath(FilenameUtils.separatorsToUnix(pathToSet.e1))
						.setTechnology(ResolveTargetHelper.toTechnology(identification.getTarget()))
						.setType(ResolveTargetHelper.toType(identification.getTarget())));
				}
			}));
		}
	}

	private Tuple2<String, Boolean> getPath(final String path, final Identification identification) {
		final String normalizedPath = FilenameUtils.separatorsToUnix(path);
		final boolean wasNotInSet = existingPaths.add(normalizedPath);
		if (wasNotInSet) {
			return new Tuple2<>(normalizedPath, Boolean.FALSE);
		} else {
			return new Tuple2<>(mapper.mapDuplicate(identification, allResources), Boolean.TRUE);
		}
	}

	private void setDetectionTypes(final Set<DetectionType> types) {
		allActiveDetectionTypes = types;
		detectionLanguagesPreparePhase = getDetectionLanguagesForPhase(DetectionPhase.PREPARE);
		detectionLanguagesMainPhase = getDetectionLanguagesForPhase(DetectionPhase.MAIN);
	}

	private void processIdentifications(final Map<Long, List<Identification>> identifications) {
		identifications.values().stream().forEach(nestedIdentifications -> {
			if ( ! nestedIdentifications.isEmpty()) {
				final Identification identification = resolveId(nestedIdentifications);
				if (identification != null) {
					if (identification.getId() == Identification.ID.MAYBE) {
						addOneMaybe(identification);
					} else if (identification.getId() == Identification.ID.YES) {
						move(identification);
						unidentifiedResources.remove(identification.resourceId);
						identifiedLanguagesMainTypeAndToken.add(identification.getTarget().getLanguage());
					}
				}
			}
		});
		waitForUpdateTasks();
	}

	private void waitForUpdateTasks() {
		updateTasks.forEach(task -> {
			try {
				task.get();
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
				LOG.error("Import task was interrupted", e);
			} catch (final ExecutionException e) {
				LOG.error("Import task threw an error", e);
			}
		});
		updateTasks.clear();
	}

	private void populateExistingDiscoverPath(final List<SourcePojo> existingSourceObject) {
		existingSourceObject
		.forEach(existingSourceObjectVal -> existingPaths.add(existingSourceObjectVal.getPath()));
	}
}
