/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.metadata;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;

import com.fasterxml.jackson.databind.ObjectMapper;
import groovy.lang.Tuple;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.lang.Nullable;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.ResultOrderTaskProcessor;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskProcessor;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.extensions.metadata.model.MetaDataBackup;
import innowake.mining.extensions.metadata.model.ModuleBackup;
import innowake.mining.server.functionalblocks.backup.FunctionalBlockBackupService;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Implementation creates a back up of {@linkplain ModulePojo modules} metadata such as
 * <ul>
 * <li>Module description
 * <li>Annotations
 * <li>Data Dictionary Entries
 * <li>Taxonomies
 * </ul>
 * It determines the modules that needs to be backed up, forks a back up task for each module and serializes the result of all the tasks to JSON file.
 */
public class MetaDataExportJob extends MiningJob<FileSystemResult> {

	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient TaxonomyService taxonomyService;
	@Autowired
	private transient AnnotationService annotationService;
	@Autowired
	private transient CustomPropertiesService customPropertiesService;
	@Autowired
	private transient ProjectService projectService;
	@Autowired
	private transient FunctionalBlockBackupService functionalBlockBackupService;
	@Autowired
	private transient ObjectMapper objectMapper;
	@Autowired
	private transient BuildProperties buildProperties;
	private final Map<String, List<String>> parameters;
	public static final String PARAM_MODULES_TO_BACKUP = "moduleIdsToBackup";
	public static final String COMPRESSED = "compressed";
	public static final String FORMAT = "format";
	public static final String FILE_IN_ZIP = "metaDataBackup.json";


	/**
	 * Initialize serializable fields.
	 *
	 * @param projectId the project id
	 * @param parameters having moduleIds with metaData to backup
	 */
	public MetaDataExportJob(final EntityId projectId, final Map<String, List<String>> parameters) {
		super(projectId);
		this.parameters = parameters;
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Export Annotations, DataDictionaryEntries, Taxonomies, Module Descriptions and Functional Blocks for Backup Purposes");
		final TaskProcessor<ModuleBackup> taskProcessor = new ResultOrderTaskProcessor<>(jobManager, assertNotNull(jobMonitor));

		/* Get all the modules with metadata */
		final List<EntityId> moduleIds = parameters.containsKey(PARAM_MODULES_TO_BACKUP) ? getModuleIdsToBackup() : moduleService.findModulesWithMetaData(projectId);
		final List<ModuleBackup> moduleBackups = new ArrayList<>(moduleIds.size());

		/* Task to retrieve all the metadata */
		progressMonitor.begin(moduleIds.size());
		final Iterator<EntityId> iterator = moduleIds.iterator();
		final TaskSource<ModuleBackup> taskSource = new TaskSource<ModuleBackup>() {

			@Override
			public boolean hasNextTask() {
				return iterator.hasNext();
			}

			@Override
			public Task<ModuleBackup> nextTask() {
				return new MetaDataExportModuleTask(progressMonitor, jobId, projectId, iterator.next());
			}
		};

		/* Process retrieved metadata back of each task */
		final ResultConsumer<ModuleBackup> resultConsumer = new ResultConsumer<ModuleBackup>(new ReportMessageExceptionHandler<>(assertNotNull(jobMonitor))) {

			@Override
			protected void handleResult(final String taskId, final Result<ModuleBackup> result) {
				moduleBackups.add(result.value);
				progressMonitor.worked(1);
			}
		};

		forkTasks(taskProcessor, taskSource, resultConsumer);

		/* Create back up object to be written to JSON file */
		final var backupDate = new Date();
		final var timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(backupDate);
		final var taxonomies = taxonomyService.find(q -> q.ofProject(projectId)); 
		final var annotationCategories = annotationService.findCategories(q -> q.ofProjectWithDefault(projectId));
		final var project = projectService.get(projectId);
		final var defaultTaxonomyCategory = taxonomyService.getCategory(Assert.assertNotNull(project.getDefaultTaxonomyCategoryId()));
		final var technicalTaxonomyCategory = taxonomyService.getCategory(Assert.assertNotNull(project.getTechnicalTaxonomyCategoryId()));
		final var defaultCategoryTaxonomyTypes = getTaxonomyTypes(taxonomies, defaultTaxonomyCategory);
		final var technicalCategoryTaxonomyTypes = getTaxonomyTypes(taxonomies, technicalTaxonomyCategory);
		final var functionalBlocks = functionalBlockBackupService.createFunctionalBlockBackup(q -> q.ofProject(projectId));

		final var backup = new MetaDataBackup();
		backup.setApiServerVersion(buildProperties.getVersion());
		backup.setBackupDate(backupDate);
		backup.setTaxonomies(taxonomies);
		backup.setAnnotationCategories(annotationCategories);
		backup.setModules(moduleBackups);
		backup.setDefaultTaxonomyCategory(defaultTaxonomyCategory);
		backup.setTechnicalTaxonomyCategory(technicalTaxonomyCategory);
		backup.setDefaultCategoryTaxonomyTypes(defaultCategoryTaxonomyTypes);
		backup.setTechnicalCategoryTaxonomyTypes(technicalCategoryTaxonomyTypes);
		backup.setFunctionalBlocks(functionalBlocks);

		/* Create a backup for Custom Properties of the Project */
		final var entities = customPropertiesService.getAssignedProperties(projectId);
		final Map<String, List<CustomPropertyMetadata>> entityCustomProperties = new HashMap<>();
		for (final var entity : entities.entrySet()) {
			for (var cpc : entity.getValue()) {
				entityCustomProperties.put(entity.getKey(), customPropertiesService.findPropertyDefinitions(q -> q.ofParent(cpc.getId())));
			}
		}
		backup.setEntityCustomProperties(entityCustomProperties);

		if (parameters.containsKey(FORMAT) && parameters.get(FORMAT).contains(COMPRESSED)) {
			/* Input is zipped */
			try (	final OutputStream out = new BufferedOutputStream(createResultFile());
					final ZipOutputStream zipOut = new ZipOutputStream(out, StandardCharsets.UTF_8)) {
				
					zipOut.putNextEntry(new ZipEntry(FILE_IN_ZIP));
					objectMapper.writeValue(zipOut, backup);
					return new Result<>(new FileSystemResult("application/zip", String.format("metaDataBackup_%s.zip", timestamp)));
			
			} catch (final IOException e) {
				return new Result<>(new Status(e));
			}
		} else {
			/* Write the back up data to a temp JSON file */
			try (final OutputStreamWriter writer = new OutputStreamWriter(createResultFile(), StandardCharsets.UTF_8)) {
				objectMapper.writeValue(writer, backup);
				return new Result<>(new FileSystemResult("application/json", String.format("metaDataBackup_%s.json", timestamp)));
			} catch (final IOException e) {
				return new Result<>(new Status(e));
			}
		}
	}

	@Override
	public String getJobName() {
		return "Metadata Export";
	}

	private Set<TaxonomyTypePojo> getTaxonomyTypes(final List<TaxonomyPojo> taxonomies, final TaxonomyCategoryPojo requiredTaxonomyCategory) {
		return Set.copyOf(taxonomies.stream()
				.map(TaxonomyPojo::getType)
				.filter(taxonomyType -> taxonomyType.getCategory().getName().equals(requiredTaxonomyCategory.getName()))
				.collect(Collectors.toSet()));
	}

	private List<EntityId> getModuleIdsToBackup() {
		return parameters.get(PARAM_MODULES_TO_BACKUP)
				.stream()
				.map(EntityId::of)
				.collect(Collectors.toList());
	}
}
