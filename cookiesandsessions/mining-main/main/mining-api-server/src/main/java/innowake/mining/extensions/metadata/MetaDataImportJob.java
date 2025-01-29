/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.metadata;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.parboiled.common.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.extensions.metadata.model.MetaDataBackup;
import innowake.mining.extensions.metadata.model.ModuleBackup;
import innowake.mining.server.functionalblocks.backup.FunctionalBlockBackupService;
import innowake.mining.server.functionalblocks.backup.model.FunctionalBlockBackup;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Implementation restores the metadata from {@linkplain MetaDataBackup back up}. Metadata consists of
 * <ul>
 * <li>Module description
 * <li>Annotations
 * <li>Data Dictionary Entries
 * <li>Taxonomies
 * </ul>
 * It creates missing taxonomies, taxonomy types and annotation categories.
 */
@JsonIgnoreProperties(ignoreUnknown = true) 
public class MetaDataImportJob extends MiningJob<String> {

	@Autowired
	private transient ObjectMapper objectMapper;
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient TaxonomyService taxonomyService;
	@Autowired
	private transient AnnotationService annotationService;
	@Autowired
	private transient DataDictionaryService dataDictionaryService;
	@Autowired
	private transient CustomPropertiesService customPropertiesService;
	@Autowired
	private transient ProjectService projectService;
	@Autowired
	private transient FunctionalBlockService functionalBlockService;
	@Autowired
	private transient FunctionalBlockBackupService functionalBlockBackupService;

	private static final Logger LOG = LoggerFactory.getLogger(MetaDataImportJob.class);

	private final File backupDataFile;
	

	/**
	 * {@link MetaDataImportJob} constructor.
	 *
	 * @param backupDataFile the back up data file
	 * @param projectId the project Id
	 */
	public MetaDataImportJob(final File backupDataFile, final EntityId projectId) {
		super(projectId);
		this.backupDataFile = backupDataFile;
	}

	@Override
	protected Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Import Annotations, DataDictionaryEntries, Taxonomies, Module Descriptions and Functional Blocks from a Backup");
		LOG.debug(() -> "Executing meta data back up on project  " + projectId);
		try {
			final String backupData = FileUtils.readAllText(backupDataFile);
			if ( ! backupDataFile.delete()) {
				LOG.warn("Temp file " + backupDataFile.getPath() + " could not be deleted.");
			} 
			final MetaDataBackup metaDataBackup = objectMapper.readValue(backupData, MetaDataBackup.class);

			/* Custom Properties restored in DB as per the metadata back up */
			restoreCustomProperties(projectId, metaDataBackup.getEntityCustomProperties());

			/* Annotation categories restored in Db as per metadata back up */
			final Map<String, Long> restoredAnnotationCategories = restoreAnnotationCategoriesFromMetadataBackup(metaDataBackup.getAnnotationCategories());
			/* Taxonomies restored in Db as per metadata back up */
			final Map<String, EntityId> restoredTaxonomies = restoreTaxonomiesFromMetadataBackup(metaDataBackup);

			/* Retrieve modules associated with the project */
			final List<ModuleBackup> moduleBackups = assertNotNull(metaDataBackup.getModules(), "No modules found to backup");
			LOG.info(() -> "Fetch all the modules associated with project id : " + projectId);

			/* the id of each Annotation is stored as part of the backup but during restore, the Annotation actually gets a new id assigned
			 * this map maps from the id in the backup (key) to the new id after restore (value)
			 *
			 * Currently, this is required to restore the "GeneratedFrom" information of functional blocks. This can be removed when Annotations
			 * have stable UUIDs. */
			final Map<Long, EntityId> restoredAnnotationIdMap = new HashMap<>();
			final Map<UUID, EntityId> restoredDDEIdMap = new HashMap<>();

			final int moduleUpdatedCount = restoreModuleMetadataData(restoredAnnotationCategories, restoredTaxonomies, restoredAnnotationIdMap, moduleBackups
					, restoredDDEIdMap);

			final List<FunctionalBlockBackup> functionalBlocks = metaDataBackup.getFunctionalBlocks();
			/* Restore the functional blocks */
			if (functionalBlocks != null && ! functionalBlocks.isEmpty()) {
				LOG.info(() -> "Removing existing Functional Blocks in project id : " + projectId);
				functionalBlockService.deleteAllOfProject(projectId);
				LOG.info(() -> "Restore Functional Blocks in project id : " + projectId);
				functionalBlockBackupService.restoreFunctionalBlockBackup(projectId, functionalBlocks, restoredAnnotationIdMap);
			}
			return new Result<>("MetaDataImportJob: Restored metadata for " + moduleUpdatedCount + " modules");
		} catch (final JsonProcessingException e) {
			LOG.error("Parsing JSON for importing metadata with project id :" + projectId + " has Error. ", e);
			throw new IllegalStateException("Parsing JSON for importing metadata with project id :" + projectId + " has Error. ", e);
		}
	}

	private void restoreCustomProperties(final EntityId projectId,
			@Nullable final Map<String, List<CustomPropertyMetadata>> entityCustomProperties) {
		Optional.ofNullable(entityCustomProperties).ifPresent(properties -> properties.forEach((key, value) -> value.forEach(property ->
			customPropertiesService.defineProperty(projectId, key, property.getName(), property))));
	}

	@Override
	public String getJobName() {
		return "Metadata Import";
	}

	private int restoreModuleMetadataData(final Map<String, Long> restoredAnnotationCategories, final Map<String, EntityId> restoredTaxonomies,
			final Map<Long, EntityId> restoredAnnotationIdMap, final List<ModuleBackup> moduleBackups, final Map<UUID, EntityId> restoredDDEIdMap) {

		final AtomicInteger warnings = new AtomicInteger();
		moduleBackups.parallelStream().forEach(moduleBackup -> {
			try {
				final var resolvedModulePojo = resolveBackedupModule(moduleBackup);
				LOG.debug(() -> "Resolve back up module");
				if (resolvedModulePojo == null) {
					LOG.warn(() -> "Module " + moduleBackup.getName() + " could not be resolved.");
					warnings.incrementAndGet();
				} else {
					boolean metaDataRestored = false;
					LOG.debug(() -> "Restore module meta data");

					final var resolvedModule = new ModulePojoPrototype().withId(resolvedModulePojo.identity());
					final String description = moduleBackup.getDescription();
					if (description != null) {
						resolvedModule.setDescription(description);
						metaDataRestored = true;
					}
					final Map<String, Object> backedUpCustomProperties = moduleBackup.getCustomProperties();
					if (backedUpCustomProperties != null && ! backedUpCustomProperties.isEmpty()) {
						resolvedModule.setCustomProperties(resolveBackedUpCustomProperties(projectService.getNid(projectId), "Module", backedUpCustomProperties.entrySet()));
						metaDataRestored = true;
					}

					metaDataRestored = restoreAnnotations(restoredAnnotationCategories, restoredAnnotationIdMap, moduleBackup.getAnnotations(), resolvedModulePojo.identity()) || metaDataRestored;
					metaDataRestored = restoreDataDictionaries(moduleBackup.getDataDictionaryEntries(), resolvedModulePojo.identity(), restoredDDEIdMap) || metaDataRestored;
					metaDataRestored = restoreTaxonomyModuleLinks(restoredTaxonomies, moduleBackup, resolvedModulePojo) || metaDataRestored;

					final byte[] backedUpHash = moduleBackup.getContentHash();
					if (moduleBackup.isRequiresReview() || (metaDataRestored && (backedUpHash == null
							|| ! Arrays.equals(backedUpHash, resolvedModulePojo.getContentHash().map(BinaryValue::get).orElse(null))))) {
						resolvedModule.setRequiresReview(true);
					}

					moduleService.update(resolvedModule);
				}
			} catch (final Exception e) {
				LOG.error("Error while restoring module meta data for: " + moduleBackup.getName(), e);
				warnings.incrementAndGet();
			}
		});

		moduleBackups.parallelStream().forEach(moduleBackup -> {
			try {
				restoreDDEAnnotationLinks(moduleBackup, restoredAnnotationIdMap, restoredDDEIdMap);
			} catch (final Exception e) {
				LOG.error("Error while restoring DDE to annotation link for module: " + moduleBackup.getName(), e);
				warnings.incrementAndGet();
			}
		});

		return moduleBackups.size() - warnings.get();
	}

	private void restoreDDEAnnotationLinks(final ModuleBackup moduleBackup, final Map<Long, EntityId> restoredAnnotationIdMap,
			final Map<UUID, EntityId> restoredDDEIdMap) {
		final List<AnnotationPojo> annotations = moduleBackup.getAnnotations();
		if (annotations != null) {
			for (final var annotation : annotations) {
				final EntityId annotationId = restoredAnnotationIdMap.get(annotation.identity().getNid());
				if (annotationId != null) {
					for (final var oldDdeId : annotation.getDataDictionaryEntries()) {
						final var newDdeId = restoredDDEIdMap.get(oldDdeId);
						if (newDdeId != null) {
							dataDictionaryService.linkAnnotations(newDdeId, annotationId);
						} else {
							LOG.info("Skipping linkage of data dictionary entry with annotation. DDE wasn't restored: " + oldDdeId + ". Annotation: " + annotation.toString());
						}
					}
				} else {
					LOG.info("Skipping linkage of annotation to data dictionary entries. Annotation wasn't restored: " + annotation.toString());
				}
			}
		}
	}

	private boolean restoreTaxonomyModuleLinks(final Map<String, EntityId> restoreTaxonomies, final ModuleBackup moduleBackup, final ModulePojo resolvedModulePojo) {
		final List<TaxonomyPojo> backedUpTaxonomies = moduleBackup.getTaxonomies();
		if (backedUpTaxonomies != null && ! backedUpTaxonomies.isEmpty()) {
			LOG.debug(() -> "Creating Taxonomy links for" + resolvedModulePojo);
			final Set<EntityId> taxonomiesToLinkModuleTo = new HashSet<>(backedUpTaxonomies.size());
			final List<EntityId> ids = taxonomyService.findIds(q -> q.ofModule(resolvedModulePojo.identity()));
			final var existingTaxonomyIds = ids.isEmpty() ? Collections.emptySet() : new HashSet<>(ids);
			for (final TaxonomyPojo backedupTaxonomy : backedUpTaxonomies) {
				final EntityId dbTaxonomyId = restoreTaxonomies.get(getTaxonomyAndTypeName(backedupTaxonomy));
				if (dbTaxonomyId != null && (existingTaxonomyIds.isEmpty() || ! existingTaxonomyIds.contains(dbTaxonomyId))) {
					taxonomiesToLinkModuleTo.add(dbTaxonomyId);
				}
			}

			if ( ! taxonomiesToLinkModuleTo.isEmpty()) {
				taxonomyService.createModuleLinks(resolvedModulePojo.identity(), taxonomiesToLinkModuleTo);
				return true;
			}
		}

		return false;
	}

	/**
	 * Matches the backup module with the database.
	 *
	 * @param moduleBackup the {@link ModuleBackup}
	 * @return the matching module from the database
	 */
	@Nullable
	private ModulePojo resolveBackedupModule(final ModuleBackup moduleBackup) {
		ModulePojo moduleFromDb = null;
		if (moduleBackup.getLinkHash() != null) {
			moduleFromDb = moduleService.findAnyModule(q -> q.ofProject(projectId)
					.withLinkHash(assertNotNull(moduleBackup.getLinkHash())))
					.orElse(null);
		}
		if (moduleFromDb == null) {
			final List<ModulePojo> modulesWithNameTechnologyType =
					moduleService.findModules(q -> q.ofProject(projectId)
							.withName(moduleBackup.getName())
							.withTechnology(moduleBackup.getTechnology())
							.withType(moduleBackup.getType()));
			if (modulesWithNameTechnologyType.size() == 1) {
				moduleFromDb = modulesWithNameTechnologyType.get(0);
			} else {
				LOG.error(() -> "Module with name: " + moduleBackup.getName() + ", technology: " + moduleBackup.getTechnology().name() + " and type:"
						+ moduleBackup.getType().name() + " could not be found.");
			}
		}
		return moduleFromDb;
	}

	/**
	 * Restores annotation categories from meta data back up to database. If the annotation category name exits, it retrieves the records from the DB,
	 * else persists the data.
	 *
	 * @param backedupAnnotationCategories list of {@link AnnotationCategory}
	 * @return Map of {@linkplain AnnotationCategory annotation categories} from database
	 */
	private Map<String, Long> restoreAnnotationCategoriesFromMetadataBackup(@Nullable final List<AnnotationCategory> backedupAnnotationCategories) {
		final Map<String, Long> restoredAnnotationCategories;
		if (backedupAnnotationCategories != null) {
			/* Restore annotation categories */
			LOG.debug(() -> "Restore annotation categories associated with projectId " + projectId);
			/* annotation types are looked up case insensitive */
			restoredAnnotationCategories = new CaseInsensitiveMap<>(backedupAnnotationCategories.size());
			for (final AnnotationCategory category : annotationService.findCategories(q -> q.ofProjectWithDefault(projectId))) {
				restoredAnnotationCategories.put(category.getName(), category.getId());
			}

			for (final AnnotationCategory category : backedupAnnotationCategories) {
				if ( ! restoredAnnotationCategories.containsKey(category.getName())) {
					restoredAnnotationCategories.put(category.getName(), 
							annotationService.createCategory(projectId, category.getName(), CollectionUtils.emptyIfNull(category.getTypes())));
				}
			}
		} else {
			LOG.warn("No annotation categories found in backup file for projectId " + projectId);
			restoredAnnotationCategories = Collections.emptyMap();
		}
		return restoredAnnotationCategories;
	}

	/**
	 * Restores taxonomies from the back up to database. If a matching taxonomy exits, it retrieves the record from the DB,
	 * else saves the taxonomy.
	 *
	 * @param metadataBackup the backed up metadata
	 */
	private Map<String, EntityId> restoreTaxonomiesFromMetadataBackup(final MetaDataBackup metadataBackup) {
		final List<TaxonomyPojo> backedupTaxonomies = metadataBackup.getTaxonomies();
		final var defaultCategoryTaxonomyTypes = metadataBackup.getDefaultCategoryTaxonomyTypes();
		final var technicalCategoryTaxonomyTypes = metadataBackup.getTechnicalCategoryTaxonomyTypes();
		final TaxonomyCategoryPojo defaultTaxonomyCategory = metadataBackup.getDefaultTaxonomyCategory();
		final TaxonomyCategoryPojo technicalTaxonomyCategory =  metadataBackup.getTechnicalTaxonomyCategory();

		if (backedupTaxonomies != null && ! backedupTaxonomies.isEmpty()) {
			/* restore taxonomy categories */
			final ProjectPojo project = projectService.get(projectId);
			final Long technicalCategoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype()
					.setProject(projectId)
					.setId(assertNotNull(project.getTechnicalTaxonomyCategoryId()))
					.setName(technicalTaxonomyCategory.getName()));
			final Long defaultCategoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype()
					.setProject(projectId)
					.setId(assertNotNull(project.getDefaultTaxonomyCategoryId()))
					.setName(defaultTaxonomyCategory.getName()));

			/* Restore required taxonomy types */
			LOG.debug(() -> "Restore taxonomy types associated with projectId " + projectId);
			Map<String, TaxonomyTypePojo> taxonomyTypes = fetchTaxonomyTypes();
			if (defaultCategoryTaxonomyTypes != null) {
				restoreTaxonomyTypes(defaultCategoryTaxonomyTypes, defaultCategoryId, defaultTaxonomyCategory, taxonomyTypes);
			}
			if (technicalCategoryTaxonomyTypes != null) {
				restoreTaxonomyTypes(technicalCategoryTaxonomyTypes, technicalCategoryId, technicalTaxonomyCategory, taxonomyTypes);
			}

			taxonomyTypes = fetchTaxonomyTypes();
			final Map<String, EntityId> taxonomyWithTypeNames = new HashMap<>();
			for (final TaxonomyPojo taxonomy : taxonomyService.find(q -> q.ofProject(projectId))) {
				taxonomyWithTypeNames.put(getTaxonomyAndTypeName(taxonomy), taxonomy.identity());
			}

			/* Restore taxonomies */
			LOG.debug(() -> "Restore taxonomies associated with projectId " + projectId);
			for (final TaxonomyPojo backedupTaxonomy : backedupTaxonomies) {
				final String taxonomyWithTypeName = getTaxonomyAndTypeName(backedupTaxonomy);
				if ( ! taxonomyWithTypeNames.containsKey(taxonomyWithTypeName)) {
					final TaxonomyTypePojo type = taxonomyTypes.get(backedupTaxonomy.getType().getName());
					if (type == null) {
						throw new IllegalStateException("Could not find imported taxonomy type with name: " + backedupTaxonomy.getType().getName());
					}

					final TaxonomyPojoPrototype taxonomy = backedupTaxonomy.convertToPrototype();
					taxonomy.nid.unset();
					taxonomy.setType(type.getId());
					/* can not use setProject() here because it calls EntityId.merge(), however, we do not want to merge, we want to override the project */
					taxonomy.project.set(projectId);
					if ( ! backedupTaxonomy.getCustomProperties().isEmpty()) {
						taxonomy.setCustomProperties(resolveBackedUpCustomProperties(projectService.getNid(projectId), "Taxonomy", backedupTaxonomy.getCustomProperties().entrySet()));
					}

					taxonomyWithTypeNames.put(taxonomyWithTypeName, taxonomyService.create(taxonomy));
				}
			}

			return taxonomyWithTypeNames;
		}

		LOG.warn("No taxomies found in backup file for projectId " + projectId);
		return Collections.emptyMap();
	}

	private static String getTaxonomyAndTypeName(final TaxonomyPojo taxonomy) {
		/* taxonomy names and taxonomy type names are looked up case insensitive */
		return (taxonomy.getName() + "_" + taxonomy.getType().getName()).toLowerCase();
	}

	private Map<String, TaxonomyTypePojo> fetchTaxonomyTypes() {
		/* taxonomy types are looked up case insensitive */
		final Map<String, TaxonomyTypePojo> taxonomyTypes = new CaseInsensitiveMap<>();
		for (final TaxonomyTypePojo type : taxonomyService.findTypes(q -> q.ofProject(projectId))) {
			taxonomyTypes.put(type.getName(), type);
		}
		return taxonomyTypes;
	}

	private void restoreTaxonomyTypes(final Set<TaxonomyTypePojo> backedupTaxonomyTypes, final Long newCategoryId, final TaxonomyCategoryPojo taxonomyCategory,
			final Map<String, TaxonomyTypePojo> taxonomyTypesFromDb) {

		for (final TaxonomyTypePojo backedupTaxonomyType : backedupTaxonomyTypes) {
			final TaxonomyTypePojo taxonomyType = taxonomyTypesFromDb.get(backedupTaxonomyType.getName());
			if (taxonomyType != null) {
				final TaxonomyCategoryPojo category = taxonomyType.getCategory();
				if ( ! newCategoryId.equals(category.getId()) || ! taxonomyCategory.getName().equals(category.getName())) {
					taxonomyService.updateType(new TaxonomyTypePojoPrototype()
							.setId(taxonomyType.getId())
							.setCategoryId(newCategoryId)
							.setProject(projectId));
				}
			} else {
				taxonomyService.createType(new TaxonomyTypePojoPrototype()
						.setCategoryId(newCategoryId)
						.setName(backedupTaxonomyType.getName())
						.setProject(projectId));
			}
		}
	}

	/**
	 * Restores the {@link DataDictionaryPojo} from the module back up.
	 *
	 * @param dataDictionariesBackup
	 * 		the list of backed up {@linkplain DataDictionaryPojo data dictionaries}
	 * @param resolvedModule
	 * 		the {@linkplain Module module} fetched from database
	 * @param restoredDDEIdMap map of DataDictionary ids:
	 * 	  key is the id of the DataDictionaries as stored in the backup, value is the id of the DataDictionary after restore
	 * @return {@code true} if anything was restored, {@code false} otherwise
	 */
	private boolean restoreDataDictionaries(@Nullable final List<DataDictionaryPojo> dataDictionariesBackup, final EntityId resolvedModule,
			final Map<UUID, EntityId> restoredDDEIdMap) {
		if (dataDictionariesBackup != null) {
			LOG.debug(() -> "Restore data dictionaries associated with module id " + resolvedModule);
			dataDictionaryService.delete(q -> q.ofModule(resolvedModule));
			try {
				for (final DataDictionaryPojo backedUpDataDictionary : dataDictionariesBackup) {
					final DataDictionaryPojoPrototype dde = backedUpDataDictionary.convertToPrototype()
							.setModule(resolvedModule);
					dde.nid.unset();
					dde.uid.unset();
					final DataDictionaryPojo ddEPojo = dataDictionaryService.create(dde);
					restoredDDEIdMap.put(backedUpDataDictionary.identity().getUid(), ddEPojo.identity());
				}
				return true;
			} catch (final ConstraintViolationException ex) {
				LOG.warn(() -> "DataDictionary with given ModuleLocation value already exists.");
			}
		}
		return false;
	}

	/**
	 * Restores the annotations from the module back up and creates HasAnnotation edge between the module and data dictionary
	 * in database.
	 *
	 * @param restoredAnnotationCategories the Map of the {@linkplain AnnotationCategory annotation categories} from database with id as keys and category name as values
	 * @param restoredAnnotationIdMap map of Annotation ids:
	 * key is the id of the Annotations as stored in the backup, value is the id of the Annotation after restore
	 * @param annotations the list of backed up annotations
	 * @param resolvedModule the id of the resolved module
	 * @return {@code true} if anything was restored, {@code false} otherwise
	 */
	private boolean restoreAnnotations(final Map<String, Long> restoredAnnotationCategories, final Map<Long, EntityId> restoredAnnotationIdMap,
			@Nullable final List<AnnotationPojo> annotations, final EntityId resolvedModule) {
		if (annotations != null) {
			LOG.debug(() -> "Restore annotations associated with module id " + resolvedModule);
			annotationService.delete(q -> q.ofModule(resolvedModule));
			for (final AnnotationPojo annotationBackup : annotations) {
				final AnnotationPojoPrototype annotationBackupProto = annotationBackup.convertToPrototype()
						.setModule(resolvedModule);
				annotationBackupProto.nid.unset();
				annotationBackupProto.uid.unset();
				annotationBackup.getCategoryName().ifPresent(name -> {
					final Long categoryId = restoredAnnotationCategories.get(name);
					if (categoryId != null) {
						annotationBackupProto.setCategoryId(categoryId);
					}
				});

				if ( ! annotationBackup.getCustomProperties().isEmpty()) {
					annotationBackupProto.setCustomProperties(resolveBackedUpCustomProperties(projectService.getNid(projectId), "Annotation",
							annotationBackup.getCustomProperties().entrySet()));
				}

				final EntityId newAnnotationId = annotationService.create(annotationBackupProto);
				restoredAnnotationIdMap.put(annotationBackup.identity().getNid(), newAnnotationId);
			}
			return true;
		}
		return false;
	}

	private NestedMap resolveBackedUpCustomProperties(final Long projectId, final String entity, final Set<Entry<String, Object>> backedUpCustomProperties) {
		final var classCustomProperties = new NestedMap();
		final String className = customPropertiesService.getDefaultClassName(projectId, entity);
		for (final Map.Entry<String, Object> entry : backedUpCustomProperties) {
			classCustomProperties.set(className, entry.getValue());
		}
		return classCustomProperties;
	}
}
