/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.service;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.shared.access.SortDirection;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import com.google.common.collect.Lists;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.error.TaxonomyImportException;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.util.ModuleLookup;
import innowake.mining.server.util.ModuleLookup.ModuleLookupParameters;
import innowake.mining.server.util.ModuleLookup.ModuleLookupResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.TaxonomyImportValidationResult;
import innowake.mining.shared.model.TaxonomyImportValidationResult.Marker;
import innowake.mining.shared.model.TaxonomyImportValidationResult.MarkerType;

/**
 * Service class to import a list of taxonomy assignments and apply them to modules.
 */
@Service
public class TaxonomyImportService implements Serializable {

	private static final String MODULE_NAME = "Module Name";
	private static final String MODULE_PATH = "Module Path";
	private static final String TECHNOLOGY = "Technology";
	private static final String TYPE = "Type";
	private static final List<String> NON_TAXONOMY_TYPE_COLUMNS = Collections.unmodifiableList(Arrays.asList(MODULE_NAME, MODULE_PATH, TECHNOLOGY, TYPE));
	private static final Logger LOG = LoggerFactory.getLogger(TaxonomyImportService.class);

	@Autowired
	private transient ModuleService moduleService;

	@Autowired
	private transient TaxonomyService taxonomyService;
	
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;
	
	@Value("${mining.taxonomies.fetchModulePartitionSize: 10000}")
	private int fetchModulePartitionSize;
	
	/**
	 * Validate the taxonomy assignments.
	 *
	 * @param projectId the project ID
	 * @param importLines the data from the uploaded CSV file 
	 * @param progressMonitor the progress monitor to report job progress
	 * @return the validation {@linkplain TaxonomyImportValidationResult result}
	 */
	public TaxonomyImportValidationResult validate(final EntityId projectId, final List<Map<String, String>> importLines, final ProgressMonitor progressMonitor) {
		LOG.info(() -> "Taxonomy import validation service start");
		progressMonitor.setJobDescription("Validating Taxonomy Assignments");
		progressMonitor.setStepDescription("Starting");
		
		/*
		 * Talked with Benni about this.
		 * This should just parse all the lines,
		 * clear all taxonomies for all related modules in project,
		 * and add all contained in the lines
		 */
		final List<String> lineHeader = new ArrayList<>(importLines.get(0).keySet());
		final List<String> taxonomyTypes = new ArrayList<>(CollectionUtils.removeAll(lineHeader, NON_TAXONOMY_TYPE_COLUMNS));
		final Set<String> taxonomyTypesFromDb = taxonomyService.find(q -> q.ofProject(projectId)).parallelStream().map(TaxonomyPojo::getType).map(TaxonomyTypePojo::getName).collect(Collectors.toSet());
		/* Validate header */
		final List<Marker> headerValidation = validateHeader(lineHeader, taxonomyTypes, taxonomyTypesFromDb);
		final boolean isInvalidHeader = headerValidation.stream().map(Marker::getMarkerType).anyMatch(MarkerType.ERROR::equals);
		if (isInvalidHeader) {
			return new TaxonomyImportValidationResult(MarkerType.ERROR, headerValidation);
		}
		final List<Marker> markers = new ArrayList<>(headerValidation);
		/* Fetch all the modules by module name and taxonomies - do not increase the partition size, it will degrade the performance */
		progressMonitor.setStepDescription("Fetching all Modules");
		final Map<String, List<ModulePojo>> fetchedModules = fetchModulesByName(projectId, importLines);
		LOG.info(() -> "Fetch all the taxonomy types associated with project id : " + projectId);
		final List<String> resolvedTaxonomyTypes = taxonomyTypes.parallelStream()
													.map(String::trim)
													.filter(taxonomyType -> StringUtils.isNotBlank(taxonomyType) &&
															taxonomyTypesFromDb.parallelStream().anyMatch(taxonomyType::equalsIgnoreCase))
													.collect(Collectors.toList());
		LOG.info(() -> "Fetch all the taxonomies associated with project id : " + projectId);
		final Map<String, List<String>> taxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeNames(resolvedTaxonomyTypes)).parallelStream()
			.collect(Collectors.groupingBy(taxonomy -> taxonomy.getType().getName(), Collectors.mapping(TaxonomyPojo::getName, Collectors.toList())));

		final int[] lineNumber = {2};
		/* Validate modules and taxonomies */
		LOG.info(() -> "Process each row of taxonomy import ");
		progressMonitor.setStepDescription("Validating");
		final Map<Long, Integer> resolvedModules = new HashMap<>();
		importLines.forEach(row -> {
			final ModuleLookupResult validatedModule = resolveAndValidateModule(lineHeader, row, fetchedModules, lineNumber[0]);
			@Nullable final ModulePojo resolvedModule = validatedModule.getModule();
			markers.addAll(validatedModule.getMarkers());
			/* Check for duplicate modules in the csv, if there is a duplicate module
			 * then the latest taxonomy assignment will replace the previous assignments */
			if (resolvedModule != null && resolvedModules.containsKey(resolvedModule.getId())) {
				final List<String> lineContent = row.values().parallelStream().collect(Collectors.toList());
				final String[] matchedModule = {resolvedModule.getName(), resolvedModule.getPath().orElse(null),
										resolvedModule.getTechnology().name(), resolvedModule.getType().name()};
				markers.add(new Marker(lineNumber[0], lineHeader, lineContent, MarkerType.WARNING,
						"Duplicate taxonomy assignment for module " + String.join(", ", matchedModule)
								+ " the assignment on this line would overwrite the previous assignment on line " 
								+ resolvedModules.get(resolvedModule.getId())));
			} else if (resolvedModule != null) {
				resolvedModules.put(resolvedModule.getId(), Integer.valueOf(lineNumber[0]));
			}
			markers.addAll(validateTaxonomies(row, lineHeader, taxonomyTypes, taxonomies, lineNumber[0]));
			lineNumber[0]++;
			progressMonitor.worked(1);
		});
		LOG.info(() -> "Validation complete");
		progressMonitor.setStepDescription("Completed");
		final Predicate<Marker> noMarker = marker -> ! marker.getMarkerType().equals(MarkerType.NONE);
		final List<Marker> markersData = markers.parallelStream().filter(noMarker).collect(Collectors.toList());
		final MarkerType overallMarker = getOverallMarker(markersData);
		return new TaxonomyImportValidationResult(overallMarker, markersData);
	}

	/**
	 * Import the list of taxonomy assignments.
	 *
	 * @param projectId the project ID
	 * @param importLines the data from the uploaded CSV file 
	 * @param progressMonitor the progress monitor to report job progress
	 * @throws TaxonomyImportException thrown if import fails due to invalid taxonomy data
	 */
	public void importTaxonomies(final EntityId projectId, final List<Map<String, String>> importLines,
			final ProgressMonitor progressMonitor) throws TaxonomyImportException {
		LOG.info(() -> "Taxonomy import service start");
		progressMonitor.setJobDescription("Importing Taxonomy Assignments");
		progressMonitor.setStepDescription("Starting");
		final Iterator<Map<String, String>> importLinesHorizontalMapIterator = importLines.iterator();
		if ( ! importLinesHorizontalMapIterator.hasNext()) {
			progressMonitor.setStepDescription("No lines to import.");
			return;
		}
		/* Process Taxonomy CSV Headers */
		final List<String> header = new ArrayList<>(importLines.get(0).keySet());
		if ( ! header.contains(MODULE_NAME)) {
			final String errorMessage = "The 'Module Name' column is missing. Please use our CSV template for importing taxonomies.";
			LOG.error(() -> errorMessage);
			throw new TaxonomyImportException(errorMessage);
		}
		final Collection<String> relevantKeys = CollectionUtils.removeAll(header, NON_TAXONOMY_TYPE_COLUMNS);

		progressMonitor.setStepDescription("Fetching all Modules");
		/* Replace the header taxonomy types with a matching TaxonomyTypePojo ignoring case from
		 * the fetchedTaxonomyTypes and also create TaxonomyTypePojo if it does not exist. */
		final List<TaxonomyTypePojo> resolvedTaxonomyTypes = resolveTaxonomyTypes(projectId, header);
		final List<String> taxonomyTypeNames = resolvedTaxonomyTypes.parallelStream()
				.map(TaxonomyTypePojo::getName)
				.collect(Collectors.toList());

		final Map<String, TaxonomyTypePojo> taxonomyTypeMappedToHeader = mapTaxonomyTypeHeaders(resolvedTaxonomyTypes, header);

		LOG.info(() -> "Fetch all taxonomies by type");
		final Map<String, Map<String, TaxonomyPojo>> allTaxonomiesByType = taxonomyService.find(q -> q.ofProject(projectId).withTypeNames(taxonomyTypeNames))
				.stream()
				.collect(Collectors.groupingBy(k -> k.getType().getName(),
						CaseInsensitiveMap::new,
						Collectors.mapping(Function.identity(),
								Collectors.toMap(TaxonomyPojo::getName, Function.identity(),
										(taxonomyPojo, taxonomyPojo2) -> taxonomyPojo/* could throw an exception here? */, CaseInsensitiveMap::new))));


		LOG.info(() -> "Fetch modules by module name");
		final Map<String, List<ModulePojo>> fetchedModulesByName = fetchModulesByName(projectId, importLines);
				
		/* Process Taxonomy CSV Body. Look up for each module entry in the csv and populate in a map. */
		LOG.info(() -> "Process each row of taxonomy import");

		final Map<EntityId, Tuple2<Set<EntityId>, List<TaxonomyPojoPrototype>>> moduleToTaxonomyIdsAndNewProtos = new HashMap<>();
		while (importLinesHorizontalMapIterator.hasNext()) {
			final Map<String, String> line = importLinesHorizontalMapIterator.next();
			if (header.size() != line.size()) {
				final var errorMessage = "Header and data column mismatch.";
				LOG.error(() -> errorMessage);
				throw new TaxonomyImportException(errorMessage);
			}

			/* Attempt to find the Module and throw exception if no module or multiple modules are found. */
			final ModulePojo resolvedModule = resolveModuleForImport(header, line, fetchedModulesByName);
			final Set<EntityId> taxonomyIds = new HashSet<>();
			final List<TaxonomyPojoPrototype> newTaxonomies = new ArrayList<>();
			for (final String headerTaxonomyType : relevantKeys) {
				final String[] values = Stream.of(line.get(headerTaxonomyType).split(","))
						.map(String::trim)
						.filter(s -> ! s.isBlank())
						.toArray(String[]::new);
				final TaxonomyTypePojo taxonomyType = taxonomyTypeMappedToHeader.get(headerTaxonomyType);
				final Map<String, TaxonomyPojo> nameToTaxonomyOfCurrentType = allTaxonomiesByType.getOrDefault(taxonomyType.getName(), new HashMap<>());

				//instead of creating them, just filter a map containing the names?
				for (final String name : values) {
					final TaxonomyPojo existingTaxonomy = nameToTaxonomyOfCurrentType.get(name);
					if (existingTaxonomy == null) {
						final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
							.setProject(projectId)
							.setType(taxonomyType.getId())
							.setName(name);
						newTaxonomies.add(taxonomy);
					} else {
						taxonomyIds.add(existingTaxonomy.identity());
					}
				}
			}
			moduleToTaxonomyIdsAndNewProtos.put(resolvedModule.identity(), new Tuple2<>(taxonomyIds, newTaxonomies));
			progressMonitor.worked(1);
		}

		//Create the Taxonomies and Relationships only after parsing all,
		//in case a parse error occurred above, none will be imported
		//same behavior as with the Legacy Orient App
		createAllTaxonomiesAndFillIdLists(moduleToTaxonomyIdsAndNewProtos.values());
		//delete the Taxonomy Links for all modules
		taxonomyService.deleteModuleLinks(q -> q.ofModules(moduleToTaxonomyIdsAndNewProtos.keySet()));
		//create the new links between the new taxonomies and the module
		for (final Map.Entry<EntityId, Tuple2<Set<EntityId>, List<TaxonomyPojoPrototype>>> entry : moduleToTaxonomyIdsAndNewProtos.entrySet()) {
			final EntityId id = entry.getKey();
			final Set<EntityId> taxonomyIds = entry.getValue().e1;
			taxonomyService.createModuleLinks(id, taxonomyIds);
		}
		progressMonitor.setStepDescription("Completed");
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
	}

	private void createAllTaxonomiesAndFillIdLists(final Collection<Tuple2<Set<EntityId>, List<TaxonomyPojoPrototype>>> moduleToTaxonomyIdsAndNewProtos) {
		final Map<String, EntityId> createdProtosWithEntityId = new HashMap<>();
		//create all new Taxonomies
		for (final Tuple2<Set<EntityId>, List<TaxonomyPojoPrototype>> entry : moduleToTaxonomyIdsAndNewProtos) {
			final Set<EntityId> taxonomyIds = entry.e1;
			final List<TaxonomyPojoPrototype> newTaxonomyProtos = entry.e2;
			final List<EntityId> previouslyCreatedIds = new ArrayList<>();

			final var iterator = newTaxonomyProtos.iterator();
			while (iterator.hasNext()) {
				final var proto = iterator.next();
				final var previousId = createdProtosWithEntityId.get(getTaxonomyNameTypeProjectKey(proto));
				if (previousId != null) {
					previouslyCreatedIds.add(previousId);
					//filtering for already existing protos
					iterator.remove();
				}
			}
			//create the not existing taxonomies
			final List<EntityId> newTaxonomyIds = taxonomyService.create(newTaxonomyProtos);
			//fill the map
			for (int i = 0; i < newTaxonomyProtos.size(); i++) {
				final var protoKey = getTaxonomyNameTypeProjectKey(newTaxonomyProtos.get(i));
				createdProtosWithEntityId.put(protoKey, newTaxonomyIds.get(i));
			}
			//add all created and previously created to the lists, to later link on a per module basis
			taxonomyIds.addAll(newTaxonomyIds);
			taxonomyIds.addAll(previouslyCreatedIds);
		}
	}

	private String getTaxonomyNameTypeProjectKey(final TaxonomyPojoPrototype proto) {
		return proto.name.get() + '_' + proto.project.get() + '_' + proto.type.get();
	}

	private Map<String, List<ModulePojo>> fetchModulesByName(final EntityId projectId, final List<Map<String, String>> importLines) {
		return Lists.partition(
				importLines.parallelStream()
				.map(data -> data.get(MODULE_NAME))
				.distinct()
				.collect(Collectors.toList()), fetchModulePartitionSize)
					.parallelStream()
					.map(names -> moduleService.findModules(b -> b.ofProject(projectId).withNames(names, false)))
					.flatMap(Collection::parallelStream)
					/* We need to distinct here because ModuleDao.findByNames() fetches module case insensitive.
					 * This leads to duplicate modules when names exist in different cases and get fetched in multiple partitioned invocations. */
					.distinct()
					.collect(Collectors.groupingBy(ModulePojo::getName));
	}

	//resolves all taxonomy types used in the header
	private List<TaxonomyTypePojo> resolveTaxonomyTypes(final EntityId projectId, final List<String> headerLine) {
		/* Fetch the taxonomy types from the database. */
		LOG.info(() -> "Resolve taxonomy types");
		final List<TaxonomyTypePojo> allProjectTaxonomyTypes = taxonomyService.findTypes(q -> q.ofProject(projectId).sortName(SortDirection.ASCENDING));
		final Set<String> distinctTaxonomyNames = headerLine.stream()
				.filter(StringUtils::isNotBlank)
				.filter(header -> ! NON_TAXONOMY_TYPE_COLUMNS.contains(header))
				.collect(Collectors.toCollection(() -> new TreeSet<>(String.CASE_INSENSITIVE_ORDER)));
		
		return distinctTaxonomyNames.stream()
				.map(taxonomyHeader -> allProjectTaxonomyTypes.parallelStream()
						.filter(fetchedTaxonomyType -> fetchedTaxonomyType.getName().equalsIgnoreCase(taxonomyHeader))
						.findAny()
						.orElseGet(() -> {
							final UUID id = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName(taxonomyHeader).setProject(projectId));
							return taxonomyService.getType(id);
						}))
				.collect(Collectors.toList());
	}

	private ModulePojo resolveModuleForImport(final List<String> headerLine, final Map<String, String> contentLine, final Map<String, List<ModulePojo>> fetchedModules)
			throws TaxonomyImportException {
		final var moduleAndMarkersPair = resolveAndValidateModule(headerLine, contentLine, fetchedModules, -1);
		final Optional<Marker> errorMarker = moduleAndMarkersPair.getMarkers().parallelStream()
																				.filter(marker -> marker.getMarkerType() == MarkerType.ERROR)
																				.findAny();
		if (errorMarker.isPresent()) {
			final String errorMessage = errorMarker.get().getMarkerText();
			LOG.error(() -> errorMessage);
			throw new TaxonomyImportException(errorMessage);
		}

		return Objects.requireNonNull(moduleAndMarkersPair.getModule());
	}

	/**
	 * Validates the header row. <br/>
	 * If the <code>Module Name</code> is not present, then an <b>ERROR</b> marker is set. <br/>
	 * If optional headers <code>Module Path</code> or <code>Technology</code> or <code>Type</code> is not present or no taxonomy types are present
	 * , a <b>WARNING</b> marker is added. <br/>
	 * If new taxonomy types are present, then <b>INFORMATION</b> marker is returned. <br/>
	 *
	 * @param header the reader row
	 * @return all the {@linkplain Marker markers} found during header validation
	 */
	private List<Marker> validateHeader(final List<String> header, final List<String> taxonomyTypeHeaders,
			final Set<String> taxonomyTypesFromDB) {
		final List<String> missingNonTaxonomyColumns = new ArrayList<>(CollectionUtils.removeAll(NON_TAXONOMY_TYPE_COLUMNS, header));
		final List<Marker> markers = new ArrayList<>();

		/* Validate if 'Module Name' or other optional headers are present. */
		if (missingNonTaxonomyColumns.contains(MODULE_NAME)) {
			markers.add(new Marker(1, header, Collections.emptyList(), MarkerType.ERROR, 
					"The 'Module Name' column is missing. Please use our CSV template for importing taxonomies."));
		}
		if ( ! missingNonTaxonomyColumns.isEmpty() && ( ! missingNonTaxonomyColumns.contains(MODULE_NAME) || missingNonTaxonomyColumns.size() > 1)) {
			markers.add(new Marker(1, header, Collections.emptyList(), MarkerType.WARNING, "The columns " + String.join(", ", missingNonTaxonomyColumns) 
					+ " are missing. Please use our CSV template for importing taxonomies. Assignments may not be accurate."));
		}
		/* Validate if new taxonomies are present. */
		if (taxonomyTypeHeaders.isEmpty()) {
			markers.add(new Marker(1, header, Collections.emptyList(), MarkerType.WARNING,
					"No taxonomies are present. Please use our CSV template for importing taxonomies"));
		}
		/* Check for duplicate taxonomy types */
		final Set<String> distinctTaxonomyTypes = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
		final Set<String> duplicates = taxonomyTypeHeaders.stream()
										.filter(taxonomyType -> ! distinctTaxonomyTypes.add(taxonomyType))
										.collect(Collectors.toSet());
		if ( ! duplicates.isEmpty()) {
			markers.add(new Marker(1, header, Collections.emptyList(), MarkerType.ERROR, "Duplicate taxonomy column " + String.join(", ", duplicates)));
		}
		/* Check for new taxonomy type */
		if ( ! taxonomyTypeHeaders.isEmpty() && ! missingNonTaxonomyColumns.contains(MODULE_NAME)) {
			final Set<String> newTaxonomyTypes = taxonomyTypeHeaders.parallelStream()
												.map(String::trim)
												.filter(taxonomyType -> StringUtils.isNotBlank(taxonomyType) &&
														taxonomyTypesFromDB.parallelStream().noneMatch(taxonomyType::equalsIgnoreCase))
												.collect(Collectors.toSet());

			if ( ! newTaxonomyTypes.isEmpty()) {
				markers.add(new Marker(1, header, Collections.emptyList(), MarkerType.INFORMATION, 
						"Creates new Taxonomy Type " + String.join(", ", newTaxonomyTypes) + " during import"));
			}
		}
		return markers;
	}
	
	/**
	 * Method to find the module using available path, technology and type information.<br/>
	 * Returns <b>ERROR</b> marker if no matching module is found or ambiguous modules are found.<br/>
	 * Returns <b>WARNING</b> marker if module is found after multiple lookups or if the module matches a previously found module. <br/>
	 * Returns <b>NONE</b> marker if the module is found successfully. 
	 *
	 * @param lineHeader the header
	 * @param importLine the row content
	 * @param fetchedModules modules fetched from DB
	 * @param lineNumber the index of line number required to add markers while validation, can be set to -1 for import 
	 * @return all the {@linkplain Marker  markers} and matched {@linkplain Module module} found during module look up
	 */
	private ModuleLookupResult resolveAndValidateModule(final List<String> lineHeader, final Map<String, String> importLine, 
			final Map<String, List<ModulePojo>> fetchedModules, final int lineNumber) {
		final String moduleName = importLine.get(MODULE_NAME);
		@Nullable final List<ModulePojo> modules = fetchedModules.get(moduleName);
		final List<String> lineContent = importLine.values().parallelStream().collect(Collectors.toList());
		if (modules == null || modules.isEmpty()) {
			final List<Marker> markers = new ArrayList<>();
			markers.add(new Marker(lineNumber, lineHeader, lineContent, MarkerType.ERROR, "Module not found for " + String.join(", ", lineContent)));
			return new ModuleLookupResult(null , markers, false);
		}
		@Nullable final String path = importLine.get(MODULE_PATH);
		@Nullable final String technology = importLine.get(TECHNOLOGY);
		@Nullable final String type = importLine.get(TYPE);
		final ModuleLookupParameters moduleLookupParameters = new ModuleLookupParameters(modules, path, technology, type);
		return ModuleLookup.lookUpModule(lineHeader, lineNumber, lineContent, moduleLookupParameters);
	}

	/**
	 * Validate the taxonomy values.
	 *
	 * @param rowMap the row data
	 * @param lineHeader the header
	 * @param taxonomyTypes all the taxonomy types
	 * @param taxonomies the taxonomies fetched from db
	 * @param lineNumber the line number
	 * @return list of markers
	 */
	private List<Marker> validateTaxonomies(final Map<String, String> rowMap,  final List<String> lineHeader, final List<String> taxonomyTypes,
			final Map<String, List<String>> taxonomies, final int lineNumber) {
		final List<String> lineContent = rowMap.values().parallelStream().collect(Collectors.toList());
		final List<Marker> markers = new ArrayList<>();
		taxonomyTypes.forEach(taxonomyType -> {
			final List<String> taxonomyAssignments = Arrays.asList(rowMap.get(taxonomyType).split(","));
			final Set<String> taxonomyValues = new TreeSet<>();
			final Optional<Marker> duplicateMarker = checkForDuplicates(lineHeader, lineNumber, lineContent, taxonomyAssignments, taxonomyValues);
            duplicateMarker.ifPresent(markers::add);
			taxonomyValues.forEach(taxonomy -> {
				if (StringUtils.isNotBlank(taxonomy) && taxonomies.containsKey(taxonomyType)
						&& taxonomies.get(taxonomyType).parallelStream().noneMatch(taxonomy::equalsIgnoreCase)) {
					markers.add(new Marker(lineNumber, lineHeader, lineContent, MarkerType.INFORMATION, "New taxonomy value " + taxonomy
							+ " will be added during import"));
					final List<String> resolvedTaxonomies = taxonomies.getOrDefault(taxonomyType, new ArrayList<>(List.of(taxonomy)));
					taxonomies.put(taxonomyType, resolvedTaxonomies);
				}
			});
		});
		return markers;
	}

	private Optional<Marker> checkForDuplicates(final List<String> lineHeader, final int lineNumber, final List<String> lineContent,
			final List<String> taxonomyAssignments, final Set<String> taxonomyValues) {
		/* Using parallel stream causes null pointer exception. */
		final Set<String> duplicates = taxonomyAssignments.stream()
				.map(String::trim)
				.filter(taxonomy -> ! taxonomyValues.add(taxonomy.toUpperCase()))
				.collect(Collectors.toSet());
		if ( ! duplicates.isEmpty()) {
			return Optional.of(new Marker(lineNumber, lineHeader, lineContent, MarkerType.WARNING, "Duplicate taxonomy value " + String.join(",", duplicates)));
		}
		return Optional.empty();
	}

	/**
	 * Returns the overall marker after validating the taxonomy assignments.
	 *
	 * @param markers all the markers found during validation
	 * @return the {@linkplain MarkerType}
	 */
	private MarkerType getOverallMarker(final List<Marker> markers) {
		final Set<MarkerType> markerTypes = markers.parallelStream().map(Marker::getMarkerType).collect(Collectors.toSet());
		if (markerTypes.contains(MarkerType.ERROR)) {
			return MarkerType.ERROR;
		} else if (markerTypes.contains(MarkerType.WARNING)) {
			return MarkerType.WARNING;
		} else if (markerTypes.contains(MarkerType.INFORMATION)) {
			return MarkerType.INFORMATION;
		}
		return MarkerType.NONE;
	}
	
	private Map<String, TaxonomyTypePojo> mapTaxonomyTypeHeaders(final List<TaxonomyTypePojo> resolvedTaxonomyTypes, final List<String> headerLine) {
		final Map<String, TaxonomyTypePojo> returnMap = new HashMap<>();
		for (final String headerTaxonomyType : CollectionUtils.removeAll(headerLine,
				NON_TAXONOMY_TYPE_COLUMNS)) {
			final TaxonomyTypePojo taxonomyType = resolvedTaxonomyTypes.parallelStream()
					.filter(resolvedTaxonomyType -> resolvedTaxonomyType.getName()
							.equalsIgnoreCase(headerTaxonomyType))
					.findAny().orElseThrow(IllegalStateException::new);
			returnMap.put(headerTaxonomyType, taxonomyType);
		}
		return returnMap;
	}
}
