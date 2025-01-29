/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.csv;

import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.extensions.export.table.LineBuilderFactory;
import innowake.mining.extensions.export.table.TableLineBuilder;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Job that exports the results of functional block analysis as CSV.
 */
public class FunctionalBlockAnalysisCsvExporterJob extends MiningJob<FileSystemResult> {

	private static final String FUNCTIONAL_BLOCK_SEQUENCE = "Functional Block Sequence (Level ";
	private static final String FUNCTIONAL_BLOCK_NAME = "Functional Block Name (Level ";
	private static final String FUNCTIONAL_BLOCK_DESCRIPTION = "Functional Block Description (Level ";
	private static final String END_PARENTHESIS = ")";
	private static final String MODULE = "Module";
	private static final String ANNOTATION_ID = "Annotation ID";
	private static final String ANNOTATION_DESC = "Annotation Description";
	private static final String ANNOTATION_TYPE = "Annotation Type";
	private static final String ANNOTATION_CATEGORY = "Annotation Category";
	private static final String ANNOTATION_STATE = "Annotation State";
	private static final String ANNOTATION_CUSTOM_PROPERTY = "AnnotationCustomProperties";
	private static final String EMPTY_SPACE = " ";
	private static final String LEVEL_SEPARATOR = "&&&&&&&";
	public static final String NO_LEVEL_STRING = "(no level ";
	public static final String DOT_STRING = ".";
	public static final String NO_LEVEL = "NO_LEVEL";
	public static final String ANNOTATION_LAST_MODIFIED = "Last Modified By";

	private final Map<String, List<String>> parameters;

	/* the SecurityContext of the user that submitted the Job */
	private final SecurityContext securityContext;

	@Autowired
	private transient FunctionalBlockService functionalBlockService;

	@Autowired
	private transient AnnotationService annotationService;
	@Autowired
	private transient ProjectService projectService;

	@Autowired
	private transient CustomPropertiesService customPropertiesService;

	@Autowired
	private transient UserNameUtil userNameUtil;

	private final List<String> customPropertyNames = new ArrayList<>();
	private final Map<UUID, AnnotationPojo>  annotationMap = new HashMap<>();
	private final Map<UUID, FunctionalBlockPojo> mapOfUuidAndPojo = new HashMap<>();
	private final Map<UUID, String> sequenceMap = new HashMap<>();

	public FunctionalBlockAnalysisCsvExporterJob(final EntityId projectId, final Map<String, List<String>> parameters, final SecurityContext securityContext) {
		super(projectId);
		this.parameters = parameters;
		this.securityContext = securityContext;
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Functional Block Analysis CSV Data Export");
		final SecurityContext previousContext = SecurityContextHolder.getContext();
		SecurityContextHolder.setContext(securityContext);

		try {
			final List<FunctionalBlockPojo> rootBlocks = getRootBlocks();

			if ( ! rootBlocks.isEmpty()) {

				final List<UUID> rootUuid = rootBlocks.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList());

				final Map<UUID, List<FunctionalBlockPojo>> tree = functionalBlockService.findChildrenDeep(
						rootBlocks.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()), -1,
						q -> q.withTypes(List.of(FunctionalBlockType.FUNCTIONAL_GROUP, FunctionalBlockType.FUNCTIONAL_UNIT)));

				populateUuidToPojoMap(rootBlocks, tree);

				final int maxDepth = getDepth(tree);

				final List<UUID> functionalUnitUUIDs = getFunctionalUnitUids(tree);

				final Map<UUID, GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(functionalUnitUUIDs);

				final Map<Long, UUID> annotationIdMap = generatedFrom.entrySet().stream()
						.collect(Collectors.toMap(entry -> entry.getValue().getAnnotationId().get().getNid(), Map.Entry::getKey));

				final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).byNids(annotationIdMap.keySet().stream().collect(Collectors.toList())));
				for (final AnnotationPojo annotation : annotations) {
					annotationMap.put(annotationIdMap.get(annotation.getId()), annotation);
				}

				final List<String> rows = new ArrayList<>();
				int sequenceNumber = 1;
				for (final UUID uid : rootUuid) {
					final List<String> row = new ArrayList<>();
					processChildren(uid, new StringBuilder(), row, 0, maxDepth + 1, sequenceNumber, new StringBuilder());
					rows.addAll(row);
					sequenceNumber++;
				}

				final List<CustomPropertyMetadata> customMetaData = customPropertiesService.findPropertyDefinitions(q -> q.ofProject(projectId).withParent(ANNOTATION_CUSTOM_PROPERTY + projectService.getNid(projectId)));
				if (!customMetaData.isEmpty()) {
					customPropertyNames.addAll(customMetaData.stream().map(CustomPropertyMetadata::getLabel).sorted().collect(Collectors.toList()));
				}
				final List<List<String>> rowsAndColumns = new ArrayList<>();
				processRow(rows, sequenceMap, rowsAndColumns, mapOfUuidAndPojo, annotationMap);

				return runInternal(progressMonitor, maxDepth, rowsAndColumns);
			} else {
				throw new IllegalArgumentException("Functional blocks must exist to download the csv");
			}
		} catch (final Exception e) {
			return new Result<>(new Status(e));
		} finally {
			SecurityContextHolder.setContext(previousContext);
		}
	}

	private void populateUuidToPojoMap(final List<FunctionalBlockPojo> rootBlocks, final Map<UUID, List<FunctionalBlockPojo>> tree) {
		for (final FunctionalBlockPojo blockPojo : rootBlocks) {
			mapOfUuidAndPojo.put(blockPojo.getUid(), blockPojo);
		}
		
		for (final List<FunctionalBlockPojo> blockPojoList : tree.values()) {
			for (final FunctionalBlockPojo blockPojo : blockPojoList) {
				mapOfUuidAndPojo.put(blockPojo.getUid(), blockPojo);
			}
		}
	}

	private List<FunctionalBlockPojo> getRootBlocks() {
		final List<String> listOfModuleIds = parameters.get("moduleIds");
		final List<String> listOfTaxonomyIds = parameters.get("taxonomyIds");
		final List<String> listOfPeers = parameters.get("peerUids");
		final List<String> ddeNames = parameters.get("ddeNames");
		
		final List<EntityId> entityIds = listOfModuleIds == null || listOfModuleIds.isEmpty() ? Collections.emptyList()
				: listOfModuleIds.stream().map(EntityId::of).collect(Collectors.toList());
		final List<EntityId> taxonomyIds = listOfTaxonomyIds == null || listOfTaxonomyIds.isEmpty() ? Collections.emptyList()
				: listOfTaxonomyIds.stream().map(EntityId::of).collect(Collectors.toList());
		final List<UUID> uuids = listOfPeers == null || listOfPeers.isEmpty() ? Collections.emptyList()
				: listOfPeers.stream().map(UUID::fromString).collect(Collectors.toList());

		return functionalBlockService.find(q -> {
			q.ofProject(projectId).withType(FunctionalBlockType.FUNCTIONAL_GROUP)
					.notWithParent(p -> p.withType(FunctionalBlockType.FUNCTIONAL_GROUP));
			applyFilters(entityIds, taxonomyIds, uuids, ddeNames, q);
			q.sortName(SortDirection.ASCENDING);
		});
	}

	@SuppressWarnings("null")
	private void applyFilters(final List<EntityId> entityIds, final List<EntityId> taxonomyIds, final List<UUID> uuids, final List<String> ddeNames,
			final FunctionalBlockInquiryBuilder q) {
		if (entityIds != null && ! entityIds.isEmpty()) {
			q.withResolvedModuleParts(entityIds);
		}
		if (uuids != null && ! uuids.isEmpty()) {
			q.withPeer(peer -> peer.byUids(uuids));
		}
		if (taxonomyIds != null && ! taxonomyIds.isEmpty()) {
			q.withResolvedModulePartHavingTaxonomies(taxonomyIds);
		}
		if (ddeNames != null && ! ddeNames.isEmpty()) {
			q.withReferenceToDataDictionaryNames(ddeNames);
		}
	}

	private void processRow(final List<String> rows, final Map<UUID, String> sequenceAndBlockUUIDs, final List<List<String>> rowsAndColumns,
			final Map<UUID, FunctionalBlockPojo> mapOfUuidAndPojo, final Map<UUID, AnnotationPojo> annotationMap) {
		for (final String row: rows) {
			final String[] cells = row.split(LEVEL_SEPARATOR);
			final List<String> actualRow = new ArrayList<>();
			int depth = 1;
			for (final String cell : cells) {
				addDataForEachFunctionBlock(sequenceAndBlockUUIDs, mapOfUuidAndPojo, annotationMap, cell, actualRow, depth);
				depth++;
			}
			rowsAndColumns.add(actualRow);
		}
	}

	private void addDataForEachFunctionBlock(final Map<UUID, String> sequenceAndBlockUUIDs, final Map<UUID, FunctionalBlockPojo> mapOfUuidAndPojo,
			final Map<UUID, AnnotationPojo> annotationMap, final String cell, final List<String> actualRow, final int depth) {
		if ( ! cell.equals(NO_LEVEL)) {
			final UUID cellUUid = UUID.fromString(cell);
			final FunctionalBlockPojo block = mapOfUuidAndPojo.get(cellUUid);
			if (isFunctionalGroup(block)){
				actualRow.add(sequenceAndBlockUUIDs.get(cellUUid));
				actualRow.add(block.getName());
				actualRow.add(block.getDescription());
			} else {
				addAnnotationRowData(annotationMap, cellUUid, actualRow);
			}
		} else {
			for (int i = 0; i < 3; i++) {
				actualRow.add(NO_LEVEL_STRING + depth + END_PARENTHESIS);
			}
		}
	}

	private void addAnnotationRowData(final Map<UUID, AnnotationPojo> annotationMap, final UUID cellUUid, final List<String> actualRow) {
		final AnnotationPojo annotation = annotationMap.get(cellUUid);
		actualRow.add(annotation != null ? annotation.getModuleName() : EMPTY_SPACE);
		actualRow.add(annotation != null ? annotation.getId().toString() : EMPTY_SPACE);
		actualRow.add(annotation != null ? annotation.getName() : EMPTY_SPACE);
		actualRow.add(annotation != null ? annotation.getType().toString() : EMPTY_SPACE);
		actualRow.add(annotation != null ? annotation.getCategoryName().orElse(EMPTY_SPACE) : EMPTY_SPACE);
		actualRow.add(annotation != null ? annotation.getState().toString() : EMPTY_SPACE);
		actualRow.add(annotation != null ? annotation.getUpdatedByUserId().map(userNameUtil::getUserName).orElse("SYSTEM") : EMPTY_SPACE);
		if ( ! customPropertyNames.isEmpty() && annotation != null) {
			addCustomPropertiesValues(actualRow,  annotation.getCustomProperties());
		}
	}

	private void processChildren(final UUID uid, final StringBuilder currentPath, final List<String> row, int currentDepth, final int maxDepth,
			final int sequenceNumber, final StringBuilder sequencePath) {
		final FunctionalBlockPojo currentFunctionalBlock = mapOfUuidAndPojo.get(uid);
		if (currentFunctionalBlock == null){
			return;
		}
		if (isFunctionalUnit(currentFunctionalBlock)) {
			for( int i= currentDepth; i < maxDepth; i++){
				currentPath.append(NO_LEVEL).append(LEVEL_SEPARATOR);
			}
			sequencePath.append(DOT_STRING).append(sequenceNumber);
			currentPath.append(currentFunctionalBlock.getUid());
			row.add(currentPath.toString());
		} else if (isFunctionalGroup(currentFunctionalBlock)) {
			currentPath.append(currentFunctionalBlock.getUid()).append(LEVEL_SEPARATOR);
			currentDepth++;
			if(sequencePath.length() == 0) {
				sequencePath.append(sequenceNumber);
			} else {
				sequencePath.append(DOT_STRING).append(sequenceNumber);
			}
			int sequenceNumberForChildren = 1;
			for(final UUID childUid: currentFunctionalBlock.getChildren()){
				processChildren(childUid, new StringBuilder(currentPath), row, currentDepth, maxDepth, sequenceNumberForChildren, new StringBuilder(sequencePath));
				sequenceNumberForChildren++;
			}
		} else {
			return;
		}
		sequenceMap.put(uid, sequencePath.toString());
	}

	@SuppressWarnings("unchecked")
	private void addCustomPropertiesValues(final List<String> row, final CustomPropertiesMap cp) {
		final var existingProperties = cp.values().stream()
				.flatMap(o -> ((Map<String, Object>) o).entrySet().stream())
				.collect(Collectors.toMap(Entry<String, Object>::getKey, Entry<String, Object>::getValue));

		for (final String property : customPropertyNames) {
			final Optional<Object> res = existingProperties.entrySet().stream()
																.filter(e -> e.getKey().equalsIgnoreCase(property))
																.map(e -> e.getValue())
																.findAny();
			if (res.isPresent()) {
				final var propertyValues = res.get();
				if (propertyValues == null) {
					row.add(EMPTY_SPACE);
				} else if (propertyValues instanceof Collection) {
					final Collection<?> col = (Collection<?>) propertyValues;
					if (col.isEmpty()) {
						row.add(EMPTY_SPACE);
					} else {
						row.add(col.stream().map(t -> t == null ? "" : t.toString()).collect(Collectors.joining(",")));
					}
				} else {
					final var value = propertyValues.toString();
					row.add(value.isEmpty() ? EMPTY_SPACE : value);
				}
			} else {
				row.add(EMPTY_SPACE);
			}
		}
	}

	private List<UUID> getFunctionalUnitUids(final Map<UUID, List<FunctionalBlockPojo>> tree) {
		final List<UUID> functionalUnitUUIDs = new ArrayList<>();
		for (final Map.Entry<UUID, List<FunctionalBlockPojo>> entry : tree.entrySet()) {
			for (final FunctionalBlockPojo block : entry.getValue()) {
				if (isFunctionalUnit(block)) {
					functionalUnitUUIDs.add(block.getUid());
				}
			}
		}
		return functionalUnitUUIDs;
	}

	boolean isFunctionalUnit(final FunctionalBlockPojo block){
		return compareType(block, FunctionalBlockType.FUNCTIONAL_UNIT.toString());
	}

	boolean isFunctionalGroup(final FunctionalBlockPojo block){
		return compareType(block, FunctionalBlockType.FUNCTIONAL_GROUP.toString());
	}

	@SuppressWarnings("unchecked")
	private boolean compareType(final FunctionalBlockPojo block, final Object type) {
		final Map<String, Object> modifiableFlags = new HashMap<>(block.getFlags());
		final List<Object> types = (List<Object>) modifiableFlags.getOrDefault("TYPE", new ArrayList<>());
		return types.contains(type);
	}
	public int getDepth(final Map<UUID, List<FunctionalBlockPojo>> tree) {
		int  maxDepth = 0;
		for (final Map.Entry<UUID, List<FunctionalBlockPojo>> entry : tree.entrySet()) {
			for (final FunctionalBlockPojo functionalBlock : entry.getValue()) {
				final int currentDepth = (Integer) functionalBlock.getFlags().get("__DEPTH");
				if(maxDepth <= currentDepth){
					maxDepth = currentDepth;
				}
			}
		}
		return maxDepth;
	}

	private Result<FileSystemResult> runInternal(final ProgressMonitor progressMonitor, final int maxDepth, final List<List<String>> rows) {

		progressMonitor.setStepDescription("Exporting data for functional analysis");

		final List<String> columnHeaders = getColumnHeaders(maxDepth);

		if ( ! customPropertyNames.isEmpty()) {
			columnHeaders.addAll(customPropertyNames);
		}
		final LineBuilderFactory factory = new CSVLineBuilderFactory();

		try (final OutputStream out = createResultFile(); final TableLineBuilder lineBuilder = factory.getTableLineBuilder(out)) {
			lineBuilder.buildHeaderLine(columnHeaders);

			for (final List<String> columnsAndRow : rows) {
				lineBuilder.buildStandardLine(columnsAndRow);
			}

		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		final String fileName =
				String.format("functional_block_analysis_%s_%s.csv", projectService.getNid(projectId),
									new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()));
		
		progressMonitor.setStepDescription("Exporting data for functional analysis finished");
		return new Result<>(new FileSystemResult("text/csv", fileName));
	}

	private static List<String> getColumnHeaders(final int maxDepth) {
		final List<String> columnHeaders = new ArrayList<>();
		for (int i = 1; i <= maxDepth + 1; i++) {
			columnHeaders.add(FUNCTIONAL_BLOCK_SEQUENCE + i + END_PARENTHESIS);
			columnHeaders.add(FUNCTIONAL_BLOCK_NAME + i + END_PARENTHESIS);
			columnHeaders.add(FUNCTIONAL_BLOCK_DESCRIPTION + i + END_PARENTHESIS);
		}
		columnHeaders.add(MODULE);
		columnHeaders.add(ANNOTATION_ID);
		columnHeaders.add(ANNOTATION_DESC);
		columnHeaders.add(ANNOTATION_TYPE);
		columnHeaders.add(ANNOTATION_CATEGORY);
		columnHeaders.add(ANNOTATION_STATE);
		columnHeaders.add(ANNOTATION_LAST_MODIFIED);
		return columnHeaders;
	}

}
