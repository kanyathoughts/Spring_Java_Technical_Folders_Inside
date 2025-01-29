/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import com.google.common.collect.Lists;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationImportJobResult;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.CustomPropertyDataType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.WorkingState;

/**
 * Service to import annotations from a CSV file.
 */
@Service
public class AnnotationImportService {

	public static final String ANNOTATION_STATE = "State";
	public static final String ANNOTATION_TYPE = "Annotation Type";
	public static final String ANNOTATION_DESCRIPTION = "Annotation Description";
	public static final String ANNOTATION_ID = "Annotation Id";
	public static final String ANNOTATION_CATEGORY = "Category";
	private static final String IMPORT_ERROR_PATTERN = "Line %d: %s";
	private static final Set<String> TYPES_NOT_LINKED_TO_CATEGORIES = Set.of(AnnotationType.DEAD_CODE.name(), AnnotationType.EXCLUDE.name());
	private static final Map<Long, Map<String, CustomPropertyMetadata>> PROJECT_STRING_CUSTOM_PROPERTIES = new ConcurrentHashMap<>();
	private static final Logger LOG = LoggerFactory.getLogger(AnnotationImportService.class);

	private final CustomPropertiesService customPropertiesService;
	private final AnnotationService annotationService;
	private final ProjectService projectService;

	@Value("${mining.annotations.csvImportBatchSize: 1000}")
	private int importBatchSize;

	public AnnotationImportService(final AnnotationService annotationService, final ProjectService projectService, final CustomPropertiesService metamodelDao) {
		this.annotationService = annotationService;
		this.projectService = projectService;
		this.customPropertiesService = metamodelDao;
	}

	/**
	 * Imports annotations from a CSV file.
	 *
	 * @param monitor {@link ProgressMonitor} Tracks progress of a job
	 * @param projectId the project ID
	 * @param linesFromCsv the lines from CSV
	 * @param requestingUser the requesting user ID
	 * @return the import {@linkplain AnnotationImportJobResult result}
	 */
	public AnnotationImportJobResult importAnnotations(final Optional<ProgressMonitor> monitor, final EntityId projectId, final List<Map<String, String>> linesFromCsv,
			final String requestingUser) {
		final var annotationImportResult = new AnnotationImportJobResult(linesFromCsv.size());
		LOG.info("Importing annotations for project {}.", projectId);
		final Map<String, CustomPropertyMetadata> stringProps;
		final var projectNid = projectService.getNid(projectId);
		if (PROJECT_STRING_CUSTOM_PROPERTIES.containsKey(projectNid)) {
			stringProps = PROJECT_STRING_CUSTOM_PROPERTIES.get(projectNid);
		} else {
			stringProps = getCustomProperties(projectId);
			PROJECT_STRING_CUSTOM_PROPERTIES.put(projectNid, stringProps);
		}
		final var categories = annotationService.findCategories(q -> q.ofProjectWithDefault(projectId)).parallelStream().collect(Collectors.toMap(AnnotationCategory::getName, cat -> cat));
		final var cpHeaders = CollectionUtils.intersection(stringProps.keySet(), linesFromCsv.get(0).keySet());
		LOG.info("Only updating the fields {} and custom properties {}.",
				List.of(ANNOTATION_DESCRIPTION, ANNOTATION_TYPE, ANNOTATION_CATEGORY, ANNOTATION_STATE), cpHeaders);

		int batchSplit = 0;
		final var csvLineParts = Lists.partition(linesFromCsv, importBatchSize);
		for (final var part : csvLineParts) {
			final var annotationIds = part.parallelStream().map(line -> line.get(ANNOTATION_ID)).filter(Objects::nonNull).map(Long::parseLong)
					.collect(Collectors.toList());
			final var annotationsFromDb = Lists.partition(annotationIds, 1000).parallelStream().flatMap(ids -> annotationService.find(q -> q.ofProject(projectId).byNids(ids)).stream())
					.collect(Collectors.toMap(AnnotationPojo::getId, annotation -> annotation));
			for (int i = 0; i < part.size(); i++) {
				monitor.ifPresent(m -> m.worked(1));
				final var lineNumber = batchSplit * importBatchSize + i + 2;
				final var line = part.get(i);
				final AnnotationPojo annotation = getAnnotation(line, annotationsFromDb, annotationImportResult.getErrors(), lineNumber);
				if (annotation == null || ! validateAllFields(line, annotationImportResult.getErrors(), lineNumber, categories)) {
					LOG.warn("Skipping {}.", annotationImportResult.getErrors().getLast());
					continue;
				}
				updateFields(projectNid, annotation, line, categories, cpHeaders, stringProps, requestingUser);
				LOG.info("Updated annotation from line {} with ID {}.", lineNumber, annotation.getId());
			}
			batchSplit++;
		}
		LOG.info("Finished importing annotations for project {}. Updated {} annotations", projectId, linesFromCsv.size() - annotationImportResult.getErrors().size());
		return annotationImportResult;
	}

	private void updateFields(final Long projectId, final AnnotationPojo anno, final Map<String, String> line,
			final Map<String, AnnotationCategory> categories, final Collection<String> cpHeaders, final Map<String, CustomPropertyMetadata> stringProps,
			final String requestingUser) {
		
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.withId(anno.identity());

		annotation.setUpdatedByUserId(requestingUser);
		if (line.containsKey(ANNOTATION_DESCRIPTION)) {
			annotation.setName(line.get(ANNOTATION_DESCRIPTION));
		}
		if (line.containsKey(ANNOTATION_TYPE)) {
			final var type = line.get(ANNOTATION_TYPE).toUpperCase();
			annotation.setType(AnnotationType.valueOf(type));
		}
		if (line.containsKey(ANNOTATION_CATEGORY)) {
			final var category = line.get(ANNOTATION_CATEGORY);
			annotation.setCategoryId(categories.get(category).getId());
		}
		if (line.containsKey(ANNOTATION_STATE)) {
			final var state = line.get(ANNOTATION_STATE);
			annotation.setState(WorkingState.fromName(state));
		}
		if ( ! anno.getCustomProperties().isEmpty()) {
			annotation.setCustomProperties(anno.getCustomProperties());
		}
		handleCustomProperties(projectId, annotation, cpHeaders, stringProps, line);
		annotationService.update(annotation);
	}

	private boolean validateAllFields(final Map<String, String> line, final List<String> result, final int lineNumber,
			final Map<String, AnnotationCategory> categories) {
		final var typeToBeUpdated = line.containsKey(ANNOTATION_TYPE);
		final var type = StringUtils.trimToEmpty(line.get(ANNOTATION_TYPE)).toUpperCase();
		if (typeToBeUpdated && ! validType(type)) {
			result.add(String.format(IMPORT_ERROR_PATTERN, lineNumber, "Annotation Type " + type + " is invalid"));
			return false;
		}
		if (line.containsKey(ANNOTATION_CATEGORY)) {
			final var category = line.get(ANNOTATION_CATEGORY);
			if ( ! categories.containsKey(category)) {
				result.add(String.format(IMPORT_ERROR_PATTERN, lineNumber, "Category " + category + " not found"));
				return false;
			}

			if ( ! (validCategoryAndType(type, category, typeToBeUpdated) || TYPES_NOT_LINKED_TO_CATEGORIES.contains(type))) {
				result.add(String.format(IMPORT_ERROR_PATTERN, lineNumber, "Annotation Type " + type + " is invalid for category " + category));
				return false;
			}
		}
		final var state = StringUtils.trimToEmpty(line.get(ANNOTATION_STATE)).toUpperCase();
		if (line.containsKey(ANNOTATION_STATE) && ! validState(state)) {
			result.add(String.format(IMPORT_ERROR_PATTERN, lineNumber, "State " + state + " is invalid"));
			return false;
		}
		return true;
	}

	@SuppressWarnings("unchecked")
	private void handleCustomProperties(final Long projectId, final AnnotationPojoPrototype annotation, final Collection<String> cpHeaders,
			final Map<String, CustomPropertyMetadata> stringProps, final Map<String, String> line) {
		if ( ! cpHeaders.isEmpty()) {
			String defaultClassName = null;
			final Map<String, Object> customProperties;
			if (annotation.customProperties.isPresent()) {
				customProperties = annotation.customProperties.getNonNull();
			} else {
				customProperties = new HashMap<>();
				annotation.setCustomProperties(customProperties);
				defaultClassName = customPropertiesService.getDefaultClassName(projectId, "Annotation");
				customProperties.put(defaultClassName, new HashMap<>());
			}
		
			final var existingProperties = customProperties.values().stream()
															.flatMap(o -> ((Map<String, Object>) o).entrySet().stream())
															.collect(Collectors.toMap(Entry<String, Object>::getKey, e -> e));

			for (final var header : cpHeaders) {
				final CustomPropertyMetadata prop = stringProps.get(header);
				if (prop == null) {
					throw new IllegalStateException("CustomPropertyMetadata does not exists for: " + header);
				}

				final var value = line.get(header);
				final var entry = existingProperties.get(prop.getName());
				if (entry != null) {
					entry.setValue(value);
				} else if (StringUtils.isNotBlank(value)) {
					if (defaultClassName == null) {
						defaultClassName = customPropertiesService.getDefaultClassName(projectId, "Annotation");
					}
					((Map<String, Object>) customProperties.get(defaultClassName)).put(prop.getName(), value);
				}
			}

		}
	}

	private boolean validCategoryAndType(final String type, final String category, final boolean typeToBeUpdated) {
		if (typeToBeUpdated) {
			if (AnnotationCategory.RuleAnnotationCategory.fromName(category) != AnnotationCategory.RuleAnnotationCategory.UNKNOWN) {
				return AnnotationType.RULE.name().equals(type);
			} else if (AnnotationCategory.DatabaseAnnotationCategory.fromName(category) != AnnotationCategory.DatabaseAnnotationCategory.UNKNOWN) {
				return AnnotationType.DATABASE.name().equals(type);
			}
		}
		return true;
	}

	private boolean validType(final String type) {
		return Arrays.stream(AnnotationType.values()).anyMatch(t -> t.name().equals(type));
	}

	private boolean validState(final String state) {
		return Arrays.stream(WorkingState.values()).anyMatch(s -> s.name().equals(state));
	}

	@Nullable
	private AnnotationPojo getAnnotation(final Map<String, String> line, final Map<Long, AnnotationPojo> annotations, final List<String> result, final int lineNumber) {
		final var annotationId = line.get(ANNOTATION_ID);

		if (annotationId == null) {
			result.add(String.format(IMPORT_ERROR_PATTERN, lineNumber, "Annotation ID is required for a successful import"));
			return null;
		}

		final var annotation = annotations.get(Long.parseLong(annotationId));
		if (annotation == null) {
			result.add(String.format(IMPORT_ERROR_PATTERN, lineNumber, "Annotation with ID " + annotationId + " not found"));
			return null;
		}
		return annotation;
	}

	@EventListener
	public void customPropertiesModificationListener(final CustomPropertiesModifiedEvent event) {
		event.getProjectId().ifPresent(projectId -> {
			final Long projectNid = projectService.getNid(projectId);
			/* Only reload custom properties of the projects in use */
			if (PROJECT_STRING_CUSTOM_PROPERTIES.containsKey(projectNid)) {
				LOG.debug(() -> String.format("Custom properties modified for project %s. Reloading custom properties", projectId));
				PROJECT_STRING_CUSTOM_PROPERTIES.put(projectNid, getCustomProperties(projectId));
			}
		});
	}

	private Map<String, CustomPropertyMetadata> getCustomProperties(final EntityId projectId) {
		return customPropertiesService.findPropertyDefinitions(q -> q.withParentEntity(projectId, "Annotation")
																			.withDataType(CustomPropertyDataType.STRING)).stream()
				.collect(Collectors.toUnmodifiableMap(
						CustomPropertyMetadata::getLabel,
						prop -> prop
				));
	}
}
