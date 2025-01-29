/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationCategory.RuleAnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Database access layer for Annotation candidates.
 */
public class AnnotationCandidates {
	
	private static final Logger LOG = LoggerFactory.getLogger(AnnotationCandidates.class);
	
	private static final EntityId DEFAULT_PROJECT = EntityId.of(0L);
	
	/**
	 * Stores the given annotations as candidates, associated with the given module. 
	 *
	 * @param rootModuleId the ID of the root module the candidates are associated with
	 * @param annotations the annotations to store
	 * @param core Database access services
	 * @return the number of stored annotations
	 */
	public Long store(final EntityId rootModuleId, final List<AnnotationPojoTemplate> annotations, final MiningDataCoreService core) {
		long storeCount = 0;
		LOG.debug(() -> String.format("Storing %d database annotation candidates for the root module with ID %s", Integer.valueOf(annotations.size()), rootModuleId));
		
		for (final AnnotationPojoTemplate annotation : annotations) {
			if (checkModule(annotation, core)) {
				storeCandidate(annotation, core);
				storeCount++;
			}
		}
		return storeCount;
	}
	
	/**
	 * Obtains the AnnotationCategory in the system-project with the specified name and returns its ID.
	 * 
	 * @param core Database access services
	 * @param categoryName name of the AnnotationCategory to be obtained
	 * @return id of the Annotation Category in project 0 with the name specified
	 */
	public Long getDefaultAnnotationCategory(final MiningDataCoreService core, final String categoryName) {
		final List<Long> categoryIds = core.annotationService.findCategoryIds(q -> q.ofProject(DEFAULT_PROJECT).withName(categoryName));
		if ( ! categoryIds.isEmpty()) {
			return categoryIds.get(0);
		}

		/* The Project 0 AnnotationCategories are created by a migration and are expected to be in place */
		throw new IllegalStateException(String.format("AnnotationCategory %s doesn't exist.", categoryName));
	}
	
	private void storeCandidate(final AnnotationPojoTemplate annotation, final MiningDataCoreService core) {
		LOG.debug(() -> String.format("Storing for module %s the annotation %s", annotation.module.get(), annotation));
		
		final EntityId annoationId;
		try {
			annoationId = core.annotationService.create(annotation);
		} catch (final Exception e) {
			LOG.error(() -> String.format("Could not insert annotation %s for the module with ID %s: %s", annotation, annotation.module.get(), e.getMessage()), e);
			return;
		}
		
		if (annotation.dataDictionaryReferences.isDefined()) {
			for (final EntityId dde: annotation.dataDictionaryReferences.getNonNull()) {
				core.dataDictionaryService.linkAnnotations(dde, annoationId);
			}
		}
	}
	
	private boolean checkModule(final AnnotationPojoPrototype annotation, final MiningDataCoreService core) {
		final ModuleLocation location = annotation.location.getNonNull();
		final Integer offset = location.getOffset();
		final Integer length = location.getLength();
		if (isAnnotationExistingInDatabase(annotation.module.getNonNull(), annotation.type.getNonNull(), location, core)) {
			updateAnnotationExistingInDatabase(annotation.module.getNonNull(), annotation.type.getNonNull(), location, core, annotation);
			LOG.debug(() -> String.format(
					"An Annotation at the Module %s with the type %s, offset %d and length %d is already existing. Skipping storage of %s",
					annotation.module.get(),
					annotation.type.get(),
					offset,
					length,
					annotation));
			return false;
		}
		final int start = offset.intValue();
		final int end = start + length.intValue();
		final Optional<String> moduleSource = core.sourceService
				.findAnyContent(q -> q.withModule(annotation.module.getNonNull())).map(s -> s.getContent().toString());
		
		final String source;
		if (moduleSource.isPresent()) {
			source = moduleSource.get();
		} else {
			LOG.error(() -> String.format("No source content for Module %s available. Skipping adding Annotation.", annotation.module.get()));
			return false;
		}
		
		if (source.length() < end) {
			LOG.debug(() -> String.format(
					"The end index of the substring %d would exceed the length of the content %d. Skipping adding Annotation for the Module with ID %s.",
					Integer.valueOf(end), Integer.valueOf(source.length()), annotation.module.get()));
			return false;
		}
		
		annotation.setSourceAttachment(new BinaryString(source.substring(start, end)));
		
		LOG.debug(() -> String.format("The source for the Annotation in the Module with ID %s [offset=%d|length=%d] is %s",
				annotation.module.get(), offset, length, annotation.sourceAttachment.get()));
		return true;
	}
	
	private boolean isAnnotationExistingInDatabase(final EntityId moduleId, final AnnotationType annotationType, final ModuleLocation location,
			final MiningDataCoreService core) {
		final List<AnnotationPojo> existingAnnotations = core.annotationService.find(q -> q.ofModule(moduleId)
																							.withType(annotationType)
																							.withLocation(location));
		return ! existingAnnotations.isEmpty();
	}
	
	private void updateAnnotationExistingInDatabase(final EntityId moduleId, final AnnotationType annotationType, final ModuleLocation location,
			final MiningDataCoreService core, final AnnotationPojoPrototype annotation) {
		if ( ! annotation.englishTranslation.isDefined()) {
			return;
		}
		final List<AnnotationPojo> existingAnnotations = core.annotationService.find(q -> q.ofModule(moduleId).withType(annotationType).withLocation(location));
		final String createdTranslation = annotation.englishTranslation.isPresent()
				? annotation.englishTranslation.get()
				: "";
		for (final AnnotationPojo currentAnnotation : existingAnnotations) {
			final Optional<String> optionalCurrentEnglishTranslation = currentAnnotation.getEnglishTranslation();
			final String currentEnglishTranslation = optionalCurrentEnglishTranslation.isPresent() ? optionalCurrentEnglishTranslation.get()
					: "";
			if (createdTranslation != null && ! createdTranslation.equals(currentEnglishTranslation) ) {
				LOG.info(() -> String.format("Update English translation of the annotation with ID %S", currentAnnotation.getId()));
				core.annotationService
						.update(new AnnotationPojoPrototype().withId(currentAnnotation.identity()).setEnglishTranslation(annotation.englishTranslation.get()));
			}
		}
	}
	
	/**
	 * Fetches a Map of {@link RuleAnnotationCategory} and ID from {@link AnnotationCategory} table
	 *
	 * @param core Database access services
	 * @param annotationCategoryMap the {@linkplain Map} to store Annotation Categories
	 * @return Map of String of name of Category and Annotation Category ID
	 */
	public synchronized Map<String, Long> loadRuleAnnotationCategories(final MiningDataCoreService core, final Map<String, Long> annotationCategoryMap) {
		for (var category : core.annotationService.findCategories(q -> q.ofProject(DEFAULT_PROJECT)
																		.withTypes(Arrays.asList(AnnotationType.RULE, AnnotationType.DATABASE)))) {
			annotationCategoryMap.put(category.getName(), category.getId());
		}
		return annotationCategoryMap;
	}

	@SuppressWarnings("unused")
	@Nullable
	private static <T> T getListElementSafe(@Nullable final List<T> list, final int index) {
		return list == null || list.size() <= index ? null : list.get(index);
	}
}
