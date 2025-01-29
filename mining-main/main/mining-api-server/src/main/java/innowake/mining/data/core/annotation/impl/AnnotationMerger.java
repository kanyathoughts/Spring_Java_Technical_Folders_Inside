/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import java.util.List;
import java.util.stream.Collectors;

import graphql.com.google.common.base.Objects;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Merges annotations based on their source code locations.
 * 
 * Nested annotations are removed by the merging process if they are part of the same file.
 * If the nesting happens in an included Module, the merging will ignore the particular annotation.
 */
public class AnnotationMerger {
	
	private AnnotationMerger() {}

	/**
	 * Merges the given annotations based on their location.
	 * 
	 * If an annotation is located within another annotation, based on the source code location, this annotation
	 * will not be part of the resulting list. The input list is not modified.
	 * 
	 * Assumptions: 
	 * <ul>
	 * <li>Every annotation within the given list has a module location.
	 * <li>The list does not contain null values
	 *
	 * @param annotations the annotations to merge
	 * @return the list of annotations, which are not located within other annotations
	 */
	public static List<AnnotationPojoTemplate> merge(final List<AnnotationPojoTemplate> annotations) {
		return annotations.stream().filter(annotation -> ! isContainedWithin(annotation, annotations)).collect(Collectors.toList());
	}
	
	private static boolean isContainedWithin(final AnnotationPojoTemplate annotation, final List<AnnotationPojoTemplate> annotations) {
		final ModuleLocation location = getLocation(annotation);
		
		for (final AnnotationPojoPrototype currentAnnotation : annotations) {
			if (annotationShouldNotBeConsidered(annotation, currentAnnotation)) {
				continue;
			}
			if (location.isWithin(getLocation(currentAnnotation))) {
				return true;
			}
		}
		return false;
	}
	
	private static ModuleLocation getLocation(final AnnotationPojoPrototype annotation) {
		return Assert.assertNotNull(annotation.location.orElse(null), String.format("Missing module location for %s", annotation)); 
	}
	
	private static boolean annotationShouldNotBeConsidered(final AnnotationPojoPrototype annotation, final AnnotationPojoPrototype otherAnnotation) {
		return annotation == otherAnnotation || ! Objects.equal(annotation.module.get(), otherAnnotation.module.get());
	}
}
