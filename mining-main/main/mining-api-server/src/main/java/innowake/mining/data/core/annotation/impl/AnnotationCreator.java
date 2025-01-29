/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Creates Annotations based on AST nodes.
 */
public class AnnotationCreator {

	private AnnotationCreator() {}

	/**
	 * Create an annotation based on an AST node.
	 *
	 * @param node the node the annotation is based on
	 * @param name the name of the created annotation
	 * @param type the type of the created annotation
	 * @param categoryId Id of the annotationCategory of the created annotation
	 * @param englishTranslation the translation of the annotation
	 * @param businessDataDictionaries Set of {@linkplain DataDictionaryPojo} referencing Business variable.
	 * @return an annotation based on the given node
	 */
	public static AnnotationPojoTemplate create(final AstNodePojo node, final String name, final AnnotationType type, @Nullable final Long categoryId,
			@Nullable final String englishTranslation, final Set<DataDictionaryPojo> businessDataDictionaries) {
		return create(node, name, type, categoryId, node.getLocation(), englishTranslation, businessDataDictionaries);
	}

	/**
	 * Create an annotation based on an AST node.
	 *
	 * @param node the node the annotation is based on
	 * @param name the name of the created annotation
	 * @param type the type of the created annotation
	 * @param categoryId Id of the annotationCategory of the created annotation
	 * @return an annotation based on the given node
	 */
	public static AnnotationPojoTemplate create(final AstNodePojo node, final String name, final AnnotationType type, @Nullable final Long categoryId) {
		return create(node, name, type, categoryId, node.getLocation(), null, Collections.emptySet());
	}
	
	/**
	 * Create an annotation based on an AST node.
	 *
	 * @param node the node the annotation is based on
	 * @param name the name of the created annotation
	 * @param type the type of the created annotation
	 * @param categoryId Id of the annotationCategory of the created annotation
	 * @param nodeLocation AST node location in a module
	 * @param englishTranslation the translation of the annotation
	 * @param businessDataDictionaries Set of {@linkplain DataDictionaryPojo} referencing Business variable.
	 * @return an annotation based on the given node
	 */
	public static AnnotationPojoTemplate create(final AstNodePojo node, final String name, final AnnotationType type, @Nullable final Long categoryId,
			final AstNodeLocation nodeLocation, @Nullable final String englishTranslation,
			final Set<DataDictionaryPojo> businessDataDictionaries) {
		final AnnotationPojoTemplate annotation = new AnnotationPojoTemplate();
		annotation
			.setModule(getModuleId(node))
			.setLocation(new ModuleLocation(nodeLocation.getRetracedOffset().orElseThrow(), nodeLocation.getRetracedLength().orElseThrow()))
			.setState(WorkingState.CANDIDATE)
			.setType(type)
			.setName(name)
			.setCategoryId(categoryId)
			.setEnglishTranslation((englishTranslation == null || englishTranslation.isEmpty()) ? null : englishTranslation);
		if (! businessDataDictionaries.isEmpty()) {
			annotation.dataDictionaryReferences.set(businessDataDictionaries.stream().map(DataDictionaryPojo::identity).collect(Collectors.toList()));
		}
		return annotation;
	}
	
	private static EntityId getModuleId(final AstNodePojo node) {
		return node.getIncludedModule().orElseGet(node::getModule);
	}
	
}
