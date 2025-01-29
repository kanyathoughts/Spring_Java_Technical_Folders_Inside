/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation.api;

import java.util.List;
import java.util.Map;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.core.annotation.impl.DatabaseAnnotationIdentifier;
import innowake.mining.data.core.annotation.impl.RuleAnnotationIdentifierStageTwo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Technology;

/**
 * Identify annotations based on the AST model.
 */
public interface AnnotationIdentifier {

	/**
	 * Returns the {@link AnnotationIdentifier} for a given type.
	 *
	 * @param type the type
	 * @return the {@link AnnotationIdentifier} for the given type
	 */
	public static AnnotationIdentifier getAnnotationIdentifier(final AnnotationType type) {
		switch (type) {
			case DATABASE:
				return new DatabaseAnnotationIdentifier();
			case RULE:
				return new RuleAnnotationIdentifierStageTwo();
			case DEAD_CODE:
			case EXCLUDE:
			default:
				throw new UnsupportedOperationException("Unsupported annotation type: " + type);
		}
	}

	/**
	 * Identifies annotations based on the given root node and {@code technology}.
	 *
	 * @param root the root node of the AST
	 * @param annotationCategoryMap Map of name of Category as String and ID of the category to be used for the Annotations
	 * @param technology The {@link Technology} of the module
	 * @param ddeAstNodeMap Map of dataDictionary related to AST
	 * @return a list of identified annotations, not {@code null}
	 */
	public List<AnnotationPojoTemplate> identify(final AstNodePojo root, final Map<String, Long> annotationCategoryMap, Technology technology,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> ddeAstNodeMap);
}
