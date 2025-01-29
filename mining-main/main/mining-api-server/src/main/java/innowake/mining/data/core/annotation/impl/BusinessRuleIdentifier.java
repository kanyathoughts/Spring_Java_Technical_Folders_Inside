/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import java.util.List;
import java.util.Map;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationCategory.RuleAnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Technology;

/**
 * New {@link AnnotationIdentifier} for business rules.
 * 
 * <p>The resulting annotations have the following attributes:</p>
 * <ul>
 * <li>Type: {@link AnnotationType#RULE}</li>
 * <li>Name: <code>Business Rule Candidate [System identified (V2)]</code></li>
 * </ul>
 */
public class BusinessRuleIdentifier implements AnnotationIdentifier {

	public static final String ANNOTATION_NAME = "Business Rule Candidate [System identified]";

	@Override
	public List<AnnotationPojoTemplate> identify(final AstNodePojo root, final Map<String, Long> annotationCategoryMap, final Technology technology,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		final BusinessRuleCollector collector;

		switch(technology) {
			case COBOL:
				collector = new CobolBusinessRuleCollector();
				break;
			case NATURAL:
				collector = new NaturalBusinessRuleCollector();
				break;
			case PL1:
				collector = new BusinessRuleCollector();
				break;
			default:
				throw new IllegalArgumentException("Technology not yet supported for Annotation Identification : " + technology);
		}

		return collector.collect(root, annotationCategoryMap.get(RuleAnnotationCategory.BUSINESS_RULE.getName()), astAndDdeMap);
	}

}
