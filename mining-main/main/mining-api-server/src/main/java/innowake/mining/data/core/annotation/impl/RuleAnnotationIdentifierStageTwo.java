/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationCategory.RuleAnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;

/**
 * New {@link AnnotationIdentifier} for business rules having business variables.
 * 
 * <p>The resulting annotations have the following attributes:</p>
 * <ul>
 * <li>Type: {@link AnnotationType#RULE}</li>
 * </ul>
 */
public class RuleAnnotationIdentifierStageTwo extends BusinessRuleIdentifier {

	@Override
	public List<AnnotationPojoTemplate> identify(final AstNodePojo root, final Map<String, Long> annotationCategoryMap, final Technology technology,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		final Set<ModuleLocation> excludedLocations = new HashSet<>();

		if (technology == Technology.COBOL) {
			final List<AnnotationPojoTemplate> annotations = new ArrayList<>();

			/* Add annotation candidates based on all the business variable references */
			annotations.addAll(new DataCenteredBusinessRuleCollector(excludedLocations, astAndDdeMap).collect(root,
					annotationCategoryMap.get(RuleAnnotationCategory.BUSINESS_RULE.getName())));

			/* Add Field computation rule annotation candidates */
			annotations.addAll(new FieldComputationRuleCollector(excludedLocations, astAndDdeMap).collect(root,
					annotationCategoryMap.get(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName())));

			/* Add Error processing rule annotation candidates */
			annotations.addAll(new ErrorProcessingRuleCollector(excludedLocations, astAndDdeMap).collect(root,
					annotationCategoryMap.get(RuleAnnotationCategory.ERROR_PROCESSING_RULE.getName())));

			/* Add Technical rule annotation candidates */
			annotations.addAll(new TechnicalRuleCollector(excludedLocations, astAndDdeMap).collect(root,
					annotationCategoryMap.get(RuleAnnotationCategory.TECHNICAL_RULE.getName())));

			/* Add Data Validation rule annotation candidates */
			annotations.addAll(new DataValidationRuleCollector(excludedLocations, astAndDdeMap).collect(root,
					annotationCategoryMap.get(RuleAnnotationCategory.VALIDATION_RULE.getName())));

			return annotations;
		}
		
		return super.identify(root, annotationCategoryMap, technology, astAndDdeMap);
	}
}
