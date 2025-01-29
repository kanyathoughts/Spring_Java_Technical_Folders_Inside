/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import static innowake.mining.data.core.api.AstNodeUtils.BRANCH_STATEMENT;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeCollectingTraverser;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.ModuleLocation;


/**
 * Collector to collect AST nodes having Annotation Rule related to Data Validation
 */
class DataValidationRuleCollector extends AnnotationRuleCollector {
	
	private static final String ANNOTATION_NAME = "Data Validation Rule Candidate [System identified]";
	
	public DataValidationRuleCollector(final Set<ModuleLocation> excludedLocations,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		super(excludedLocations, astAndDdeMap);
	}

	@Override
	protected List<AstNodePojo> getCandidateNodes(final AstNodePojo root) {
		final AstNodeCollectingTraverser nodeTraverser = createNodeTraverser(node -> AstNodeUtils.hasAnySuperType(node, BRANCH_STATEMENT));
		nodeTraverser.traverse(root);
		return nodeTraverser.collect().stream().filter(node -> AstNodeUtils.getBranchConditions(node).stream()
						.anyMatch(n -> FIELD_REFERENCE_COLLECTOR.firstDeep(n).isPresent()))
				.collect(Collectors.toList());
	}
	
	@Override
	protected String getAnnotationName() {
		return ANNOTATION_NAME;
	}

}
