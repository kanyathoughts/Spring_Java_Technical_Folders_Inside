/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import static innowake.mining.data.core.api.AstNodeUtils.ARITHMETIC_STATEMENT;

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
 * Collector to collect AST nodes having Business Rule related to Field Computation
 */
class FieldComputationRuleCollector extends AnnotationRuleCollector {
	
	private static final String ANNOTATION_NAME = "Field Computation Rule Candidate [System identified]";
	
	public FieldComputationRuleCollector(final Set<ModuleLocation> excludedLocations,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		super(excludedLocations, astAndDdeMap);
	}

	@Override
	protected List<AstNodePojo> getCandidateNodes(final AstNodePojo root) {
		final AstNodeCollectingTraverser computationTraverser = createNodeTraverser(node -> AstNodeUtils.hasAnySuperType(node, ARITHMETIC_STATEMENT));
		computationTraverser.traverse(root);
		return computationTraverser.collect().stream().filter(n -> ! AstNodeUtils.isEnclosedInSuperType(BRANCH_LOOP_STATEMENTS, n) &&
				FIELD_REFERENCE_COLLECTOR.firstDeep(n).isPresent()).collect(Collectors.toList());
	}

	@Override
	protected String getAnnotationName() {
		return ANNOTATION_NAME;
	}
}
