/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import static innowake.mining.data.core.api.AstNodeUtils.ERROR_PROCESSING_STATEMENT;

import java.util.List;
import java.util.Map;
import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeCollectingTraverser;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Collector to collect AST nodes having Business Rule related to Error Processing
 */
class ErrorProcessingRuleCollector extends AnnotationRuleCollector {
	
	private static final String ANNOTATION_NAME = "Error Processing Rule Candidate [System identified]";
	
	public ErrorProcessingRuleCollector(final Set<ModuleLocation> excludedLocations,
			Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		super(excludedLocations, astAndDdeMap);
	}

	@Override
	protected List<AstNodePojo> getCandidateNodes(final AstNodePojo root) {
		final AstNodeCollectingTraverser nodeTraverser = createNodeTraverser(node -> AstNodeUtils.hasAnySuperType(node, ERROR_PROCESSING_STATEMENT));
		nodeTraverser.traverse(root);
		return nodeTraverser.collect();
	}

	@Override
	protected String getAnnotationName() {
		return ANNOTATION_NAME;
	}
}
