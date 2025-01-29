/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import innowake.mining.data.core.NaturalAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.JumpStatementStorage;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.ndt.core.parsing.ast.model.Invocable;
import innowake.ndt.naturalparser.model.NaturalCallNode;

/**
 * Jump statement storage for natural, stores jump targets and information which jumps have already been calculated.
 */
public class NaturalJumpStatetmentStorage implements JumpStatementStorage<NaturalStatement>{
	
	final Map<AstNodePojo, AstNodePojo> performInternalSubroutines = new HashMap<>();
	
	final Map<AstNodePojo, Set<NaturalStatement>> alreadyCalculated = new HashMap<>();
	
	public NaturalJumpStatetmentStorage(final AstNodePojo root) {
		final Map<String, AstNodePojo> invocables = new HashMap<>();
		AstNodeUtils.getChildrenDeep(root, AstNodeUtils.INVOCABLE).forEach(
			node -> invocables.put((String) node.getProperties().get(Invocable.INVOCATION_NAME), node)
		);
		final List<AstNodePojo> children = AstNodeUtils.getChildrenDeep(root, AstNodeUtils.CALL_INTERNAL_STATEMENT);
		for (final AstNodePojo jump : children) {
			if (NaturalAstNodeType.PERFORM.equals(jump.getType()) && jump.getSuperTypes().contains(AstNodeUtils.CALL_INTERNAL_STATEMENT)) {
				final String targetName = (String) jump.getProperties().get(NaturalCallNode.TARGET_NAME);
				if (targetName != null) {
					performInternalSubroutines.put(jump, invocables.get(targetName));
				}
			}
		}
	}

	@Override
	public Set<AstNodePojo> getJumpTargets(final AstNodePojo jumpStatement) {
		if (NaturalAstNodeType.PERFORM.equals(jumpStatement.getType()) && jumpStatement.getSuperTypes().contains(AstNodeUtils.CALL_INTERNAL_STATEMENT)) {
			final AstNodePojo target = performInternalSubroutines.get(jumpStatement);
			if (target != null) {
				return Collections.singleton(target);
			}
		}
		return Collections.emptySet();
	}

	@Override
	public boolean shouldCalculateJumpTarget(final NaturalStatement jumpStatement, final AstNodePojo target) {
		return alreadyCalculated.computeIfAbsent(target, node -> new HashSet<>()).add(jumpStatement);
	}
}
