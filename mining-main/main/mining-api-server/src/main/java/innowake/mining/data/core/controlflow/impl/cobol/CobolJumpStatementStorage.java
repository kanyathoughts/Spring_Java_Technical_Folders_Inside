/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.ndt.core.parsing.ast.model.Invocable;

/**
 * Contains the control flow results for GO TO and PERFORM statements.
 */
public class CobolJumpStatementStorage {
	
	private final Set<AstNodePojo> alreadyHandledPerforms = new HashSet<>();
	private final Set<Tuple2<AstNodePojo, Optional<AstNodePojo>>> alreadyHandledGoTos = new HashSet<>();
	private final Map<Tuple2<AstNodePojo, Optional<AstNodePojo>>, ControlFlowSubResult<DefaultStatement>> evaluatedPerforms = new HashMap<>();
	
	private final Map<UUID, List<AstNodePojo>> goTos = new HashMap<>();
	private final Map<UUID, Tuple2<Optional<AstNodePojo>, Optional<AstNodePojo>>> performs = new HashMap<>();
	
	/**
	 * Constructor
	 * 
	 * @param procedureDivisionNode the AST node representing the procedure division
	 */
	public CobolJumpStatementStorage(final AstNodePojo procedureDivisionNode) {
		init(procedureDivisionNode);
	}
	
	private void init(final AstNodePojo procedureDivisionNode) {
		final Map<String, AstNodePojo> labels = new HashMap<>();
		final Map<AstNodePojo, Map<String, AstNodePojo>> duplicateLables = new HashMap<>();
		final Set<String> foundLabels = new HashSet<>();
		AstNodeUtils.getChildrenDeep(procedureDivisionNode, AstNodeUtils.INVOCABLE).forEach(node -> addLabel(node, foundLabels, labels, duplicateLables));
		final List<AstNodePojo> children = AstNodeUtils.getChildrenDeep(procedureDivisionNode, AstNodeUtils.JUMP_STATEMENT);
		for (final AstNodePojo jump : children) {
			if (CobolAstNodeType.PERFORM.getType().equals(jump.getType())) {
				addPerformStatement(labels, duplicateLables, jump);
			} else {
				addGoTo(labels, duplicateLables, jump);
			}
		}
	}

	private void addGoTo(final Map<String, AstNodePojo> labels, final Map<AstNodePojo, Map<String, AstNodePojo>> duplicateLables, final AstNodePojo jump) {
		final String targets = jump.getProperties().get("targets").toString();
		final List<String> targetList = getTargetList(targets);
		final List<AstNodePojo> targetNodes = targetList.stream()
				.map(String::trim)
				.map(label -> searchLabel(label, jump, labels, duplicateLables))
				.flatMap(Optional::stream)
				.collect(Collectors.toList());
		goTos.put(jump.getId(), targetNodes);
	}

	private List<String> getTargetList(final String targets) {
		if (targets.startsWith("[") && targets.endsWith("]")) {
			return Arrays.asList(targets.substring(1, targets.length() - 1).split(","));
		} else {
			return Arrays.asList(targets.split(","));
		}
	}
	
	private void addPerformStatement(final Map<String, AstNodePojo> labels, final Map<AstNodePojo, Map<String, AstNodePojo>> duplicateLables, final AstNodePojo jump) {
		final Object target = jump.getProperties().get("target");
		if (target != null) {
			final Optional<AstNodePojo> targetNode = searchLabel(target.toString(), jump, labels, duplicateLables);
			final Object thru = jump.getProperties().get("thru");
			if (thru != null) {
				final Optional<AstNodePojo> thruNode = searchLabel(thru.toString(), jump, labels, duplicateLables);
				performs.put(jump.getId(), Tuple2.of(targetNode, thruNode));
			} else {
				performs.put(jump.getId(), Tuple2.of(targetNode, targetNode));
			}
		}
	}
	
	private void addLabel(final AstNodePojo labelNode, final Set<String> foundLabels, final Map<String, AstNodePojo> labels, final Map<AstNodePojo, Map<String, AstNodePojo>> duplicateLables) {
		final String label = (String) labelNode.getProperties().get(Invocable.INVOCATION_NAME);
		if (foundLabels.contains(label)) {
			final AstNodePojo previousLabel =labels.remove(label);
			if (previousLabel != null) {
				addDuplicateLabel(previousLabel, duplicateLables, label);
			}
			addDuplicateLabel(labelNode, duplicateLables, label);	
		} else {
			foundLabels.add(label);
			labels.put(label, labelNode);
		}
	}

	private void addDuplicateLabel(final AstNodePojo labelNode, final Map<AstNodePojo, Map<String, AstNodePojo>> duplicateLables, final String label) {
		AstNodePojo section = AstNodeUtils.getSection(labelNode);
		duplicateLables.computeIfAbsent(section, k -> new HashMap<>()).put(label, labelNode);
	}
	
	private Optional<AstNodePojo> searchLabel(final String label, final AstNodePojo jump, final Map<String, AstNodePojo> labels, final Map<AstNodePojo, Map<String, AstNodePojo>> duplicateLables) {
		AstNodePojo labelNode = labels.get(label);
		if (labelNode == null) {
			AstNodePojo section = AstNodeUtils.getSection(jump);
			final Map<String, AstNodePojo> sectionLables = duplicateLables.get(section);
			if (sectionLables != null) {
				labelNode = sectionLables.get(label);
			}
		}
		return Optional.ofNullable(labelNode);
	}
	
	Optional<Tuple2<Optional<AstNodePojo>, Optional<AstNodePojo>>> getPerformTarget(final AstNodePojo perform) {
		return Optional.ofNullable(performs.get(perform.getId()));
	}
	
	Optional<List<AstNodePojo>> getGoToTargets(final AstNodePojo goTo) {
		return Optional.ofNullable(goTos.get(goTo.getId()));
	}
	
	/**
	 * Stores that a PERFORM statement is getting handled.
	 *
	 * @param perform the PERFORM statement
	 */
	void addHandledPerform(final AstNodePojo perform) {
		alreadyHandledPerforms.add(perform);
	}
	
	/**
	 * Stores that a GO TO statement with certain return conditions is getting handled.
	 *
	 * @param target the target of GO TO statement
	 * @param enclosingPerform the currently handled PERFORM statement or {@code null} if no such statement exists
	 */
	void addHandledGoTo(final AstNodePojo target, @Nullable final AstNodePojo enclosingPerform) {
		alreadyHandledGoTos.add(Tuple2.of(target, Optional.ofNullable(enclosingPerform)));
	}
	
	/**
	 * Checks if a PERFORM statement should be handled.
	 *
	 * @param perform the PERFORM statement
	 * @return {@code true} if the PERFORM target should be visited; {@code false} otherwise
	 */
	boolean shouldVisitPerform(final AstNodePojo perform) {
		return ! alreadyHandledPerforms.contains(perform);
	}
	
	/**
	 * Checks if a GO TO statement should be handled.
	 *
	 * @param goTo the GO TO statement
	 * @param enclosingPerform the currently handled PERFORM statement or {@code null} if no such statement exists
	 * @return {@code true} if the GO TO target should be visited; {@code false} otherwise
	 */
	boolean shouldVisitGoTo(final AstNodePojo goTo, @Nullable final AstNodePojo enclosingPerform) {
		return ! alreadyHandledGoTos.contains(Tuple2.of(goTo, Optional.ofNullable(enclosingPerform)));
	}

	/**
	 * Adds a control flow result for a PERFORM statement to the storage.
	 *
	 * @param result the control flow result of the PERFORM statement
	 * @param traget the target for the PERFORM statement
	 * @param jumpBackLabels the labels after which the statement should return
	 */
	void addPerformResult(final ControlFlowSubResult<DefaultStatement> result, final Tuple2<AstNodePojo, Optional<AstNodePojo>> target) {
		evaluatedPerforms.put(target, result);
	}
	
	/**
	 * Returns the control flow result for a stored PERFORM statement.
	 *
	 * @param target the target for the PERFORM statement
	 * @return the control flow result, or {@code null} if no stored result is present
	 */
	@Nullable
	ControlFlowSubResult<DefaultStatement> getPerformResult(final Tuple2<AstNodePojo, Optional<AstNodePojo>> target) {
		return evaluatedPerforms.get(target);
	}
}
