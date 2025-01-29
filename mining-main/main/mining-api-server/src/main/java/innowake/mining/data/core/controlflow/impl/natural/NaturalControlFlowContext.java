/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.collections4.map.HashedMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.ControlFlowContext;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.ndt.core.parsing.ast.model.LoopStatement;
import innowake.ndt.naturalparser.model.ArtificialNaturalAstNode;
import innowake.ndt.naturalparser.model.NaturalLoopEventStatement;
import innowake.ndt.naturalparser.model.NaturalLoopNode;

/**
 * Context for the Natural control flow calculation.
 */
public class NaturalControlFlowContext extends ControlFlowContext<NaturalStatement, NaturalControlFlowContext>{
	
	private final Map<AstNodePojo, Set<AstNodePojo>> loopEvents = new HashedMap<>();
	
	/**
	 * Constructor
	 * @param root the root node
	 */
	public NaturalControlFlowContext(final AstNodePojo root) {
		super(new NaturalJumpStatetmentStorage(root), new NaturalLabelResolver(), new NaturalSpecificHandler());
		extractLoopEvents(root);
	}

	private void extractLoopEvents(final AstNodePojo root) {
		final List<AstNodePojo> events = AstNodeUtils.getChildrenDeep(root, AstNodeUtils.LOOP_EVENT_STATEMENT);
		for (final AstNodePojo event : events) {
				if (event.getType().equals(ArtificialNaturalAstNode.IF_NO_RECORDS)) {
					final AstNodePojo findStmt = event.getParent().orElse(null);
					if (findStmt != null) {
						loopEvents.computeIfAbsent(findStmt, x -> new HashSet<>()).add(event);
					}
				} else {
					final Object label = event.getProperties().get(NaturalLoopEventStatement.TARGET_EVENT_LOOP_LABEL);
					final Optional<AstNodePojo> loop;
					if (label != null) {
						loop = findLabeledLoop(event, label.toString(), root);
					} else {
						loop = findMostOuterLoop(event, root);
					}
					if (loop.isPresent()) {
						loopEvents.computeIfAbsent(loop.get(), x -> new HashSet<>()).add(event);
					}
				}
		}
	}
	
	private Optional<AstNodePojo> findMostOuterLoop(final AstNodePojo event, final AstNodePojo root) { 
		AstNodePojo loop = null;
		AstNodePojo node = event.getParent().orElse(null);
		final Object eventType = event.getProperties().get(NaturalLoopEventStatement.EVENT_LOOP_TYPE);
		while (node != root && node != null) {
			final boolean supportedEventType = isSupportedEventType(eventType, node);
			if (node.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT) && supportedEventType) {
				loop = node;
			}
			node = node.getParent().orElse(null);
		}
		return Optional.ofNullable(loop);
	}
	
	private boolean isSupportedEventType(@Nullable final Object eventType, final AstNodePojo node) {
		final Set<String> superTypes = node.getSuperTypes();
		if (superTypes.contains(AstNodeUtils.DATABASE_ACCESS_STATEMENT) || superTypes.contains(AstNodeUtils.FILE_ACCESS_STATEMENT)) {
			return true;
		}
		if (eventType == null || ! NaturalLoopEventStatement.LoopEventType.BREAK_PROCESSING.name().equals(eventType.toString())) {
			return false;
		}
		final Object supportsBreakProcessing = node.getProperties().get(NaturalLoopNode.SUPPORTS_BREAK_PROCESSING);
		return supportsBreakProcessing != null && Boolean.parseBoolean(supportsBreakProcessing.toString());
	}

	private Optional<AstNodePojo> findLabeledLoop(final AstNodePojo event, final String label, final AstNodePojo root) {
		AstNodePojo node = event.getParent().orElse(null);
		while (node != root && node != null) {
			if (node.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT) && label.equals(node.getProperties().get(LoopStatement.LABEL))) {
				return Optional.of(node);
			}
			node = node.getParent().orElse(null);
		}
		return Optional.empty();
	}
	
	/**
	 * Returns all loop events for a given loop.
	 *
	 * @param loop the ast node of the loop
	 * @return all events associated with the given loop
	 */
	public Set<AstNodePojo> getLoopEventsForLoop(final AstNodePojo loop) {
		return loopEvents.getOrDefault(loop, Collections.emptySet());
	}
}
