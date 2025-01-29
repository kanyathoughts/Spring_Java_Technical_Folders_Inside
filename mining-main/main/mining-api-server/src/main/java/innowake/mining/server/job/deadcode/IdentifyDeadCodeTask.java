/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.deadcode;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.IntStream;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.server.controller.cfg.CalculateControlFlowService;
import innowake.mining.server.job.base.GenericProgramModulesTask;
import innowake.mining.server.service.UnreachableDependencyService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.lang.HashCache;
import innowake.mining.shared.model.RelationshipDirection;

/**
 * {@link Task} implementation that will identify the dead code for one single module.
 */
public class IdentifyDeadCodeTask extends GenericProgramModulesTask {
	
	private static final Logger LOG = LoggerFactory.getLogger(innowake.mining.server.discovery.Logging.DEAD_CODE);

	@Autowired
	private transient UnreachableDependencyService unreachableDependencyService;
	@Autowired
	private transient AstService astService;
	@Autowired
	private transient CalculateControlFlowService calculateControlFlowService;
	
	public IdentifyDeadCodeTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId, projectId, moduleId);
	}

	@Override
	protected void run(final EntityId moduleId) {
		final var module = moduleService.getModule(moduleId);
		if ( ! module.isSourceCodeAvailable()) {
			return;
		}
		try {
			calculateCfg(moduleId);
			final Integer deadCodeLineCount = calculateDeadCodeLineCount(moduleId);
			final var sourceMetrics = module.getSourceMetrics();
			if (sourceMetrics.isPresent() && ! Objects.equals(sourceMetrics.get().getDeadCodeLines(), deadCodeLineCount)) {
				moduleService.putSourceMetrics(new SourceMetricsPojoPrototype()
													.setModule(moduleId)
													.setDeadCodeLines(deadCodeLineCount));
			}
		} catch(final Exception e) {
			LOG.error(() -> "Error while calculating dead code for  " + module.getPath(), e);
		}

		moduleService.setFromDeadCode(q -> q.ofModuleInDirection(moduleId, RelationshipDirection.OUT), false);
		unreachableDependencyService.createDeadCodeAnnotations(module.getProject(), module.identity());
	}

	/**
	 * Calculate and return the count of dead code lines for {@link Module}.
	 * The dead code calculation will happen as below:
	 * 
	 *1 IDENTIFICATION DIVISION. 
	 *2 PROGRAM-ID. A. 
	 *3 ENVIRONMENT DIVISION. 
	 *4 DATA DIVISION.
	 *5 PROCEDURE DIVISION.
	 *6 L1.
	 *7      DISPLAY '1'
	 *8      PERFORM LEND.
	 *9      DISPLAY '2'
	 *10      GOBACK.
	 *11 L3.
	 *12     DISPLAY 'TEST'.
	 *13     GOBACK.
	 *14 LEND.
	 *15     DISPLAY 'END'.
	 *
	 * In above Cobol file, L3 is the block node which is unreferenced and lines in the L3 label is 12 and 13 and these
	 * lines will be calculated as dead code and return.
	 * 
	 * @param moduleId id of the {@link Module}
	 * @return the count of dead code lines for {@link Module}
	 */
	private Integer calculateDeadCodeLineCount(final EntityId moduleId) {
		final List<UUID> unreferencedBlockNodesIds = astService.findIds(q -> q.ofModule(moduleId)
															.withSuperTypes(AstNodeUtils.CFG_COLLAPSIBLE_NODE)
															.withRelationshipCount(AstRelationshipType.FLOW, RelationshipDirection.IN, Comperator.EQUAL, 0)
															.withModuleRelationshipCount(AstModuleRelationshipType.CONTROL_FLOW_TERMINALS, Comperator.EQUAL, 0));

		if (! unreferencedBlockNodesIds.isEmpty()) {
			final var ast = new HashCache<UUID, AstNodePojo>();
			ast.putAll(astService.find(q -> q.ofModule(moduleId).usingCache(ast)), AstNodePojo::getId);
			final List<AstNodePojo> unreferencedBlockNodes = unreferencedBlockNodesIds.stream().map(ast::get).toList();
	
			final long deadCodeLineCount = unreferencedBlockNodes.stream()
				/* We need to consider all children and the AstNode itself as potential dead code */
				.flatMap(astNode -> {
					final List<AstNodePojo> list = new ArrayList<>(AstNodeUtils.getChildrenDeep(astNode, "Statement"));
					list.add(astNode);
					return list.stream();
				})
				.map(AstNodePojo::getLocation)
	
				/* We need to check the lines within the same file with unassembled content and below check will validate that.
				 * The retraced location should be equal to root relative location of node. */
				.filter(moduleLocation -> Objects.equals(moduleLocation.getRootRelativeOffset(), moduleLocation.getRetracedOffset())
						&& Objects.equals(moduleLocation.getRootRelativeLength(), moduleLocation.getRetracedLength()))
	
				/* We will get all unreferenced nodes locations, which will have root relative start line number and end line number.
				 * The code between these line numbers is dead code, so to count the dead code line numbers, 
				 * we will collect all unique line numbers between them. */
				.flatMapToInt(moduleLocation -> {
					final Integer endLineNumber = moduleLocation.getRootRelativeEndLineNumber().orElse(0);
					final Integer startLineNumber = moduleLocation.getRootRelativeStartLineNumber().orElse(0);
					return IntStream.rangeClosed(startLineNumber.intValue(), endLineNumber.intValue());
				})
				.distinct()
				.count();
			return Integer.valueOf(Math.toIntExact(deadCodeLineCount));
		}
		
		return 0;
	}

	private void calculateCfg(final EntityId moduleId) {
		if (astService.findModuleRelationships(q -> q.ofModule(moduleId).withType(AstModuleRelationshipType.ENTRY)).isEmpty()) {
			calculateControlFlowService.calculateControlFlowGraph(moduleId);
		}
	}

}
