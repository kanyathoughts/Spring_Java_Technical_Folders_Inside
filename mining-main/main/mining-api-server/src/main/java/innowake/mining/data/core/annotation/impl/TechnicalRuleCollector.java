/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import static innowake.mining.data.core.api.AstNodeUtils.ASSIGNING_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.DATABASE_ACCESS_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.FILE_ACCESS_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.JUMP_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.STATEMENT;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeCollectingTraverser;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.cobol.parser.ast.statement.CobolSelectStmt;


/**
 * Collector to collect AST nodes having Technical Annotation Rule
 */
public class TechnicalRuleCollector extends AnnotationRuleCollector {
	
	private static final String ANNOTATION_NAME = "Technical Rule Candidate [System identified]";
	
	public TechnicalRuleCollector(final Set<ModuleLocation> excludedLocations,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		super(excludedLocations, astAndDdeMap);
	}

	@Override
	protected List<AstNodePojo> getCandidateNodes(final AstNodePojo root) {
		final String sqlCode = "SQLCODE";
		final List<AstNodePojo> candidates = new ArrayList<>();
		
		/* Annotation Rules having File/Database access statement enclosed by Branch/Jump statements or File/Database access statements enclosing Branch */
		final AstNodeCollectingTraverser nodeTraverser = createNodeTraverser(n1 -> AstNodeUtils.hasAnySuperType(n1, DATABASE_ACCESS_STATEMENT,
				FILE_ACCESS_STATEMENT));
		nodeTraverser.traverse(root);
		
		final List<AstNodePojo> fileDataAccessStatements = nodeTraverser.collect();
		final Set<String> fieldReferences = fileDataAccessStatements.stream().map(FIELD_REFERENCE_COLLECTOR::allDeep)
				.flatMap(Collection::stream)
				.map(AstNodeUtils::getFieldDefinitionNameFromReference)
				.collect(Collectors.toSet());
		
		for (final AstNodePojo node : fileDataAccessStatements) {
			final AstNodePojo parent = node.getParent().orElse(null);
			if (parent != null && CollectionUtils.containsAny(parent.getSuperTypes(), BRANCH_LOOP_STATEMENTS)) {
				checkBranchJumpConditions(parent, fieldReferences, candidates);
			}
			candidates.addAll(node.getChildren().stream().map(BRANCH_COLLECTOR::all).flatMap(Collection::stream).collect(Collectors.toList()));
		}
		
		/* Annotation Rules where Branch/Jump statement conditions uses field references identified in CobolSelectStmt */
		final AstNodeCollectingTraverser selectTraverser = createNodeTraverser(n2 -> CobolSelectStmt.class.getSimpleName().equals(n2.getType()));
		selectTraverser.traverse(root);
		
		final AstNodeCollectingTraverser branchLoopTraverser = createNodeTraverser(n3 -> AstNodeUtils.hasAnySuperType(n3, BRANCH_STATEMENT, JUMP_STATEMENT));
		branchLoopTraverser.traverse(root);
		final Set<String> technicalFields = selectTraverser.collect().stream().map(FIELD_REFERENCE_COLLECTOR::allDeep)
				.flatMap(Collection::stream)
				.map(AstNodeUtils::getFieldDefinitionNameFromReference)
				.collect(Collectors.toSet());
		technicalFields.add(sqlCode);
		
		collectAssigningStatements(root, technicalFields, Arrays.asList(Sets.newHashSet(ASSIGNING_STATEMENT, STATEMENT), Sets.newHashSet(ASSIGNING_STATEMENT)),
				sqlCode);
		
		branchLoopTraverser.collect().forEach(stmt -> checkBranchJumpConditions(stmt, technicalFields, candidates));
		return candidates;
	}
	
	/**
	 * This method collects the AstNode for input types, and check if any of the field contains input technicalField and add it to input field list
	 *
	 * @param root AST node root
	 * @param technicalFields list of field references
	 * @param types requested types
	 * @param technicalField field to be compared with all field references
	 */
	private void collectAssigningStatements(final AstNodePojo root, final Set<String> technicalFields, final List<Set<String>> types, final String technicalField) {
		final AstNodeCollectingTraverser assigningStmtTraverser = createNodeTraverser(node -> hasAllSuperType(node, types));
		assigningStmtTraverser.traverse(root);
		for (final AstNodePojo node : assigningStmtTraverser.collect()) {
			final Set<String> fields = FIELD_REFERENCE_COLLECTOR.allDeep(node).stream()
					.map(AstNodeUtils::getFieldDefinitionNameFromReference)
					.collect(Collectors.toSet());
			if (fields.stream().anyMatch(technicalField::contains)) {
				technicalFields.addAll(fields);
			}
		}
	}
	
	/**
	 * Informs about if any Set of {@code types} are matches {@code AstNode#getSuperTypes()}.
	 *
	 * @param node the AST node
	 * @param types the requested super types
	 * @return {@code true} if any Set of {@code types} are matches {@code AstNode#getSuperTypes()}
	 */
	private boolean hasAllSuperType(final AstNodePojo node, final List<Set<String>> types) {
		return types.stream().anyMatch(typeList -> typeList.equals(node.getSuperTypes()));
	}
	
	private void checkBranchJumpConditions(final AstNodePojo node, final Set<String> fieldReferences, final List<AstNodePojo> candidates) {
		final List<AstNodePojo> conditions =  AstNodeUtils.isBranchStatementNode(node) ? AstNodeUtils.getBranchConditions(node)
				: CollectionUtils.emptyIfNull(node.getChildren()).stream().filter(n -> COMPARISON_EXP_TYPES.contains(n.getType()))
				.collect(Collectors.toList());
		if (conditions.stream().anyMatch(n -> CollectionUtils.containsAny(FIELD_REFERENCE_COLLECTOR.allDeep(n).stream()
				.map(AstNodeUtils::getFieldDefinitionNameFromReference).collect(Collectors.toSet()), fieldReferences))) {
			candidates.add(node);
		}
	}

	@Override
	protected String getAnnotationName() {
		return ANNOTATION_NAME;
	}

}
