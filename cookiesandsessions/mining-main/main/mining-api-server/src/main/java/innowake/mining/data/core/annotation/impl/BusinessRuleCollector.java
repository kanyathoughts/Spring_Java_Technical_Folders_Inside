/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.data.core.api.AstNodeUtils.ARITHMETIC_EXPRESSION;
import static innowake.mining.data.core.api.AstNodeUtils.ARITHMETIC_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.BREAK_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.CONSTANT_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.CONTINUE_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.DATABASE_ACCESS_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.DEFAULT_BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_DEFINITION;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.FILE_ACCESS_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.JUMP_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.LOOP_CONTROL_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.hasAnySuperType;
import static innowake.mining.shared.model.AnnotationType.RULE;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang.StringUtils.isNotBlank;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.core.api.AstNodeCollectingTraverser;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.AnnotationMetaDataReasonEnum;
import innowake.mining.shared.model.AnnotationMetadata;
import innowake.ndt.cobol.parser.ast.statement.CobolComputeStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolEvaluateStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolReadStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSelectStmt;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlClose;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlConnect;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareCursor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlFetch;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlSelect;
import innowake.ndt.naturalparser.ast.Statement.ReadStmt;
import innowake.ndt.naturalparser.ast.imp.ImpStatement.ReadWorkFileStmt;
import innowake.ndt.naturalparser.model.NaturalLoopFileAccessNode;
import innowake.ndt.pl1parser.ast.model.statement.data.record.ReadStatement;

/**
 * Business rule collector to collect AST nodes which are considered AS business rule metadataList. All technology specific business rule
 * collectors should extend this one.
 * <p>This collector is <b>stateful</b> for performance reasons, see method {@code #getFieldDefinitions()}.</p>
 */
class BusinessRuleCollector {

	/* In case of GRP.VARIABLE we only want VARIABLE */
	private final AstNodeCollector fieldReferencesCollector = new AstNodeCollector(node -> node.getSuperTypes().contains(FIELD_REFERENCE));
	private final AstNodeCollector variablesCollector = 
		new AstNodeCollector(node ->
			node.getSuperTypes().contains(FIELD_REFERENCE) &&
				node.getChildren().stream().noneMatch(child -> fieldReferencesCollector.firstDeep(child).isPresent()));
	private final AstNodeCollector constantsCollector = new AstNodeCollector(node -> node.getSuperTypes().contains(CONSTANT_REFERENCE));
	private final AstNodeCollector blockCollector = new AstNodeCollector(node -> hasAnySuperType(node, BRANCH, DEFAULT_BRANCH));
	private final AstNodeCollector arithmExprCollector = new AstNodeCollector(node -> hasAnySuperType(node, ARITHMETIC_EXPRESSION, ARITHMETIC_STATEMENT));
	private final AstNodeCollector fileDataCollector = new AstNodeCollector(node -> hasAnySuperType(node, FILE_ACCESS_STATEMENT, DATABASE_ACCESS_STATEMENT));
	private final AstNodeCollector loopStmtCollector = new AstNodeCollector(node -> hasAnySuperType(node, LOOP_CONTROL_STATEMENT, JUMP_STATEMENT,
			BREAK_STATEMENT, CONTINUE_STATEMENT));
	private final AstNodeCollector branchStmtCollector = new AstNodeCollector(node -> node.getSuperTypes().contains(BRANCH_STATEMENT));
	@Nullable
	private Map<String, List<AstNodePojo>> fieldDefinitions;

	/**
	 * Returns a new instance of {@link AstNodeCollectingTraverser} which traverses AST nodes searching for business rule metadataList with super
	 * type {@code BRANCH_STATEMENT}.
	 * <p>The traverser stops visiting child nodes of AST nodes with super type {@code INCLUSION_NODE}.</p>
	 * 
	 * @param traverseChildrenPredicate the {@link Predicate}
	 */
	AstNodeCollectingTraverser createBrBranchNodeTraverser() {
		return new AstNodeCollectingTraverser(AstNodeUtils::isBranchStatementNode) {

			@Override
			public AstNodePojo traverse(final AstNodePojo node) {
				return ! AstNodeUtils.isInclusionNode(node) ? super.traverse(node) : node;
			}
		};
	}

	/**
	 * Traverses the given {@code root} node deep and collects all business rule metadataList.
	 *
	 * @param root the node from which the collection will start
	 * @param categoryId Id of the annotationCategory of the annotation
	 * @param astAndDdeMap dataDictionary related to the Node
	 * @return list of identified candidate annotations
	 */
	List<AnnotationPojoTemplate> collect(final AstNodePojo root, @Nullable final Long categoryId,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		final AstNodeCollectingTraverser branchTraverser = createBrBranchNodeTraverser();
		branchTraverser.traverse(root);
		final List<AnnotationPojoTemplate> annotations = new ArrayList<>();
		for (final AstNodePojo node : branchTraverser.collect()) {
			final List<AnnotationMetadata> metadataList = new ArrayList<>();
			checkForIfElseCondition(metadataList, node, root);
			
			checkForMultiExpIfElseCondition(metadataList, node);
			
			checkForLoopCondition(metadataList, node);
			
			checkForCblEvaluateCondition(metadataList, node);
			
			checkForNaturalLoopFileAccessCondition(metadataList, node);
			
			checkForSelectCondition(metadataList, node);
			
			checkForComputeCondition(metadataList, node);
			
			checkForInputOutputExtDataSrc(metadataList, node);
			
			checkForNestedCondition(metadataList, node);
			
			if ( ! metadataList.isEmpty()) {
				final var annotation = AnnotationCreator.create(node, BusinessRuleIdentifier.ANNOTATION_NAME, RULE, categoryId);
				annotation.setReasons(metadataList.stream().map(m -> m.getReason().name()).collect(Collectors.toList()));
				annotations.add(annotation);
			}
		}
		return annotations;
	}

	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#IF_ELSE_CONDITION}
	 * by testing its {@code conditions}.
	 * 
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param node the AST node that is examined
	 * @param root the root node from which the collection will start
	 */
	void checkForIfElseCondition(final List<AnnotationMetadata> metadataList, final AstNodePojo node, final AstNodePojo root) {
		final List<AstNodePojo> conditions = AstNodeUtils.getBranchConditions(node);
		for (final AstNodePojo condition : conditions) {
			final List<AstNodePojo> fieldReferences = variablesCollector.allDeep(condition);
			/* The condition of the statement contains at least two CONSTANT_REFERENCE or at least two FIELD_REFERENCE */
			if (constantsCollector.allDeep(condition).size() > 1 || fieldReferences.size() > 1) {
				metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION));
				return;
			}
			/* Test if the condition contains at least one FIELD_REFERENCE from a Copybook */
			else {
				for (final AstNodePojo fieldReference : fieldReferences) {
					if (isDataFieldFromCopy(fieldReference, root)) {
						metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION));
						return;
					}
				}
			}
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#MULTI_EXPRESSION_IF_ELSE_CONDITION}
	 * by testing its {@code conditions}.
	 *
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param node the AST node that is examined
	 */
	void checkForMultiExpIfElseCondition(final List<AnnotationMetadata> metadataList, final AstNodePojo node) {
		if (node.getSuperTypes().contains(BRANCH_STATEMENT) && (node.getLabel().contains(" AND ")
				|| node.getLabel().contains(" OR ") || node.getLabel().contains(" & ") || node.getLabel().contains(" | "))) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.MULTI_EXPRESSION_IF_ELSE_CONDITION));
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#LOOP_CONDITION}
	 * by testing its {@code conditions}.
	 *
	 * @param metadataList the {@link AnnotationMetadata} to be populated 
	 * @param node the AST node that is examined
	 */
	void checkForLoopCondition(final List<AnnotationMetadata> metadataList, final AstNodePojo node) {
		final Optional<AstNodePojo> optNode = loopStmtCollector.firstDeep(node);
		if (optNode.isPresent()) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.LOOP_CONDITION));
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#COBOL_EVALUATE_CONDITION}
	 * by testing its {@code conditions}.
	 *
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param node the AST node that is examined
	 */
	void checkForCblEvaluateCondition(final List<AnnotationMetadata> metadataList, final AstNodePojo node) {
		final List<AstNodePojo> nodes = blockCollector.all(node);
		for(final AstNodePojo node2 : nodes) {
			final Optional<AstNodePojo> optNode = branchStmtCollector.firstDeep(node2);
			if(optNode.isPresent() && CobolEvaluateStmt.class.getSimpleName().equals(optNode.get().getType())) {
				metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.COBOL_EVALUATE_CONDITION));
				break;
			}
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#NATURAL_LOOP_FILE_ACCESS_CONDITION}
	 * by testing its {@code conditions}.
	 *
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param node the AST node that is examined
	 */
	void checkForNaturalLoopFileAccessCondition(final List<AnnotationMetadata> metadataList, final AstNodePojo node) {
		if (loopStmtCollector.allDeep(node).stream().anyMatch(node2 -> fileDataCollector.firstDeep(node2).isPresent()
				&& NaturalLoopFileAccessNode.class.getSimpleName().equals(node2.getType()))) {
				metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.NATURAL_LOOP_FILE_ACCESS_CONDITION));
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#SELECT_CONDITION}
	 * by testing its {@code conditions}.
	 *
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param node the AST node that is examined
	 */
	void checkForSelectCondition(final List<AnnotationMetadata> metadataList, final AstNodePojo node) {
		final Optional<AstNodePojo> optNode = branchStmtCollector.firstDeep(node);
		if (optNode.isPresent() && CobolSelectStmt.class.getSimpleName().equals(optNode.get().getType())) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.SELECT_CONDITION));
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#INPUT_FROM_EXTERNAL_DATA_SOURCE}
	 * or {@linkplain AnnotationMetaDataReasonEnum#OUTPUT_TO_EXTERNAL_DATA_SOURCE}
	 * by testing its {@code conditions}.
	 *
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param node the AST node that is examined
	 */
	void checkForInputOutputExtDataSrc(final List<AnnotationMetadata> metadataList, final AstNodePojo node) {
		final Optional<Optional<AstNodePojo>> resultNode = blockCollector.all(node).stream()
				.map(fileDataCollector::firstDeep)
				.filter(Optional::isPresent)
				.findFirst();
		if (resultNode.isPresent() && resultNode.get().isPresent()) {
			final Optional<AstNodePojo> optNode = resultNode.get();
			if (Stream.of(ReadStmt.class.getSimpleName(), ReadStatement.class.getSimpleName(), ExecSqlConnect.class.getSimpleName(),
					ExecSqlSelect.class.getSimpleName(), ExecSqlDeclareCursor.class.getSimpleName(), ExecSqlFetch.class.getSimpleName(),
					ExecSqlClose.class.getSimpleName(), ReadWorkFileStmt.class.getSimpleName(), CobolReadStmt.class.getSimpleName()
					).anyMatch(optNode.get().getType()::equals)) {
				metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.INPUT_FROM_EXTERNAL_DATA_SOURCE));
			} else {
				metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.OUTPUT_TO_EXTERNAL_DATA_SOURCE));
			}
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#COMPUTATION} by testing its {@code conditions}.
	 *
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param node the AST node that is examined
	 */
	void checkForComputeCondition(final List<AnnotationMetadata> metadataList, final AstNodePojo node) {
		if (arithmExprCollector.firstDeep(node).isPresent() || CobolComputeStmt.class.getSimpleName().equals(node.getType())) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.COMPUTATION));
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#NESTED_CONDITION}
	 * by testing its {@code conditions}.
	 *
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param node the AST node that is examined
	 */
	void checkForNestedCondition(final List<AnnotationMetadata> metadataList, final AstNodePojo node) {
		if (blockCollector.all(node).stream().anyMatch(node2 -> branchStmtCollector.firstDeep(node2).isPresent())) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.NESTED_CONDITION));
		}
	}

	private boolean isDataFieldFromCopy(final AstNodePojo fieldReference, final AstNodePojo root) {
		/* Check if there is an edge from the field reference to the data field. If so test if it is located in a copy */
		for (final AstRelationshipPojo edge : fieldReference.getOutgoingRelations()) {
			if (edge.getType().equals(AstRelationshipType.REFERS) && AstNodeUtils.isFromInclusion(edge.getDstNode(), fieldReference)) {
				return true;
			}
		}

		/* Fallback: try to find by field name which can lead to some false positives */
		String fieldName = AstService.Properties.UNRESOLVED_FIELD_NAME.getFrom(fieldReference.getProperties());
		if (fieldName != null) {
			/* Get VAR_NAME from GRP_NAME.SUB_GRP_NAME.VAR_NAME */
			final int dotIndex = fieldName.lastIndexOf('.');
			if (dotIndex != -1) {
				fieldName = fieldName.substring(dotIndex + 1);
			}

			final List<AstNodePojo> fieldNodes = getFieldDefinitions(root).get(fieldName);
			if (fieldNodes != null && fieldNodes.stream().anyMatch(node2 -> AstNodeUtils.isFromInclusion(node2, root))) {
				return true;
			}
		}

		return false;
	}
	
	private Map<String, List<AstNodePojo>> getFieldDefinitions(final AstNodePojo root) {
		if (fieldDefinitions == null) {
			final AstNodeCollector fieldDefinitionsCollector = new AstNodeCollector(node -> node.getSuperTypes().contains(FIELD_DEFINITION));
			fieldDefinitions = fieldDefinitionsCollector.allDeep(root).stream()
				.filter(node -> isNotBlank(AstService.Properties.FIELD_NAME.getFrom(node.getProperties())))
				.collect(groupingBy(node -> AstService.Properties.FIELD_NAME.getFrom(node.getProperties()), mapping(node -> node, toList())));
		}

		return assertNotNull(fieldDefinitions);
	}
}
