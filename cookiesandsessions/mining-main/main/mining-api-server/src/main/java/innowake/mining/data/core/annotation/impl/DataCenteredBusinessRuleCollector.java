/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import static innowake.mining.data.core.api.AstNodeUtils.ASSIGNING_STATEMENT;

import static innowake.mining.data.core.api.AstNodeUtils.ARITHMETIC_EXPRESSION;
import static innowake.mining.data.core.api.AstNodeUtils.ARITHMETIC_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.BREAK_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.CONTINUE_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.DATABASE_ACCESS_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.DEFAULT_BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.FILE_ACCESS_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.JUMP_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.LOOP_CONTROL_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.hasAnySuperType;
import static innowake.mining.shared.model.AnnotationType.RULE;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationMetaDataReasonEnum;
import innowake.mining.shared.model.AnnotationMetadata;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.cobol.parser.ast.statement.CobolEvaluateStmt;

/**
 * Data Centered Business rule collector to collect AST nodes which are considered AS business rule metadataList and having business variables.
 */
class DataCenteredBusinessRuleCollector extends AnnotationRuleCollector {
	
	private static final AstNodeCollector ASSIGNMENT_COLLECTOR = new AstNodeCollector(node -> hasAnySuperType(node, ASSIGNING_STATEMENT));
	private static final AstNodeCollector ARITHMETIC_COLLECTOR = new AstNodeCollector(node -> hasAnySuperType(node, ARITHMETIC_EXPRESSION,
			ARITHMETIC_STATEMENT));
	private static final AstNodeCollector FILE_DATA_COLLECTOR = new AstNodeCollector(node -> hasAnySuperType(node, FILE_ACCESS_STATEMENT,
			DATABASE_ACCESS_STATEMENT));
	private static final AstNodeCollector LOOP_STATEMENT_COLLECTOR = new AstNodeCollector(node -> hasAnySuperType(node, LOOP_CONTROL_STATEMENT, JUMP_STATEMENT,
			BREAK_STATEMENT, CONTINUE_STATEMENT));
	private static final AstNodeCollector BRANCH_STATEMENT_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(BRANCH_STATEMENT));
	
	private final List<AstNodePojo> businessVariableReferences;
	
	public DataCenteredBusinessRuleCollector(final Set<ModuleLocation> excludedLocations,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		super(excludedLocations, astAndDdeMap);
		businessVariableReferences = new ArrayList<>();
		astAndDdeMap.getOrDefault(true, Collections.emptyMap()).entrySet()
				.forEach(ad -> ad.getValue().forEach(v -> AstNodeUtils.getDefinitionNameAndUsageFromReference(v.a, businessVariableReferences, false)));
	}
	
	/**
	 * Traverses through the list of AST nodes and searches all the annotation Candidates
	 *
	 * @param root Root AST node
	 * @param categoryId Id of the annotationCategory of the annotation
	 * @return list of identified candidate annotations
	 */
	@Override
	protected List<AnnotationPojoTemplate> collect(final AstNodePojo root, final Long categoryId) {
		final List<AnnotationPojoTemplate> annotations = new ArrayList<>();
		final Set<String> businessVariables = businessVariableReferences.stream()
				.map(AstNodeUtils::getFieldDefinitionNameFromReference).collect(Collectors.toSet());
		final Set<ModuleLocation> visitedLocations = new HashSet<>();
		for (final AstNodePojo node : businessVariableReferences) {
			AstNodePojo currentNode = node.getParent().orElse(null);
			while (currentNode != null && currentNode.getParentId().isPresent()) {
				if (CollectionUtils.containsAny(currentNode.getSuperTypes(), BRANCH_LOOP_STATEMENTS)) {
					final ModuleLocation location = getAdjustedRuleLocation(currentNode).convertToSharedModuleLocation();
					if (visitedLocations.contains(location)) {
						getExcludedLocations().add(location);
						break;
					}
					visitedLocations.add(location);
					final List<AnnotationMetadata> metadataList = collectAnnotationMetadata(currentNode, businessVariables);
					if ( ! metadataList.isEmpty()) {
						createAnnotations(currentNode, annotations, categoryId, metadataList, getAdjustedRuleLocation(currentNode));
					}
				}
				currentNode = currentNode.getParent().orElse(null);
			}
		}
		return removeNestedAnnotations(annotations);
	}
	
	/**
	 * Traverses the input {@code node} and collects all business metadata
	 *
	 * @param node The AST node for traversing candidates
	 * @param annotations List of {@linkplain Annotation}
	 * @param businessVariable The label of Business variable
	 * @param categoryId Id of the annotationCategory of the annotation
	 */
	private List<AnnotationMetadata> collectAnnotationMetadata(final AstNodePojo node, final Set<String> businessVariables) {
		final List<AnnotationMetadata> metadataList = new ArrayList<>();
		final boolean hasBranchMetadata = checkForBranchMetadata(node, businessVariables);
		
		if (hasBranchMetadata) {
			metadataList.add(new AnnotationMetadata(isEvaluate(node) ? AnnotationMetaDataReasonEnum.COBOL_EVALUATE_CONDITION
					: AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION));
		}

		final List<AstNodePojo> blocks = AstNodeUtils.isBranchStatementNode(node) ? BRANCH_COLLECTOR.all(node) : LOOP_STATEMENT_COLLECTOR.all(node);
		
		final boolean hasLoopMetadata = checkForLoopCondition(node, businessVariables);
		
		if (hasLoopMetadata) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.LOOP_CONDITION));
		}
		
		checkForMultiExpIfElseCondition(node, businessVariables, metadataList);
		
		checkForComputeCondition(blocks, businessVariables, metadataList);
		
		checkForInputOutputExtDataSrc(blocks, businessVariables, metadataList);
		
		checkForBusinessVariableTransformation(blocks, businessVariables, metadataList);
		
		/* Nested conditions should have Business Variables in outer if-else/loop/evaluate and inner if-else/loop/evaluate statements */
		if (hasBranchMetadata || hasLoopMetadata) {
			checkForNestedIfElseCondition(blocks, businessVariables, metadataList);
			
			checkForNestedCblEvaluateCondition(blocks, businessVariables, metadataList);
			
			checkForNestedLoopCondition(blocks, businessVariables, metadataList);
		}
		
		return metadataList;
	}

	private void createAnnotations(final AstNodePojo node, final List<AnnotationPojoTemplate> annotations, @Nullable final Long categoryId,
			final List<AnnotationMetadata> metadataList,
			final AstNodeLocation location) {
		metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
		final AnnotationPojoTemplate annotation = AnnotationCreator.create(node, getAnnotationName(), RULE, categoryId, location,
				translateAnnotation(node), getIncludedDataDictionaries(node));
		getExcludedLocations().add(location.convertToSharedModuleLocation());
		annotation.setReasons(metadataList.stream().map(m -> m.getReason().name()).collect(Collectors.toList()));
		annotations.add(annotation);
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of a Branch condition
	 * 
	 * @param node the AST node that is examined
	 * @param businessVariables Set of business variable labels
	 * @return boolean indicating if the candidate has Branch metadata
	 */
	private boolean checkForBranchMetadata(final AstNodePojo node, final Set<String> businessVariables) {
		final List<AstNodePojo> conditions = AstNodeUtils.getBranchConditions(node);
		for (final AstNodePojo condition : conditions) {
			final Set<String> variables = VARIABLE_COLLECTOR.allDeep(condition).stream()
					.map(AstNodeUtils::getFieldDefinitionNameFromReference)
					.collect(Collectors.toSet());
			if (CollectionUtils.containsAny(variables, businessVariables)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#MULTI_EXPRESSION_IF_ELSE_CONDITION}
	 *
	 * @param node the AST node that is examined
	 * @param businessVariables Set of business variable labels
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 */
	private void checkForMultiExpIfElseCondition(final AstNodePojo node, final Set<String> businessVariables, final List<AnnotationMetadata> metadataList) {
		if ((node.getSuperTypes().contains(BRANCH_STATEMENT) && (node.getLabel().contains(" AND ")
				|| node.getLabel().contains(" OR "))) && checkForBranchMetadata(node, businessVariables) && ! isEvaluate(node)) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.MULTI_EXPRESSION_IF_ELSE_CONDITION));
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#NESTED_IF_ELSE_CONDITION}
	 *
	 * @param blocks list of AST node to be checked
	 * @param businessVariables Set of business variable labels
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 * @param businessDataDictionaries Set of {@link DataDictionaryPojo} to be populated with the data dictionary referencing Business variable
	 */
	private void checkForNestedIfElseCondition(final List<AstNodePojo> blocks, final Set<String> businessVariables, final List<AnnotationMetadata> metadataList) {
		final List<AstNodePojo> branches = blocks.stream().map(BRANCH_STATEMENT_COLLECTOR::all).flatMap(List::stream).collect(Collectors.toList());
		if (branches.stream().anyMatch(node2 -> checkForBranchMetadata(node2, businessVariables) && ! isEvaluate(node2))) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.NESTED_IF_ELSE_CONDITION));
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#NESTED_EVALUATE_CONDITION}
	 *
	 * @param blocks list of AST node to be checked
	 * @param businessVariables Set of business variable labels
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 */
	private void checkForNestedCblEvaluateCondition(final List<AstNodePojo> blocks, final Set<String> businessVariables,
			final List<AnnotationMetadata> metadataList) {
		final List<AstNodePojo> branches = blocks.stream().map(BRANCH_STATEMENT_COLLECTOR::all).flatMap(List::stream).collect(Collectors.toList());
		if (branches.stream().anyMatch(node2 -> checkForBranchMetadata(node2, businessVariables) && isEvaluate(node2))) {
			metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.NESTED_EVALUATE_CONDITION));
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#COMPUTATION}
	 *
	 * @param blocks list of AST node to be checked
	 * @param businessVariables Set of business variable labels
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 */
	private void checkForComputeCondition(final List<AstNodePojo> blocks, final Set<String> businessVariables, final List<AnnotationMetadata> metadataList) {
		for (final AstNodePojo block : blocks) {
			final Set<String> labels = ARITHMETIC_COLLECTOR.all(block).stream().map(expression ->
					VARIABLE_COLLECTOR.allDeep(expression).stream().map(AstNodeUtils::getFieldDefinitionNameFromReference).collect(Collectors.toList()))
							.flatMap(List::stream)
							.collect(Collectors.toSet());
			if (CollectionUtils.containsAny(labels, businessVariables)) {
				metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.COMPUTATION));
				return;
			}
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#INPUT_FROM_EXTERNAL_DATA_SOURCE}
	 * or {@linkplain AnnotationMetaDataReasonEnum#OUTPUT_TO_EXTERNAL_DATA_SOURCE}
	 *
	 * @param blocks list of AST node to be checked
	 * @param businessVariable the Set of BusinessVariables
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 */
	private void checkForInputOutputExtDataSrc(final List<AstNodePojo> blocks, final Set<String> businessVariables, final List<AnnotationMetadata> metadataList) {
		final List<AstNodePojo> fileDbNodes = blocks.stream()
				.map(FILE_DATA_COLLECTOR::all)
				.flatMap(List::stream)
				.collect(Collectors.toList());
		boolean inputPresent = false;
		boolean outputPresent = false;
		for (final AstNodePojo node : fileDbNodes) {
			final Set<String> variables = node.getChildren().stream().filter(n -> ! hasAnySuperType(n, BRANCH, DEFAULT_BRANCH))
					.map(VARIABLE_COLLECTOR::allDeep)
					.flatMap(List::stream)
					.map(AstNodeUtils::getFieldDefinitionNameFromReference)
					.collect(Collectors.toSet());
			if (CollectionUtils.containsAny(variables, businessVariables)) {
				if ( ! inputPresent && FILE_READ_TYPES.contains(node.getType())) {
					metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.INPUT_FROM_EXTERNAL_DATA_SOURCE));
					inputPresent = true;
				} else {
					metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.OUTPUT_TO_EXTERNAL_DATA_SOURCE));
					outputPresent = true;
				}
			}
			
			if (inputPresent && outputPresent) {
				return;
			}
		}
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#LOOP_CONDITION}
	 *
	 * @param node the AST node that is examined
	 * @param businessVariables Set of business variable labels
	 * @return boolean indicating if the candidate has loop metadata
	 */
	private boolean checkForLoopCondition(final AstNodePojo node, final Set<String> businessVariables) {
		final List<AstNodePojo> nodes = LOOP_STATEMENT_COLLECTOR.all(node);
		for (final AstNodePojo astNode : nodes) {
			if (AstNodeUtils.isCobolPerformStatementLoop(astNode)) {
				final List<AstNodePojo> comparisonNodes = CollectionUtils.emptyIfNull(astNode.getChildren()).stream()
						.filter(n -> COMPARISON_EXP_TYPES.contains(n.getType()))
						.collect(Collectors.toList());
				final Set<String> fields = comparisonNodes.stream().map(n -> FIELD_REFERENCE_COLLECTOR.allDeep(n).stream()
						.map(AstNodeUtils::getFieldDefinitionNameFromReference).collect(Collectors.toList()))
						.flatMap(List::stream)
						.collect(Collectors.toSet());
				if (CollectionUtils.containsAny(fields, businessVariables)) {
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * Checks if the given {@code node} is a BR candidate because of {@linkplain AnnotationMetaDataReasonEnum#NESTED_LOOP_CONDITION}
	 *
	 * @param blocks the AST node that is examined
	 * @param businessVariables Set of business variable labels
	 * @param metadataList the {@link AnnotationMetadata} to be populated
	 */
	private void checkForNestedLoopCondition(final List<AstNodePojo> blocks, final Set<String> businessVariables, final List<AnnotationMetadata> metadataList) {
		for (final AstNodePojo node : blocks) {
			if (CollectionUtils.emptyIfNull(node.getChildren()).stream().anyMatch(n2 -> checkForLoopCondition(n2, businessVariables))) {
				metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.NESTED_LOOP_CONDITION));
				return ;
			}
		}
	}
	
	private void checkForBusinessVariableTransformation(final List<AstNodePojo> blocks, final Set<String> businessVariables,
			final List<AnnotationMetadata> metadataList) {
		for (final AstNodePojo block : blocks) {
			final List<AstNodePojo> assignmentNodes = ASSIGNMENT_COLLECTOR.all(block);
			for (final AstNodePojo assignmentNode : assignmentNodes) {
			if (CollectionUtils.containsAny(getAssignedVariableNames(assignmentNode), businessVariables)) {
				metadataList.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.BV_TRANSFORMATION));
				return;
			}
		}
		}
	}
	
	private List<String> getAssignedVariableNames(final AstNodePojo astNode){
		return AstNodeUtils.getLeftOperands(astNode).stream()
				.map(AstNodeUtils::getFieldDefinitionNameFromReference)
				.collect(Collectors.toList());
	}
	
	/**
	 * Traverses through the list of {@linkplain Annotation} annotations and removes all the Incorporate Nested Conditions
	 * 
	 * @param annotations the list of {@linkplain Annotation} annotations
	 */
	private List<AnnotationPojoTemplate> removeNestedAnnotations(final List<AnnotationPojoTemplate> annotations) {
		final List<AnnotationPojoTemplate> filteredAnnotations = new ArrayList<>();
		final Comparator<AnnotationPojoTemplate> comparator = (ann1, ann2) -> {
			if (getStartOffset(ann1).equals(getStartOffset(ann2))) {
				return Integer.compare(getEndOffset(ann2), getEndOffset(ann1));
			}
			return Integer.compare(getStartOffset(ann1), getStartOffset(ann2));
		};
		annotations.sort(comparator);
		int firstpointer = 0;
		int secondpointer = 1;
		ModuleLocation moduleLocation1 = annotations.isEmpty() ? null : annotations.get(firstpointer).location.get();
		while (firstpointer <= secondpointer && secondpointer < annotations.size() && moduleLocation1 != null) {
			final ModuleLocation moduleLocation2 = Assert.assertNotNull(annotations.get(secondpointer).location.get());
			if(moduleLocation2.isWithin(moduleLocation1)) {
				final AnnotationPojoPrototype firstPointerAnnotation = annotations.get(firstpointer);
				final Set<String> combinedMetadata = new HashSet<>(firstPointerAnnotation.reasons.getNonNull());
				combinedMetadata.addAll(annotations.get(secondpointer).reasons.getNonNull());
				firstPointerAnnotation.setReasons(new ArrayList<>(combinedMetadata));

				secondpointer++;
			} else {
				filteredAnnotations.add(annotations.get(firstpointer));
				firstpointer = secondpointer;
				secondpointer++;
				moduleLocation1 = annotations.get(firstpointer).location.get();
			}
		}
		if (firstpointer < annotations.size()) {
			filteredAnnotations.add(annotations.get(firstpointer));
		}
		return filteredAnnotations;
	}

	private Integer getStartOffset(final AnnotationPojoPrototype annotation) {
		final ModuleLocation moduleLocation = annotation.location.getNonNull();
		return moduleLocation.getOffset();
	}
	
	private Integer getEndOffset(final AnnotationPojoPrototype annotation) {
		final ModuleLocation moduleLocation = annotation.location.getNonNull();
		return moduleLocation.getOffset() + moduleLocation.getLength();
	}
	
	
	private boolean isEvaluate(final AstNodePojo node) {
		return CobolEvaluateStmt.class.getSimpleName().equals(node.getType());
	}

	@Override
	protected List<AstNodePojo> getCandidateNodes(final AstNodePojo root) {
		return Collections.emptyList();
	}

	@Override
	protected String getAnnotationName() {
		return BusinessRuleIdentifier.ANNOTATION_NAME;
	}
}
