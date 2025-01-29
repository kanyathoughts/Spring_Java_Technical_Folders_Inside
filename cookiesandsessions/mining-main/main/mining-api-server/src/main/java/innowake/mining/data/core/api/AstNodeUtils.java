/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.ndt.cobol.parser.ast.model.CobolAndExpression;
import innowake.ndt.cobol.parser.ast.model.CobolComparisonExpression;
import innowake.ndt.cobol.parser.ast.model.CobolOrExpression;
import innowake.ndt.core.parsing.ast.model.ArithmeticExpression;
import innowake.ndt.core.parsing.ast.model.Branch;
import innowake.ndt.core.parsing.ast.model.BranchStatement;
import innowake.ndt.core.parsing.ast.model.BreakStatement;
import innowake.ndt.core.parsing.ast.model.ConstantReference;
import innowake.ndt.core.parsing.ast.model.ContinueStatement;
import innowake.ndt.core.parsing.ast.model.DefaultBranch;
import innowake.ndt.core.parsing.ast.model.Directive;
import innowake.ndt.core.parsing.ast.model.Entry;
import innowake.ndt.core.parsing.ast.model.ErrorProcessingStatement;
import innowake.ndt.core.parsing.ast.model.HaltStatement;
import innowake.ndt.core.parsing.ast.model.Invocable;
import innowake.ndt.core.parsing.ast.model.LoopControlStatement;
import innowake.ndt.core.parsing.ast.model.LoopEventStatement;
import innowake.ndt.core.parsing.ast.model.LoopStatement;
import innowake.ndt.core.parsing.ast.model.ModuleReturnStatement;
import innowake.ndt.core.parsing.ast.model.ReturnStatement;
import innowake.ndt.core.parsing.ast.model.Statement;
import innowake.ndt.core.parsing.ast.model.cfg.CfgCollapsibleNode;
import innowake.ndt.core.parsing.ast.model.statement.ArithmeticStatement;
import innowake.ndt.core.parsing.ast.model.statement.AssigningStatement;
import innowake.ndt.core.parsing.ast.model.statement.BindingNode;
import innowake.ndt.core.parsing.ast.model.statement.CallExternalStatement;
import innowake.ndt.core.parsing.ast.model.statement.CallInternalStatement;
import innowake.ndt.core.parsing.ast.model.statement.CallStatement;
import innowake.ndt.core.parsing.ast.model.statement.DatabaseAccessStatement;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.ast.model.statement.FieldReference;
import innowake.ndt.core.parsing.ast.model.statement.FileAccessStatement;
import innowake.ndt.core.parsing.ast.model.statement.JumpStatement;
import innowake.ndt.core.parsing.ast.model.statement.UiStatement;
import innowake.ndt.pl1parser.ast.model.declaration.StructureDeclaration;
import innowake.ndt.pl1parser.ast.model.statement.block.BeginBlock;
import innowake.ndt.pl1parser.ast.model.statement.block.PackageBlock;
import innowake.ndt.pl1parser.ast.model.statement.block.PreprocessorProcedureBlock;
import innowake.ndt.pl1parser.ast.model.statement.block.ProcedureBlock;

/**
 * Helper class for interacting with {@link AstNodePojo}. 
 */
public final class AstNodeUtils {

	/** Super type {@link Statement}. */
	public static final String STATEMENT = Statement.class.getSimpleName();
	/** Super type {@link Invocable}. */
	public static final String INVOCABLE = Invocable.class.getSimpleName();
	/** Super type {@link Entry}. */
	public static final String ENTRY = Entry.class.getSimpleName();
	/** Super type {@link BranchStatement}. */
	public static final String BRANCH_STATEMENT = BranchStatement.class.getSimpleName();
	/** Super type {@link Branch}. */
	public static final String BRANCH = Branch.class.getSimpleName();
	/** Super type {@link DefaultBranch}. */
	public static final String DEFAULT_BRANCH = DefaultBranch.class.getSimpleName();
	/** Super type {@link Directive}. */
	public static final String DIRECTIVE = Directive.class.getSimpleName();
	/** Super type {@link JumpStatement}. */
	public static final String JUMP_STATEMENT = JumpStatement.class.getSimpleName();
	/** Super type {@link FileAccessStatement}. */
	public static final String FILE_ACCESS_STATEMENT = FileAccessStatement.class.getSimpleName();
	/** Super type {@link DatabaseAccessStatement}. */
	public static final String DATABASE_ACCESS_STATEMENT = DatabaseAccessStatement.class.getSimpleName();
	/** Super type {@link UiStatement}. */
	public static final String UI_STATEMENT = UiStatement.class.getSimpleName();
	/** Super type {@link ArithmeticStatement}. */
	public static final String ARITHMETIC_STATEMENT = ArithmeticStatement.class.getSimpleName();
	/** Super type {@link LoopStatement}. */
	public static final String LOOP_STATEMENT = LoopStatement.class.getSimpleName();
	/** Super type {@link LoopControlStatement}. */
	public static final String LOOP_CONTROL_STATEMENT = LoopControlStatement.class.getSimpleName();
	/** Super type {@link ContinueStatement}. */
	public static final String CONTINUE_STATEMENT = ContinueStatement.class.getSimpleName();
	/** Super type {@link BreakStatement}. */
	public static final String BREAK_STATEMENT = BreakStatement.class.getSimpleName();
	/** Super type {@link ReturnStatement}. */
	public static final String RETURN_STATEMENT = ReturnStatement.class.getSimpleName();
	/** Super type {@link ModuleReturnStatement}. */
	public static final String MODULE_RETURN_STATEMENT = ModuleReturnStatement.class.getSimpleName();
	/** Super type {@link CallStatement}. */
	public static final String CALL_STATEMENT = CallStatement.class.getSimpleName();
	/** Super type {@link CallStatement}. */
	public static final String CALL_INTERNAL_STATEMENT = CallInternalStatement.class.getSimpleName();
	/** Super type {@link CallStatement}. */
	public static final String CALL_EXTERNAL_STATEMENT = CallExternalStatement.class.getSimpleName();
	/** Super type {@link HaltStatement}. */
	public static final String HALT_STATEMENT = HaltStatement.class.getSimpleName();
	/** Super type {@link LoopEventStatement}. */
	public static final String LOOP_EVENT_STATEMENT = LoopEventStatement.class.getSimpleName();
	/** Super type {@link FieldDefinition}. */
	public static final String FIELD_DEFINITION = FieldDefinition.class.getSimpleName();
	/** Super type {@link FieldReference}. */
	public static final String FIELD_REFERENCE = FieldReference.class.getSimpleName();
	/** Super type {@link ConstantReference}. */
	public static final String CONSTANT_REFERENCE = ConstantReference.class.getSimpleName();
	/** Type {@link ArithmeticExpression}. */
	public static final String ARITHMETIC_EXPRESSION = ArithmeticExpression.class.getSimpleName();
	/** Type {@link AssigningStatement}. */
	public static final String ASSIGNING_STATEMENT = AssigningStatement.class.getSimpleName();
	/** Type {@link BindingNode} */
	public static final String BINDING_NODE = BindingNode.class.getSimpleName();
	/** Type Cobol or Natural inclusion node. */
	public static final String INCLUSION_NODE = "INCLUSION_NODE";
	/** Type {@link ErrorProcessingStatement} */
	public static final String ERROR_PROCESSING_STATEMENT = ErrorProcessingStatement.class.getSimpleName();
	/** Type {@link CfgCollapsibleNode} */
	public static final String CFG_COLLAPSIBLE_NODE = CfgCollapsibleNode.class.getSimpleName();
	/** Property for storing paths of {@link BranchStatement}s to its conditions */
	public static final String PROPERTY_CONDITION_PATHS = "conditionPaths";
	/** Type {@link ProcedureBlock} */
	public static final String PROCEDURE_BLOCK = ProcedureBlock.class.getSimpleName();
	/** Type {@link PreprocessorProcedureBlock} */
	public static final String PREPROCESSOR_PROCEDURE_BLOCK = PreprocessorProcedureBlock.class.getSimpleName();
	/** Type {@link PackageBlock} */
	public static final String PACKAGE_BLOCK = PackageBlock.class.getSimpleName();
	/** Type {@link BeginBlock} */
	public static final String BEGIN_BLOCK = BeginBlock.class.getSimpleName();
	/** Type {@link StructureDeclaration} */
	public static final String STRUCTURAL_DECLARATION = StructureDeclaration.class.getSimpleName();

	private static final Set<String> COMPARISON_EXP_TYPES = Sets.newHashSet(CobolComparisonExpression.class.getSimpleName(),
			CobolAndExpression.class.getSimpleName(), CobolOrExpression.class.getSimpleName());
	private static final String EQUALS_OPERATOR = "=";
	private static final AstNodeCollector FIELD_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(FIELD_REFERENCE));

	private AstNodeUtils() {}
	
	/**
	 * Informs about if at least one of the {@code types} are contained in {@code AstNode#getSuperTypes()}.
	 *
	 * @param node the {@link AstNodePojo}
	 * @param types the requested super types
	 * @return {@code true} if at least one of the {@code types} are contained in {@code AstNode#getSuperTypes()}
	 */
	public static boolean hasAnySuperType(final AstNodePojo node, final String... types) {
		return Arrays.stream(types).anyMatch(node.getSuperTypes()::contains);
	}
	
	/**
	 * Informs about if at least one of the {@code types} are equals in {@code AstNode#getType()}.
	 *
	 * @param node the {@link AstNodePojo}
	 * @param types the requested super types
	 * @return {@code true} if at least one of the {@code types} are contained in {@code AstNode#getType()}
	 */
	public static boolean hasAnyType(final AstNodePojo node, final String... types) {
		return Arrays.stream(types).anyMatch(node.getType():: equals);
	}

	/**
	 * Informs about if at least one of the {@code types} are contained in at least one of the {@code nodes} {@code AstNode#getSuperTypes()}.
	 *
	 * @param nodes the {@link AstNodePojo}s
	 * @param types the requested super types
	 * @return {@code true} if at least one of the {@code types} are contained in at least one of the {@code nodes} {@code AstNode#getSuperTypes()}
	 */
	public static boolean hasAnySuperType(final List<AstNodePojo> nodes, final String... types) {
		return nodes.stream().anyMatch(node -> hasAnySuperType(node, types));
	}
	
	/**
	 * Returns all direct (non-deep) children with any of the given super types.
	 *
	 * @param node the node to traverse
	 * @param superTypes the super types
	 * @return the children
	 */
	public static List<AstNodePojo> getChildren(final AstNodePojo node, final String... superTypes) {
		return new AstNodeCollector(n -> ! node.equals(n) &&  hasAnySuperType(n, superTypes)).all(node);
	}
	
	/**
	 * Returns all direct and deep children with any of the given super types.
	 *
	 * @param node the node to traverse
	 * @param superTypes the super types
	 * @return the children
	 */
	public static List<AstNodePojo> getChildrenDeep(final AstNodePojo node, final String... superTypes) {
		return new AstNodeCollector(n -> ! node.equals(n) &&  hasAnySuperType(n, superTypes)).allDeep(node);
	}

	public static List<AstNodePojo> getChildrenDeepWithType(final AstNodePojo node, final String... types) {
		return new AstNodeCollector(n -> ! node.equals(n) &&  hasAnyType(n, types)).allDeep(node);
	}
	
	/**
	 * Returns the first direct (non-deep) child with any of the given super types.
	 *
	 * @param node the node to traverse
	 * @param superTypes the super types
	 * @return the child
	 */
	public static Optional<AstNodePojo> getFirstChild(final AstNodePojo node, final String... superTypes) {
		return new AstNodeCollector(n -> ( ! node.equals(n) && hasAnySuperType(n, superTypes))).first(node);
	}
	
	/**
	 * Returns the last direct (non-deep) child with any of the given super types.
	 *
	 * @param node the node to traverse
	 * @param superTypes the super types
	 * @return the child
	 */
	public static Optional<AstNodePojo> getLastChild(final AstNodePojo node, final String... superTypes) {
		return new AstNodeCollector(n -> ! node.equals(n) &&  hasAnySuperType(n, superTypes)).last(node);
	}

	/**
	 * Returns the next sibling or direct (non-deep) or indirect (deep) parent sibling after the given node with any of the given super types.
	 *
	 * @param node the node to traverse
	 * @param superTypes the super types
	 * @return the child
	 */
	public static Optional<AstNodePojo> getNextSiblingDeep(final AstNodePojo node, final String... superTypes) {
		return node.getNextSibling().flatMap(sibling -> hasAnySuperType(sibling, superTypes) ? Optional.of(sibling) : getNextSiblingDeep(sibling, superTypes));
	}
	
	/**
	 * Returns the next sibling or direct (non-deep) or indirect (deep) parent sibling after the given node with any of the given super types.
	 *
	 * @param node the node to traverse
	 * @param superTypes the super types
	 * @return the child
	 */
	public static Optional<AstNodePojo> getNextSiblingOrParentSiblingDeep(final AstNodePojo node, final String... superTypes) {
		return node.getNextSibling()
			.flatMap(sibling -> hasAnySuperType(sibling, superTypes) ? Optional.of(sibling) : getNextSiblingOrParentSiblingDeep(sibling, superTypes))
			.or(() -> node.getParent().flatMap(parent -> getNextSiblingOrParentSiblingDeep(parent, superTypes)));
	}
	
	/**
	 * Returns the next sibling or direct (non-deep) or indirect (deep) parent sibling after the given node
	 *
	 * @param node the node to traverse
	 * @return the sibling
	 */
	public static Optional<AstNodePojo> getNextSiblingOrParentSiblingDeep(final AstNodePojo node) {
		return node.getNextSibling().or(() -> node.getParent().flatMap(AstNodeUtils::getNextSiblingOrParentSiblingDeep));
	}
	
	/**
	 * Returns the paragraph a node is part of.
	 *
	 * @param currentNode the node
	 * @return the paragraph the node is part of, or {@code null} if the node is not part of a paragraph.
	 */
	@Nullable
	public static AstNodePojo getParagraph(final AstNodePojo currentNode) {
		AstNodePojo parent = currentNode.getParent().orElse(null);
		while (parent != null) {
			if (CobolAstNodeType.LABEL.getType().equals(parent.getType())) {
				return parent;
			}
			parent = parent.getParent().orElse(null);
		}
		return null;
	}
	
	/**
	 * Returns the section a node is part of.
	 *
	 * @param currentNode the node
	 * @return the section that the node is part of, or {@code null} if the node is not part of a section.
	 */
	@Nullable
	public static AstNodePojo getSection(final AstNodePojo currentNode) {
		AstNodePojo parent = currentNode.getParent().orElse(null);
		while (parent != null) {
			if (CobolAstNodeType.SECTION.getType().equals(parent.getType())) {
				return parent;
			}
			parent = parent.getParent().orElse(null);
		}
		return null;
	}
	
	/**
	 * Checks if a certain property is set for an {@link AstNodePojo}
	 *
	 * @param node the node that is checked for the property
	 * @param propertyName the name of the property the node is checked for
	 * @return {@code true} if the property is set and has a value equal to true; {@code false} otherwise
	 */
	public static boolean isTrue(final AstNodePojo node, final String propertyName) {
		final Object propertyValue = node.getProperties().get(propertyName);
		return propertyValue != null && Boolean.parseBoolean(propertyValue.toString());
	}

	/**
	 * Returns {@code true} if the given {@code node} is an inclusion node or an ancestor of one.
	 * <p>The upwards traversing stops if a parent node matches with the given {@code stopNode}.
	 *
	 * @param node The {@link AstNodePojo} to test
	 * @param stopNode The {@link AstNodePojo} to stop traversing
	 * @return {@code true} if the given {@code node} is an inclusion node or an ancestor of one. Otherwise {@code false}
	 */
	public static boolean isFromInclusion(final AstNodePojo node, final AstNodePojo stopNode) {
		AstNodePojo test = node;
		while (test != null && test != stopNode) {
			if (test.getSuperTypes().contains(AstNodeUtils.INCLUSION_NODE)) {
				return true;
			}

			test = test.getParent().orElse(null);
		}

		return false;
	}

	/**
	 * @param branchStatement The {@link AstNodePojo} representing a branch node
	 * @return list of condition {@link AstNodePojo AstNodes} of the given {@code branchStatement}
	 */
	public static List<AstNodePojo> getBranchConditions(final AstNodePojo branchStatement) {
		final String paths = AstService.Properties.CONDITION_PATHS.getFrom(branchStatement.getProperties());
		if (paths == null) {
			return Collections.emptyList();
		}

		final ArrayList<String> pathArray = getPathElements(paths);
		final List<AstNodePojo> conditions = new ArrayList<>(pathArray.size());
		for (final String path : pathArray) {
			final ArrayList<String> indexArray = getPathElements(path);

			AstNodePojo node = branchStatement;
			for (final String sIndex : indexArray) {
				node = node.getChildren().get(Integer.parseInt(sIndex));
			}

			conditions.add(node);
		}

		return conditions;
	}

	/**
	 * @param loopStatement The {@link AstNodePojo} representing a loop node
	 * @return list of condition {@link AstNodePojo AstNodes} of the given {@code loopStatement}
	 */
	public static List<AstNodePojo> getLoopConditions(final AstNodePojo loopStatement){
		if (AstNodeUtils.isCobolPerformStatementLoop(loopStatement)) {
			return CollectionUtils.emptyIfNull(loopStatement.getChildren()).stream()
					.filter(n -> COMPARISON_EXP_TYPES.contains(n.getType()))
					.collect(Collectors.toList());
		}
		return Collections.emptyList();
	}

	/**
	 * Returns the top elements from the given {@code str} by first removing the most outer '[' and ']' and by splitting the result around the outer commas.
	 * <ul>
	 * <li>For <b>"[1, 2, 3]"</b> a list with the strings: <b>"1", "2", "3"</b> is returned</li>
	 * <li>For <b>"[1, [a, b, c], 3, [d, e]]"</b> a list with the strings: <b>"1", "[a, b, c]", "3", "[d, e]"</b> is returned</li>
	 * </ul>
	 *
	 * @param str the string to split
	 * @return the top elements as a list
	 */
	private static ArrayList<String> getPathElements(final String str) {
		int start = str.indexOf('[');
		if (start == -1) {
			throw new IllegalArgumentException(String.format("Condition path must start with '[' but is: >%s<", str));
		}

		final int end = str.lastIndexOf(']');
		if (end == -1) {
			throw new IllegalArgumentException(String.format("Condition path must end with ']' but is: >%s<", str));
		}

		final ArrayList<String> paths = new ArrayList<>();
		boolean ignore = false;
		start++;
		for (int i = start; i < end; i++) {
			switch (str.charAt(i)) {
				case ',':
					if ( ! ignore) {
						paths.add(str.substring(start, i));
						start = i + 1;
					}
					break;
				case '[':
					ignore = true;
					break;
				case ']':
					ignore = false;
					break;
				default:
					break;
			}
		}
		if (start < end) {
			paths.add(str.substring(start, end));
		}

		return paths;
	}

	/**
	 * Sets the path of the conditions of the given {@code branchStatement} into the properties map of {@code astNode}.
	 *
	 * @param branchStatement the {@link BranchStatement} whose conditions are examined
	 * @param astNode StoreAstNode to set the paths into
	 */
	public static void setConditionPaths(final BranchStatement branchStatement, final AstNodePojoPrototype astNode) {
		final List<innowake.ndt.core.parsing.ast.AstNode> conditions = branchStatement.getConditions();
		/* Natural READ WORK FILE is a branch statement without conditions */
		if ( ! conditions.isEmpty()) {
			/* Contains all paths from the branch statement to all of its condition nodes. A path consists of the indices of child nodes */
			final StringBuilder paths = new StringBuilder("[");
			for (int i = 0; i < conditions.size(); i++) {
				if (i != 0) {
					paths.append(',');
				}
				paths.append('[');
				getAstNodeIndex(branchStatement, conditions.get(i), paths);
				paths.append(']');
			}
			paths.append(']');

			AstService.Properties.CONDITION_PATHS.setIn(astNode.properties.getOrSet(HashMap::new), paths.toString());
		}
	}
	
	/**
	 * @param node The {@link AstNodePojo} to test
	 * @return {@code true} if the given {@code node} is an {@code BRANCH_STATEMENT}. Otherwise {@code false}
	 */
	public static boolean isBranchStatementNode(final AstNodePojo node) {
		return node.getSuperTypes().contains(BRANCH_STATEMENT);
	}
	
	/**
	 * @param node The {@link AstNodePojo} to test
	 * @return {@code true} if the given {@code node} is a Loop. Otherwise {@code false}
	 */
	public static boolean isCobolPerformStatementLoop(final AstNodePojo node) {
		return node.getProperties().get("isLoop") != null && (Boolean) node.getProperties().get("isLoop");
	}
	
	/**
	 * Traverse through the parents of node to check if the supertypes of the parent have input superType or not
	 *
	 * @param superTypes Types to be checked for parent nodes
	 * @param node {@link AstNodePojo} object
	 * @return {@code true} if the supertype of parents of given {@code node} contains any input supertype. Otherwise {@code false}
	 */
	public static boolean isEnclosedInSuperType(final Set<String> superTypes, final AstNodePojo node) {
		Optional<AstNodePojo> currentNode = node.getParent();
		while (currentNode.flatMap(AstNodePojo::getParent).isPresent()) {
			if (CollectionUtils.containsAny(currentNode.get().getSuperTypes(), superTypes)) {
				return true;
			}
			currentNode = currentNode.get().getParent();
		}
		return false;
	}
	
	/**
	 * @param node The {@link AstNodePojo} to test
	 * @return {@code true} if the given {@code node} is a Cobol Exit Paragraph. Otherwise {@code false}
	 */
	public static boolean isCobolExitParagraph(final AstNodePojo node) {
		/* Skip Cobol EXIT paragraphs for the business rule candidates identification */
		if (node.getSuperTypes().contains(INVOCABLE) && CobolAstNodeType.LABEL.getType().equals(node.getType())) {
			final Object name = node.getProperties().get(Invocable.INVOCATION_NAME);
			return name instanceof String && ((String) name).endsWith("-EXIT");
		}

		return false;
	}
	
	/**
	 * @param node The {@link AstNodePojo} to test
	 * @return {@code true} if the given {@code node} is an {@code INCLUSION_NODE}. Otherwise {@code false}
	 */
	public static boolean isInclusionNode(final AstNodePojo node) {
		return node.getSuperTypes().contains(INCLUSION_NODE);
	}
	
	/**
	 * Gets the name of the Field from a {@linkplain FieldDefinition} or {@linkplain FieldReference}.
	 *
	 * @param reference the {@linkplain FieldDefinition} or {@linkplain FieldReference} {@linkplain AstNodePojo}
	 * @return the name of the Field or empty string if unable to determine
	 */
	public static String getFieldDefinitionNameFromReference(final AstNodePojo reference) {
		return getDefinitionNameAndUsageFromReference(reference, null, false).a;
	}
	
	/**
	 * Gets the {@linkplain FieldDefinition} from a {@linkplain FieldReference}.
	 *
	 * @param reference the {@linkplain FieldReference} {@linkplain AstNodePojo}
	 * @return the {@linkplain FieldDefinition} or {@linkplain FieldReference} itself if unable to determine
	 */
	public static AstNodePojo getFieldDefinitionFromReference(final AstNodePojo reference) {
		return getDefinitionNameAndUsageFromReference(reference, null, true).b;
	}

	/**
	 * Gets the name of the Field from a {@linkplain FieldDefinition} or {@linkplain FieldReference} along with the {@linkplain FieldDefinition} and all
	 * it's usages in the current {@linkplain Module}.
	 *
	 * @param reference the {@linkplain FieldDefinition} or {@linkplain FieldReference} {@linkplain AstNodePojo}
	 * @param allFieldReferences a list to store all the references of the field in the code, <em>use {@code null} if <b>not</b> required</em>
	 * @param definitionNeeded {@code true} if {@linkplain FieldDefinition} is needed
	 * @return the name of the Field or empty string if unable to determine
	 */
	public static Tuple2<String, AstNodePojo> getDefinitionNameAndUsageFromReference(final AstNodePojo reference,
			final @Nullable List<AstNodePojo> allFieldReferences, final boolean definitionNeeded) {
		if (reference.getSuperTypes().contains(FIELD_DEFINITION)) {
			if (allFieldReferences != null) {
				allFieldReferences.addAll(reference.getIncomingRelations().stream()
						.filter(r -> r.getType() == AstRelationshipType.BINDING)
						.map(AstRelationshipPojo::getSrcNode).collect(Collectors.toList()));
			}
			return Tuple2.of(StringUtils.trimToEmpty(AstService.Properties.FIELD_NAME.getFrom(reference.getProperties())), reference);
		}
		
		final String fieldName = AstService.Properties.UNRESOLVED_FIELD_NAME.getFrom(reference.getProperties());
		if (fieldName == null || definitionNeeded || allFieldReferences != null) {
			final List<AstNodePojo> definition = reference.getOutgoingRelations().stream()
															.filter(r -> r.getType() == AstRelationshipType.BINDING)
															.map(AstRelationshipPojo::getDstNode).collect(Collectors.toList());
			
			if (definition.isEmpty()) {
				return fieldName == null ? Tuple2.of(StringUtils.EMPTY, reference) : Tuple2.of(fieldName, reference);
			}
			final var fieldDefinition = definition.get(0);
			
			if (allFieldReferences != null) {
				allFieldReferences.addAll(fieldDefinition.getIncomingRelations().stream()
															.filter(r -> r.getType() == AstRelationshipType.BINDING)
															.map(AstRelationshipPojo::getSrcNode).collect(Collectors.toList()));
			}
			return Tuple2.of(StringUtils.trimToEmpty(AstService.Properties.FIELD_NAME.getFrom(fieldDefinition.getProperties())), fieldDefinition);
		}
		return Tuple2.of(fieldName, reference);
	}

	/**
	 * Returns the parent, if any, that has the provided {@code superType}.
	 *
	 * @param astNode the {@linkplain AstNodePojo}
	 * @param superType the superType
	 * @return the parent if exists, or empty
	 */
	public static Optional<AstNodePojo> getParentOfSuperType(final AstNodePojo astNode, final String superType) {
		AstNodePojo parent = astNode;
		while (parent != null) {
			if (parent.getSuperTypes().contains(superType)) {
				return Optional.of(parent);
			}
			parent = parent.getParent().orElse(null);
		}
		return Optional.empty();
	}

	/**
	 * Get statement node and return the left operands of statement
	 *
	 * @param node the {@linkplain AstNodePojo}
	 * @return list of AstNodePojo which are related to left operand of statements
	 */
	public static List<AstNodePojo> getLeftOperands(final AstNodePojo node) {
		final List<AstNodePojo> allFields = FIELD_COLLECTOR.allDeep(node);
		final List<AstNodePojo> leftOperandNodes = new ArrayList<>();
		for (final AstNodePojo fieldReferenceNode : allFields) {
			if (findResultVariableNames(node).contains(fieldReferenceNode.getLabel())
					&& leftOperandNodes.stream().noneMatch(existNode -> existNode.getLabel().equals(fieldReferenceNode.getLabel()))
					&& fieldReferenceNode.getOutgoingRelations().stream().filter(r -> r.getType() == AstRelationshipType.BINDING).count() != 0) {
				leftOperandNodes.add(fieldReferenceNode);
			}
		}
		return leftOperandNodes;
	}

	private static void getAstNodeIndex(final BranchStatement branchStatement, final innowake.ndt.core.parsing.ast.AstNode node, final StringBuilder indices) {
		/* Recursion. If the current parent != branchStatement, continue immediately with the parent of the parent.
		 * Sets the indices in the right order for the path from the branchStatement to the condition node */
		if (node.getParent() != branchStatement) {
			getAstNodeIndex(branchStatement, node.getParent(), indices);
			indices.append(',');
		}
		final int index = node.getParent().getChildren().indexOf(node);
		if (index == -1) {
			throw new IllegalStateException("Index of child node must not be -1");
		}

		indices.append(index);
	}
	
	private static List<String> findResultVariableNames(final AstNodePojo node) {
		final String nodeLable = node.getLabel();
		String resultPartOfStatement = null;
		if (nodeLable.contains(EQUALS_OPERATOR)) {
			resultPartOfStatement = removeKeyWords(node.getLabel().substring(0, nodeLable.indexOf(EQUALS_OPERATOR)));
		} else {
			resultPartOfStatement = findRightSideOfStatement(nodeLable);
		}
		return Arrays.asList(resultPartOfStatement.split("\\s+"));
	}
	
	private static String findRightSideOfStatement(final String nodeLable) {
		int index = -1;
		if ((index = nodeLable.indexOf(" GIVING ")) != -1) {
			return removeKeyWords(nodeLable.substring(index + 8, nodeLable.length()));
		} else if ((index = nodeLable.indexOf(" INTO ")) != -1) {
			return removeKeyWords(nodeLable.substring(index + 6, nodeLable.length()));
		} else if ((index = nodeLable.indexOf(" TO ")) != -1) {
			return removeKeyWords(nodeLable.substring(index + 4, nodeLable.length()));
		} else if ((index = nodeLable.indexOf(" FROM ")) != -1) {
			return removeKeyWords(nodeLable.substring(index + 6, nodeLable.length()));
		} else if ((index = nodeLable.indexOf(" BY ")) != -1) {
			return removeKeyWords(nodeLable.substring(index + 4, nodeLable.length()));
		}
		return nodeLable;
	}
	
	private static String removeKeyWords(final String statement) {
		return statement.replaceAll("REMAINDER|COMPUTE", ""); 
	}
}
