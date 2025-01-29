/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.core.candidate;

import static innowake.mining.data.core.api.AstNodeUtils.ARITHMETIC_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.DATABASE_ACCESS_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.DEFAULT_BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.FILE_ACCESS_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.ASSIGNING_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.hasAnySuperType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Provides entry points for business variable identification.
 */
class BusinessVariableIdentifier {

	private static final AstNodeCollector ARITHMETIC_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(ARITHMETIC_STATEMENT));
	private static final AstNodeCollector FIELD_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(FIELD_REFERENCE));
	private static final AstNodeCollector VARIABLE_COLLECTOR =
			new AstNodeCollector(node ->
				node.getSuperTypes().contains(FIELD_REFERENCE) &&
					node.getChildren().stream().noneMatch(child -> FIELD_COLLECTOR.firstDeep(child).isPresent()));
	private static final AstNodeCollector FILE_DATA_COLLECTOR = new AstNodeCollector(node -> hasAnySuperType(node, FILE_ACCESS_STATEMENT,
			DATABASE_ACCESS_STATEMENT));
	protected static final AstNodeCollector ASSIGNMENT_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(ASSIGNING_STATEMENT));
	@Nullable
	private final AstNodePojo rootNode;
	private final Map<String, List<DataDictionaryPojo>> ddeNameMap;
	
	/**
	 * The Constructor.
	 *
	 * @param rootNode the root AST node of the Module
	 * @param ddeNameMap a map of {@linkplain DataDictionaryPojo} dataElementName to the {@linkplain DataDictionaryPojo} entries
	 */
	public BusinessVariableIdentifier(final @Nullable AstNodePojo rootNode, final Map<String, List<DataDictionaryPojo>> ddeNameMap) {
		this.rootNode = rootNode;
		this.ddeNameMap = ddeNameMap;
	}

	/**
	 * @return The root AST node of the Module.
	 */
	@Nullable
	public AstNodePojo getRootNode() {
		return rootNode;
	}
	
	/**
	 * Returns ddeNameMap a map of {@linkplain DataDictionaryPojo} dataElementName to the {@linkplain DataDictionaryPojo} entries
	 *
	 * @return map of {@linkplain DataDictionaryPojo} dataElementName to the {@linkplain DataDictionaryPojo} entries
	 */
	public Map<String, List<DataDictionaryPojo>> getDdeNameMap() {
		return ddeNameMap;
	}

	/**
	 * Returns a map of {@linkplain DataDictionaryPojo} name and IDs which has been identified as Business Related.
	 *
	 * @return a map of name and IDs which has been identified as Business Related.
	 */
	public Map<String, EntityId> getBusinessDataDictionaryIds() {
		final Map<String, List<DataDictionaryPojo>> dataDictionaryEntriesNameMap = getDdeNameMap();
		if (getRootNode() != null) {
			final List<AstNodePojo> businessUsages = getBusinessVariableReferences(dataDictionaryEntriesNameMap.keySet());
			final Map<String, EntityId> ddeNameToIdMap = new HashMap<>();
			for (final AstNodePojo usage : businessUsages) {
				final Tuple2<String, AstNodePojo> nameAndDefinition =
						AstNodeUtils.getDefinitionNameAndUsageFromReference(usage, null, true);
				if ( ! ddeNameToIdMap.containsKey(nameAndDefinition.a)) {
					final List<DataDictionaryPojo> dataDictionaryEntries = dataDictionaryEntriesNameMap.get(nameAndDefinition.a);
					if (dataDictionaryEntries != null) {
						findDataDictionaryUsingAstOffset(ddeNameToIdMap, nameAndDefinition, dataDictionaryEntries);
					}
				}
			}
			return ddeNameToIdMap;
		}
		throw new IllegalStateException("AstNodes are required for Business Variable Identification");
	}

	protected List<AstNodePojo> getFileAndDatabaseAccessStatements() {
		return FILE_DATA_COLLECTOR.allDeep(Assert.assertNotNull(getRootNode()));
	}

	protected List<AstNodePojo> getArithmeticStatements() {
		return ARITHMETIC_COLLECTOR.allDeep(Assert.assertNotNull(getRootNode()));
	}

	private void findDataDictionaryUsingAstOffset(final Map<String, EntityId> ddeNameToIdMap, final Tuple2<String, AstNodePojo> nameAndDefinition,
			final List<DataDictionaryPojo> dataDictionaryEntries) {
		if (dataDictionaryEntries.size() == 1) {
			ddeNameToIdMap.put(nameAndDefinition.a, dataDictionaryEntries.get(0).identity());
		} else if (dataDictionaryEntries.size() > 1) {
			final int offset = nameAndDefinition.b.getLocation().getRetracedOffset().orElseThrow();
			final int length = nameAndDefinition.b.getLocation().getRetracedLength().orElseThrow();
			for (final DataDictionaryPojo dde : dataDictionaryEntries) {
				final Integer ddeOffset = dde.getLocation().orElseThrow().getOffset();
				if (ddeOffset >= offset && (offset + length) >= ddeOffset) {
					ddeNameToIdMap.put(nameAndDefinition.a, dde.identity());
				}
			}
		}
	}

	private List<AstNodePojo> getBusinessVariableReferences(final Set<String> ddeNameSet) {
		/* Collect the File and Database Access Statements but exclude any branches that may be present inside them */
		final Stream<List<AstNodePojo>> fileDataVariableStream = getFileAndDatabaseAccessStatements().stream()
				.map(AstNodePojo::getChildren).flatMap(List::stream)
				.filter(node -> ! hasAnySuperType(node, BRANCH, DEFAULT_BRANCH))
				.map(VARIABLE_COLLECTOR::allDeep);
		
		/* Collect the arithmetic statements that are not counter updates (fields updating themselves with a constant value)  */
		final Stream<List<AstNodePojo>> arithmeticVariableStream = getArithmeticStatements().stream()
				.filter(this::isNotCounterUpdate)
				.map(this::findBusinessVariables);
		
		return Stream.concat(fileDataVariableStream, arithmeticVariableStream)
				.flatMap(List::stream)
				.filter(n -> existsInDdeSetAndReferenced(n, ddeNameSet))
				.collect(Collectors.toList());
	}

	private boolean existsInDdeSetAndReferenced(final AstNodePojo node, final Set<String> ddeNameSet) {
		final List<AstNodePojo> nodes = new ArrayList<>();
		final boolean existsInDdeSet = ddeNameSet.contains(AstNodeUtils.getDefinitionNameAndUsageFromReference(node, nodes, false).a);
		return (nodes.size() > 1) && existsInDdeSet;
	}

	private boolean isNotCounterUpdate(final AstNodePojo node) {
		final int variableCount = VARIABLE_COLLECTOR.allDeep(node).stream()
				.map(AstNodeUtils::getFieldDefinitionNameFromReference).collect(Collectors.toSet()).size();
		return variableCount > 1 || (variableCount == 1 && containsSpecificOperators(node.getLabel()));
	}
	
	private List<AstNodePojo> findBusinessVariables(final AstNodePojo node) {
		final List<AstNodePojo> leftOperands = AstNodeUtils.getLeftOperands(node);
		if (leftOperands.size() == 1 || node.getLabel().contains("REMAINDER")) {
			return leftOperands;
		} else {
			final List<AstNodePojo> bvAstNode = new ArrayList<>();
			final List<AstNodePojo> allFields = FIELD_COLLECTOR.allDeep(node);
			allFields.removeAll(leftOperands);
			for (final AstNodePojo leftOperand : leftOperands) {
				if (allFields.stream().
						anyMatch(astNode -> ! astNode.getLabel().equals(leftOperand.getLabel()))) {
					bvAstNode.add(leftOperand);
				}
			}
			return bvAstNode;
		}
	}
	
	private static boolean containsSpecificOperators(final String str) {
		return str.contains("*") || str.contains("/") || str.contains("%");
	}
}
