/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import static innowake.mining.data.core.api.AstNodeUtils.BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.DEFAULT_BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.JUMP_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.hasAnySuperType;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.core.api.AstNodeCollectingTraverser;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.cobol.parser.ast.model.CobolAndExpression;
import innowake.ndt.cobol.parser.ast.model.CobolComparisonExpression;
import innowake.ndt.cobol.parser.ast.model.CobolOrExpression;
import innowake.ndt.cobol.parser.ast.statement.CobolReadStmt;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlClose;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlConnect;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareCursor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlFetch;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlSelect;
import innowake.ndt.naturalparser.ast.Statement.ReadStmt;
import innowake.ndt.naturalparser.ast.imp.ImpStatement.ReadWorkFileStmt;
import innowake.ndt.pl1parser.ast.model.statement.data.record.ReadStatement;

/**
 * Template for Business Rule collector to collect AST nodes and return Annotation candidates
 */
abstract class AnnotationRuleCollector {
	
	private static final Logger LOG = LoggerFactory.getLogger(AnnotationRuleCollector.class);
	/* In case of GRP.VARIABLE we only want VARIABLE */
	protected static final AstNodeCollector FIELD_REFERENCE_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(FIELD_REFERENCE));
	protected static final AstNodeCollector BRANCH_COLLECTOR = new AstNodeCollector(node -> hasAnySuperType(node, BRANCH, DEFAULT_BRANCH));
	protected static final AstNodeCollector VARIABLE_COLLECTOR =
		new AstNodeCollector(node ->
			node.getSuperTypes().contains(FIELD_REFERENCE) &&
				node.getChildren().stream().noneMatch(child -> FIELD_REFERENCE_COLLECTOR.firstDeep(child).isPresent()));
	protected static final Set<String> FILE_READ_TYPES = Sets.newHashSet(ReadStatement.class.getSimpleName(), ReadWorkFileStmt.class.getSimpleName(),
			ExecSqlConnect.class.getSimpleName(), ExecSqlDeclareCursor.class.getSimpleName(), ExecSqlSelect.class.getSimpleName(),
			ExecSqlFetch.class.getSimpleName(), ExecSqlClose.class.getSimpleName(), ReadStmt.class.getSimpleName(), CobolReadStmt.class.getSimpleName());
	protected static final Set<String> COMPARISON_EXP_TYPES = Sets.newHashSet(CobolComparisonExpression.class.getSimpleName(),
			CobolAndExpression.class.getSimpleName(), CobolOrExpression.class.getSimpleName());
	protected static final Set<String> BRANCH_LOOP_STATEMENTS = Sets.newHashSet(BRANCH_STATEMENT, JUMP_STATEMENT);
	
	private final Set<ModuleLocation> excludedLocations;
	private final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap;
	
	/**
	 * Constructor.
	 * 
	 * @param excludedLocations excludedLocation the set of ModuleLocation
	 * @param astAndDdeMap a map of Field Definitions and Data Dictionaries to their name to their business status
	 */
	protected AnnotationRuleCollector(final Set<ModuleLocation> excludedLocations,
			final Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> astAndDdeMap) {
		this.excludedLocations = excludedLocations;
		this.astAndDdeMap = astAndDdeMap;
	}

	/**
	 * return the ModuleLocation 
	 *
	 * @return excludedLocation the set of ModuleLocation
	 */
	protected Set<ModuleLocation> getExcludedLocations() {
		return excludedLocations;
	}

	/**
	 * return the ModuleLocation 
	 *
	 * @return excludedLocation the set of ModuleLocation
	 */
	protected Map<Boolean, Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>>> getAstAndDdeMap() {
		return astAndDdeMap;
	}
	
	/**
	 * Returns a new instance of {@link AstNodeCollectingTraverser} which traverses AST nodes searching for business rule candidates based on 
	 * the input filterPredicate 
	 *
	 * @param filterPredicate filter condition for node traversal
	 * @return instance of {@link AstNodeCollectingTraverser}
	 */
	protected AstNodeCollectingTraverser createNodeTraverser(final Predicate<AstNodePojo> filterPredicate) {
		return new AstNodeCollectingTraverser(filterPredicate) {

			@Override
			public AstNodePojo traverse(final AstNodePojo node) {
				return ! AstNodeUtils.isInclusionNode(node) && ! AstNodeUtils.isCobolExitParagraph(node) ? super.traverse(node) : node;
			}
		};
	}

	/**
	 * Traverses the given {@code root} node deep and collects all business rule metadataList.
	 *
	 * @param root the node from which the collection will start
	 * @param categoryId Id of the annotationCategory of the annotation
	 * @return list of identified candidate annotations
	 */
	protected List<AnnotationPojoTemplate> collect(final AstNodePojo root, final Long categoryId) {
		final List<AstNodePojo> candidateNodes = getCandidateNodes(root);
		final Set<ModuleLocation> collectedLocations = new HashSet<>();
		final List<AnnotationPojoTemplate> annotationList = new ArrayList<>();
		candidateNodes.forEach(node -> {
			final ModuleLocation nodeLocation = node.getLocation().convertToSharedModuleLocation();
			if ( ! getExcludedLocations().contains(nodeLocation) && ! isContainedInCollectedNodes(collectedLocations, nodeLocation)) {
				collectedLocations.add(nodeLocation);
				getExcludedLocations().add(nodeLocation);
				final AnnotationPojoTemplate annotation = AnnotationCreator.create(node, getAnnotationName(), AnnotationType.RULE, categoryId, getAdjustedRuleLocation(node),
						translateAnnotation(node), getIncludedDataDictionaries(node));
				annotationList.add(annotation);
			} else {
				getExcludedLocations().add(nodeLocation);
			}
		});
		return annotationList;
	}
	
	/**
	* translate the annotation to English 
	*
	* @param node the node related to the annotation
	* @return English translation of the annotation
	*/
	protected String translateAnnotation(final AstNodePojo node) {
		try {
			final Map<String, Tuple2<AstNodePojo, DataDictionaryPojo>> ddeNameMap = new HashMap<>();
			getAstAndDdeMap().entrySet().forEach(e -> e.getValue().entrySet().forEach(es -> ddeNameMap.put(es.getKey(), es.getValue().get(0))));
			final AnnotationTranslator translator = new CobolAnnotationTranslator(ddeNameMap);
			return translator.formatTranslate(translator.translate(0, node));
		} catch (final Exception e) {
			LOG.info("An exception occured at the time of translating the Annotation: " + e);
			return StringUtils.EMPTY;
		}
	}
	
	/**
	 * Return the Location Corresponding to the Location of last child Node if Present.
	 *
	 * @param currentNode the current AST node
	 * @return Location of the Business Rule.
	 */
	protected AstNodeLocation getAdjustedRuleLocation(final AstNodePojo currentNode) {
		var location = currentNode.getLocation();
		final List<AstNodePojo> children = currentNode.getChildren();
		if ( ! children.isEmpty()) {
			final AstNodePojo lastChild = children.get(children.size() - 1);
			final Integer offset = location.getRetracedOffset().orElseThrow();
			final Integer childPosition = lastChild.getLocation().getRetracedOffset().orElseThrow() + lastChild.getLocation().getRetracedLength().orElseThrow();
			if (childPosition > offset + location.getRetracedLength().orElseThrow()) {
				location = new AstNodeLocation(offset, Integer.valueOf(childPosition - offset), location.getAssembledOffset().orElse(null),
						location.getAssembledLength().orElse(null), location.getRetracedOffset().orElse(null), location.getRootRelativeLength().orElse(null),
						location.getRootRelativeStartLineNumber().orElse(null), location.getRootRelativeEndLineNumber().orElse(null));
			}
		}
		return location;
	}
	
	/**
	 * Collects the Data Dictionaries referenced in a Ast Node
	 *
	 * @param node the ast node
	 * @return set of Data Dictionaries referenced
	 */
	protected Set<DataDictionaryPojo> getIncludedDataDictionaries(final AstNodePojo node) {
		final Map<String, List<Tuple2<AstNodePojo, DataDictionaryPojo>>> allDdeMap = new HashMap<>();
		astAndDdeMap.entrySet().forEach(e -> allDdeMap.putAll(e.getValue()));
		return FIELD_REFERENCE_COLLECTOR.allDeep(node).parallelStream()
				.map(ref -> {
					final String name = AstNodeUtils.getFieldDefinitionNameFromReference(ref);
					final List<Tuple2<AstNodePojo, DataDictionaryPojo>> ddesWithSameName = allDdeMap.get(name);
					if ( ! (ddesWithSameName == null || ddesWithSameName.isEmpty())) {
						if (ddesWithSameName.size() == 1) {
							return Optional.of(ddesWithSameName.get(0).b);
						} else {
							final Optional<AstRelationshipPojo> def = ref.getIncomingRelations().stream().filter(r -> r.getType() == AstRelationshipType.BINDING
																														&& "definedBy".equals(r.getLabel().orElse(null)))
																										 .findAny();
							if (def.isPresent()) {
								final AstNodePojo defNode = def.get().getSrcNode();
								final ModuleLocation location = defNode.getLocation().convertToSharedModuleLocation();
								return ddesWithSameName.parallelStream()
										.filter(dde -> dde.a.getLocation().convertToSharedModuleLocation() == location)
										.map(t -> t.b)
										.findAny();
							}
						}
					}
					return Optional.<DataDictionaryPojo> empty();
				})
				.filter(Optional :: isPresent)
				.map(Optional :: get)
				.collect(Collectors.toSet());
	}
	
	private boolean isContainedInCollectedNodes(final Set<ModuleLocation> collectedLocations, final ModuleLocation moduleLocation) {
		return collectedLocations.stream().anyMatch(moduleLocation::isWithin);
	}

	/**
	 * Returns a list of AST node generated by traversing the root node 
	 *
	 * @param root the node from which the collection will start
	 * @return list of AST node generated by traversing the root node
	 */
	protected abstract List<AstNodePojo> getCandidateNodes(final AstNodePojo root);
	
	/**
	 * @return the name of Annotation candidates 
	 */
	protected abstract String getAnnotationName();
}
