/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.astmodel;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolFigurativeConstantsReference;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolComputeStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolDisplayStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolMoveStmt;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.Statement;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.ast.model.statement.FieldReference;
import innowake.ndt.fieldtracing.cobol.bytelayout.CobolByteLayoutProvider;
import innowake.ndt.fieldtracing.model.ByteLayout;

/**
 * Traces the internal data flow of a Cobol program and discovers proxy containers using the AST model.
 */
public class CobolDataLineage extends AbstractAstModelDataLineage {

	private static final Logger LOG = LoggerFactory.getLogger(CobolDataLineage.class);

	private final Map<CobolDataField, ByteLayout> byteLayoutMap;

	/**
	 * COnstructor
	 * @param context contextual information about a data lineage operation.
	 * @param astModel the parsed AstModel of a Module
	 * @param module the Module
	 */
	public CobolDataLineage(final DataLineageContext context, final CobolModel astModel, final ModuleLightweightPojo module) {
		super(context, astModel, module);
		byteLayoutMap = new CobolByteLayoutProvider().process(astModel);
	}

	@Override
	protected Set<FieldDefinition<?>> getRelatedFields(final FieldDefinition<?> field) {
		final ByteLayout byteLayout = byteLayoutMap.get(field.getAstNode());
		return byteLayoutMap.entrySet().stream()
				.filter(other -> other.getValue() != byteLayout && byteLayout.overlapsWith(other.getValue()))
				.map(Entry::getKey)
				.collect(Collectors.toSet());
	}

	@Override
	protected Collection<FieldAccess> getFieldAccesses(final Statement statement, final List<FieldReference<?>> fieldReferences) {
		final AstNode astNode = statement.getAstNode();
		final List<FieldReference<?>> filteredFieldReferences = filterFieldReferences(fieldReferences);

		if (astNode.getGenericType(CobolMoveStmt.class) != null) {
			return handleMoveStatement(assertNotNull(astNode.getGenericType(CobolMoveStmt.class)), filteredFieldReferences);
		}
		if (astNode.getGenericType(CobolComputeStmt.class) != null) {
			return handleComputeStatement(assertNotNull(astNode.getGenericType(CobolComputeStmt.class)));
		}
		if (astNode.getGenericType(CobolDisplayStmt.class) != null) {
			return handleDisplayStatement(filteredFieldReferences);
		}
		if (astNode.getGenericType(CobolCallStmt.class) != null) {
			return handleCallStatement(assertNotNull(astNode.getGenericType(CobolCallStmt.class)));
		}

		LOG.debug(() -> "unhandled statement: " + statement.getClass().getSimpleName());

		return Collections.emptyList();
	}

	private Collection<FieldAccess> handleMoveStatement(final CobolMoveStmt moveStmt, final List<FieldReference<?>> fieldReferences) {
		/* MOVE can move either a figurative constant ("LOW-VALUES" etc.) or a field into other field(s) */
		final List<CobolFigurativeConstantsReference> constants = moveStmt.getFrom().getChildrenDeep(CobolFigurativeConstantsReference.class);
		final List<FieldReference<?>> fromReferences = getFieldReferences(moveStmt.getFrom());
		final List<FieldReference<?>> toReferences = fieldReferences.stream()
				.filter(fieldReference -> ! fromReferences.contains(fieldReference))
				.collect(Collectors.toList());

		final List<FieldAccess> fieldAccesses = new ArrayList<>();
		for (final CobolFigurativeConstantsReference constant : constants) {
			for (final FieldReference<?> toReference : toReferences) {
				fieldAccesses.add(FieldAccess.moves(constant.getValue(), toReference));
			}
		}
		for (final FieldReference<?> fromReference : fromReferences) {
			for (final FieldReference<?> toReference : toReferences) {
				fieldAccesses.add(FieldAccess.moves(fromReference, toReference));
			}
		}

		return fieldAccesses;
	}

	private Collection<FieldAccess> handleComputeStatement(final CobolComputeStmt computeStmt) {
		final List<FieldReference<?>> sources = getFieldReferences(computeStmt.getSource());
		final List<FieldReference<?>> targets = computeStmt.getTargets().stream()
				.flatMap(target -> getFieldReferences(target).stream())
				.collect(Collectors.toList());

		final List<FieldAccess> fieldAccesses = new ArrayList<>();
		for (final FieldReference<?> fromReference : sources) {
			for (final FieldReference<?> toReference : targets) {
				fieldAccesses.add(FieldAccess.moves(fromReference, toReference));
			}
		}

		return fieldAccesses;
	}

	private Collection<FieldAccess> handleDisplayStatement(final List<FieldReference<?>> fieldReferences) {
		/* display statement never writes anything */
		return fieldReferences.stream().map(FieldAccess::reads).collect(Collectors.toList());
	}


	private Collection<FieldAccess> handleCallStatement(final CobolCallStmt callStmt) {
		final List<FieldAccess> fieldAccesses = new ArrayList<>();
		final List<FieldReference<?>> targetReferences = getFieldReferences(callStmt.getTarget());
		/* for dynamic call: variable containing the target program name is read (the list should contain only 1 entry) */
		for (final FieldReference<?> targetReference : targetReferences) {
			fieldAccesses.add(FieldAccess.reads(targetReference));
		}

		for (final CobolCallStmt.CobolUsing using : callStmt.getUsings()) {
			for (final FieldReference<?> usingReference : getFieldReferences(using.getExpression())) {
				fieldAccesses.add(FieldAccess.reads(usingReference));
				/* BY_REFERENCE semantics are simulated by also writing the field from the CALL statement (because the callee might modify the field) */
				if (using.getUsingType() == CobolCallStmt.UsingType.BY_REFERENCE) {
					fieldAccesses.add(FieldAccess.writes(usingReference));
				}
			}
		}

		return fieldAccesses;
	}

	private List<FieldReference<?>> getFieldReferences(final AstNode node) {
		@SuppressWarnings("unchecked")
		final List<FieldReference<?>> fieldReferences = filterFieldReferences((List<FieldReference<?>>) (List<?>) node.getChildrenDeep(FieldReference.class));
		return fieldReferences;
	}

	private List<FieldReference<?>> filterFieldReferences(final List<FieldReference<?>> fieldReferences) {
		return fieldReferences.stream()
				/* filter out field references that are inside another field reference. this combination means that the inner field reference
				 * is either used as an array subscript, substring operand or "OF" qualifier - ignore this field reference */
				.filter(fieldReference -> fieldReference.getAstNode().getParent().getGenericType(FieldReference.class) == null)
				.collect(Collectors.toList());
	}

	@Override
	protected DataLineageResult handleSpecialStatement(final Statement statement) {
		/* here CALL, EXEC SQL, READ, WRITE etc. will be handled (statements that interact with things "outside" of the program) */
		return DataLineageResult.empty();
	}
}

