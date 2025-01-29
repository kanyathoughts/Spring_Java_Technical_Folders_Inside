/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import java.util.HashMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.statement.CobolDivideStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolMathStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolMultiplyStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolWhenStmt;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.parsing.ast.AdvancedLocationProvider;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Core implementation for {@link AstNodeToMiningNode}.
 * 
 * @param <T> the concrete type of {@link Retracing}
 */
public final class CobolNodeToMiningNodeFunctionImpl<T> extends AstNodeToMiningNodeFunctionImpl<T> {

	/**
	 * Constructor.
	 * 
	 * @param source the assembled source code of the module to be transformed
	 * @param moduleProvider the module provider
	 * @param locationProvider the location provider
	 * @param retracingProvider the retracing provider
	 * @param document {@link Document} with unassembled content
	 */
	public CobolNodeToMiningNodeFunctionImpl(
			final String source, 
			final ModuleProvider<T> moduleProvider, 
			@Nullable
			final AdvancedLocationProvider<T> locationProvider,
			final RetracingProvider<T> retracingProvider,
			final Document document) {
		super(source, moduleProvider, locationProvider, retracingProvider, document);
	}
	
	@Override
	public StoreAstPrototype apply(final innowake.ndt.core.parsing.ast.AstNode node) {
		final StoreAstPrototype astNode = super.apply(node);
		if (node instanceof CobolMathStmt) {
			final CobolMathStmt stmt = (CobolMathStmt) node;
			final StringBuilder leftOperands = new StringBuilder();
			stmt.getGivings().forEach(g -> leftOperands.append(g.getExpression()).append(","));
			stmt.getToOperands().forEach(to -> leftOperands.append(to.getExpression()).append(","));

			if (stmt instanceof CobolMultiplyStmt) {
				((CobolMultiplyStmt) stmt).getRightOperands().forEach(ro -> leftOperands.append(ro.getExpression()).append(","));
			} else if (stmt instanceof CobolDivideStmt) {
				leftOperands.append(((CobolDivideStmt) stmt).getRemainder());
			}

			astNode.properties.getOrSet(HashMap::new).put("leftOperands", leftOperands.toString());
		} else if (node instanceof CobolWhenStmt) {
			return handle((CobolWhenStmt) node, astNode);
		}

		return astNode;
	}

	/**
	 * Creates an outgoing edge of type REDEFINES from the given {@code astNode} to its redefinition, if exists and stores the given {@code node} and
	 * {@code astNode} in the {@code dataFieldToAstNode} map of this function.
	 * <p>Make sure that this method is called only once for an AST node or you will get an Exception </p>
	 *
	 * @param node the node from parser
	 * @param astNode the {@link StoreAstPrototype}
	 */
	@Override
	protected void handle(final FieldDefinition<?> node, final StoreAstPrototype astNode) {
		super.handle(node, astNode);

		@Nullable
		final CobolFieldReference redefineReference = (CobolFieldReference) ((CobolDataField) node).getRedefineReference();
		if (redefineReference != null) {

			@Nullable
			final StoreAstPrototype dataFieldAstNode = dataFieldToAstNode.get(redefineReference.getField());
			if (dataFieldAstNode != null) {
				astNode.addRelationshipTo(dataFieldAstNode, AstRelationshipType.REDEFINES);
			}
		}
	}

	private static StoreAstPrototype handle(final CobolWhenStmt node, final StoreAstPrototype astNode) {
		if (node.isOther()) {
			/* CobolWhenStmt.isOther() defines the default branch. Theres is no special class which implements DefaultBranch. */
			astNode.addSuperType(AstNodeUtils.DEFAULT_BRANCH);
		}
		return astNode;
	}
}
