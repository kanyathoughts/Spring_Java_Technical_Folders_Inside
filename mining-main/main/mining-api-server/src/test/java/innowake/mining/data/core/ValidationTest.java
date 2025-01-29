/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;

import innowake.ndt.cobol.parser.ast.model.CobolAtEndBlock;
import innowake.ndt.cobol.parser.ast.model.CobolConstantReference;
import innowake.ndt.cobol.parser.ast.model.CobolElseBlock;
import innowake.ndt.cobol.parser.ast.model.CobolInvalidKeyBlock;
import innowake.ndt.cobol.parser.ast.model.CobolReferenceExpression;
import innowake.ndt.cobol.parser.ast.model.CobolSection;
import innowake.ndt.cobol.parser.ast.model.CobolSizeErrorStatement;
import innowake.ndt.cobol.parser.ast.model.CobolThenBlock;
import innowake.ndt.cobol.parser.ast.model.ProcedureDivision;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolCopyStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolDisplayStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolEvaluateStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolExitStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolGoBackStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolGoToStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolIfStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolLabelStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolPerformStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolReadStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolReturnStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolRewriteStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSearchStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSizeGuardedStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolStartStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolStopStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolWhenStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolWriteStmt;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsLink;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReturn;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsXctl;

/**
 * Validates constants
 */
public class ValidationTest {

	/**
	 * Validates that the type names in {@link CobolAstNodeType} are still equivalent to the represented class names.
	 */
	@Test
	public void validateCobolTypeNames() {
		for (final CobolAstNodeType type : CobolAstNodeType.values()) {
			switch (type) {
				case CONSTANT_REFERENCE:
					assertEquals(CobolAstNodeType.CONSTANT_REFERENCE.getType(), CobolConstantReference.class.getSimpleName());
					break;
				case COPY:
					assertEquals(type.getType(), CobolCopyStmt.class.getSimpleName());
					break;
				case DISPLAY:
					assertEquals(type.getType(), CobolDisplayStmt.class.getSimpleName());
					break;
				case EVALUATE:
					assertEquals(type.getType(), CobolEvaluateStmt.class.getSimpleName());
					break;
				case GO_BACK:
					assertEquals(type.getType(), CobolGoBackStmt.class.getSimpleName());
					break;
				case GO_TO:
					assertEquals(type.getType(), CobolGoToStmt.class.getSimpleName());
					break;
				case EXIT:
					assertEquals(type.getType(), CobolExitStmt.class.getSimpleName());
					break;
				case STOP:
					assertEquals(type.getType(), CobolStopStmt.class.getSimpleName());
					break;
				case IF:
					assertEquals(type.getType(), CobolIfStmt.class.getSimpleName());
					break;
				case THEN:
					assertEquals(type.getType(), CobolThenBlock.class.getSimpleName());
					break;
				case ELSE:
					assertEquals(type.getType(), CobolElseBlock.class.getSimpleName());
					break;
				case LABEL:
					assertEquals(type.getType(), CobolLabelStmt.class.getSimpleName());
					break;
				case PERFORM:
					assertEquals(type.getType(), CobolPerformStmt.class.getSimpleName());
					break;
				case PROCEDURE_DIVISION:
					assertEquals(type.getType(), ProcedureDivision.class.getSimpleName());
					break;
				case REFERENCE_EXPRESSION:
					assertEquals(type.getType(), CobolReferenceExpression.class.getSimpleName());
					break;
				case CICS_XCTL:
					assertEquals(type.getType(), ExecCicsXctl.class.getSimpleName());
					break;
				case CICS_RETURN:
					assertEquals(type.getType(), ExecCicsReturn.class.getSimpleName());
					break;
				case CICS_LINK:
					assertEquals(type.getType(), ExecCicsLink.class.getSimpleName());
					break;
				case CALL:
					assertEquals(type.getType(), CobolCallStmt.class.getSimpleName());
					break;
				case SECTION:
					assertEquals(type.getType(), CobolSection.class.getSimpleName());
					break;
				case SIZE_GUARDED:
					assertEquals(type.getType(), CobolSizeGuardedStmt.class.getSimpleName());
					break;
				case RETURN:
					assertEquals(type.getType(), CobolReturnStmt.class.getSimpleName());
					break;
				case READ:
					assertEquals(type.getType(), CobolReadStmt.class.getSimpleName());
					break;
				case WRITE:
					assertEquals(type.getType(), CobolWriteStmt.class.getSimpleName());
					break;
				case REWRITE:
					assertEquals(type.getType(), CobolRewriteStmt.class.getSimpleName());
					break;
				case START:
					assertEquals(type.getType(), CobolStartStmt.class.getSimpleName());
					break;
				case SEARCH:
					assertEquals(type.getType(), CobolSearchStmt.class.getSimpleName());
					break;
				case AT_END:
					assertEquals(type.getType(), CobolAtEndBlock.class.getSimpleName());
					break;
				case INVALID_KEY:
					assertEquals(type.getType(), CobolInvalidKeyBlock.class.getSimpleName());
					break;
				case ON_SIZE_ERROR:
					assertEquals(type.getType(), CobolSizeErrorStatement.class.getSimpleName());
					break;
				case WHEN:
					assertEquals(type.getType(), CobolWhenStmt.class.getSimpleName());
					break;
				case DEFAULT_STATEMENT:
					break;
				default:
					fail("Type " + type.getType() + "was not verified.");	
			} 
		}
	}
}
