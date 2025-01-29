/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer.cobol.rule;

import static innowake.lib.core.lang.Assert.assertNotNull;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.dna.sequencer.AbstractDNASequencerRule;
import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.cobol.parser.ast.model.CobolComparisonExpression;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolNode;
import innowake.ndt.cobol.parser.ast.model.CobolOpenMode;
import innowake.ndt.cobol.parser.ast.model.CobolOpenOperand;
import innowake.ndt.cobol.parser.ast.statement.CobolAddStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolMoveStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolOpenStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolPerformStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolReadStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSubtractStmt;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsFile;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSendReceive;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;

/**
 * Dna sequencer rule to extract Cobol method calls as dna.
 */
public class CobolMethodRule extends AbstractDNASequencerRule<Tuple2<CobolModel, IAssembling<SourcePojo>>>
		implements SequencerRule<Tuple2<CobolModel, IAssembling<SourcePojo>>> {

	@Override
	public DnaSequencer getId() {
		return DnaSequencer.COBOL_METHOD_RULE;
	}

	@Override
	public void apply(final DNACollector<Tuple2<CobolModel, IAssembling<SourcePojo>>> collector) {
		new MethodCobolModelTraverser(collector).visit(collector.getParseResult().e1); 
	}
	
	class MethodCobolModelTraverser extends CustomCobolModelTraverser {
		
		public MethodCobolModelTraverser(final DNACollector<Tuple2<CobolModel, IAssembling<SourcePojo>>> model) {
			super(model);
		}

		@Override
		public void handle(final @Nullable Object model) {
			if (model != null) {
				final String simpleName = model.getClass().getSimpleName();
				if (model instanceof CobolNode) {
					final CobolNode cobolNode = (CobolNode)model;
					final String category = findCobolCategory(simpleName, cobolNode);
					if (category != null) {
						collector.add(() -> category, cobolNode);
					}
				} else if (model instanceof ExecNode<?>) {
					handleBaseExecSql((ExecNode<?>)model, collector);
				}
			}
			super.handle(model);
		}
		
		private @Nullable String findCobolCategory(final String simpleName, final CobolNode cobolNode) {
			String resultCategory = null;
			switch (simpleName) {
				case "CobolMoveStmt":
					if (cobolNode instanceof CobolMoveStmt) {
						final CobolMoveStmt node = (CobolMoveStmt) cobolNode;
						if (node.isAll()) {
							resultCategory = "moveAll";
						} else if (node.isCorresponding()) {
							resultCategory = "moveCorresponding";
						} else {
							resultCategory = "move";
						}
					}
					break;
				case "CobolComputeStmt":
					resultCategory = "compute";
					break;
				case "CobolInitializeStmt":
					resultCategory = "initialize";
					break;
				case "CobolSetStmt":
					resultCategory = "set";
					break;
				case "CobolComparisonExpression":
					if (cobolNode instanceof CobolComparisonExpression) {
						final CobolComparisonExpression node = (CobolComparisonExpression) cobolNode;
						switch (node.getComparison()) {
							case EQ:
								resultCategory = "EQ";
								break;
							case NE:
								resultCategory = "NE";
								break;
							case LT:
								resultCategory = "LT";
								break;
							case LE:
								resultCategory = "LE";
								break;
							case GT:
								resultCategory = "GT";
								break;
							case GE:
								resultCategory = "GE";
								break;
							case BETWEEN:
								resultCategory = "BETWEEN";
								break;
							case IN:
								resultCategory = "IN";
								break;
							case IS_ALPHABETIC:
							case IS_ALPHABETIC_LOWER:
							case IS_ALPHABETIC_UPPER:
							case IS_ANY:
							case IS_CLASS:
							case IS_NEGATIVE:
							case IS_NOT_ALPHABETIC:
							case IS_NOT_ALPHABETIC_LOWER:
							case IS_NOT_ALPHABETIC_UPPER:
							case IS_NOT_CLASS:
							case IS_NOT_NEGATIVE:
							case IS_NOT_NUMERIC:
							case IS_NOT_POSITIVE:
							case IS_NOT_ZERO:
							case IS_NUMERIC:
							case IS_POSITIVE:
							case IS_ZERO:
							case NOT_BETWEEN:
								resultCategory = "IS";
								break;
							case SETID:
								resultCategory = "SETID";
								break;
							case THRU:
								resultCategory = "THRU";
								break;
							default:
								break;
						}
					}
					break;
				case "CobolIfStmt":
					resultCategory = "if";
					break;
				case "ExecCicsWriteQTd":
					resultCategory = "writeqTd";
					break;
				case "ExecCicsWriteQTs":
					resultCategory = "writeqTs";
					break;
				case "ExecCicsReadQTd":
					resultCategory = "readqTd";
					break;
				case "ExecCicsReadQTs":
					resultCategory = "readqTs";
					break;
				case "ExecCicsDeleteQTd":
					resultCategory = "deleteqTd";
					break;
				case "ExecCicsDeleteQTs":
					resultCategory = "deleteqTs";
					break;
				case "ExecCicsSetTdQueue":
				case "ExecCicsInquireTsqname":
					resultCategory = "queue";
					break;
				case "CobolCallStmt":
					resultCategory = "call";
					break;
				case "CobolGoToStmt":
					resultCategory = "goTo";
					break;
				case "CobolGoBackStmt":
					resultCategory = "goBack";
					break;
				case "CobolPerformStmt":
					if (cobolNode instanceof CobolPerformStmt) {
						final CobolPerformStmt node = (CobolPerformStmt) cobolNode;
						if (node.getThru() != null) {
							resultCategory = "performThru";
						} else if (node.getTimes() != null) {
							resultCategory = "performTimes";
						} else {
							resultCategory = "perform";
						}
					}
					break;
				case "CobolStopStmt":
					resultCategory = "stop";
					break;
				case "CobolNextSentenceStmt":
					resultCategory = "nextSentence";
					break;
				case "CobolExitStmt":
					resultCategory = "exit";
					break;
				case "ExecCicsReturn":
					resultCategory = "return_";
					break;
				case "ExecCicsXctl":
					resultCategory = "xctl";
					break;
				case "ExecCicsLink":
					resultCategory = "link";
					break;
				case "ExecCicsSendReceive":
					if (cobolNode instanceof ExecCicsSendReceive) {
						final ExecCicsSendReceive node = (ExecCicsSendReceive) cobolNode;
						if (node.getOperation() != null) {
							switch (assertNotNull(node.getOperation())) {
								case SEND_MAP:
									resultCategory = "sendMap";
									break;
								case RECEIVE_MAP:
									resultCategory = "receiveMap";
									break;
							}
						}
					}
					break;
				case "CobolReadStmt":
					if (cobolNode instanceof CobolReadStmt) {
						final CobolReadStmt node = (CobolReadStmt) cobolNode;
						if (node.isNext()) {
							resultCategory = "readNext";
						} else {
							resultCategory = "read";
						}
					}
					break;
				case "CobolDeleteStmt":
					resultCategory = "delete";
					break;
				case "CobolOpenStmt":
					if (cobolNode instanceof CobolOpenStmt) {
						final CobolOpenStmt node = (CobolOpenStmt) cobolNode;
						boolean found = false;
						for (final CobolOpenOperand iterable_element : node.getOperands()) {
							if (iterable_element.getIntent() != null && assertNotNull(iterable_element.getIntent()).equals(CobolOpenMode.INPUT)) {
								resultCategory = "openInput";
								found = true;
								break;
							} else if (iterable_element.getIntent() != null && assertNotNull(iterable_element.getIntent()).equals(CobolOpenMode.EXTEND)) {
								resultCategory = "openExtend";
								found = true;
								break;
							} else if (iterable_element.getIntent() != null && assertNotNull(iterable_element.getIntent()).equals(CobolOpenMode.OUTPUT)) {
								resultCategory = "openOutput";
								found = true;
								break;
							} else if (iterable_element.getIntent() != null && assertNotNull(iterable_element.getIntent()).equals(CobolOpenMode.IO)) {
								resultCategory = "openIO";
								found = true;
								break;
							}
						}
						if ( ! found) {
							resultCategory = "open";
						}
					}
					break;
				case "CobolRewriteStmt":
					resultCategory = "rewrite";
					break;
				case "ExecCicsFile":
					if (cobolNode instanceof ExecCicsFile) {
						final ExecCicsFile node = (ExecCicsFile) cobolNode;
						switch (node.getOperation()) {
							case STARTBR:
								resultCategory = "cics.startbr";
								break;
							case RESETBR:
								resultCategory = "cics.resetbr";
								break;
							case ENDBR:
								resultCategory = "cics.endbr";
								break;
							case UNLOCK:
								resultCategory = "cics.unlock";
								break;
							case READ:
								resultCategory = "cics.read";
								break;
							case READNEXT:
								resultCategory = "cics.readNext";
								break;
							case READPREV:
								resultCategory = "cics.readPrevious";
								break;
							case DELETE:
								resultCategory = "cics.delete";
								break;
							case REWRITE:
								resultCategory = "cics.rewrite";
								break;
							case WRITE:
								resultCategory = "cics.write";
								break;
							default:
								break;
						}
					}
					break;
				case "CobolStartStmt":
					resultCategory = "start";
					break;
				case "CobolAcceptStmt":
					resultCategory = "accept";
					break;
				case "CobolDisplayStmt":
					resultCategory = "display";
					break;
				case "CobolAddStmt":
					if (cobolNode instanceof CobolAddStmt) {
						final CobolAddStmt node = (CobolAddStmt) cobolNode;
						if (node.isCorresponding()) {
							resultCategory = "addCorresponding";
						} else if ( ! node.getToOperands().isEmpty()) {
							resultCategory = "addTo";
						} else {
							resultCategory = "add";
						}
					}
					break;
				case "CobolSubtractStmt":
					if (cobolNode instanceof CobolSubtractStmt) {
						final CobolSubtractStmt node = (CobolSubtractStmt) cobolNode;
						if (node.isCorresponding()) {
							resultCategory = "substractCorresponding";
						} else {
							resultCategory = "substract";
						}
					}
					break;
				case "CobolMultiplyStmt":
					resultCategory = "multiply";
					break;
				case "CobolDivideStmt":
					resultCategory = "divide";
					break;
				default:
					break;
			}
			return resultCategory;
		}
	}
	
}
