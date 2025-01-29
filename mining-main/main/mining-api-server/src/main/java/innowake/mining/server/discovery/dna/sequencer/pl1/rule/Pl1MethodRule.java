/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer.pl1.rule;

import java.util.Optional;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.dna.sequencer.AbstractDNASequencerRule;
import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider.Pl1ParseResult;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.pl1parser.ast.Pl1Node;
import innowake.ndt.pl1parser.ast.model.Pl1ModelTraverser;

/**
 * Dna sequencer rule to extract Pl1 method calls as dna.
 */
public class Pl1MethodRule extends AbstractDNASequencerRule<Pl1ParseResult> implements SequencerRule<Pl1ParseResult> {

	@Override
	public DnaSequencer getId() {
		return DnaSequencer.PLI_METHOD_RULE;
	}

	@Override
	public void apply(final DNACollector<Pl1ParseResult> collector) throws DiscoveryException {
		final Pl1ParseResult parseResult = collector.getParseResult();

		final Optional<AstNode> rootNode = parseResult.getProgramModel().getRoot();

		if ( ! rootNode.isPresent()) {
			/* this program does not appear to be parseable */
			return;
		}

		new Pl1ModelTraverser() {

			@Override
			public boolean handle(final @Nullable ExecNode<?> node) {
				handleBaseExecSql(node, collector);
				return true;
			}
			
			@Override
			public boolean handleDefault(final AstNode node) {
				if (node instanceof Pl1Node) {
					final Pl1Node pl1Node = (Pl1Node) node;
					final String simpleName = node.getClass().getSimpleName();
					final String category = findPl1Category(simpleName);
					if (category != null) {
						collector.add(() -> category, pl1Node);
					}
					return true;
				}
				return false;
			}

			private String findPl1Category(final String simpleName) {
				String resultCategory = null;
				switch (simpleName) {
					case "GetStatement":
						resultCategory = "get";
						break;
					case "PutStatement":
						resultCategory = "put";
						break;
					case "AllocateStatement":
						resultCategory = "allocate";
						break;
					case "AssertStatement":
						resultCategory = "assert";
						break;
					case "AssignmentStatement":
						resultCategory = "assignment";
						break;
					case "CallStatement":
						resultCategory = "call";
						break;
					case "DelayStatement":
						resultCategory = "delay";
						break;
					case "DisplayStatement":
						resultCategory = "display";
						break;
					case "DoStatement":
						resultCategory = "do";
						break;
					case "EntryStatement":
						resultCategory = "entry";
						break;
					case "ExitStatement":
						resultCategory = "exit";
						break;
					case "FetchStatement":
						resultCategory = "fetch";
						break;
					case "FreeStatement":
						resultCategory = "free";
						break;
					case "GoToStatement":
						resultCategory = "go";
						break;
					case "IfStatement":
						resultCategory = "if";
						break;
					case "NullStatement":
						resultCategory = "null";
						break;
					case "OtherwiseStatement":
						resultCategory = "otherwise";
						break;
					case "PreprocessorDeclareStatement":
						resultCategory = "preprocessDeclare";
						break;
					case "ReInitStatement":
						resultCategory = "reInit";
						break;
					case "ReleaseStatement":
						resultCategory = "release";
						break;
					case "ReturnStatement":
						resultCategory = "return";
						break;
					case "SelectStatement":
						resultCategory = "select";
						break;
					case "StopStatement":
						resultCategory = "stop";
						break;
					case "WhenStatement":
						resultCategory = "when";
						break;
					case "PreprocessorIfStatement":
						resultCategory = "preprocessorIf";
						break;
					case "OnStatement":
						resultCategory = "on";
						break;
					case "LineDirective":
						resultCategory = "lineD";
						break;
					case "NoPrintDirective":
						resultCategory = "noprintD";
						break;
					case "NoteDirective":
						resultCategory = "noteD";
						break;
					case "PageDirective":
						resultCategory = "pageD";
						break;
					case "PopDirective":
						resultCategory = "popD";
						break;
					case "PrintDirective":
						resultCategory = "printD";
						break;
					case "ProcessDirective":
						resultCategory = "processD";
						break;
					case "PushDirective":
						resultCategory = "pushD";
						break;
					case "SkipDirective":
						resultCategory = "skipD";
						break;
					case "IterateStatement":
						resultCategory = "iterate";
						break;
					case "PreprocessEndStatement":
						resultCategory = "preprocessorEnd";
						break;
					case "ReSignalStatement":
						resultCategory = "resignal";
						break;
					case "AttachStatement":
						resultCategory = "attach";
						break;
					case "RevertStatement":
						resultCategory = "revert";
						break;
					case "SignalStatement":
						resultCategory = "signal";
						break;
					case "CancelStatement":
						resultCategory = "cancel";
						break;
					case "DetachStatement":
						resultCategory = "detach";
						break;
					case "WaitStatement":
						resultCategory = "wait";
						break;
					case "CloseStatement":
						resultCategory = "close";
						break;
					case "FlushStatement":
						resultCategory = "flush";
						break;
					case "DeleteStatement":
						resultCategory = "delete";
						break;
					case "LocateStatement":
						resultCategory = "locate";
						break;
					case "OpenStatement":
						resultCategory = "open";
						break;
					case "ReadStatement":
						resultCategory = "read";
						break;
					case "ReWriteStatement":
						resultCategory = "rewrite";
						break;
					case "WriteStatement":
						resultCategory = "write";
						break;
					case "DeclareStatement":
						resultCategory = "declare";
						break;
					case "DefaultStatement":
						resultCategory = "default";
						break;
					case "DefineStatement":
						resultCategory = "define";
						break;
					case "FormatStatement":
						resultCategory = "format";
						break;
					case "GotoStatement":
						resultCategory = "goto";
						break;
					case "LeaveStatement":
						resultCategory = "leave";
						break;
					case "PreprocessDeclareStatement":
						resultCategory = "preprocessDeclare";
						break;
					default:
						break;
				}
				return resultCategory;
			}

		}.traverse(rootNode.get());
	}
}
