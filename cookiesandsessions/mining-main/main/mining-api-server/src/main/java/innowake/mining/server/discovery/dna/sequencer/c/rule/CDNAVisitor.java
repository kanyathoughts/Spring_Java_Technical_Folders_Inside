/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dna.sequencer.c.rule;

import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.ndt.antlr.c.ast.CFunctionDefinition;
import innowake.ndt.antlr.c.ast.CFunctionReference;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecSqlNode;
import innowake.ndt.core.parsing.ast.visitor.Visitor;

/**
 * {@link Visitor} to capture the DNA strings
 */
abstract class CDNAVisitor implements Visitor<AstNode> {

	final DNACollector<AstModel> collector;

	CDNAVisitor(final DNACollector<AstModel> collector) {
		this.collector = collector;
	}

	protected abstract Map<String, String> getStringMap();

	@Override
	public boolean visit(@Nullable final AstNode node) {
		if (node == null) {
			return false;
		}

		final String classSimpleName = node.getClass().getSimpleName();

		if (getStringMap().containsKey(classSimpleName)) {
			switch (classSimpleName) {
				case "CFunctionDefinition":
					final CFunctionDefinition funcDefine = (CFunctionDefinition) node;
					if (funcDefine.isMain()) {
						collector.add(() -> "main_function_definition", node);
					} else {
						final String funcName = funcDefine.getFunctionName();
						collector.add(() -> "function_definition: " + funcName, node);
					}
					break;
				case "CFunctionReference":
					final CFunctionReference funcRef = (CFunctionReference) node;
					collector.add(() -> "function_reference: " + funcRef.getFunctionName(), node);
					break;
				default:
					final String dnaStringElement = getStringMap().get(classSimpleName);
					collector.add(() -> dnaStringElement, node);
			}
		} else if ((node instanceof ExecSqlNode) && getStringMap().containsKey("ExecSql")) {
			collector.add(() -> "exec_sql", node);
		}

		return visitChildren(node);
	}

	private boolean visitChildren(final AstNode node) {
		boolean result = true;
		for (final AstNode child : node.getChildren()) {
			result &= this.visit(child);
		}
		return result;
	}

}
