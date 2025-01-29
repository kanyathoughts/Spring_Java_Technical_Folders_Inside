/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.c;

import innowake.mining.shared.entities.DependencyDefinitionPojo;
import org.antlr.v4.runtime.ParserRuleContext;
import innowake.mining.server.discovery.parser.AbstractAntlrParseResult;
import innowake.ndt.antlr.ast.AntlrAstNode;
import innowake.ndt.antlr.ast.AntlrParseResult;
import innowake.ndt.core.parsing.ast.AstModel;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Wrapper class around C parser that handles the invoking of the parser and also sets up return
 * result.
 */
public class CAntlrParseResult extends AbstractAntlrParseResult<CAntlrParseResult, AntlrAstNode<ParserRuleContext>, AstModel> {

	private Set<DependencyDefinitionPojo> cHeaderDependencies = new LinkedHashSet<>();
	private Set<DependencyDefinitionPojo> cFunctionDependencies = new LinkedHashSet<>();
	/**
	 * Wrapper class around C parser that handles invoking of parser and setup the return result and
	 * provide only the C-specific sub parts.
	 * 
	 * @param antlrParseResult of type {@link AntlrParseResult}
	 */
	protected CAntlrParseResult(final AntlrParseResult<AstModel> antlrParseResult) {
		super(antlrParseResult);
	}

	public Set<DependencyDefinitionPojo> getHeaderDependencies() {
		return cHeaderDependencies;
	}

	public Set<DependencyDefinitionPojo> getFunctionDependencies() {
		return cFunctionDependencies;
	}

	public CAntlrParseResult setHeaderDependencies(final Set<DependencyDefinitionPojo> headerDependencies) {
		this.cHeaderDependencies = headerDependencies;
		return this;
	}

	public CAntlrParseResult setFunctionDependencies(final Set<DependencyDefinitionPojo> functionDependencies) {
		this.cFunctionDependencies = functionDependencies;
		return this;
	}
}
