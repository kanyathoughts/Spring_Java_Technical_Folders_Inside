/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.parser;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.antlr.v4.runtime.ParserRuleContext;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;

import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.ndt.antlr.ast.AntlrAstNode;
import innowake.ndt.antlr.ast.AntlrParseResult;
import innowake.ndt.antlr.base.BaseAntlrParserError;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Base class for all language specific AntlrParseResults.
 * 
 * @param <T> of language specific {@link AntlrParseResult}
 * @param <N> of type {@link AntlrAstNode}
 * @param <M> of type {@link AstModel}
 */
public abstract class AbstractAntlrParseResult<T extends AbstractAntlrParseResult<T, N, M>, N extends AntlrAstNode<ParserRuleContext>, M extends AstModel> {

	private final M astModel;
	private List<BaseAntlrParserError> syntaxErrors;
	private long numberOfStatements;
	private long complexity;
	private long linesOfComments;
	private long linesOfCode;
	private MultiValuedMap<String, String> assignmentValues = new ArrayListValuedHashMap<>();
	private Set<DependencyDefinitionPojo> unvalidatedDependencies = new LinkedHashSet<>();
	
	/**
	 * Base class for all language specific AntlrParseResult to reuse implementations for e.g.
	 * complexity,errors,AstModel of type M etc.
	 * 
	 * @param parseResult {@link AntlrParseResult} instance
	 */
	protected AbstractAntlrParseResult(final AntlrParseResult<M> parseResult) {
		astModel = parseResult.getAstModel();
		syntaxErrors = parseResult.getSyntaxErrors();
	}
	
	public long getLinesOfCode() {
		return linesOfCode;
	}

	public long getLinesOfComments() {
		return linesOfComments;
	}

	public long getComplexity() {
		return complexity;
	}

	public long getNumberOfStatements() {
		return numberOfStatements;
	}

	public List<BaseAntlrParserError> getErrors() {
		return syntaxErrors;
	}

	public Set<DependencyDefinitionPojo> getUnvalidatedDependencies() {
		return unvalidatedDependencies;
	}

	public MultiValuedMap<String, String> getAssignmentValues() {
		return assignmentValues;
	}

	public M getAstModel() {
		return astModel;
	}
	
	public T setUnvalidatedDependencies(final Set<DependencyDefinitionPojo> dependencies) {
		this.unvalidatedDependencies  = dependencies;
		return getThis();
	}
	
	public T setLinesOfCode(final long linesOfCode) {
		this.linesOfCode = linesOfCode;
		return getThis();
	}

	public T setLinesOfComments(final long linesOfComments) {
		this.linesOfComments = linesOfComments;
		return getThis();
	}

	public T setComplexity(final long complexity) {
		this.complexity = complexity;
		return getThis();
	}

	public T setNumberOfStatements(final long numberOfStatements) {
		this.numberOfStatements = numberOfStatements;
		return getThis();
	}
	
	public T setAssignmentValues(final MultiValuedMap<String, String> assignmentValues) {
		this.assignmentValues = assignmentValues;
		return getThis();
	}

	@SuppressWarnings("unchecked")
	private T getThis() {
		return (T) this;
	}
}
