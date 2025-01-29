/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.cobol;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.stream.Stream;

import innowake.mining.server.discovery.metrics.cobol.CobolOffsetModel;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolProgram;
import innowake.ndt.cobol.parser.ast.model.CobolUnknownToken;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaUnknownStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaUnknownToken;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.AstUtil;
import innowake.ndt.parsing.parser.exec.ExecUnknownToken;
/**
 * Result of a parser run.
 *
 */
public final class CobolParseResult {

	private final SourcePojo input;
	private final CobolModel model;
	private final CobolOffsetModel offsetModel;

	public CobolParseResult(final SourcePojo input, final CobolModel model, final CobolOffsetModel offsetModel) {
		this.input = input;
		this.model = model;
		this.offsetModel = offsetModel;
	}

	/**
	 * Get the origin input file.
	 * @return The input file instance.
	 */
	public SourcePojo getInput() {
		return input;
	}

	/**
	 * Access the parsed model.
	 * @return The parsed model.
	 */
	public CobolModel getModel() {
		return model;
	}

	/**
	 * Access the cobol offset model.
	 * @return The offset model.
	 */
	public CobolOffsetModel getOffsetModel() {
		return offsetModel;
	}
	
	/**
	 * Check the parse result for unknown tokens.
	 *
	 * @return {@link ParseResultState#ERROR} if unknown tokens are found, otherwise {@link ParseResultState#SUCCESS}.
	 */
	public ParseResultState getParseState() {
		return findChildren(CobolUnknownToken.class, AdaUnknownToken.class, AdaUnknownStmt.class, ExecUnknownToken.class).count() == 0 ? ParseResultState.SUCCESS : ParseResultState.ERROR;
	}
	
	@SafeVarargs
	private final Stream<AstNode> findChildren(final Class<? extends AstNode>... clazz) {
		final CobolProgram program = model.getCobolProgram();
		final Stream<AstNode> idStream = AstUtil.findChildrenAndAttributes(assertNotNull(program.getIdentificationDivision()), clazz).stream();
		final Stream<AstNode> edStream = AstUtil.findChildrenAndAttributes(assertNotNull(program.getEnvironmentDivision()), clazz).stream();
		final Stream<AstNode> ddStream = AstUtil.findChildrenAndAttributes(assertNotNull(program.getDataDivision()), clazz).stream();
		final Stream<AstNode> pdStream = AstUtil.findChildrenAndAttributes(assertNotNull(program.getProcedureDivision()), clazz).stream();
		return Stream.of(idStream, edStream, ddStream, pdStream)
				.reduce(Stream::concat)
				.orElseGet(Stream::empty);
	}
}
