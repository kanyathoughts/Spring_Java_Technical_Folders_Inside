/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.easytrieve;

import java.util.Collections;
import java.util.List;
import org.antlr.v4.runtime.ParserRuleContext;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.discovery.parser.AbstractAntlrParseResult;
import innowake.ndt.antlr.ast.AntlrAstNode;
import innowake.ndt.antlr.ast.AntlrParseResult;
import innowake.ndt.core.parsing.ast.AstModel;	

/**
 * Wrapper class around easytrieve parser that handles the invoking of the parser and also sets up return result.
 */
public class EasytrieveAntlrParseResult extends AbstractAntlrParseResult<EasytrieveAntlrParseResult, AntlrAstNode<ParserRuleContext>, AstModel>  {

	@Nullable
	private List<String> inlineMacroNames;
	
	@Nullable
	private List<ErrorMarker> modelErrors;
	
	/**
	 * Constructor
	 * 
	 * @param parseResult of type {@link AntlrParseResult}
	 */
	protected EasytrieveAntlrParseResult(final AntlrParseResult<AstModel> parseResult) {
		super(parseResult);
	}

	/**
	 * Gets the inline macro names
	 *
	 * @return List of inline macro names
	 */
	public List<String> getInlineMacroNames() {
		return inlineMacroNames != null ? inlineMacroNames : Collections.emptyList();
	}

	/**
	 * setter for  the inline macro names
	 *
	 * @param inlineMacroNames the macro names
	 * @return {@link EasytrieveAntlrParseResult}
	 */
	public EasytrieveAntlrParseResult setInlineMacroNames(final List<String> inlineMacroNames) {
		this.inlineMacroNames = inlineMacroNames;
		return this;
	}
	
	/**
	 * Adds additional errors to the EasytrieveAntlrParseResult
	 *
	 * @param errors the {@link ErrorMarker}
	 * @return {@link EasytrieveAntlrParseResult}
	 */
	public EasytrieveAntlrParseResult addModelErrors(final List<ErrorMarker> errors) {
		if (modelErrors != null) {
			modelErrors.addAll(errors);
		} else {
			modelErrors = errors;
		}
		
		return this;
	}

	/**
	 * @return the additional {@link ErrorMarker ModelErrors} that have been added.
	 */
	public List<ErrorMarker> getModelErrors() {
		return modelErrors != null ? modelErrors : Collections.emptyList();
	}

}
