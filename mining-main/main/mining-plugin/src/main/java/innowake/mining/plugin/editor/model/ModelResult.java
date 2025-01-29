/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.model;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Generic Model Result.
 */
public class ModelResult {
	
	private final Status status;
	@Nullable private final AstModel astModel;
	
	/**
	 * Provides status of the current {@link AstModel} member in the current {@link ModelResult}.
	 */
	public enum Status {
		OK, TIMEOUT, ERROR
	}
	
	/**
	 * Constructor to initialize model result.
	 * 
	 * @param status of the current ModelResult.
	 * @param astModel present in the ModelResult
	 */
	
	public ModelResult(final Status status, @Nullable final AstModel astModel) {
		this.status = status;
		this.astModel = astModel;
	}
	
	/**
	 * Returns the status of the model result.
	 *
	 * @return status of the model result
	 */
	public Status getStatus() {
		return status;
	}
	
	/**
	 * Returns the AST model of the model result.
	 *
	 * @return AST model of the model result
	 */
	@Nullable
	public AstModel getModel() {
		return astModel;
	}

}
