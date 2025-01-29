/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Model containing the actual parser model and if available its assembling.
 */
public interface Model {

	/**
	 * @return the actual parser model
	 */
	AstModel getParseModel();
	
	/**
	 * @return the optional assembling for the parser model
	 */
	Object getAssembling();

	/**
	 * @return the complete assembled source content if an assembling is available or {@code null}
	 */
	@Nullable
	String getAssembledContent();
}
