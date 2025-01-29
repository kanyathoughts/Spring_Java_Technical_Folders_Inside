/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.basic;

import org.eclipse.jface.text.IDocument;

import innowake.mining.plugin.editor.generic.DocumentAssemblingProvider;
import innowake.mining.plugin.editor.model.BaseModelProvider;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.parsing.base.BaseParser;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.BasicParser;

/**
 * Class representing the model provider for BASIC language.
 */
public class BasicModelProvider extends BaseModelProvider<IDocument, BasicModel> {
	
	@Override
	protected BaseParser<IDocument, BasicModel> createParser() {
		return new BasicParser<>(createParserConfiguration());
	}

	@Override
	protected IAssemblingDataProvider<IDocument> getAssemblingDataProvider() {
		return new DocumentAssemblingProvider();
	}
	
}
