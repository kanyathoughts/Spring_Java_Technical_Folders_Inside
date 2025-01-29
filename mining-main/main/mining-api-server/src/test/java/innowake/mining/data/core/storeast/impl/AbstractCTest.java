/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.junit.Assert.fail;

import java.util.function.Predicate;

import C.CLexer;
import C.CParser;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.DummyIAssembling;
import innowake.mining.data.core.storeast.api.AstModelToMiningAst;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.Technology;
import innowake.ndt.antlr.base.BaseAntlrErrorListener;
import innowake.ndt.antlr.base.BaseAntlrParseException;
import innowake.ndt.antlr.c.CAntlr2AstListener;
import innowake.ndt.antlr.c.CAntlrParser;
import innowake.ndt.antlr.c.CProgramPreProcessor;
import innowake.ndt.antlr.c.ast.CBindingResolver;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.base.BindingConfiguration;
import innowake.ndt.core.parsing.spi.Document;

public abstract class AbstractCTest extends AbstractStoreAstTest {

	@Override
	public StoreAstPrototype storeAst(final String folderName, @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> traverseChildrenPredicate,
			final String... moduleNames) {
		try {
			final CAntlrParser antlrParser = new CAntlrParser().setPreProcessor(new CProgramPreProcessor()).setErrorListener(new BaseAntlrErrorListener());
			final String content = getContent(folderName, moduleNames[0]);
			final CLexer lexer = antlrParser.tokenizeText(content);
			final CParser cParser = antlrParser.setupParser(lexer);
			final AstModel parseModel = antlrParser.parse(cParser, new CAntlr2AstListener()).getAstModel();
			final CBindingResolver bindingResolver = new CBindingResolver();
			bindingResolver.resolveBindingsForAstModel(parseModel, BindingConfiguration.RESOLVE_ALL);
			final AstNode cRoot = parseModel.getRoot().get();
			final ModulePojo module = createTestModuleDescription(1L, moduleNames[0], content).build();
			/* Create a dummy retracing provider for C */
			final RetracingProvider<ModulePojo> retracingProvider = new DummyRetracingProviderImpl<>(new DummyIAssembling(module));
			final ModuleProvider<ModulePojo> moduleProvider = new TestModuleProvider<>(retracingProvider);
			final Document document = getDocument(content);
			final AstNodeToMiningNode function = AstNodeToMiningNode.createInstance(content, moduleProvider,
					parseModel.getAdvancedLocationProvider(ModulePojo.class), retracingProvider, Technology.C, document);
			final AstModelToMiningAst impl = traverseChildrenPredicate == null ? AstModelToMiningAst.createInstance(function)
					: AstModelToMiningAst.createInstance(function, traverseChildrenPredicate);
			return impl.traverse(cRoot);
		} catch (final BaseAntlrParseException e) {
			fail(e.getMessage());
		}
		throw new IllegalStateException();
	}

}
