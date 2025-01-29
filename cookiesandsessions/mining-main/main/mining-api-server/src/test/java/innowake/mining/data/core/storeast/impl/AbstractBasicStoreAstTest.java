/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.junit.Assert.assertNotNull;

import java.util.Optional;
import java.util.function.Predicate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.DummyIAssembling;
import innowake.mining.data.core.storeast.api.AstModelToMiningAst;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.Technology;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.BasicParser;

/**
 * Abstract base class for store AST tests of BASIC Language.
 */
public class AbstractBasicStoreAstTest extends AbstractStoreAstTest {

	@Override
	public StoreAstPrototype storeAst(final String folderName, @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> traverseChildrenPredicate,
			final String... moduleNames) {
		final TestAssemblingProvider assemblingProvider = new TestAssemblingProvider();
		String mainModuleContent = null;
		ModulePojo mainModule = null;
		long moduleCount = 1;
		for (int i = 0; i < moduleNames.length; i++) {
			final String moduleName = moduleNames[i];
			final String content = getContent(folderName, moduleName);
			final ModulePojo module = createTestModuleDescription(Long.valueOf(moduleCount++), moduleName, content).build();
			assemblingProvider.add(module);
			if (i == 0) {
				mainModuleContent = content;
				mainModule = module;
			}
		}
		assertNotNull(mainModuleContent);
		assertNotNull(mainModule);
		final BaseParserConfiguration<ModulePojo> basicParserConfiguration =
				new BaseParserConfiguration.Builder<ModulePojo>().setAssemblingDataProvider(new TestAssemblingProvider()).build();
		final BasicParser<ModulePojo> basicParser = new BasicParser<>(basicParserConfiguration);
		final Optional<BasicModel> model = basicParser.parse(mainModule);
		final AstNode rootNode = model.get().getRoot().get();
		/* Create a dummy retracing provider for Assembler */
		final RetracingProvider<ModulePojo> retracingProvider = new DummyRetracingProviderImpl<>(new DummyIAssembling(mainModule));
		final ModuleProvider<ModulePojo> moduleProvider = new TestModuleProvider<>(retracingProvider);
		final Document document = getDocument(mainModuleContent);
		final AstNodeToMiningNode function = AstNodeToMiningNode.createInstance(mainModuleContent, moduleProvider,
				model.get().getAdvancedLocationProvider(ModulePojo.class), retracingProvider, Technology.ASSEMBLER, document);
		final AstModelToMiningAst impl = traverseChildrenPredicate == null ? AstModelToMiningAst.createInstance(function)
				: AstModelToMiningAst.createInstance(function, traverseChildrenPredicate);
		return impl.traverse(rootNode);
	}

}
