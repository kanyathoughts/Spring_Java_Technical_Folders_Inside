/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.junit.Assert.assertNotNull;
import java.util.function.Predicate;
import innowake.lib.core.IProgress;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.DummyIAssembling;
import innowake.mining.data.core.storeast.api.AstModelToMiningAst;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.Technology;
import innowake.ndt.cobol.parser.bms.BMSParserAst;
import innowake.ndt.cobol.parser.bms.model.BmsModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Abstract base class for store AST tests of BMS Language.
 */
public class AbstractBmsStoreAstTest extends AbstractStoreAstTest {

	private static IProgress createProgress() {
		return new IProgress() {

			@Override
			public boolean isCanceled() {
				return false;
			}

			@Override
			public void done() {
			}

			@Override
			public void worked(final int work) {
			}

			@Override
			public void subTask(final @Nullable String name) {
			}

			@Override
			public void setCanceled(final boolean canceled) {
			}

			@Override
			public @Nullable IProgress createSubProgress(final int ticks) {
				return null;
			}

			@Override
			public void beginTask(final @Nullable String name, final int totalWork) {
			}
		};
	}
	
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
		final BMSParserAst parser = new BMSParserAst(LOG,createProgress());
		final BmsModel model = parser.parse(mainModuleContent);
		final AstNode rootNode = model;
		/* Create a dummy retracing provider for Assembler */
		final RetracingProvider<ModulePojo> retracingProvider = new DummyRetracingProviderImpl<>(new DummyIAssembling(mainModule));
		final ModuleProvider<ModulePojo> moduleProvider = new TestModuleProvider<ModulePojo>(retracingProvider);
		final Document document = getDocument(mainModuleContent);
		final AstNodeToMiningNode function = AstNodeToMiningNode.createInstance(mainModuleContent, moduleProvider, null, retracingProvider,
				Technology.ASSEMBLER, document);
		final AstModelToMiningAst impl = traverseChildrenPredicate == null ? AstModelToMiningAst.createInstance(function)
				: AstModelToMiningAst.createInstance(function, traverseChildrenPredicate);
		return impl.traverse(rootNode);
	}

}
