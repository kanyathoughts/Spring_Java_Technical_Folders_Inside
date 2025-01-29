/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.Optional;
import java.util.function.Predicate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.PL1AssemblerConfiguration;
import innowake.mining.data.core.storeast.api.AstModelToMiningAst;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.Technology;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.base.BindingConfiguration;
import innowake.ndt.core.parsing.replacement.ReplacementMode;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.parsing.assembling.pl1.Pl1Assembler;
import innowake.ndt.pl1parser.ast.model.Pl1Model;
import innowake.ndt.pl1parser.parser.PL1ParseConfiguration;
import innowake.ndt.pl1parser.parser.PL1ParseConfiguration.PL1ParseConfigurationBuilder;
import innowake.ndt.pl1parser.parser.Pl1NodeFactory;
import innowake.ndt.pl1parser.parser.Pl1Parser;
import innowake.ndt.pl1parser.parser.Pl1Rules;

public abstract class AbstractPl1Test extends AbstractStoreAstTest {

	@Override
	public StoreAstPrototype storeAst(final String folderName, @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> traverseChildrenPredicate,
			final String... moduleNames) {
		try {
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
			final Pl1Assembler<ModulePojo> assembler = new Pl1Assembler<>(assemblingProvider, 2, 72, -1);
			final PL1AssemblerConfiguration pl1AssemblerConfiguration = new PL1AssemblerConfiguration(ReplacementMode.FAST, true);
			final PL1ParseConfiguration<ModulePojo, Pl1Rules> pl1ParserConfiguration = new PL1ParseConfigurationBuilder<ModulePojo, Pl1Rules>()
					.setAssemblingDataProvider(assembler.getDataProvider())
					.setNodeFactory(new Pl1NodeFactory())
					.setRulesClass(Pl1Rules.class)
					.setNodeBindingConfiguration(BindingConfiguration.RESOLVE_ALL)
					.enableNodeLookup(true)
					.build();
			final IAssembling<ModulePojo> assembling = assembler.assemble(mainModule, mainModuleContent, pl1AssemblerConfiguration);

			final Pl1Parser<ModulePojo> pl1Parser = new Pl1Parser<>(pl1ParserConfiguration);
			final Optional<Pl1Model> pl1Model = pl1Parser.parse(mainModule);
			final AstNode rootNode = pl1Model.get().getRoot().get();
			final RetracingProviderImpl<ModulePojo> retracingProvider = new RetracingProviderImpl<>(assembling);
			final ModuleProvider<ModulePojo> moduleProvider = new TestModuleProvider<>(retracingProvider);
			final Document document = getDocument(mainModuleContent);
			final AstNodeToMiningNode function = AstNodeToMiningNode.createInstance(assembling.getAssembledContent(), moduleProvider,
					pl1Model.get().getAdvancedLocationProvider(ModulePojo.class), retracingProvider, Technology.PL1, document);
			final AstModelToMiningAst impl = traverseChildrenPredicate == null ? AstModelToMiningAst.createInstance(function)
					: AstModelToMiningAst.createInstance(function, traverseChildrenPredicate);

			return impl.traverse(rootNode);
		} catch (final AssemblingException e) {
			fail(e.getMessage());
		}

		throw new IllegalStateException();
	}

}
