/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

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
import innowake.ndt.core.parsing.base.BindingConfiguration;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.parsing.parser.java.JavaParserAst;
import innowake.ndt.parsing.parser.java.JavaParserConfiguration;
import innowake.ndt.parsing.parser.java.model.JavaModel;

public abstract class AbstractJavaTest extends AbstractStoreAstTest {

	@Override
	public StoreAstPrototype storeAst(final String folderName, @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> traverseChildrenPredicate,
			final String... moduleNames) {
			final String content = getContent(folderName, moduleNames[0]);
			final JavaParserConfiguration<ModulePojo> javaConfig =
					new JavaParserConfiguration.JavaParserConfigurationBuilder<ModulePojo>()
					.setAssemblingDataProvider(new TestAssemblingProvider())
					.setNodeBindingConfiguration(BindingConfiguration.RESOLVE_ALL)
					.enableNodeLookup(true)
					.build();
			final ModulePojo module = createTestModuleDescription(1L, moduleNames[0], content).build();
			final JavaModel parseModel = new JavaParserAst<>(javaConfig).parse(module)
					.orElseThrow(() -> new IllegalStateException("Could not parse Java module " + module.getName() + ". See the log for more details."));
			final AstNode javaRootNode = parseModel.getRoot().get();
			/* Create a dummy retracing provider for Java */
			final RetracingProvider<ModulePojo> retracingProvider = new DummyRetracingProviderImpl<>(new DummyIAssembling(module));
			final ModuleProvider<ModulePojo> moduleProvider = new TestModuleProvider<>(retracingProvider);
			final Document document = getDocument(content);
			final AstNodeToMiningNode function = AstNodeToMiningNode.createInstance(content, moduleProvider,
					parseModel.getAdvancedLocationProvider(ModulePojo.class), retracingProvider, Technology.JAVA, document);
			final AstModelToMiningAst impl = traverseChildrenPredicate == null ? AstModelToMiningAst.createInstance(function)
					: AstModelToMiningAst.createInstance(function, traverseChildrenPredicate);
			return impl.traverse(javaRootNode);
	}

}
