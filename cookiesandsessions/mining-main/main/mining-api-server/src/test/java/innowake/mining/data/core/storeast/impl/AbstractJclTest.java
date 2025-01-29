/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.junit.Assert.fail;

import java.util.List;
import java.util.function.Predicate;

import org.apache.commons.io.FilenameUtils;

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
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.jcl.parser.api.JCLConfigParam;
import innowake.ndt.jcl.parser.api.JclLine;
import innowake.ndt.jcl.parser.assembling.DefaultContentProvider;
import innowake.ndt.jcl.parser.assembling.JCLAssembler;
import innowake.ndt.jcl.parser.assembling.mvs.JCLMvsAssembler;
import innowake.ndt.jcl.parser.ast.JclAstConverter;
import innowake.ndt.jcl.parser.env.JCLCoreParserException;
import innowake.ndt.jcl.parser.model.JclContent;
import innowake.ndt.jcl.parser.model.JclModel;
import innowake.ndt.jcl.parser.parser.mvs.JCLMvsParser;

/**
 * Abstract test class for JCL store ast tests.
 */
public abstract class AbstractJclTest extends AbstractStoreAstTest {
	
	@Override
	public StoreAstPrototype storeAst(final String folderName, @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> traverseChildrenPredicate,
			final String... moduleNames) {
		try {
			final JCLConfigParam properties = new JCLConfigParam();
			final JCLMvsAssembler assembler = new JCLMvsAssembler(new JCLMvsParser(), true, properties);
			final String content = getContent(folderName, moduleNames[0]);
			final JclContent assembledContent = assembler.execute(content, new DefaultContentProvider(properties), FilenameUtils.getBaseName(moduleNames[0]));
			final List<JclLine> jclLines = JCLAssembler.split(content, assembler.getOsType());
			final JCLMvsParser parser = new JCLMvsParser(assembler.getJclErrors());
			final JclModel model = JclAstConverter.createAstModel(parser.parse(properties, assembledContent),
					new JclContent(jclLines, FilenameUtils.getBaseName(moduleNames[0])));
			final AstNode rootNode = model.getRoot().get();
			final ModulePojo module = createTestModuleDescription(1L, moduleNames[0], content).build();
			/* Create a dummy retracing provider for Jcl */
			final RetracingProvider<ModulePojo> retracingProvider = new DummyRetracingProviderImpl<>(new DummyIAssembling(module));
			final ModuleProvider<ModulePojo> moduleProvider = new TestModuleProvider<>(retracingProvider);
			final Document document = getDocument(content);
			final AstNodeToMiningNode function = AstNodeToMiningNode.createInstance(content, moduleProvider,
					model.getAdvancedLocationProvider(ModulePojo.class), retracingProvider, Technology.JCL, document);
			final AstModelToMiningAst impl = traverseChildrenPredicate == null ? AstModelToMiningAst.createInstance(function)
					: AstModelToMiningAst.createInstance(function, traverseChildrenPredicate);
			return impl.traverse(rootNode);
		} catch (final JCLCoreParserException e) {
			fail(e.getMessage());
		}
		throw new IllegalStateException();
	}

}
