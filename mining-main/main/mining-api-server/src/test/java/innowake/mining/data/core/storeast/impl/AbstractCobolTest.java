/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.apache.commons.io.FilenameUtils.concat;
import static org.apache.commons.io.FilenameUtils.separatorsToUnix;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.function.Predicate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.core.storeast.api.AstModelToMiningAst;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.shared.TestResourceUtil;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.Technology;
import innowake.ndt.cobol.parser.CobolAssemblerConfiguration;
import innowake.ndt.cobol.parser.ResourceDataProvider;
import innowake.ndt.cobol.parser.ast.CobolNodeRegistry;
import innowake.ndt.cobol.parser.ast.CobolParseConfiguration;
import innowake.ndt.cobol.parser.ast.CobolParserAst;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolProgram;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.assembling.cobol.CobolAssembler;
import innowake.ndt.core.parsing.spi.Document;

public abstract class AbstractCobolTest extends AbstractStoreAstTest {

	@Override
	public StoreAstPrototype storeAst(final String folderName,  @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> traverseChildrenPredicate, final String... moduleNames) {
		try {
			final String fileName = moduleNames[0];
			final String folderPath = getResourceFolderPath(folderName);
			final String testFilePath = separatorsToUnix(concat(folderPath, fileName));	

			final TestAssemblingDataProvider provider = new TestAssemblingDataProvider(CHARSET, folderPath);
			final String content = Assert.assertNotNull(provider.getSource(testFilePath));
			
			final CobolAssemblerConfiguration<String> assemblerConfiguration = new CobolAssemblerConfiguration<String>(testFilePath, content, provider, true, false);
			final CobolAssembler<String> assembler = new CobolAssembler<>(assemblerConfiguration.getAssemblingProvider());
			final IAssembling<String> assembling = assembler.assemble(assemblerConfiguration.getObject(), assemblerConfiguration.getContent(), assemblerConfiguration);
			final CobolParseConfiguration<String> configuration = new CobolParseConfiguration.Builder<>(testFilePath, content, provider)
					.shouldValidateNodeHierarchy(true).setAssembling(assembling).build();
			
			CobolNodeRegistry.getInstance().setEnabled(true);
			final CobolParserAst parser = new CobolParserAst(LOG);
			final CobolModel model = parser.parse(configuration);
			final CobolProgram program = model.getCobolProgram();

			final RetracingProviderImpl<String> retracingProvider = new RetracingProviderImpl<>(assembling);
			final ModuleProvider<String> moduleProvider = new TestModuleProvider<>(retracingProvider);
			final Document document = getDocument(content);
			final AstNodeToMiningNode function = AstNodeToMiningNode.createInstance(assembling.getAssembledContent(), moduleProvider,
					model.getAdvancedLocationProvider(String.class), retracingProvider, Technology.COBOL, document);
			final AstModelToMiningAst impl = traverseChildrenPredicate == null
												? AstModelToMiningAst.createInstance(function)
												: AstModelToMiningAst.createInstance(function, traverseChildrenPredicate);

			return impl.traverse(program);
		} catch (final AssemblingException e) {
			fail(e.getMessage());
		}

		throw new IllegalStateException();
	}

	/** Decorator on a {@link ResourceDataProvider} that normalize line endings in source content */
	private static class TestAssemblingDataProvider implements IAssemblingDataProvider<String> {

		private final ResourceDataProvider provider;
		private final Charset charset;

		private TestAssemblingDataProvider(final Charset charset, final String copyFolder) {
			this.provider = new ResourceDataProvider(charset, copyFolder);
			this.charset = charset;
		}

		@Nullable
		@Override
		public String find(final String root, final String name, final IAssemblingObjectType expectedType) {
			return provider.find(root, name, expectedType);
		}

		@Nullable
		@Override
		public String getPath(final String object) {
			return provider.getPath(object);
		}

		@Nullable
		@Override
		public String getSource(final String object) throws AssemblingException {
			try {
				return TestResourceUtil.getContentNormalized(object, charset);
			} catch (IOException e) {
				throw new AssemblingException(object, e);
			}
		}

		@Nullable
		@Override
		public IAssemblingObjectType getType(final String object) {
			return provider.getType(object);
		}

		@Nullable
		@Override
		public String getName(final String object) {
			return provider.getName(object);
		}

		@Nullable
		@Override
		public Object getHashable(final String object) {
			return provider.getHashable(object);
		}

		@Override
		public boolean isObjectProxy(final String object) {
			return provider.isObjectProxy(object);
		}
		
	}
}
