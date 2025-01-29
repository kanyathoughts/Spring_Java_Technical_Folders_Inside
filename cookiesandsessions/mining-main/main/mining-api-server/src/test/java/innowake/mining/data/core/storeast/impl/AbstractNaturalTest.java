/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static innowake.ndt.core.assembling.natural.INaturalAssemblingTypes.ASSEMBLE_COPYCODES;
import static innowake.ndt.core.assembling.natural.INaturalAssemblingTypes.ASSEMBLE_DATA_AREAS_AGGRESSIVELY;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.function.Predicate;

import org.apache.commons.io.FilenameUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.util.StringUtil;
import innowake.mining.data.core.storeast.api.AstModelToMiningAst;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.assembling.natural.INaturalAssemblingDataProvider;
import innowake.ndt.core.assembling.natural.NaturalAssembler;
import innowake.ndt.core.assembling.natural.NaturalAssemblerConfiguration;
import innowake.ndt.core.assembling.natural.NaturalAssemblingObjectType;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.base.BindingConfiguration;
import innowake.ndt.core.parsing.natural.INaturalParseResult;
import innowake.ndt.core.parsing.replacement.ReplacementMode;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.NaturalModelParser;
import innowake.ndt.naturalparser.NaturalObjectType;
import innowake.ndt.naturalparser.model.NaturalAstModel;

/**
 * Abstract test class for natural store ast tests.
 */
public abstract class AbstractNaturalTest extends AbstractStoreAstTest {

	@Override
	public StoreAstPrototype storeAst(final String folderName, @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> traverseChildrenPredicate, final String... moduleNames) {
		try {
			final NaturalTestAssemblingProvider assemblingProvider = new NaturalTestAssemblingProvider();
			
			String mainModuleContent = null;
			NaturalObjectType mainModuleObjectType = null;
			ModulePojo mainModule = null;
			long moduleCount = 1;
			for (int i = 0; i < moduleNames.length; i++) {
				final String moduleName = moduleNames[i];
				final String content = getContent(folderName, moduleName);
				final NaturalObjectType natType = getNatObjectType(moduleName);
				final String baseName = FilenameUtils.getBaseName(moduleName);
				final ModulePojo module = createTestModuleDescription(Long.valueOf(moduleCount++), baseName, content)
						.prepare(m -> m.setTechnology(Technology.NATURAL).setType(getType(natType))).build();
				assemblingProvider.add(module);

				if (i == 0) {
					mainModuleContent = content;
					mainModuleObjectType = natType;
					mainModule = module;
				}
			}
			
			assertNotNull(mainModuleContent);
			assertNotNull(mainModuleObjectType);
			assertNotNull(mainModule);

			/* process main content. */
			final NaturalAssembler<ModulePojo> assembler = new NaturalAssembler<>(assemblingProvider);
			final IAssembling<ModulePojo> assembling = assembler.assemble(mainModule, mainModuleContent,
					new NaturalAssemblerConfiguration(ReplacementMode.FAST_LOCATIONS, false, ASSEMBLE_COPYCODES | ASSEMBLE_DATA_AREAS_AGGRESSIVELY));
			final INaturalModel model = new NaturalModelParser(true, null, null, assembler, mainModule)
					.parse(mainModule.getName(), mainModuleObjectType, assembling.getAssembledContent(), false, false, assembling);
			assertTrue(model.getProgramObject().getErrors().isEmpty());
			final NaturalAstModel astModel = NaturalAstModel.from(model, BindingConfiguration.RESOLVE_ALL);

			final RetracingProviderImpl<ModulePojo> retracingProvider = new RetracingProviderImpl<>(assembling);
			final ModuleProvider<ModulePojo> moduleProvider = new TestModuleProvider<>(retracingProvider);
			final Document document = getDocument(mainModuleContent);
			final AstNodeToMiningNode function = AstNodeToMiningNode.createInstance(assembling.getAssembledContent(), moduleProvider,
					astModel.getAdvancedLocationProvider(ModulePojo.class), retracingProvider,
					Technology.NATURAL, document);
			final AstModelToMiningAst impl = traverseChildrenPredicate == null
													? AstModelToMiningAst.createInstance(function)
													: AstModelToMiningAst.createInstance(function, traverseChildrenPredicate);

			return impl.traverse(astModel.getRoot().get());
		} catch (final AssemblingException e) {
			fail(e.getMessage());
		}

		throw new IllegalStateException();
	}
	
	private NaturalObjectType getNatObjectType(final String fileName) {
		return NaturalObjectType.getByFilename(StringUtil.divide(fileName, ".", false, true)[1]);
	}
	
	public Type getType(NaturalObjectType natType) {
		if (natType == NaturalObjectType.TYPE_LDA || natType == NaturalObjectType.TYPE_IW_LDA) {
			return Type.LDA;
		} else if (natType == NaturalObjectType.TYPE_PDA || natType == NaturalObjectType.TYPE_IW_PDA) {
			return Type.PDA;
		} else if (natType == NaturalObjectType.TYPE_GDA || natType == NaturalObjectType.TYPE_IW_GDA) {
			return Type.GDA;
		} else if (natType == NaturalObjectType.TYPE_COPYCODE) {
			return Type.COPYCODE;
		}
		return Type.UNKNOWN;
	}
	
	/**
	 * Provides Module instances, source content and {@link IAssemblingObjectType}s to the {@link NaturalAssembler}.
	 */
	public static class NaturalTestAssemblingProvider extends TestAssemblingProvider implements INaturalAssemblingDataProvider<ModulePojo> {

		@Override
		@Nullable
		public IAssemblingObjectType getType(final ModulePojo object) {
			switch (object.getType()) {
				case COPYCODE:
					return NaturalAssemblingObjectType.COPYCODE;
				case LDA:
					return NaturalAssemblingObjectType.LDA;
				case GDA:
					return NaturalAssemblingObjectType.GDA;
				case PDA:
					return NaturalAssemblingObjectType.PDA;
				default:
					return null;
			}
		}
		
		@Override
		@Nullable
		public IAst<? extends ModulePojo> getDependencyLightweightAst(@Nullable final ModulePojo object) throws Exception {
			return null;
		}

		@Override
		@Nullable
		public INaturalParseResult getParseResult(@Nullable final ModulePojo object) throws Exception {
			return null;
		}
		
	}

}
