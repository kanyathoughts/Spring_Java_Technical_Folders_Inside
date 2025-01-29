/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.List;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.api.DataProvider;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.assembling.cobol.CobolAssembler.CobolAssemblingObjectType;

/**
 * Parsing data provider for Mining entities.
 */
public final class MiningContentProvider implements DataProvider {
	
	protected static final Logger LOG = LoggerFactory.getLogger(MiningContentProvider.class);

	private final MiningDataCoreService core;
	private final ModulePojo module;

	/**
	 * Constructor:
	 * 
	 * @param core the {@link MiningDataCoreService}
	 * @param module the {@link ModulePojo}
	 */
	public MiningContentProvider(final MiningDataCoreService core, final ModulePojo module) {
		this.core = core;
		this.module = module;
	}
	
	@Nullable
	@Override
	public ModulePojo find(final ModulePojo module, final String name, final IAssemblingObjectType assemblingObjectType) {
		final Type type;
		try {
			type = getType(module, assemblingObjectType);
		} catch (final IllegalArgumentException e) {
			LOG.error(() -> String.format("Module with id %s has the unsupported type '%s'", module.identity(), assemblingObjectType), e);
			return null;
		}
		/* For now the Relationship is hardcoded to INCLUDES, when we support more technologies it may be necessary to use the module technology and 
		 * the assemblingObjectType to determine which reference needs to be used */
		return createModuleDescription(module, name, RelationshipType.INCLUDES, type);
	}
	
	@Override
	@Nullable
	public ModulePojo find2(final ModulePojo root, final String name, final IAssemblingObjectType... assemblingObjectTypes) {
		ModulePojo result = null;
		
		for (final IAssemblingObjectType assemblingObjectType : assemblingObjectTypes) {
			result = find(module, name, assemblingObjectType);
			if (result != null) {
				break;
			}
		}
		
		return result;
	}

	@Nullable
	@Override
	public Object getHashable(final ModulePojo module) {
		return module;
	}

	@Nullable
	@Override
	public String getName(final ModulePojo module) {
		return module.getName();
	}

	@Nullable
	@Override
	public String getPath(final ModulePojo module) {
		return null;
	}

	@Nullable
	@Override
	public String getSource(final ModulePojo module) throws AssemblingException {
		return module.getContent().orElse(null);
	}

	@Nullable
	@Override
	public IAssemblingObjectType getType(final ModulePojo module) {
		return null;
	}

	@Override
	public boolean isObjectProxy(final ModulePojo module) {
		return false;
	}
	
	@Nullable
	@Override
	public String getProc(@Nullable final String procName, final List<String> jclLibOrder) {
		final ModulePojo moduleDescription = createModuleDescription(module, assertNotNull(procName), RelationshipType.CALLS, Type.PROC);
		return moduleDescription != null ? moduleDescription.getContent().orElse(null) : null;
	}

	@Nullable
	@Override
	public String getIncludeMember(final String memberName, final List<String> jclLibOrder) {
		final ModulePojo moduleDescription = createModuleDescription(module, memberName, RelationshipType.INCLUDES, Type.INCLUDE);
		return moduleDescription != null ? moduleDescription.getContent().orElse(null) : null;
	}
	
	@Nullable
	public ModulePojo createModuleDescription(final ModulePojo module, final String includeeName, final RelationshipType relationship, final Type type) {
		LOG.debug(() -> String.format("Trying to determine content for %s of type %s referenced by module %s", includeeName, type, module.identity()));

		final List<ModulePojo> modules = core.moduleService.findModules(q -> q.withTransitiveSourceRelationshipsFrom(module.identity(), relationship)
																				.withName(includeeName, true)
																				.ofProject(module.getProject())
																				.withTechnology(module.getTechnology())
																				.withType(type)
																				.includeContent(true));
		for (final var mod : modules) {
			if (mod.getContent().isPresent()) {
				return mod;
			}
		}

		LOG.warn(() -> String.format("Could not find content for '%s' included by module '%s'.", includeeName, module.getName()));
		return null;
	}
	
	private Type getType(final ModulePojo module, final IAssemblingObjectType assemblingObjectType) {
		switch (module.getTechnology()) {
			case COBOL:
				return CobolAssemblingObjectType.COPYLIB.equals(assemblingObjectType) ? Type.COPYLIB : Type.COPYBOOK;
			case PL1:
				return Type.COPYBOOK;
			default:
				return Type.valueOf(assemblingObjectType.name());
		}
	}
}
