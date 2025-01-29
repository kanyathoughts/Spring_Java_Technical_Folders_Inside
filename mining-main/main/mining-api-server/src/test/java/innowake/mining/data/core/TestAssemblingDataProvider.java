/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.Type;
import innowake.ndt.cobol.parser.ResourceDataProvider;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.assembling.cobol.CobolAssembler.CobolAssemblingObjectType;

/**
 * Implementation of {@link IAssemblingDataProvider} for tests.
 */
public class TestAssemblingDataProvider implements IAssemblingDataProvider<ModulePojo> {
	
	private static final Logger LOG = LoggerFactory.getLogger(ResourceDataProvider.class);
	private final Charset charset;
	private final String copyFolder;
	private final Map<String, Long> moduleIds = new HashMap<>();
	private long nextModuleId;

	public TestAssemblingDataProvider(final Charset charset, final String copyFolder) {
		this.charset = charset;
		this.copyFolder = copyFolder;
		nextModuleId = 2;
	}
	
	@Nullable
	@Override
	public ModulePojo find(final ModulePojo root, final String name, final IAssemblingObjectType expectedType) {
		final Long moduleId;
		if (moduleIds.containsKey(name)) {
			moduleId = moduleIds.get(name);
		} else {
			moduleId = Long.valueOf(nextModuleId);
			moduleIds.put(name, moduleId);
			nextModuleId++;
		}
		return new ModulePojoDummy().prepare(q -> q.setNid(moduleId).setName(name)).build();
	}

	@Nullable
	@Override
	public String getPath(final ModulePojo object) {
		return object.getName();
	}

	@Nullable
	@Override
	public String getSource(final ModulePojo object) throws AssemblingException {
		final Path path = Paths.get(copyFolder, object.getName() + (object.getType().equals(Type.MAP) ? ".map" : ".cpy"));
		try (final InputStream in = Files.newInputStream(path)) {
			if (in == null) {
				LOG.warn("Could not get source for {}", object.getName());
				return null;
			}
			
			return IOUtils.toString(in, charset);
			
		} catch (final NoSuchFileException e) {
			LOG.warn("Could not get source for {}", object.getName());
			return null;
		} catch (final IOException e) {
			throw new AssemblingException(object, e);
		}
	}

	@Nullable
	@Override
	public IAssemblingObjectType getType(final ModulePojo object) {
		return CobolAssemblingObjectType.COPYBOOK;
	}

	@Nullable
	@Override
	public String getName(final ModulePojo object) {
		return FilenameUtils.getName(object.getName());
	}

	@Nullable
	@Override
	public Object getHashable(final ModulePojo object) {
		return object;
	}

	@Override
	public boolean isObjectProxy(ModulePojo object) {
		return false;
	}

}
