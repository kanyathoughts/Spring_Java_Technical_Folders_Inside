/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.ProfilingHelper.executeWithProfiling;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Imports {@code module} entities and {@code ExcelSheetModules} from Discovery.
 */
@Service
public class DiscoveryModuleImporter {

	private static final String PROFILING_CATEGORY = "discovery.moduleimporter";
	
	protected static final Logger LOG = LoggerFactory.getLogger(Logging.IO);
	private static final Profiler PROFILER = ProfilingFactory.getProfilingSession().getProfiler(PROFILING_CATEGORY);

	private static final String MISSING_MODULE_NAME = "<missing name>";

	private final ModuleService moduleService;

	/**
	 * Public constructor.
	 * 
	 * @param moduleService the {@link ModuleService}
	 */
	public DiscoveryModuleImporter(final ModuleService moduleService) {
		this.moduleService = moduleService;
	}

	/**
	 * Imports a {@link Module} from Discovery.
	 *
	 * @param projectId the Project ID property
	 * @param name the Name property
	 * @param path the Path property
	 * @param technology the Technology property
	 * @param type the Type property
	 * @param storage the Storage property
	 * @param sourceMetrics the {@link SourceMetricsPojoPrototype} of the module
	 * @param representation the Representation property
	 * @param location of the artifact inside the module
	 * @param moduleParameters the additional module parameters
	 * @return the new {@link Module} created
	 */
	public EntityId importModule(
			final EntityId projectId, 
			final String name, 
			final String path, 
			final Technology technology, 
			final Type type, 
			final Storage storage, 
			final SourceMetrics sourceMetrics, 
			final Representation representation, 
			@Nullable final ModuleLocation location,
			final ModuleParameters moduleParameters) {
		return updateOnExistingModule(projectId, name, path, technology, type, storage, sourceMetrics, representation, location, null, false, true, moduleParameters);
	}

	/**
	 * Imports the {@link ModelArtifact} into the database
	 * 
	 * @param projectId the project Id
	 * @param modelArtifact the {@link ModelArtifact} to import
	 * @param moduleParameters the additional module parameters
	 * @return the imported {@link Module}
	 */
	public EntityId importModule(final EntityId projectId, final ModelArtifact modelArtifact, @Nullable final ModuleParameters moduleParameters) {
		return executeWithProfiling(() -> importOrUpdateModule(projectId, modelArtifact, false, true, moduleParameters), PROFILER, "importModule");
	}

	/**
	 * Updates the {@link ModelArtifact} in the database
	 * 
	 * @param projectId the project Id
	 * @param artifact the {@link ModelArtifact} to update
	 * @param moduleParameters the additional module parameters
	 * @return the updated {@link Module}
	 */
	public EntityId updateModule(final EntityId projectId, final ModelArtifact artifact, @Nullable final ModuleParameters moduleParameters) {
		return executeWithProfiling(() -> importOrUpdateModule(projectId, artifact, true, false, moduleParameters), PROFILER, "updateModule");
	}

	private EntityId importOrUpdateModule(final EntityId projectId, final ModelArtifact modelArtifact, final boolean update, final boolean generateUid,
			@Nullable final ModuleParameters moduleParameters) {
		final Technology technology = ResolveTargetHelper.toTechnology(modelArtifact.getLanguage());
		final ResolveTarget modelType = modelArtifact.getType();
		final Type type = ResolveTargetHelper.toType(modelType);
		Storage storage = Storage.from(technology, type);
		if (modelArtifact.getPath().isPresent() && storage == Storage.FILE_SECTION) {
			storage = Storage.FILE;
		}
		final String name = modelArtifact.getName();
		final String actualName = StringUtils.isBlank(name) ? MISSING_MODULE_NAME : name;

		final SourceMetrics sourceMetrics;
		if (modelArtifact instanceof LazyModelArtifact) {
			sourceMetrics = ((LazyModelArtifact) modelArtifact).getResolvedSourceMetrics();
		} else {
			sourceMetrics = modelArtifact.getSourceMetrics();
		}
		
		return updateOnExistingModule(projectId, actualName, modelArtifact.getPath().orElse(null), technology, type, storage, sourceMetrics,
				modelArtifact.getRepresentation(), modelArtifact.getLocation().orElse(null), modelArtifact, update, generateUid, moduleParameters);

	}

	private EntityId updateOnExistingModule(final EntityId projectId, final String name, final String path, final Technology technology, final Type type,
			final Storage storage, @Nullable final SourceMetrics sourceMetrics,
			final Representation representation, @Nullable final ModuleLocation location, @Nullable final ModelArtifact modelArtifact, final boolean update,
			final boolean generateUid, @Nullable final ModuleParameters moduleParameters) {
		EntityId moduleId;
		try {
			moduleId = importOrUpdateModuleInternal(projectId, name, path, technology, type, storage, sourceMetrics,
					representation, location, modelArtifact, update, generateUid, moduleParameters);
		} catch (final ConstraintViolationException e) {
			//TODO npr which exception is thrown by Postgres?
			LOG.warn(() -> "Exception occured while trying to import the module", e);

			try {
				/* Currently only the path can lead to this constraint violation, as the Id is always handled by a sequence. */
				final String lookUpPath = assertNotNull(modelArtifact).getPath().orElseThrow(() -> new IllegalStateException("Module path must be present"));
				final ModulePojo existingModule = moduleService.findAnyModule(b -> b.ofProject(projectId).withPath(lookUpPath))
																.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: " + lookUpPath));
				if (LOG.isWarnEnabled()) {
					LOG.warn("Tried to import module that already exists in DB! Existing module was {}.", existingModule.getPath());
					LOG.warn("Existing module: %s. New module: name={}, path={}", existingModule, name, path);
				}
				assertNotNull(modelArtifact).setModuleId(existingModule.identity());

				moduleId = importOrUpdateModuleInternal(projectId, name, path, technology, type, storage, sourceMetrics,
						representation, location, modelArtifact, true, generateUid, moduleParameters);
			} catch (final ConstraintViolationException | MiningEntityNotFoundException exp) {
				throw new IllegalStateException(String.format("Module artifact %s with path %s is failed to import: %s", assertNotNull(modelArtifact).getName(),
						assertNotNull(modelArtifact).getPath(), exp));
			}
		}

		return moduleId;
	}

	private EntityId importOrUpdateModuleInternal(final EntityId projectId, final String name, final String path, final Technology technology, final Type type,
			final Storage storage, @Nullable final SourceMetrics sourceMetrics, final Representation representation, @Nullable final ModuleLocation location,
			@Nullable final ModelArtifact modelArtifact, final boolean update, final boolean generateUid, @Nullable final ModuleParameters moduleParameters) {

		final var module = new ModulePojoPrototype();
		module.setName(StringUtils.isBlank(name) ? MISSING_MODULE_NAME : name);
		if (StringUtils.isNotBlank(path)) {
			module.setPath(path);
		}
		module.setProject(projectId);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(storage);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		module.setRepresentation(representation);
		if (moduleParameters != null) { /* if null, the modified date is handled in dao layer */
			module.setMetricsDate(moduleParameters.getMetricsDate());
			module.setModifiedDate(moduleParameters.getMetricsDate());
		}

		if (sourceMetrics != null && sourceMetrics.isModified()) {
			module.setSourceMetrics(sourceMetrics.convertToPojoPrototype());
		}

		if (location != null) {
			module.setLocation(location);
		}

		if (modelArtifact != null) {
			if ( ! generateUid) {
				module.setUid(assertNotNull(modelArtifact.getModuleId()).getUid());
			}
			if (update) {
				module.setNid(assertNotNull(modelArtifact.getModuleId()).getNid());
			}

			final var parent = modelArtifact.getParentModule();
			if (parent != null) {
				if (parent.getModuleId() != null) {
					module.setParent(assertNotNull(parent.getModuleId()));
				}
				parent.getPath().ifPresent(module::setParentPath);
			} else {
				modelArtifact.getParentPath().ifPresent(module::setParentPath);
			}

			module.setInfo(modelArtifact.getMetadata());
		}

		final EntityId moduleId;
		if (update) {
			moduleService.update(module);
			moduleId = module.identityProvisional();
		} else {
			moduleId = moduleService.create(module);
		}

		return moduleId;
	}

}
