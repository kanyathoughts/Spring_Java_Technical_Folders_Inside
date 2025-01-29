/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.RelationshipType;

/**
 * Retrieves the information for {@linkplain DependencyModule dependency modules}.
 */
public class DependencyModuleDataFetcher {
	
	private static final Logger LOG = LoggerFactory.getLogger(DependencyModuleDataFetcher.class);

	private DependencyModuleDataFetcher() {}

	/**
	 * Returns the dependency module associated with the given Module ID.
	 * <p>
	 * This will only get the direct neighbors and will not traverse transitively.
	 * @param projectId Project of the Module
	 * @param moduleId the ID of the Module
	 * @param core the {@link MiningDataCoreService} to access entity services
	 * @return the dependency module associated with the given Module ID
	 */
	public static Optional<DependencyModule> getModule(final EntityId projectId, final EntityId moduleId, final MiningDataCoreService core) {
		final Optional<ModuleLightweightPojo> moduleInformationOpt = core.moduleService.findAnyModuleLightweight(q -> q.ofProject(projectId).byId(moduleId));

		if (moduleInformationOpt.isEmpty()) {
			LOG.warn(() -> String.format("Could not find Module with ID %s", moduleId));
			return Optional.empty();
		}

		final ModuleLightweightPojo module = moduleInformationOpt.get();
		final DefaultDependecyModule result = new DefaultDependecyModule(projectId, module.getTechnology(), module.getType(), module.getName(), null);

		addIncomings(projectId, moduleId, core, result);
		addOutgoings(projectId, moduleId, core, result);

		return Optional.of(result);
	}

	private static void addOutgoings(final EntityId project, final EntityId moduleId, final MiningDataCoreService core, final DefaultDependecyModule result) {
		final List<ModuleRelationshipPojo> outgoings = core.moduleService.findRelationship(q -> q.ofSource(moduleId).withTypes(RelationshipType.DEPENDENCY_TYPES));
		if ( ! outgoings.isEmpty()) {
			final Set<UUID> moduleUids = outgoings.stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet());
			final Map<UUID, ModuleLightweightPojo> modules = core.moduleService.findModulesLightweight(q -> q.byUids(moduleUids)).stream()
																		.collect(Collectors.toMap(ModuleLightweightPojo::getUid, pojo -> pojo));
			outgoings.forEach(r -> {
				final var dst = modules.get(r.getDstModule());
				if (dst == null) {
					throw new IllegalStateException("Destination module " + r.getDstModule() + " was not loaded properly from DB");
				}

				final Map<String, Object> properties = r.getProperties().orElse(Collections.emptyMap());
				result.addOutgoing(new DefaultDependecyModule(project, dst.getTechnology(), dst.getType(), dst.getName(), r.getSrcLocation().orElse(null)),
									r.getRelationship(), properties);
			});
		}
	}

	private static void addIncomings(final EntityId project, final EntityId moduleId, final MiningDataCoreService core, final DefaultDependecyModule result) {
		final List<ModuleRelationshipPojo> incomings = core.moduleService.findRelationship(q -> q.ofDestination(moduleId).withTypes(RelationshipType.DEPENDENCY_TYPES));
		if ( ! incomings.isEmpty()) {
			final Set<UUID> moduleUids = incomings.stream().map(ModuleRelationshipPojo::getSrcModule).collect(Collectors.toSet());
			final Map<UUID, ModuleLightweightPojo> modules = core.moduleService.findModulesLightweight(q -> q.byUids(moduleUids)).stream()
																		.collect(Collectors.toMap(ModuleLightweightPojo::getUid, pojo -> pojo));
			incomings.forEach(r -> {
				final var src = modules.get(r.getSrcModule());
				if (src == null) {
					throw new IllegalStateException("Source module " + r.getSrcModule() + " was not loaded properly from DB");
				}

				final Map<String, Object> properties = r.getProperties().orElse(Collections.emptyMap());
				result.addIncoming(new DefaultDependecyModule(project, src.getTechnology(), src.getType(), src.getName(), r.getSrcLocation().orElse(null)),
									r.getRelationship(), properties);
			});
		}
	}
}
