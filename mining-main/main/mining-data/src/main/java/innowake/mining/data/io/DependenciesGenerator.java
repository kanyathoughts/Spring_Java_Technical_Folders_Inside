/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Creates the Dependencies sheet of a Discovery result file.
 */
abstract class DependenciesGenerator implements ModuleMappingConsumer {

	private static final Integer VALUE_UNKNOWN = Integer.valueOf(-1);
	private static final Long SYSTEM_UTILITY_NID = Long.valueOf(DiscoveryUidUtils.UTILITY_UID);
	private static final Tuple2<Long, String> SYSTEM_UTILITY_UID_AND_TYPE = new Tuple2<>(SYSTEM_UTILITY_NID, Type.UTILITY.name());

	@Autowired
	private ModuleService moduleService;

	/**
	 * Processes all Dependencies for a Discovery result file.
	 *
	 * @param projectId the project ID
	 * @param moduleMapping mapping between Module ID and a {@link Tuple2} of UID and Discovery type
	 * @param sorted if {@code true}, sorts the result
	 * @throws IOException if an error occurs
	 */
	@Override
	public void process(final EntityId projectId,
			final Map<Long, String> moduleMapping,
			final DiscoveryExportOptions options,
			final boolean sorted) throws IOException {
		final Optional<Table> data = moduleService.getModuleRelationshipExport(projectId, sorted);
		if (data.isPresent()) {
			final var iterator = data.get().iterator();
			if (iterator.hasNext()) {
				final var missingArtifacts = getMissingArtifacts(projectId);
				final Map<Long, Tuple2<Long, String>> nidToTypes = new HashMap<>(data.get().size());
				while (iterator.hasNext()) {
					final var row = iterator.next();

					final var sourceNidAndType = nidToTypes.computeIfAbsent(row.getNonNull("src_nid"),
																			nid -> getNidAndType(moduleMapping, nid, projectId, missingArtifacts));
					final var targetNidAndType = nidToTypes.computeIfAbsent(row.getNonNull("dst_nid"),
																			nid -> getNidAndType(moduleMapping, nid, projectId, missingArtifacts));
					final ModuleLocation srcLocation = row.getNullable("src_location");
					final ModuleLocation dstLocation = row.getNullable("dst_location");
					/* CALLS -> Calls */
					final var referenceName = StringUtils.capitalize(((String) row.getNonNull("type")).toLowerCase());

					final List<Long> reachedFromModules = row.getNonNull("reached_from_module");
					final String reachedFromUids = ! reachedFromModules.isEmpty() ? reachedFromModules.stream()
																						.sorted().collect(Collectors.toList()).toString()
							: StringUtils.EMPTY;

					createRow(sourceNidAndType.a,
								row.getNonNull("src_name"),
								targetNidAndType.a,
								row.getNonNull("dst_name"),
								row.getNonNull("dst_technology"),
								targetNidAndType.b,
								referenceName,
								reachedFromUids,
								row.get("dependency_binding"),
								row.get("dependency_attributes"),
								defaultInteger(srcLocation == null ? null : srcLocation.getOffset()),
								defaultInteger(srcLocation == null ? null : srcLocation.getLength()),
								defaultInteger(dstLocation == null ? null : dstLocation.getOffset()),
								defaultInteger(dstLocation == null ? null : dstLocation.getLength()));
				}
			}
		}
	}

	/**
	 * Returns the number of dependencies associated with the given project. 
	 *
	 * @param projectId the ID of the project to search for dependencies
	 * @return the number of dependencies
	 */
	public long getDependenciesCount(final EntityId projectId) {
		return moduleService.countRelationships(b -> b.ofProject(projectId));
	}

	private static Integer defaultInteger(@Nullable final Integer input) {
		return input == null ? VALUE_UNKNOWN : input;
	}

	private Tuple2<Long, String> getNidAndType(final Map<Long, String> moduleMapping, final Long module, final EntityId project, final Map<Long, String> missings) {
		final var type = moduleMapping.get(module);
		if (type != null) {
			return new Tuple2<>(module, type);
		}

		if (missings.containsKey(module)) {
			return new Tuple2<>(DiscoveryUidUtils.MISSING_UID_LONG, missings.get(module));
		}

		final var modules = moduleService.findModulesLightweight(b -> b.ofProject(project).byNid(module));
		return modules.isEmpty() ? SYSTEM_UTILITY_UID_AND_TYPE : new Tuple2<>(SYSTEM_UTILITY_NID, modules.get(0).getType().name());
	}

	private Map<Long, String> getMissingArtifacts(final EntityId projectId) {
		return moduleService.findModulesLightweight(q -> q.ofProject(projectId).withIdentified(false)).stream()
				.collect(Collectors.toMap(ModuleLightweightPojo::getId,
											module -> ResolveTargetHelper.fromTechnologyAndType(module.getTechnology(), module.getType()).name()));
	}
}
