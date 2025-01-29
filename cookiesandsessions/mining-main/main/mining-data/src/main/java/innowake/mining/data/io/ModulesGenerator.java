/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Creates the Modules of a Discovery result file.
 */
abstract class ModulesGenerator implements ModuleMappingSupplier {

	@Autowired
	private ModuleService moduleService;

	@Override
	public Map<Long, String> process(final EntityId projectId, final DiscoveryExportOptions options, final boolean sorted) throws IOException {
		final List<ModulePojo> modules = moduleService.findModules(b -> {
			b.ofProject(projectId);
			if (sorted) {
				b.sortName(SortDirection.ASCENDING)
				 .sortPath(SortDirection.ASCENDING)
				 .sortTechnology(SortDirection.ASCENDING)
				 .sortType(SortDirection.ASCENDING);
			}
		});

		final Map<Long, String> moduleMapping = new HashMap<>();
		for (final ModulePojo module : modules) {
			if (isInvalidEntry(module)) {
				continue;
			}

			final var location = module.getLocation();
			final var excelType = module.getTechnology() == Technology.UNKNOWN && module.getType() == Type.PROGRAM ? 
																/* Don't add ModuleType.UNKNOWN_PROGRAM(Technology.UNKNOWN, Type.PROGRAM) */
																Type.PROGRAM.toString() : 
																ModuleType.fromTechnologyAndType(module.getTechnology(), module.getType()).toString();
			moduleMapping.put(module.getId(), excelType);
			final var sourceMetrics = module.getSourceMetrics();
			final var parent = module.getParent();
			createRow(
					module.getId(),
					parent.map(EntityId::getNid).orElse(null),
					module.getName(),
					module.getPath().orElse(null),
					module.getTechnology().name(),
					excelType,
					module.getRepresentation().orElse(module.getPath().isPresent() ? Representation.PHYSICAL : Representation.VIRTUAL).name(), 
					sourceMetrics.map(SourceMetricsPojo::getComplexityMcCabe).orElse(-1),
					module.getErrors(),
					sourceMetrics.map(SourceMetricsPojo::getCodeLines).orElse(-1),
					sourceMetrics.map(SourceMetricsPojo::getCommentLines).orElse(-1),
					sourceMetrics.map(SourceMetricsPojo::getPhysicalLines).orElse(-1),
					module.getStatements(),
					module.getSqlStatements(),
					location.map(ModuleLocation::getOffset).orElse(-1),
					location.map(ModuleLocation::getLength).orElse(-1));
		}
		return Collections.unmodifiableMap(moduleMapping);
	}

	/**
	 * Creates a row of a single Module.
	 *
	 * @param values the values of a single Module
	 * @throws IOException if an error occurs
	 */
	@Override
	public abstract void createRow(final Object... values) throws IOException;

	long getModuleCount(final EntityId project) {
		return moduleService.countModules(q -> q.ofProject(project));
	}
	
	@SuppressWarnings("null")
	private boolean isInvalidEntry(final ModulePojo module) {
		return module.getIdentification() == Identification.MISSING || Type.UTILITY == module.getType() || module.getOrigin() == Origin.ENVIRONMENT
				|| (Technology.NONE == module.getTechnology() && Type.UNKNOWN == module.getType() && module.getPath().isPresent()
				&& Representation.PHYSICAL == module.getRepresentation().orElse(null));
	}
}
