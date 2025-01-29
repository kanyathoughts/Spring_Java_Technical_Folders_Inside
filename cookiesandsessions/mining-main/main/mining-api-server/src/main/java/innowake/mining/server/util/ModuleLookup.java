/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.TaxonomyImportValidationResult.Marker;
import innowake.mining.shared.model.TaxonomyImportValidationResult.MarkerType;

/**
 * Utility class to find a suitable match of the {@linkplain ModulePojo module}.
 */
public class ModuleLookup {
	
	private ModuleLookup() {}
	
	/**
	 * Method to find the best matching {@link ModulePojo module} with the given path, technology, type and module name.
	 *
	 * @param parameters the module look up parameters
	 * @return the best match module
	 */
	public static ModuleLookupResult lookUpModule(final ModuleLookupParameters parameters) {
		final var resolvedModule = lookUpModule(Collections.emptyList(), 0, Collections.emptyList(), parameters);

		if (resolvedModule.module != null && resolvedModule.markers.stream()
																	.map(Marker::getMarkerType)
																	.anyMatch(markerType -> MarkerType.WARNING == markerType)) {
				resolvedModule.requiresReview = true;
		}

		return resolvedModule;
	}
	
	/**
	 * Method to find the best matching {@link ModulePojo module} with the given path, technology, type and module name.
	 *
	 * @param lineHeader the line header
	 * @param lineNumber the line number
	 * @param lineContent the line content
	 * @param moduleLookupParameters the module look up parameters
	 * @return the best matching module
	 */
	public static ModuleLookupResult lookUpModule(final List<String> lineHeader, final int lineNumber, 
			final List<String> lineContent, final ModuleLookupParameters moduleLookupParameters) {
		final List<ModulePojo> modules = moduleLookupParameters.modules;
		final String path = moduleLookupParameters.path;
		final String technology = moduleLookupParameters.technology;
		final String type = moduleLookupParameters.type;
		final List<Marker> markers = new ArrayList<>();
		final Predicate<ModulePojo> pathMatch = module ->
				StringUtils.isBlank(path) && StringUtils.isBlank(module.getPath().orElse(null)) && 
				StringUtils.isBlank(module.getParentPath().orElse(null)) || path != null &&
				(path.trim().equalsIgnoreCase(module.getPath().orElse(null)) || path.trim().equalsIgnoreCase(module.getParentPath().orElse(null)));
		final Predicate<ModulePojo> technologyAndTypeMatch = module -> (technology != null &&  technology.trim().equalsIgnoreCase(module.getTechnology().name())) && 
				(type !=  null && type.trim().equalsIgnoreCase(module.getType().name()));
		final List<ModulePojo> exactMatch = modules.stream().filter(pathMatch.and(technologyAndTypeMatch)).collect(Collectors.toList());
		ModulePojo resolvedModule = null;
		boolean requiresReview = false;
		if (exactMatch.size() == 1) {
			resolvedModule = exactMatch.get(0);
		} else if (exactMatch.isEmpty()) {
			final List<ModulePojo> partialMatchedModules = modules.stream().filter(technologyAndTypeMatch).collect(Collectors.toList());
			if (partialMatchedModules.size() == 1) {
				resolvedModule = partialMatchedModules.get(0);
			}
			if ((partialMatchedModules.size() == 1 && path != null) || (partialMatchedModules.isEmpty() && modules.size() == 1)) {
				final ModulePojo module = partialMatchedModules.isEmpty() ? modules.get(0): partialMatchedModules.get(0);
				final String[] foundValues = {
						module.getName(), module.getPath().orElse(null), module.getTechnology().name(), module.getType().name()
				};
				markers.add(new Marker(lineNumber, lineHeader, lineContent, MarkerType.WARNING,
						"Could not find exact match. Using Module " + String.join(", ", foundValues) + " instead of " + String.join(", ", lineContent)));
				resolvedModule = module;
				requiresReview = true;
			}
		}
		if (resolvedModule == null) {
			if (modules.size() > 1) {
				markers.add(new Marker(lineNumber, lineHeader, lineContent, MarkerType.ERROR,
						"Several matching Modules found for " + String.join(", ", lineContent)));
			} else {
				markers.add(new Marker(lineNumber, lineHeader, lineContent, MarkerType.ERROR, "Module not found for " + String.join(", ", lineContent)));
			}
		}

		return new ModuleLookupResult(resolvedModule, markers, requiresReview);
	}

	public static class ModuleLookupResult {

		@Nullable
		private final ModulePojo module;
		private final List<Marker> markers;
		private boolean requiresReview;

		public ModuleLookupResult(@Nullable final ModulePojo module, final List<Marker> markers, final boolean requiresReview) {
			this.module = module;
			this.requiresReview = requiresReview;
			this.markers = markers;
		}

		/**
		 * @return {@code true} if the module requires a review
		 */
		public boolean isRequiresReview() {
			return requiresReview;
		}

		/**
		 * @return the resolved module or {@code null}, if no match was found
		 */
		@Nullable
		public ModulePojo getModule() {
			return module;
		}

		/**
		 * @return the markers, that have been created while doing the lookup
		 */
		public List<Marker> getMarkers() {
			return markers;
		}
	}

	/**
	 * The parameters for the module look up.
	 */
	public static class ModuleLookupParameters {
		
		private final List<ModulePojo> modules;
		@Nullable
		private final String path;
		@Nullable
		private final String technology;
		@Nullable
		private final String type;
		
		/**
		 * Constructor.
		 * 
		 * @param modules the modules fetched from Db
		 * @param path the path
		 * @param technology the technology
		 * @param type the type
		 */
		public ModuleLookupParameters(final List<ModulePojo> modules, @Nullable final String path, @Nullable final String technology, @Nullable final String type) {
			this.modules = modules;
			this.path = path;
			this.technology = technology;
			this.type = type;
		}
	}

}
