/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.callchain;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.preset.AbstractBuilder;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;

/**
 * The parameters for the {@link CallChainService}.
 * <p>
 * IMPORTANT: If you add a new parameter, make sure to add a proper description and example to the JavaDoc of {@link CallChainExporter}
 * and extend {@link Builder#fromMap(Map)}!
 */
public class Parameters implements Serializable {

	/**
	 * Name of the {@code ignored taxonomy IDs} parameter
	 */
	public static final String PARAMETER_IGNORED_BY_TAXONOMY = "ignoredTaxonomy";
	/**
	 * Name of the {@code parallel} parameter
	 */
	public static final String PARAMETER_PARALLEL = "parallel";
	/**
	 * Name of the {@code depth} parameter
	 */
	public static final String PARAMETER_DEPTH = "depth";
	/**
	 * Name of the {@code filtered module} {@link Type Types} parameter
	 */
	public static final String PARAMETER_FILTERED_TYPE = "filteredType";
	/**
	 * Name of the {@code data access based} parameter
	 */
	public static final String PARAMETER_DATA_ACCESS_BASED = "dataAccessBased";
	/**
	 * Name of the {@code filtered module names} parameter
	 */
	public static final String PARAMETER_FILTERED_NAME = "filteredName";
	/**
	 * Name of the {@code call types} parameter from, see {@link CallChainExporterJob#ALL_RELATIONSHIP_TYPES}
	 */
	public static final String PARAMETER_CALL_TYPE = "callType";
	/**
	 * Name of the {@link innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection CallChainDirections} parameter
	 */
	public static final String PARAMETER_DIRECTIONS = "direction";
	/**
	 * Name of the {@code end module} {@link Type Types} parameter
	 */
	public static final String PARAMETER_END_MODULE_TYPES = "endModuleType";
	/**
	 * Name of the {@code end module names} parameter
	 */
	public static final String PARAMETER_END_MODULE_NAMES = "endModuleName";
	/**
	 * Name of the {@code end module IDs} parameter
	 */
	public static final String PARAMETER_END_MODULE_IDS = "endModuleId";
	/**
	 * Name of the {@code start module} {@link Type Types} parameter
	 */
	public static final String PARAMETER_START_MODULE_TYPES = "startModuleType";
	/**
	 * Name of the {@code start module IDs} parameter
	 */
	public static final String PARAMETER_START_MODULE_IDS = "startModuleId";
	/**
	 * Name of the {@code compressed} parameter
	 */
	public static final String PARAMETER_COMPRESSED = "compressed";
	/**
	 * Name of the {@code edge property filter} parameter
	 */
	public static final String PARAMETER_EDGE_PROPERTY_FILTER = "edgePropertyFilter";
	/**
	 * Name of the {@code export format} parameter, {@code CSV} or {@code GraphML}
	 */
	public static final String PARAMETER_EXPORT_FORMAT = "exportFormat";

	private final EntityId projectId;
	private final List<EntityId> startModuleIds;
	private final Set<Type> startModuleTypes;
	private final List<EntityId> endModuleIds;
	private final Set<Type> endModuleTypes;
	private final List<CallChain.CallChainDirection> directions;
	private final Set<RelationshipType> callTypes;
	private final Map<String, Serializable> edgePropertyFilter;
	private final Set<String> filteredModuleNames;
	private final Set<Type> filteredModuleTypes;
	private final boolean dataAccessBased;
	private final int depth;
	private final int parallel;
	private final List<EntityId> ignoredTaxonomy;
	private final boolean compressed;
	private final CallChainExporterJob.ExportFormat exportFormat;

	private Parameters(final Builder builder) {
		this.projectId = assertNotNull(builder.projectId, "Project ID must not be null");
		this.startModuleIds = assertNotNull(builder.startModuleIds, "start module IDs must not be null");
		this.startModuleTypes = assertNotNull(builder.startModuleTypes, "start module types  must not be null");
		this.endModuleIds = assertNotNull(builder.endModuleIds, "end module IDs must not be null");
		this.endModuleTypes = assertNotNull(builder.endModuleTypes, "end module types must not be null");
		this.directions = assertNotNull(builder.directions, "Call chain direction list must not be null");
		this.callTypes = assertNotNull(builder.callTypes, "Call types must not be null");
		this.edgePropertyFilter = assertNotNull(builder.edgePropertyFilter, "Edge property filter must not be null");
		this.filteredModuleNames = assertNotNull(builder.filteredModuleNames, "filtered module names must not be null");
		this.filteredModuleTypes = assertNotNull(builder.filteredModuleTypes, "filtered module types must not be null");
		this.ignoredTaxonomy = assertNotNull(builder.ignoredTaxonomy, "List of ignored taxonomies must not be null");
		this.dataAccessBased = builder.dataAccessBased;
		this.depth = builder.depth;
		this.parallel = builder.parallel;
		this.compressed = builder.compressed;
		this.exportFormat = assertNotNull(builder.exportFormat, "Export format must not be null");
	}

	@Override
	public String toString() {
		return String.format(
				"[startModuleIds=%s, startModuleTypes=%s, endModuleIds=%s, endModuleTypes=%s, directions=%s, callTypes=%s, dataAccessBased=%b, "
						+ "filteredNames=%s, filteredTypes=%s, ignoredTaxonomy=%s, depth=%d, edgePropertyFilter=%s]",
				EntityId.allNids(startModuleIds),
				startModuleTypes.stream().map(Type::name).sorted().collect(Collectors.toList()),
				EntityId.allNids(endModuleIds),
				endModuleTypes.stream().map(Type::name).sorted().collect(Collectors.toList()),
				directions,
				callTypes.stream().map(RelationshipType::name).sorted().collect(Collectors.toList()),
				dataAccessBased,
				filteredModuleNames.stream().sorted().collect(Collectors.toList()),
				filteredModuleTypes.stream().map(Type::name).sorted().collect(Collectors.toList()),
				EntityId.allNids(ignoredTaxonomy),
				depth, 
				new TreeMap<>(edgePropertyFilter));
	}

	/**
	 * @return the project id
	 */
	public EntityId getProjectId() {
		return projectId;
	}

	/**
	 * @return the IDs of the modules for which the {@code Call Chains} must be exported
	 */
	public List<EntityId> getStartModuleIds() {
		return startModuleIds;
	}

	/**
	 * @return the {@link Type Types} for which the {@code Call Chains} must be exported
	 */
	public Set<Type> getStartModuleTypes() {
		return startModuleTypes;
	}

	/**
	 * @return the {@link Type Types} of modules for which to stop the {@code Call Chain} export
	 */
	public Set<Type> getEndModuleTypes() {
		return endModuleTypes;
	}

	/**
	 * @return {@link innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection CallChainDirections} for the {@code Call Chain} export
	 */
	public List<CallChain.CallChainDirection> getDirections() {
		return directions;
	}

	/**
	 * @return the {@link RelationshipType Relationships} call types that must be included in the {@code Call Chain} export
	 */
	public Set<RelationshipType> getCallTypes() {
		return callTypes;
	}

	/**
	 * @return a filter which determines which edges to follow based on properties that are present on the edges.
	 */
	public Map<String, Serializable> getEdgePropertyFilter() {
		return edgePropertyFilter;
	}

	/**
	 * @return the names of modules which must not be included in the {@code Call Chain} export
	 */
	public Set<String> getFilteredModuleNames() {
		return filteredModuleNames;
	}

	/**
	 * @return the {@link Type Types} of modules which must not be included in the {@code Call Chain} export
	 */
	public Set<Type> getFilteredModuleTypes() {
		return filteredModuleTypes;
	}

	/**
	 * @return {@code true} if data access based {@code Call Chain} export is required. Otherwise {@code false}
	 */
	public boolean isDataAccessBased() {
		return dataAccessBased;
	}

	/**
	 * @return {@code true} if the result of the {@code Call Chain} should be returned in a ZIP compressed file (zip). Otherwise {@code false}
	 */
	public boolean isCompressed() {
		return compressed;
	}

	/**
	 * @return the maximum depth of the export or -1 for no depth limit
	 */
	public int getDepth() {
		return depth;
	}

	/**
	 * @return maximum number of parallel running export tasks
	 */
	public int getParallel() {
		return parallel;
	}

	/**
	 * @return the IDs of to be ignored Taxonomies
	 */
	public List<EntityId> getIgnoredTaxonomy() {
		return ignoredTaxonomy;
	}

	/**
	 * @return the {@link CallChainExporterJob.ExportFormat}
	 */
	public CallChainExporterJob.ExportFormat getExportFormat() {
		return exportFormat;
	}

	/**
	 * @return the IDs of {@link Module} for which to stop the {@code Call Chain} export
	 */
	public List<EntityId> getEndModuleIds() {
		return endModuleIds;
	}

	/**
	 * Builder for the {@link Parameters} of the {@link CallChainService}.
	 */
	public static class Builder extends AbstractBuilder<Parameters, Builder> {

		@Nullable
		private EntityId projectId;
		@Nullable
		private List<EntityId> startModuleIds = Collections.emptyList();
		@Nullable
		private Set<Type> startModuleTypes = Collections.emptySet();
		@Nullable
		private List<EntityId> endModuleIds = Collections.emptyList();
		@Nullable
		private Set<Type> endModuleTypes = Collections.emptySet();
		@Nullable
		private List<CallChain.CallChainDirection> directions = Collections.emptyList();
		@Nullable
		private Set<RelationshipType> callTypes = Collections.emptySet();
		@Nullable
		private Map<String, Serializable> edgePropertyFilter = Collections.emptyMap();
		@Nullable
		private Set<String> filteredModuleNames = Collections.emptySet();
		@Nullable
		private Set<Type> filteredModuleTypes = Collections.emptySet();
		@Nullable
		private List<EntityId> ignoredTaxonomy = Collections.emptyList();

		private boolean compressed = true;
		private boolean dataAccessBased;
		private int depth = -1;
		private int parallel;
		private CallChainExporterJob.ExportFormat exportFormat = CallChainExporterJob.ExportFormat.CSV;

		/**
		 * Creates the builder from a parameter map.
		 * <p>
		 * {@link #setEdgePropertyFilter(Map)} has to be called explicitly with the appropriate map as this is not
		 * called from within this method.
		 *
		 * @param parameters a map of parameters, see the constants in {@link Parameters} for the parameter keys
		 * @return the builder instance
		 */
		public Builder fromMap(final Map<String, List<String>> parameters) {
			/* read parameters and apply defaults */
			setStartModuleIds(Optional.ofNullable(parameters.get(Parameters.PARAMETER_START_MODULE_IDS))
					.orElse(Collections.emptyList())
					.stream()
					.map(EntityId::of)
					.collect(Collectors.toList()));
			setStartModuleTypes(Optional.ofNullable(parameters.get(Parameters.PARAMETER_START_MODULE_TYPES))
					.orElse(Collections.emptyList())
					.stream()
					.map(this::getType)
					.collect(Collectors.toCollection(() -> EnumSet.noneOf(Type.class))));
			setEndModuleIds(Optional.ofNullable(parameters.get(Parameters.PARAMETER_END_MODULE_IDS))
					.orElse(Collections.emptyList())
					.stream()
					.map(EntityId::of)
					.collect(Collectors.toList()));
			setEndModuleTypes(Optional.ofNullable(parameters.get(Parameters.PARAMETER_END_MODULE_TYPES))
					.orElse(Collections.emptyList())
					.stream()
					.map(this::getType)
					.collect(Collectors.toCollection(() -> EnumSet.noneOf(Type.class))));
			setDirections(Optional.ofNullable(parameters.get(Parameters.PARAMETER_DIRECTIONS))
					.orElse(Collections.singletonList(CallChain.CallChainDirection.OUT.name()))
					.stream()
					.map(CallChain.CallChainDirection::valueOf)
					.collect(Collectors.toList()));
			setCallTypes(Optional.ofNullable(parameters.get(Parameters.PARAMETER_CALL_TYPE))
					.orElse(CallChainExporterJob.ALL_RELATIONSHIP_TYPES)
					.stream()
					.map(RelationshipType::from)
					.collect(Collectors.toCollection(() -> EnumSet.noneOf(RelationshipType.class))));
			setFilteredModuleNames(new HashSet<>(
					Optional.ofNullable(parameters.get(Parameters.PARAMETER_FILTERED_NAME)).orElse(Collections.emptyList())));
			setFilteredModuleTypes(Optional.ofNullable(parameters.get(Parameters.PARAMETER_FILTERED_TYPE))
					.orElse(Collections.emptyList())
					.stream()
					.map(this::getType)
					.collect(Collectors.toCollection(() -> EnumSet.noneOf(Type.class))));
			setDataAccessBased(Optional.ofNullable(parameters.get(Parameters.PARAMETER_DATA_ACCESS_BASED)).map(t -> Boolean.valueOf(t.get(0))).orElse(Boolean.FALSE));
			setDepth(Optional.ofNullable(parameters.get(Parameters.PARAMETER_DEPTH))
					.map(t -> Integer.valueOf(t.get(0)))
					.orElse(-1));
			/* "hidden" parameter that allows to limit concurrency - set to 1 for testing to produce deterministic results */
			setParallel(Optional.ofNullable(parameters.get(Parameters.PARAMETER_PARALLEL))
					.map(t -> Integer.valueOf(t.get(0)))
					.orElse(-1));
			setIgnoredTaxonomies(Optional.ofNullable(parameters.get(Parameters.PARAMETER_IGNORED_BY_TAXONOMY))
					.orElse(Collections.emptyList())
					.stream()
					.map(EntityId::of)
					.collect(Collectors.toList()));
			setCompressed(Optional.ofNullable(parameters.get(Parameters.PARAMETER_COMPRESSED))
					.map(c -> Boolean.valueOf(c.get(0)))
					.orElse(Boolean.TRUE));
			setExportFormat(Optional.ofNullable(parameters.get(Parameters.PARAMETER_EXPORT_FORMAT))
					.map(value -> CallChainExporterJob.ExportFormat.valueOf(value.get(0).toUpperCase()))
					.orElse(CallChainExporterJob.ExportFormat.CSV));

			return getThis();
		}
		/**
		 * Sets the project Id.
		 *
		 * @param projectId the project Id
		 * @return this builder instance
		 */
		public Builder setProjectId(final EntityId projectId) {
			this.projectId = projectId;
			return getThis();
		}

		/**
		 * Sets the IDs of the modules for which the {@code Call Chains} must be exported.
		 * <p>If the start module IDs and start module types are not provided, then the start module IDs are automatically determined based on modules with
		 * no incoming references</p>
		 *
		 * @param startModuleIds the start module IDs
		 * @return this builder instance
		 */
		public Builder setStartModuleIds(final List<EntityId> startModuleIds) {
			this.startModuleIds = startModuleIds;
			return getThis();
		}

		/**
		 * Sets the {@link Type Types} for which the {@code Call Chains} must be exported.
		 * <p>If the start module IDs and start module types are not provided, then the start module IDs are automatically determined based on modules with
		 * no incoming references</p>
		 *
		 * @param startModuleTypes the start module types
		 * @return this builder instance
		 */
		public Builder setStartModuleTypes(final Set<Type> startModuleTypes) {
			this.startModuleTypes = startModuleTypes;
			return getThis();
		}

		/**
		 * Sets the IDs of {@link Module} for which to stop the {@code Call Chain} export
		 *
		 * @param endModuleIds the end module IDs
		 * @return this builder instance
		 */
		public Builder setEndModuleIds(final List<EntityId> endModuleIds) {
			this.endModuleIds = endModuleIds;
			return getThis();
		}

		/**
		 * Sets the {@link Type Types} of modules for which to stop the {@code Call Chain} export
		 *
		 * @param endModuleTypes the end module {@link Type Types}
		 * @return this builder instance
		 */
		public Builder setEndModuleTypes(final Set<Type> endModuleTypes) {
			this.endModuleTypes = endModuleTypes;
			return getThis();
		}

		/**
		 * Sets the {@link innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection CallChainDirections} for the {@code Call Chain} export
		 *
		 * @param directions the {@link innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection CallChainDirections}
		 * @return this builder instance
		 */
		public Builder setDirections(final List<CallChain.CallChainDirection> directions) {
			this.directions = directions;
			return getThis();
		}

		/**
		 * Sets the {@link RelationshipType Relationships} call types that must be included in the {@code Call Chain} export.
		 * <p>If it is empty then no references are exported</p>
		 *
		 * @param callTypes the call types to include
		 * @return this builder instance
		 */
		public Builder setCallTypes(final Set<RelationshipType> callTypes) {
			this.callTypes = callTypes;
			return getThis();
		}

		/**
		 * Sets a filter which determines which edges to follow based on properties that are present on the edges.
		 *
		 * @param edgePropertyFilter the filter description
		 * @return this builder instance
		 */
		public Builder setEdgePropertyFilter(final Map<String, Serializable> edgePropertyFilter) {
			this.edgePropertyFilter = edgePropertyFilter;
			return getThis();
		}

		/**
		 * Sets the names of modules which must not be included in the {@code Call Chain} export.
		 *
		 * @param filteredModuleNames the names of the to be skipped modules
		 * @return this builder instance
		 */
		public Builder setFilteredModuleNames(final Set<String> filteredModuleNames) {
			this.filteredModuleNames = filteredModuleNames;
			return getThis();
		}

		/**
		 * Sets the {@link Type Types} of modules which must not be included in the {@code Call Chain} export.
		 *
		 * @param filteredModuleTypes the {@link Type Types} modules that must be skipped
		 * @return this builder instance
		 */
		public Builder setFilteredModuleTypes(final Set<Type> filteredModuleTypes) {
			this.filteredModuleTypes = filteredModuleTypes;
			return getThis();
		}

		/**
		 * Sets the IDs of to be ignored Taxonomies.
		 * <p>Use if certain modules must be ignored based on taxonomy.</p>
		 *
		 * @param ignoredTaxonomy the to be ignored Taxonomy IDs
		 * @return this builder instance
		 */
		public Builder setIgnoredTaxonomies(final List<EntityId> ignoredTaxonomy) {
			this.ignoredTaxonomy = ignoredTaxonomy;
			return getThis();
		}

		/**
		 * Sets if data access based {@code Call Chain} export is required.
		 *
		 * @param dataAccessBased {@code true} for data access based {@code Call Chain} export. Otherwise {@code false}
		 * @return this builder instance
		 */
		public Builder setDataAccessBased(final boolean dataAccessBased) {
			this.dataAccessBased = dataAccessBased;
			return getThis();
		}

		/**
		 * Sets if the result of the {@code Call Chain} should be a compressed file (zip) or not.
		 *
		 * @param compressed {@code true} for compressed ZIP file. Otherwise {@code false}.
		 * @return this builder instance
		 */
		public Builder setCompressed(final boolean compressed) {
			this.compressed = compressed;
			return getThis();
		}

		/**
		 * Sets the maximum depth of the {@code Call Chain} export. The export will stop when the given {@code depth} is reached for a start module.
		 * <p>Set {@code -1} for no depth limit (Default).</p>
		 *
		 * @param depth the maximum depth of the export or -1 for no depth limit
		 * @return this builder instance
		 */
		public Builder setDepth(final int depth) {
			this.depth = depth;
			return getThis();
		}

		/**
		 * Sets the maximum number of parallel running export tasks.
		 * <p>Set {@code -1} to use all available processors (Default). Set {@code 1} to limit concurrency for testing to produce deterministic results.</p>
		 *
		 * @param parallel the maximum number of parallel running export tasks
		 * @return this builder instance
		 */
		public Builder setParallel(final int parallel) {
			this.parallel = parallel;
			return getThis();
		}

		/**
		 * Sets the {@link CallChainExporterJob.ExportFormat}.
		 *
		 * @param exportFormat the {@link CallChainExporterJob.ExportFormat}.
		 * @return this builder instance
		 */
		public Builder setExportFormat(final CallChainExporterJob.ExportFormat exportFormat) {
			this.exportFormat = exportFormat;
			return getThis();
		}

		@Override
		protected Builder reset() {
			projectId = null;
			startModuleIds = Collections.emptyList();
			startModuleTypes = Collections.emptySet();
			endModuleIds = Collections.emptyList();
			endModuleTypes = Collections.emptySet();
			directions = Collections.emptyList();
			callTypes = Collections.emptySet();
			filteredModuleNames = Collections.emptySet();
			filteredModuleTypes = Collections.emptySet();
			ignoredTaxonomy = Collections.emptyList();
			edgePropertyFilter = Collections.emptyMap();
			dataAccessBased = false;
			compressed = true;
			depth = -1;
			parallel = 0;
			exportFormat = CallChainExporterJob.ExportFormat.CSV;

			return getThis();
		}

		@Override
		protected Parameters internalBuild() {
			return new Parameters(this);
		}

		private Type getType(final String type) {
			/* We need to check this as type.fromName will return Type Unknown even when invalid Type has been passed,
			 * and Unknown is a valid Type */
			if (type.equalsIgnoreCase(Type.UNKNOWN.name())) {
				return Type.UNKNOWN;
			}
			if (Type.fromName(type).equals(Type.UNKNOWN)) {
				throw new IllegalArgumentException("Type " + type + " is not supported");
			}
			return Type.fromName(type);
		}
	}
}
