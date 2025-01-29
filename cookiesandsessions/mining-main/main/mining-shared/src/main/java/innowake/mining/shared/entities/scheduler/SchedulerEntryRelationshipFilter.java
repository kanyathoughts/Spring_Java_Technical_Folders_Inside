package innowake.mining.shared.entities.scheduler;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * Filter to resolve scheduler entry relationships.
 */
public class SchedulerEntryRelationshipFilter {

	private final EntryFilter predecessorFilter;
	private final EntryFilter successorFilter;
	private final boolean isOk;
	@Nullable
	private final String identifier;

	/**
	 * Creates a new filter for scheduler entry relationships.
	 *
	 * @param predecessorFilter the filter for the predecessor
	 * @param successorFilter   the filter for the successor
	 */
	public SchedulerEntryRelationshipFilter(final EntryFilter predecessorFilter, final EntryFilter successorFilter) {
		this.predecessorFilter = predecessorFilter;
		this.successorFilter = successorFilter;
		isOk = true;
		identifier = null;
	}

	/**
	 * Creates a new filter for scheduler entry relationships.
	 *
	 * @param predecessorFilter the filter for the predecessor
	 * @param successorFilter   the filter for the successor
	 * @param isOk              the flag to indicate if the successor should be reachable from the predecessor if the completion status is successful
	 * @param identifier        the identifier of the conditional entry
	 */
	public SchedulerEntryRelationshipFilter(final EntryFilter predecessorFilter, final EntryFilter successorFilter, final boolean isOk,
			final String identifier) {
		this.predecessorFilter = predecessorFilter;
		this.successorFilter = successorFilter;
		this.isOk = isOk;
		this.identifier = identifier;
	}

	public EntryFilter getPredecessorFilter() {
		return predecessorFilter;
	}

	public EntryFilter getSuccessorFilter() {
		return successorFilter;
	}

	public boolean isOk() {
		return isOk;
	}

	@Nullable
	public String getIdentifier() {
		return identifier;
	}

	@Override
	public String toString() {
		return "SchedulerEntryRelationshipFilter{" + "predecessorFilter=" + predecessorFilter + ", successorFilter=" + successorFilter + ", isOk=" + isOk
				+ ", identifier='" + identifier + '\'' + '}';
	}

	/**
	 * Filter to resolve the scheduler entries.
	 */
	public static class EntryFilter {

		private final List<UUID> entryIds = new ArrayList<>();
		private final List<UUID> moduleIds = new ArrayList<>();
		private final List<String> moduleNames = new ArrayList<>();
		private final List<String> containedInIdentifiers = new ArrayList<>();
		private final List<Tuple2<Technology, Type>> technologyTypes = new ArrayList<>();
		private final List<String> pathPatterns = new ArrayList<>();

		/**
		 * Adds the entry uid to the filter.
		 *
		 * @param entryIds the entry identifiers
		 * @return the filter
		 */
		public EntryFilter addEntryIds(final UUID... entryIds) {
			this.entryIds.addAll(List.of(entryIds));
			return this;
		}

		/**
		 * Adds the module uid to the filter.
		 *
		 * @param moduleIds the module uids
		 * @return the filter
		 */
		public EntryFilter addModuleIds(final UUID... moduleIds) {
			this.moduleIds.addAll(List.of(moduleIds));
			return this;
		}

		/**
		 * Adds the module uid to the filter.
		 *
		 * @param moduleIds the module uids
		 * @return the filter
		 */
		public EntryFilter addModuleIds(final List<UUID> moduleIds) {
			this.moduleIds.addAll(moduleIds);
			return this;
		}

		/**
		 * Adds the module name to the filter.
		 *
		 * @param moduleNames the module names
		 * @return the filter
		 */
		public EntryFilter addModuleNames(final String... moduleNames) {
			this.moduleNames.addAll(List.of(moduleNames));
			return this;
		}

		/**
		 * Adds the module name to the filter.
		 *
		 * @param moduleNames the module names
		 * @return the filter
		 */
		public EntryFilter addModuleNames(final List<String> moduleNames) {
			this.moduleNames.addAll(moduleNames);
			return this;
		}

		/**
		 * Adds the contained in identifier to the filter. Typically it's the identifier of the parent entry.
		 *
		 * @param containedInIdentifiers the contained in identifiers
		 * @return the filter
		 */
		public EntryFilter addContainedInIdentifiers(final String... containedInIdentifiers) {
			this.containedInIdentifiers.addAll(List.of(containedInIdentifiers));
			return this;
		}

		/**
		 * Adds the technology type of the modules to the filter.
		 *
		 * @param technology the technology
		 * @param type       the type
		 * @return the filter
		 */
		public EntryFilter addTechnologyTypes(final Technology technology, final Type type) {
			this.technologyTypes.add(new Tuple2<>(technology, type));
			return this;
		}

		/**
		 * Adds the technology type of the modules to the filter.
		 *
		 * @param technologyTypes the technology types
		 * @return the filter
		 */
		public EntryFilter addTechnologyTypes(final List<Tuple2<Technology, Type>> technologyTypes) {
			this.technologyTypes.addAll(technologyTypes);
			return this;
		}

		/**
		 * Adds the path pattern to the filter. Typically the path of the module.
		 *
		 * @param pathPatterns the path patterns
		 * @return the filter
		 */
		public EntryFilter addPathPatterns(final String... pathPatterns) {
			this.pathPatterns.addAll(List.of(pathPatterns));
			return this;
		}

		public List<UUID> getEntryIds() {
			return entryIds;
		}

		public List<UUID> getModuleIds() {
			return moduleIds;
		}

		public List<String> getModuleNames() {
			return moduleNames;
		}

		public List<String> getContainedInIdentifiers() {
			return containedInIdentifiers;
		}

		public List<Tuple2<Technology, Type>> getTechnologyTypes() {
			return technologyTypes;
		}

		public List<String> getPathPatterns() {
			return pathPatterns;
		}

		@Override
		public String toString() {
			return "EntryFilter{" + "entryIds=" + entryIds + ", moduleIds=" + moduleIds + ", moduleNames=" + moduleNames + ", containedInIdentifiers="
					+ containedInIdentifiers + ", technologyTypes=" + technologyTypes + ", pathPatterns=" + pathPatterns + '}';
		}
	}
}
