/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.io.Serializable;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonSetter;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;

/**
 * Describes a possible set of target modules.
 */
public class ModuleFilter implements Serializable {

	private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:SS");

	private Set<EntityId> moduleIds = Collections.emptySet();
	private Set<String> names = Collections.emptySet();
	private Set<ModuleType> types = Collections.emptySet();
	private Set<String> paths = Collections.emptySet();
	private Set<String> pathPatterns = Collections.emptySet();
	@Nullable
	private ModuleFilter containedIn;
	@Nullable
	private ModuleFilter not;
	@Nullable
	private Instant metricsDate;
    private boolean isPhysical;
	@Nullable 
	private Origin origin;
    @Nullable
	private Identification identification;

	
	/**
	 * Creates an empty module filter
	 */
	public ModuleFilter() {
		/* nothing to be done here */
	}
	
	/**
	 * Copies an existing module filter.
	 * 
	 * @param other another module filter
	 */
	public ModuleFilter(final ModuleFilter other) {
		moduleIds = other.moduleIds;
		names = new HashSet<>(other.names);
		types = other.types.isEmpty() ? EnumSet.noneOf(ModuleType.class) : EnumSet.copyOf(other.types);
		paths = new HashSet<>(other.paths);
		pathPatterns = new HashSet<>(other.pathPatterns);
		containedIn = other.containedIn;
		not = other.not;
		metricsDate = other.metricsDate;
		isPhysical = other.isPhysical;
		origin = other.origin;
		identification = other.identification;
	}

	/**
	 * Sets the id of the module described by this module filter. Setting this property effectively makes all other filter attributes obsolete
	 * as the module is fully identified by its id.
	 *
	 * @param id the module id
	 * @return this module filter
	 */
	public ModuleFilter setModuleIds(final EntityId id) {
		this.moduleIds = Collections.singleton(id);
		return this;
	}
	
	/**
	 * Sets the id's of the module described by this module filter. Setting this property effectively makes all other filter attributes obsolete
	 * as the module is fully identified by its id.
	 *
	 * @param ids the set of the module id
	 * @return this module filter
	 */
	@JsonSetter
	public ModuleFilter setModuleIds(final Collection<EntityId> ids) {
		if (ids instanceof Set) {
			this.moduleIds = (Set<EntityId>) ids;
		} else {
			this.moduleIds = new HashSet<>(ids);
		}

		return this;
	}
	
	/**
	 * Sets the possible names of the target module.
	 *
	 * @param names the set of possible names
	 * @return this module filter
	 */
	public ModuleFilter setNames(final String ...names) {
		this.names = new HashSet<>(Arrays.asList(names));
		return this;
	}
	
	/**
	 * Sets the possible names of the target module.
	 *
	 * @param names the set of possible names
	 * @return this module filter
	 */
	@JsonSetter
	public ModuleFilter setNames(final Collection<String> names) {
		this.names = new HashSet<>(names);
		return this;
	}
	
	/**
	 * Sets the possible types of the target module.
	 *
	 * @param types the set of possible types
	 * @return this module filter
	 */
	public ModuleFilter setTypes(final ModuleType ...types) {
		this.types = EnumSet.copyOf(Arrays.asList(types));
		return this;
	}
	
	/**
	 * Sets the possible types of the target module.
	 *
	 * @param types the set of possible types
	 * @return this module filter
	 */
	@JsonSetter
	public ModuleFilter setTypes(final Collection<ModuleType> types) {
		if( ! types.isEmpty()) {
			this.types = EnumSet.copyOf(types);
		}
		return this;
	}

	/**
	 * Sets the possible paths of the target module.
	 *
	 * @param paths the set of possible paths
	 * @return this module filter
	 */
	public ModuleFilter setPaths(final String ...paths) {
		this.paths = new HashSet<>(Arrays.asList(paths));
		return this;
	}
	
	/**
	 * Sets the possible paths of the target module.
	 *
	 * @param paths the set of possible paths
	 * @return this module filter
	 */
	@JsonSetter
	public ModuleFilter setPaths(final Collection<String> paths) {
		this.paths = new HashSet<>(paths);
		return this;
	}
	
	/**
	 * Sets possible path patterns that the target module must match.
	 *
	 * @param patterns a set of possible path patterns that the target module must match
	 * @return this module filter
	 */
	public ModuleFilter setPathPatterns(final String ...patterns) {
		this.pathPatterns = new HashSet<>(Arrays.asList(patterns));
		return this;
	}
	
	/**
	 * Sets possible path patterns that the target module must match.
	 *
	 * @param patterns a set of possible path patterns that the target module must match
	 * @return this module filter
	 */
	@JsonSetter
	public ModuleFilter setPathPatterns(final Collection<String> patterns) {
		this.pathPatterns = new HashSet<>(patterns);
		return this;
	}
	
	/**
	 * Defines that the possible target modules need to be contained in another module that is described by the given module filter.
	 *
	 * @param otherModule a module filter identifying the possible parent modules
	 * @return this module filter
	 */
	public ModuleFilter setContainedIn(@Nullable final ModuleFilter otherModule) {
		this.containedIn = otherModule;
		return this;
	}
	
	/**
	 * Defines the possible target modules that should be ignored.
	 *
	 * @param not the {@link ModuleFilter} that defines the modules to be ignored
	 * @return {@link ModuleFilter}
	 */
	public ModuleFilter setNot(final ModuleFilter not) {
		this.not = not;
		return this;
	}
	
	/**
	 * Sets the {@code metricsDate} that the target module must match with.
	 *
	 * @param metricsDate the metrics date the target module must match with
	 * @return this module filter
	 */
	public ModuleFilter setMetricsDate(final Instant metricsDate) {
		this.metricsDate = metricsDate;
		return this;
	}
	
	/**
	 * Sets whether the module is (PHYSICAL/VIRTUAL) that the target module must match with.
	 *
	 * @param isPhysical the (PHYSICAL/VIRTUAL) of the target module must match with
	 * @return this module filter
	 */
	public ModuleFilter setPhysical(final boolean isPhysical) {
		this.isPhysical = isPhysical;
		return this;
	}
	
	/**
	 * Returns the ids of the module described by this module filter, if explicitly set.
	 *
	 * @return the ids of the module described by this module filter
	 */
	public Set<EntityId> getModuleIds() {
		return moduleIds;
	}

	/**
	 * Returns the possible names of the target module.
	 *
	 * @return the possible names of the target module
	 */
	public Set<String> getNames() {
		return Collections.unmodifiableSet(names);
	}

	/**
	 * Returns the possible types of the target module.
	 *
	 * @return the possible types of the target module
	 */
	public Set<ModuleType> getTypes() {
		return Collections.unmodifiableSet(types);
	}

	/**
	 * Returns the set of possible paths.
	 *
	 * @return the set of possible paths
	 */
	public Set<String> getPaths() {
		return Collections.unmodifiableSet(paths);
	}

	/**
	 * Returns a set of possible path patterns that the target module must match.
	 *
	 * @return a set of possible path patterns that the target module must match
	 */
	public Set<String> getPathPatterns() {
		return Collections.unmodifiableSet(pathPatterns);
	}

	/**
	 * Returns a module filter identifying the possible parent modules.
	 *
	 * @return a module filter identifying the possible parent modules
	 */
	public Optional<ModuleFilter> getContainedIn() {
		return Optional.ofNullable(containedIn);
	}

	/**
	 * Defines the possible target modules that should be ignored.
	 *
	 * @return {@link ModuleFilter} that defines the modules to be ignored
	 */
	public Optional<ModuleFilter> getNot() {
		return Optional.ofNullable(not);
	}

	/**
	 * Returns whether the module is (PHYSICAL/VIRTUAL) that the target module must match with.
	 *
	 * @return the true if the module is PHYSICAL
	 */
	public boolean isPhysical() {
		return isPhysical;
	}
	
	/**
	 * Returns the module's identification (IDENTIFIED/MISSING) that the target module must match with.
	 *
	 * @return the module's identification
	 */
	public Optional<Identification> getIdentification() {
		return Optional.ofNullable(identification);
	}

	/**
	 * Sets the module's identification (IDENTIFIED/MISSING) that the target module must match with.
	 *
	 * @param identification the {@link Identification}
	 * @return this module filter
	 */
	public ModuleFilter setIdentification(final Identification identification) {
		this.identification = identification;
		return this;
	}

	/**
	 * Returns the {@code metricsDate} that the target module must match with or an empty if no metricsDate is present.
	 *
	 * @return {@link Optional} with the metrics date or empty
	 */
	public Optional<Instant> getMetricsDate() {
		return Optional.ofNullable(metricsDate);
	}
	
	/**
	 * Sets the module's {@link Origin}.
	 *
	 * @param origin the {@link Origin}
	 * @return this module filter
	 */
	public ModuleFilter setOrigin(@Nullable final Origin origin) {
		this.origin = origin;
		return this;
	}
	
	/**
	 * Returns the {@link Origin} of the target module if present.
	 *
	 * @return origin the {@link Origin}
	 */
	public Optional<Origin> getOrigin() {
		return Optional.ofNullable(origin);
	}

	@Override
	public String toString() {
		final StringBuilder builder =  new StringBuilder().append("\n");
		if ( ! getNames().isEmpty()) {
			builder.append("names: ").append(getNames()).append("\n");
		}
		if ( ! getPaths().isEmpty()) {
			builder.append("paths: ").append(getPaths()).append("\n");
		}
		if ( ! getPathPatterns().isEmpty()) {
			builder.append("path patterns: ").append(getPathPatterns()).append("\n");
		}
		if ( ! getTypes().isEmpty()) {
			builder.append("types: ").append(getTypes()).append("\n");
		}
		if ( ! moduleIds.isEmpty()) {
			builder.append("module Id: ").append(moduleIds).append("\n");
		}
		getContainedIn().ifPresent(containedInFilter -> builder.append("\n").append("containedIn: ").append(containedInFilter).append("\n"));

		if (metricsDate != null) {
			builder.append("metrics date: ").append(DATE_FORMATTER.withZone(ZoneId.systemDefault()).format(metricsDate)).append("\n");
		}

		return builder.toString();
	}

	@Override
	public boolean equals(@Nullable final Object o) {
		if (this == o) {
			return true;
		}
		if ( ! (o instanceof ModuleFilter)) {
			return false;
		}

		final ModuleFilter that = (ModuleFilter) o;
		return Objects.equals(moduleIds, that.moduleIds) && 
				Objects.equals(names, that.names) && 
				Objects.equals(types, that.types) && 
				Objects.equals(paths, that.paths) && 
				Objects.equals(pathPatterns, that.pathPatterns) && 
				Objects.equals(containedIn, that.containedIn) && 
				Objects.equals(not, that.not) && 
				Objects.equals(metricsDate, that.metricsDate) && 
				Objects.equals(isPhysical, that.isPhysical) &&
				Objects.equals(origin, that.origin) &&
				Objects.equals(identification, that.identification);
	}

	@Override
	public int hashCode() {
		return Objects.hash(moduleIds, names, types, paths, pathPatterns, containedIn, not, metricsDate, isPhysical, identification, origin);
	}
}
