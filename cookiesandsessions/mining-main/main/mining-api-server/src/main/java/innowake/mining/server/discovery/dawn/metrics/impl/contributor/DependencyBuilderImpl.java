/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.contributor;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.DependencyBuilder;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;

/**
 * Implementation for {@link DiscoveryBuilder.DependencyBuilder}.
 *
 * @see DiscoveryBuilder.ModuleBuilder#declareDependency(RelationshipType, ModuleFilter, ResolutionFlag...)
 */
class DependencyBuilderImpl implements DiscoveryBuilder.DependencyBuilder {

	final ModuleBuilderImpl parentBuilder;

	final RelationshipType relationship;
	final List<ModuleFilter> targets = new ArrayList<>();
	final Set<ResolutionFlag> flags;
	@Nullable
	final ModuleBuilderImpl targetBuilder;
	@Nullable
	ModuleLocation location;
	List<ModuleFilter> reachedFromModules = Collections.emptyList();
	Binding binding = Binding.UNKNOWN;
	final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
	@Nullable
	ModuleBuilderImpl reachedFromModuleBuilder;

	/**
	 * Creates a dependency builder for {@link DiscoveryBuilder.ModuleBuilder#declareDependency(RelationshipType, ModuleFilter, ResolutionFlag...)}
	 *
	 * @param parentBuilder the parent ModuleBuilder of this DependencyBuilder
	 * @param target the module filter identifying the dependency target
	 * @param relationship the relationship type of the dependency
	 * @param resolutionFlags the resolution flags to use when locating the dependency target
	 */
	DependencyBuilderImpl(final ModuleBuilderImpl parentBuilder, final RelationshipType relationship,
			final ModuleFilter target, final ResolutionFlag... resolutionFlags) {
		this(parentBuilder, relationship, Collections.singletonList(target), resolutionFlags);
	}

	DependencyBuilderImpl(final ModuleBuilderImpl parentBuilder, final RelationshipType relationship,
			final List<ModuleFilter> targets, final ResolutionFlag... resolutionFlags) {
		this.parentBuilder = parentBuilder;
		this.relationship = relationship;
		this.targets.addAll(targets);
		this.flags = Arrays.asList(resolutionFlags).isEmpty() ? EnumSet.noneOf(ResolutionFlag.class) : EnumSet.copyOf(Arrays.asList(resolutionFlags));
		this.targetBuilder = null;
	}
	
	/**
	 * Creates a dependency builder for {@link DiscoveryBuilder.ModuleBuilder#declareDependency(RelationshipType, DiscoveryBuilder.ModuleBuilder)}
	 *
	 * @param parentBuilder the parent ModuleBuilder of this DependencyBuilder
	 * @param targetBuilder the builder for the target module
	 * @param relationship the relationship type of the dependency
	 */
	DependencyBuilderImpl(final ModuleBuilderImpl parentBuilder, final RelationshipType relationship, final ModuleBuilderImpl targetBuilder) {
		this.parentBuilder = parentBuilder;
		this.relationship = relationship;
		this.flags = EnumSet.noneOf(ResolutionFlag.class);
		this.targetBuilder = targetBuilder;
	}
	
	@Override
	public DiscoveryBuilder.DependencyBuilder setLocation(final ModuleLocation location) {
		this.location = location;
		return this;
	}

	@Override
	public DiscoveryBuilder.DependencyBuilder setBinding(final Binding binding) {
		this.binding = binding;
		return this;
	}

	@Override
	public DiscoveryBuilder.DependencyBuilder addAttribute(final ModelAttributeKey key, final Object attribute) {
		attributes.put(key, attribute);
		return this;
	}
	
	@Override
	public DependencyBuilder setAttributes(final ModelAttributeMap<?> attributeMap) {
		attributeMap.forEach(attributes::put);
		return this;
	}

	@Override
	public DependencyBuilder setReachedFromModules(final List<ModuleFilter> reachedFromModules) {
		this.reachedFromModules = reachedFromModules;
		return this;
	}

	@Override
	public DependencyBuilder setReachedFromModules(final ModuleFilter reachedFromModule) {
		this.reachedFromModules = Collections.singletonList(reachedFromModule);
		return this;
	}

	@Override
	public DependencyBuilder setReachedFromModules(final DiscoveryBuilder.ModuleBuilder reachedFromModule) {
		this.reachedFromModuleBuilder = (ModuleBuilderImpl) reachedFromModule;
		return this;
	}

	@Override
	public DiscoveryBuilder.DependencyBuilder createIfMissing(final String name, final ModuleType type) {
		anchorIfMissing(name, type, null);
		return this;
	}

	@Override
	public DiscoveryBuilder.DependencyBuilder createIfMissing(final String name, final ModuleType type, final Identification identification) {
		anchorIfMissing(name, type, identification);
		return this;
	}

	private void anchorIfMissing(final String name, final ModuleType type, @Nullable final Identification identification) {
		if (targets.isEmpty()) {
			throw new IllegalArgumentException("orCreateIfMissing() can not be used when declaring a dependency with a fixed target" +
					" (i.e. the target was specified directly via the ModuleBuilder for the target Module) because in that case the target is always" +
					" guaranteed to exist.");
		}

		targets.forEach(target -> {
			if (identification == null) {
				parentBuilder.parentBuilder.anchorTo(target, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY).createIfMissing(name, type);
			} else {
				parentBuilder.parentBuilder.anchorTo(target, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY)
						.createIfMissing(name, type, identification);
			}
		});
	}

	@Override
	public DiscoveryBuilder.DependencyBuilder createIfMissing(final String name, final ModuleType type, final Identification identification,
			final ResolutionFlag flag) {
		if (targets.isEmpty()) {
			throw new IllegalArgumentException("orCreateIfMissing() can not be used when declaring a dependency with a fixed target" +
					" (i.e. the target was specified directly via the ModuleBuilder for the target Module) because in that case the target is always" +
					" guaranteed to exist.");
		}

		targets.forEach(target -> parentBuilder.parentBuilder.anchorTo(assertNotNull(target), ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY, flag).createIfMissing(name, type,
				identification));
		return this;
	}
	
	@Override
	public DiscoveryBuilder.DependencyBuilder createIfMissing(final String name, final ModuleType type, final Origin origin) {
		if (targets.isEmpty()) {
			throw new IllegalArgumentException("orCreateIfMissing() can not be used when declaring a dependency with a fixed target"
					+ " (i.e. the target was specified directly via the ModuleBuilder for the target Module) because in that case the target is always"
					+ " guaranteed to exist.");
		}

		targets.forEach(target -> parentBuilder.parentBuilder.anchorTo(assertNotNull(target), ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY).createIfMissing(name,
				type, origin));
		return this;
	}	
}
