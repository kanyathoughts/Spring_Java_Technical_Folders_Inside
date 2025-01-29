/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints.builder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointBuilder.EnumBuilder;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;

/**
 * Implementation of {@link EnumBuilder}.
 */
class EnumBuilderImpl implements EnumBuilder {
	
	private final MiningDataPointBuilderImpl parentBuilder;
	private final String name;
	@Nullable
	private long[] projectIds;
	@Nullable
	private Class<? extends Enum<?>> klass;
	private List<String> values = Collections.emptyList();

	/**
	 * Create new EnumBuilder. The defined enum will be registered on the {@code parentBuilder}. The parent builder instance
	 * is also returned from {@link #add()}.
	 * 
	 * @param parentBuilder the parent builder
	 * @param name name of the enum to define
	 */
	EnumBuilderImpl(final MiningDataPointBuilderImpl parentBuilder, final String name) {
		this.parentBuilder = parentBuilder;
		this.name = name;
	}

	@Override
	public EnumBuilder representedBy(final Class<? extends Enum<?>> klass) {
		this.klass = klass;
		return this;
	}
	
	@Override
	public EnumBuilder withValues(final String... values) {
		this.values = Arrays.asList(values);
		return this;
	}

	@Override
	public EnumBuilder onlyOnProjects(final long... projectIds) {
		this.projectIds = projectIds;
		return this;
	}

	@Override
	public MiningDataPointBuilder add() {
		final Class<? extends Enum<?>> klassNonNull = klass;
		if (klassNonNull == null && values.isEmpty()) {
			throw new IllegalArgumentException("The enum " + name + " has no representing type and no values. You must call representedBy() or withValues().");
		}
		final List<String> enumValues = new ArrayList<>(values);
		if (klassNonNull != null) {
			final Enum<?>[] enumConstants = klassNonNull.getEnumConstants();
			if (enumConstants == null) {
				throw new IllegalArgumentException("The class " + klassNonNull.getName() + " representing " + name + " is not an enum.");
			}
			enumValues.addAll(Arrays.asList(enumConstants).stream().map(Enum::name).collect(Collectors.toList()));
		}
		final MiningEnumDefinition enumDef = new MiningEnumDefinition(name, klass, enumValues);
		enumDef.setProjectIds(projectIds);
		parentBuilder.addEnumDefinition(enumDef);
		return parentBuilder;
	}

}
