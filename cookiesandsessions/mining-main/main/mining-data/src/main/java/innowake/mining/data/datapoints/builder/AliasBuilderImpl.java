/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints.builder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointBuilder.AliasBuilder;
import innowake.mining.shared.datapoints.definition.AliasDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;

/**
 * Implementation of {@link AliasBuilder}.
 */
class AliasBuilderImpl implements AliasBuilder {

	private final MiningDataPointBuilderImpl parentBuilder;
	private final String name;
	private final String parentTypeName;
	@Nullable
	private long[] projectIds;
	
	@Nullable
	private String aliasFor;
	@Nullable
	private String subSelection;
	@Nullable
	private String jsonPath;
	private String displayName = "";
	private String description = "";
	
	private final Set<String> usages = new HashSet<>();
	private final Map<String, Map<String, String>> usageAttributes = new HashMap<>();
	
	private final List<String> parameters = new ArrayList<>();

	/**
	 * Create new AliasBuilder. The defined alias will be registered on the {@code parentBuilder}. The parent builder instance
	 * is also returned from {@link #add()}.
	 * 
	 * @param parentBuilder the parent builder
	 * @param name name of the alias to define
	 * @param parentTypeName name of the type on which to define the alias
	 */
	AliasBuilderImpl(final MiningDataPointBuilderImpl parentBuilder, final String name, final String parentTypeName) {
		this.parentBuilder = parentBuilder;
		this.name = name;
		this.parentTypeName = parentTypeName;
	}
	
	@Override
	public AliasBuilder forDataPoint(final String name) {
		aliasFor = name;
		return this;
	}

	@Override
	public AliasBuilder onlyOnProjects(@Nullable final long... projectIds) {
		this.projectIds = projectIds;
		return this;
	}

	@Override
	public AliasBuilder withSubSelection(final String selection) {
		subSelection = selection;
		return this;
	}

	@Override
	public AliasBuilder withJsonPath(final String jsonPath) {
		this.jsonPath = jsonPath;
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final Integer value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final Long value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final long value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final Float value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final Double value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final double value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final String value) {
		parameters.add(name + ": \"" + value + "\"");
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final Boolean value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final boolean value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withParameter(final String name, final Enum<?> value) {
		parameters.add(name + ": " + value);
		return this;
	}

	@Override
	public AliasBuilder withDisplayName(final String displayName) {
		this.displayName = displayName; 
		return this;
	}

	@Override
	public AliasBuilder withDescription(final String description) {
		this.description = description;
		return this;
	}

	@Override
	public AliasBuilder withUsage(final String dataPointUsage) {
		usages.add(dataPointUsage);
		return this;
	}

	@Override
	public AliasBuilder withUsageAttribute(final String dataPointUsage, final String key, final String value) {
		usageAttributes.computeIfAbsent(dataPointUsage, k -> new HashMap<>()).put(key, value);
		return this;
	}
	
	@Override
	public MiningDataPointBuilder add() {
		final String alias = aliasFor;
		if (alias == null) {
			throw new IllegalArgumentException("Must define the target data point of this alias with forDataPoint() before calling add()");
		}
		
		final MiningDataPointDefinition dataPoint = new MiningDataPointDefinition(name, parentTypeName, 
				new AliasDefinition(alias, StringUtils.trimToEmpty(subSelection), StringUtils.trimToEmpty(jsonPath), parameters));
		
		dataPoint.setDisplayName(displayName);
		dataPoint.setDescription(description);
		dataPoint.addUsages(usages);
		dataPoint.setProjectIds(projectIds);
		usageAttributes.entrySet().forEach(entry -> dataPoint.addUsageAttributes(entry.getKey(), entry.getValue()));
		
		parentBuilder.addDataPointDefinition(new MiningDataPointDefinitionWithCustomFetch(dataPoint));
		return parentBuilder;
	}
}
