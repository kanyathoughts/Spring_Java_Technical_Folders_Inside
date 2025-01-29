/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.contributor;

import java.util.Collections;
import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.StatementBuilder;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;

/**
 * Implementation for {@link DiscoveryBuilder.StatementBuilder}.
 *
 * @see DiscoveryBuilder.ModuleBuilder#declareStatement(StatementType)
 */
class StatementBuilderImpl implements DiscoveryBuilder.StatementBuilder {

	final StatementType statementType;

	String text = "";

	Map<String, Object> properties = Collections.emptyMap();

	@Nullable
	Technology technology;

	public StatementBuilderImpl(final StatementType statementType) {
		this.statementType = statementType;
	}

	@Override
	public DiscoveryBuilder.StatementBuilder setText(final String text) {
		this.text = text;
		return this;
	}

	@Override
	public StatementBuilder setProperties(final Map<String, Object> properties) {
		this.properties = properties;
		return this;
	}

	@Override
	public StatementBuilder setTechnology(final Technology technology) {
		this.technology = technology;
		return this;
	}
}
