/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collections;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Abstract implementation of {@link SourceObjectDependency}.
 */
public abstract class AbstractSourceObjectDependency implements SourceObjectDependency {

	protected final SourceObjectResolver resolver;
	protected final SourcePojo source;
	protected final String targetName;
	protected final Technology technology;

	@Nullable
	protected final Type targetType;
	@Nullable
	protected Set<SourcePojo> targets;

	protected AbstractSourceObjectDependency(
			final Technology technology,
			final SourcePojo source,
			final String targetName,
			final SourceObjectResolver sourceObjectResolver) {

		this.technology = technology;
		this.source = source;
		this.targetName = targetName;
		this.targetType = null;
		this.resolver = sourceObjectResolver;
	}

	protected AbstractSourceObjectDependency(
			final Technology technology,
			final SourcePojo source,
			final String targetName,
			final Type targetType,
			final SourceObjectResolver sourceObjectResolver) {

		this.technology = technology;
		this.source = source;
		this.targetName = targetName;
		this.targetType = targetType;
		this.resolver = sourceObjectResolver;
	}

	@Override
	public String getTargetName() {
		return targetName;
	}

	@Override
	@Nullable
	public Type getTargetType() {
		return targetType;
	}

	@Override
	public boolean targetExists() {
		return ! getTargets().isEmpty();
	}

	@Override
	public Set<SourcePojo> getTargets() {
		if (targets == null) {
			final SourcePojo sourceObject = resolver.resolveObject(source, targetName, new SourceObjectMatcher(technology, targetType));
			targets = sourceObject != null ? Collections.singleton(sourceObject) : Collections.emptySet();
		}
		return assertNotNull(targets);
	}
}
