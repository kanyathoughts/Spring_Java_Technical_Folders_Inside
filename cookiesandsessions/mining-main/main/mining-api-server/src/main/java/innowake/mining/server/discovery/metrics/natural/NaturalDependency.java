/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.AbstractSourceObjectDependency;
import innowake.mining.server.discovery.metrics.SourceObjectAliasProvider;
import innowake.mining.server.discovery.metrics.SourceObjectDependency;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Natural implementation of {@link SourceObjectDependency}.
 */
public class NaturalDependency extends AbstractSourceObjectDependency {

	private final SourceObjectAliasProvider aliasProvider;
	/* differ between definitely known types (targetType) and potential types which indicate that the exact type is one of the potential types */
	private final Set<Type> potentialTargetTypes;

	private final int line;
	private final boolean isStaticBinding;

	NaturalDependency(
			final SourcePojo source,
			final String targetName,
			final SourceObjectResolver sourceObjectResolver,
			final SourceObjectAliasProvider aliasProvider,
			final int line,
			final boolean isStaticBinding) {

		super(Technology.NATURAL, source, targetName, sourceObjectResolver);
		this.aliasProvider = aliasProvider;
		this.line = line;
		potentialTargetTypes = Collections.emptySet();
		this.isStaticBinding = isStaticBinding;
	}

	NaturalDependency(
			final SourcePojo source,
			final String targetName,
			final Type targetType,
			final SourceObjectResolver sourceObjectResolver,
			final SourceObjectAliasProvider aliasProvider,
			final int line,
			final boolean isStaticBinding) {

		super(Technology.NATURAL, source, targetName, targetType, sourceObjectResolver);
		this.aliasProvider = aliasProvider;
		this.line = line;
		potentialTargetTypes = Collections.emptySet();
		this.isStaticBinding = isStaticBinding;
	}

	NaturalDependency(
			final SourcePojo source,
			final String targetName,
			final SourceObjectResolver sourceObjectResolver,
			final SourceObjectAliasProvider aliasProvider,
			final Set<Type> potentialTargetTypes,
			final int line,
			final boolean isStaticBinding) {

		super(Technology.NATURAL, source, targetName, sourceObjectResolver);
		this.aliasProvider = aliasProvider;
		this.line = line;
		this.potentialTargetTypes = Collections.unmodifiableSet(potentialTargetTypes);
		this.isStaticBinding = isStaticBinding;
	}

	public int getLine() {
		return line;
	}

	@Override
	public Set<SourcePojo> getTargets() {
		if (targets == null) {
			final SourceObjectMatcher matcher = potentialTargetTypes.isEmpty() ? new SourceObjectMatcher(technology, targetType) :
				new SourceObjectMatcher(technology, potentialTargetTypes.toArray(new Type[0]));

			final SourcePojo sourceObject = resolver.resolveObject(source, targetName, matcher);
			if (sourceObject != null) {
				targets = Collections.singleton(sourceObject);
			} else {
				targets = aliasProvider.getSourceObjectNamesForAlias(targetName)
						.stream()
						.map(name -> resolver.resolveObject(source, name, matcher))
						.filter(Objects::nonNull)
						.collect(Collectors.toSet());
			}
		}
		return assertNotNull(targets);
	}

	/**
	 * Indicates whether the dependency invocation is a static or dynamic call.
	 * For instance., CALLNAT 'PGM' is a static invocation since the target name is in quotes. if it's not surrounded by quotes then it's a dynamic call.
	 *
	 *
	 * @return {@code true} if the invocation is static
	 */
	public boolean isStaticBinding() {
		return isStaticBinding;
	}
}
