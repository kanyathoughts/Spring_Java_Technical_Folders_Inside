/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.model;

import java.io.Serializable;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore;
import innowake.mining.shared.access.EntityId;

/**
 * Serializable definition of a deferred action on a module.
 * Can be executed via {@link DiscoveryCore#executeDeferredAction(DiscoveryContext, EntityId, DeferredActionDefinition)}.
 */
public class DeferredActionDefinition implements Serializable {

	private final String contributorClassName;
	private final String name;
	@Nullable
	private final Serializable context;
	
	/**
	 * Creates a deferred action definition.
	 * 
	 * @param contributorClassName fully qualified name of the contributor class containing the deferred action
	 *        (a class implementing {@link DiscoveryContributor} or {@link DiscoveryContributorFromSource}).
	 * @param name the name of the deferred action
	 */
	public DeferredActionDefinition(final String contributorClassName, final String name) {
		this.contributorClassName = contributorClassName;
		this.name = name;
		this.context = null;
	}
	
	/**
	 * Creates a deferred action definition.
	 * 
	 * @param contributorClassName fully qualified name of the contributor class containing the deferred action
	 * @param name the name of the deferred action
	 * @param context the context object provided for the deferred action
	 */
	public DeferredActionDefinition(final String contributorClassName, final String name, final Serializable context) {
		this.contributorClassName = contributorClassName;
		this.name = name;
		this.context = context;
	}

	/**
	 * Returns the fully qualified name of the contributor class containing the deferred action
	 *
	 * @return the fully qualified name of the contributor class containing the deferred action
	 */
	public String getContributorClassName() {
		return contributorClassName;
	}

	/**
	 * Returns the name of the deferred action.
	 *
	 * @return the name of the deferred action
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns the context object provided for the deferred action.
	 *
	 * @return the context object provided for the deferred action
	 */
	public Optional<Serializable> getContext() {
		return Optional.ofNullable(context);
	}

	@Override
	public String toString() {
		return "DeferredActionDefinition{" + "contributorClassName='" + contributorClassName + '\'' + ", name='" + name + '\'' + '}';
	}
}
