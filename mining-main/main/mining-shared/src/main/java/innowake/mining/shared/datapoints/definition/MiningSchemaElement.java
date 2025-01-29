/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import java.util.HashSet;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;

public abstract class MiningSchemaElement {

	protected final String name;
	@Nullable
	protected long[] projectIds;
	protected final Set<String> providedBy;

	protected MiningSchemaElement(final String name) {
		this.name = name;
		this.providedBy = new HashSet<>();
	}

	/**
	 * Gets the name of the schema element.
	 *
	 * @return the name of the data type
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the project IDs for which this type is valid.
	 * @return array of IDs or {@code null} if the validity is not limited to specific projects
	 */
	public @Nullable long[] getProjectIds() {
		return projectIds;
	}

	/**
	 * Sets the project IDs for which this schema element is valid.
	 * @param projectIds array of IDs or {@code null} if the validity is not limited to specific projects
	 */
	public void setProjectIds(@Nullable final long[] projectIds) {
		this.projectIds = projectIds;
	}

	/**
	 * Gets the names of the component or class that provided or extended this schema element.
	 * @return name of the provider
	 */
	public Set<String> getProvidedBy() {
		return providedBy;
	}

	/**
	 * Checks who provided or extended this schema element.
	 * @param providedBy name of the provider
	 * @return If this schema element was built by the given provider.
	 */
	public boolean isProvidedBy(final String providedBy) {
		return this.providedBy.contains(providedBy);
	}

	/**
	 * Adds the name of the component or class that provided or extended this schema element.
	 * @param providedBy name of the provider
	 */
	public void addProvidedBy(final String providedBy) {
		this.providedBy.add(providedBy);
	}

}
