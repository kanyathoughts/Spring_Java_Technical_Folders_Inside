/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import innowake.lib.core.api.lang.Nullable;

/**
 * Base class for an entity with a name.
 */
public abstract class NameableEntity extends Entity {

	@Nullable
	protected String name;
	
	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {
		return assertNotNull(name, "Name must not be null.");
	}
	
	/**
	 * Sets the name.
	 *
	 * @param name the name
	 */
	public void setName(final String name) {
		this.name = name;
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("rid", recordId)
				.append("name", name)
				.toString();
   }
}
