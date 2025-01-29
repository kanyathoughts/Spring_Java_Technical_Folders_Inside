/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import innowake.lib.core.api.lang.Nullable;

public class MiningSchemaClass extends MiningSchemaElement {

	@Nullable
	protected final String className;
	
	public MiningSchemaClass(final String name, @Nullable final String className) {
		super(name);
		this.className = className;
	}
	
	public MiningSchemaClass(final String name, @Nullable final Class<?> representedBy) {
		super(name);
		if (representedBy != null) {
			className = representedBy.getName();
		} else {
			className = null;
		}
	}

	/**
	 * Gets the name of the Java class that represents this schema class.
	 * 
	 * @return the name of the Java class or {@code null}
	 */
	@Nullable
	public String getClassName() {
		return className;
	}

}
