/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * The type associated with an annotation.
 */
@Entity(name = "AnnotationTypeEnum")
@MiningDataType(name = "AnnotationType")
public enum AnnotationType {
	
	DATABASE("Database"),
	DEAD_CODE("Dead Code"),
	EXCLUDE("Exclude"),
	RULE("Rule"),
	FUNCTIONAL("Functional");
	
	private String displayName;
	
	private AnnotationType(final String displayName) {
		this.displayName = displayName;
	}
	
	/**
	 * @return the non-technical, user facing name
	 */
	public String getDisplayName() {
		return displayName;
	}
}
