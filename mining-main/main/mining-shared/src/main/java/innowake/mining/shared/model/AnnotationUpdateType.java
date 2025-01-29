/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * Annotation update type Enum
 */
public enum AnnotationUpdateType {

	BUSINESS_RELATED_FROM_YES_TO_NO("Business Related from YES to NO"),
	BUSINESS_RELATED_FROM_NO_TO_YES("Business Related from NO to YES");
	
	private final String type;
	
	private AnnotationUpdateType(final String type) {
		this.type = type;
	}
	
	/**
	 * Gets the type of the Annotation update
	 *
	 * @return the type of the Annotation update
	 */
	public String getType() {
		return type;
	}

}
