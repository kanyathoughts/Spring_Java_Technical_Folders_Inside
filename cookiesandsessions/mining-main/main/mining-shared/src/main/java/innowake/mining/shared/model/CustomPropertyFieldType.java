/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.lib.core.api.lang.Nullable;

/**
 * Defines how a custom property is handled/rendered on the UI.
 */
public enum CustomPropertyFieldType {

	DEFAULT,
	TAG,
	SELECT,
	NUMBER,
	URL;

	public static CustomPropertyFieldType fromNullableString(@Nullable final String s) {
		return s != null ? CustomPropertyFieldType.valueOf(s) : DEFAULT;
	}
	
}
