/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping.util;

import java.util.Optional;
import com.orientechnologies.orient.core.metadata.schema.OType;
import com.orientechnologies.orient.core.record.OElement;
import innowake.lib.core.lang.Nullable;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;

/**
 * Utility class for OElement.
 */
public class OElementAdapter {
	private final ClassDefinition classDefinition;

	/**
	 * 
	 * Get instance of the Adapter class
	 *
	 * @param classDefinition instance used to create a orientDB session
	 * @return instance of OElementAdapter
	 */
	public static OElementAdapter getOElementAdapter(final ClassDefinition classDefinition) {
		return new OElementAdapter(classDefinition);
	}

	/**
	 * 
	 * Parameterized constructor to initialize {@link ClassDefinition} instances.
	 * 
	 * @param classDefinition instance used to create a orientDB session.
	 */
	private OElementAdapter(final ClassDefinition classDefinition) {
		this.classDefinition = classDefinition;
	}

	/**
	 *
	 * Gets a property given its name
	 * 
	 * @param oElement element to get the property
	 * @param fieldName the property name
	 * @return Returns the property value
	 */
	@Nullable
	public <T> T getProperty(final OElement oElement, final String fieldName) {
		final Optional<String> propertyFieldName = this.classDefinition.getDbFieldNameFromJavaFieldName(fieldName);
		if (propertyFieldName.isPresent()) {
			return oElement.getProperty(propertyFieldName.get());
		} else {
			return oElement.getProperty(fieldName);
		}
	}

	/**
	 * Sets a property value
	 * 
	 * @param oElement element to set the property
	 * @param fieldName the property name
	 * @param value the property value
	 */
	public void setProperty(final OElement oElement, final String fieldName, @Nullable final Object value) {
		final Optional<String> propertyFieldName = this.classDefinition.getDbFieldNameFromJavaFieldName(fieldName);
		if (propertyFieldName.isPresent()) {
			oElement.setProperty(propertyFieldName.get(), value);
		} else {
			oElement.setProperty(fieldName, value);
		}
	}

	/**
	 * Sets a property value
	 * 
	 * @param oElement element to set the property
	 * @param fieldName the property name
	 * @param value the property value
	 * @param fieldType Forced type (not auto-determined)
	 */
	public void setProperty(final OElement oElement, final String fieldName, @Nullable final Object value, final OType... fieldType) {
		oElement.setProperty(fieldName, value, fieldType);
	}
}
