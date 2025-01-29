/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping.custom;

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import com.orientechnologies.orient.core.id.ORecordId;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.model.CustomPropertyDataType;
import innowake.spring.data.orientdb.commons.exception.EntityProxyMappingException;

/**
 * Converts the value of a custom property into a corresponding Java object.
 * <p>
 * If the value is not convertible then the raw string value will be returned and the database should take care of any error handling.
 */
public class CustomPropertyConverter {
	
	private static final Pattern RECORD_ID_PATTERN = Pattern.compile("#\\d+:\\d+");
	
	private static final Gson GSON = new Gson();
	
	private CustomPropertyConverter() {}
	
	/**
	 * Maps the value of custom property to given type.
	 *
	 * @param customProperty the custom property
	 * @return value mapped to specified type in custom property
	 */
	@Nullable
	public static Object getPropertyValue(final CustomProperty customProperty) {
		final Object javaObject = CustomPropertyConverter.getJavaObject(customProperty);
		final String customPropertyName = customProperty.getName();
		
		if (javaObject != null && customProperty.getDataType() == CustomPropertyDataType.LINKLIST) {
			if ( ! (javaObject instanceof List)) {
				final String message = String.format("The value type of a custom list property must be a List but was '%s' for the custom property '%s'",
						javaObject.getClass().getSimpleName(), customPropertyName);
				throw new EntityProxyMappingException(message);
			}
			@SuppressWarnings("unchecked")
			final List<String> values = (List<String>) javaObject;
			if ( ! values.stream().allMatch(CustomPropertyConverter::isValidRecordId)) {
				final String message = String.format("The value '%s' of the custom property '%s' is not of a valid list of record IDs",
						values, customPropertyName);
				throw new EntityProxyMappingException(message);
			}
			return values.stream().map(ORecordId::new).collect(Collectors.toList());
		} else {
			return javaObject;
		}
	}
	
	/**
	 * Returns the Java object which corresponds to the type of the custom property and its value.
	 *
	 * @param customProperty the custom property from which the object should be converted from
	 * @return the java object corresponding to the custom property
	 */
	@Nullable
	public static Object getJavaObject(final CustomProperty customProperty) {
		final String rawValue = customProperty.getValue();
		if (rawValue == null) return null;
		
		final String value = Assert.assertNotNull(rawValue);
		final CustomPropertyDataType dataType = Assert.assertNotNull(customProperty.getDataType());
		switch (dataType) {
			case BOOLEAN:
				return getBoolean(value);
			case INTEGER:
				return getInteger(value);
			case SHORT:
				return getShort(value);
			case LONG:
				return getLong(value);
			case FLOAT:
				return getFloat(value);
			case DOUBLE:
				return getDouble(value);
			case STRING:
				return value;
			case DATE:
				return getDate(value);
			case DATETIME:
				return getDatetime(value);
			case BYTE:
				return getByte(value);
			case DECIMAL:
				return getDecimal(value);
			case REFERENCE:
				return getReferenceValue(value);
			case EMBEDDEDLIST:
			case LINKLIST:
				return getList(value);
			case EMBEDDEDMAP:
				return getMap(value);
			default:
				final String message = String.format("Unsupported data type '%s' for value '%s'", dataType, value);
				throw new UnsupportedOperationException(message);
		}
	}

	private static Object getReferenceValue(final String value) {
		final String recordId = StringUtils.substringAfter(value, "#");
		if (recordId != null) {
			return "#" + StringUtils.substringBefore(recordId, "{");
		} else {
			throw new UnsupportedOperationException("Not a valid record Id " + recordId);
		}
	}
	
	private static Object getBoolean(final String value) {
		if ("true".equalsIgnoreCase(value)) {
			return Boolean.TRUE;
		} else if ("false".equalsIgnoreCase(value)) {
			return Boolean.FALSE;
		} else {
			return value;
		}
	}
	
	private static Object getInteger(final String value) {
		try {
			return Integer.valueOf(value);
		} catch (final NumberFormatException e) {
			return value;
		}
	}
	
	private static Object getShort(final String value) {
		try {
			return Short.valueOf(value);
		} catch (final NumberFormatException e) {
			return value;
		}
	}

	private static Object getLong(final String value) {
		try {
			return Long.valueOf(value);
		} catch (final NumberFormatException e) {
			return value;
		}
	}

	private static Object getFloat(final String value) {
		try {
			return Float.valueOf(value);
		} catch (final NumberFormatException e) {
			return value;
		}
	}
	
	private static Object getDouble(final String value) {
		try {
			return Double.valueOf(value);
		} catch (final NumberFormatException e) {
			return value;
		}
	}
	
	private static Object getDate(final String value) {
		try {
			return new SimpleDateFormat("yyyy-MM-dd").parse(value);
		} catch (final ParseException e) {
			return value;
		}
	}
	
	private static Object getDatetime(final String value) {
		try {
			return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(value);
		} catch (final ParseException e) {
			return value;
		}
	}
	
	private static Object getList(final String value) {
		try {
			return GSON.fromJson(value, List.class);
		} catch (final JsonParseException e) {
			return value;
		}
	}

	private static Object getMap(final String value) {
		try {
			return GSON.fromJson(value, Map.class);
		} catch (final JsonParseException e) {
			return value;
		}
	}

	private static Object getByte(final String value) {
		try {
			return Byte.valueOf(value);
		} catch (final NumberFormatException e) {
			return value;
		}
	}
	
	private static Object getDecimal(final String value) {
		try {
			return new BigDecimal(value);
		} catch (final NumberFormatException e) {
			return value;
		}
	}
	
	private static boolean isValidRecordId(final Object value) {
		if ( ! (value instanceof String)) {
			return false;
		}
		final Matcher matcher = RECORD_ID_PATTERN.matcher((String) value);
		return matcher.matches();
	}
}
