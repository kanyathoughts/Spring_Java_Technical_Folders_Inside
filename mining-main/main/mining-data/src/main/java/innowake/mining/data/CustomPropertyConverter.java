/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data;

import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.CustomPropertyDataType;

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;

/**
 * Converts the value of a custom property into a corresponding Java object.
 * <p>
 * If the value is not convertible then the raw string value will be returned and the database should take care of any error handling.
 */
public class CustomPropertyConverter {

    private static final Gson GSON = new Gson();

    private CustomPropertyConverter() {}

    /**
     * Returns the Java object which corresponds to the type of the custom property and its value.
     *
     * @param value    of the custom property
     * @param dataType of the custom propert
     * @return the java object corresponding to the custom property
     */
    @Nullable
    public static Object getJavaObject(final CustomPropertyDataType dataType, @Nullable final String value) {
        if (value == null) return null;

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
                return value;
            case EMBEDDEDLIST:
            case LINKLIST:
                return getList(value);
            default:
                final String message = String.format("Unsupported data type '%s' for value '%s'", dataType, value);
                throw new UnsupportedOperationException(message);
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

}
