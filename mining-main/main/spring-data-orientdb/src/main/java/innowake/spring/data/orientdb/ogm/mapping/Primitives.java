/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.mapping;

import java.math.BigDecimal;
import java.util.Date;
import java.util.IdentityHashMap;
import java.util.Map;

import com.orientechnologies.orient.core.metadata.schema.OType;

/**
 * Supported data types mapping with orientdb types.
 */
public class Primitives {
	
	/** Map of primitive classes with the corresponding orient-db types*/
	private static final Map<Class<?>, OType> PRIMITIVE_MAP = new IdentityHashMap<>();
	
	/**
	 * Returns true if the data type is supported by orientDb.
	 *
	 * @param dataType the data type
	 * @return true if the data type is supported by orientDb
	 */
	public static boolean isSupportedDataType(final Class<?> dataType) {
		if (dataType.isArray()) {
			if (dataType.getComponentType() == byte.class) {
				return true;
			} else {
				throw new IllegalArgumentException("Only byte array is supported");
			}
		}
		return PRIMITIVE_MAP.containsKey(dataType);
	}
	
	static {
		PRIMITIVE_MAP.put(Boolean.class, OType.BOOLEAN);
		PRIMITIVE_MAP.put(boolean.class, OType.BOOLEAN);
		PRIMITIVE_MAP.put(Byte.class, OType.BYTE);
		PRIMITIVE_MAP.put(byte.class, OType.BYTE);
		PRIMITIVE_MAP.put(Double.class, OType.DOUBLE);
		PRIMITIVE_MAP.put(Float.class, OType.FLOAT);
		PRIMITIVE_MAP.put(Integer.class, OType.INTEGER);
		PRIMITIVE_MAP.put(Long.class, OType.LONG);
		PRIMITIVE_MAP.put(Short.class, OType.SHORT);
		PRIMITIVE_MAP.put(BigDecimal.class, OType.DECIMAL);

		PRIMITIVE_MAP.put(String.class, OType.STRING);

		PRIMITIVE_MAP.put(Date.class, OType.DATETIME);
	}
	
	private Primitives() {}

}
