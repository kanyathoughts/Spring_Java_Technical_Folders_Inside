/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * Data types which can be used for custom properties in the metamodel.
 */
public enum CustomPropertyDataType {
	
	BOOLEAN,
	INTEGER,
	SHORT,
	LONG,
	FLOAT,
	DOUBLE,
	DATETIME,
	STRING,
	REFERENCE,
	EMBEDDEDLIST,
	LINKLIST,
	EMBEDDEDMAP,
	BYTE, 
	DATE, 
	DECIMAL;

	/**
	 * Returns the data type associated with a given OrientDB type number.
	 * <p>
	 * The mapping is based on https://orientdb.com/docs/last/Types.html
	 *
	 * @param type the type number of OrientDB
	 * @return the associated data type
	 */
	public static CustomPropertyDataType fromOrientType(final Integer type) {
		switch (type.intValue()) {
			case 0:
				return BOOLEAN;
			case 1:
				return INTEGER;
			case 2:
				return SHORT;
			case 3:
				return LONG;
			case 4:
				return FLOAT;
			case 5:
				return DOUBLE;
			case 6:
				return DATETIME;
			case 7:
				return STRING;
			case 13:
				return REFERENCE;
			case 10:
				return EMBEDDEDLIST;
			case 12:
				return EMBEDDEDMAP;
			case 14:
				return LINKLIST;
			case 17:
				return BYTE;
			case 19:
				return DATE;
			case 21:
				return DECIMAL;
			default:
				final String message = String.format("Unsupported OrientDB data type '%d'. Check https://orientdb.com/docs/last/Types.html", type);
				throw new UnsupportedOperationException(message);
		}
	}
	
	/**
	 * Determines if this data type represents a numeric value.
	 * 
	 * @return If this data type is any kind of number, whole or fraction.
	 */
	public boolean isNumeric() {
		return this == INTEGER
			|| this == SHORT
			|| this == LONG
			|| this == FLOAT
			|| this == DOUBLE
			|| this == BYTE
			|| this == DECIMAL;
	}
	
	/**
	 * Method to map the given name to a value of {@link CustomPropertyDataType}
	 *
	 * @param name The name to map
	 * @return Corresponding value of {@link CustomPropertyDataType}
	 */
	public static CustomPropertyDataType fromName(final String name) {
		for (final CustomPropertyDataType value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return value;
			}
		}
		throw new IllegalArgumentException("Invalid value for CustomPropertyDataType: " + name);
	}
	
}
