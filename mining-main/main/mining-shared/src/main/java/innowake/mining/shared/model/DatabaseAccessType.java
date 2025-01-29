/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.model;

/**
 * The database access type associated with a read write reference.
 */
public enum DatabaseAccessType {

	READ("Read"), WRITE("Write"), STORE("Store"), UPDATE("Update"), DELETE("Delete"), OTHER("Other");

	private String value;

	DatabaseAccessType(final String value) {
		this.value = value;
	}

	/**
	 * Returns {@link DatabaseAccessType} value
	 *
	 * @return {@link DatabaseAccessType} value
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Returns the matching {@link DatabaseAccessType} value for the given string, else throws exception.
	 * @param value the access type {@link DatabaseAccessType} value
	 * @return the matching {@link DatabaseAccessType} value for the given string, else throws exception
	 */
	public static DatabaseAccessType getByValue(final String value) {
		for (final DatabaseAccessType constant : values()) {
			if (constant.getValue().equals(value)) {
				return constant;
			}
		}
		throw new IllegalArgumentException("No enum constant with value " + value);
	}
}
