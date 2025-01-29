/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import java.math.BigDecimal;

import innowake.mining.shared.springdata.annotations.Entity;

/**
 * An entity class with only primitive field values.
 */
@Entity
public class EntityWithPrimitiveValues {
	
	private boolean booleanValue;
	private byte bytes;
	private Boolean wrappedBooleanValue;
	private Byte wrappedByteValue;
	private Double doubleValue;
	private Float floatValue;
	private Integer intValue;
	private Long longValue;
	private Short shortValue;
	private BigDecimal decimalValue;

	/**
	 * Constructor.
	 * 
	 * @param booleanValue the boolean value
	 * @param bytes the bytes
	 * @param wrappedBooleanValue the Boolean value
	 * @param wrappedByteValue the Byte value
	 * @param doubleValue the Double value
	 * @param floatValue the Float value
	 * @param intValue the Integer Value
	 * @param longValue the Long value
	 * @param shortValue the Short value
	 * @param decimalValue the Decimal value
	 */
	public EntityWithPrimitiveValues(final boolean booleanValue, final byte bytes, final Boolean wrappedBooleanValue, final Byte wrappedByteValue,
			final Double doubleValue, final Float floatValue, final Integer intValue, final Long longValue, final Short shortValue,
			final BigDecimal decimalValue) {
		super();
		this.booleanValue = booleanValue;
		this.bytes = bytes;
		this.wrappedBooleanValue = wrappedBooleanValue;
		this.wrappedByteValue = wrappedByteValue;
		this.doubleValue = doubleValue;
		this.floatValue = floatValue;
		this.intValue = intValue;
		this.longValue = longValue;
		this.shortValue = shortValue;
		this.decimalValue = decimalValue;
	}

	/**
	 * Gets the boolean value.
	 *
	 * @return the boolean value
	 */
	public boolean isBooleanValue() {
		return booleanValue;
	}

	/**
	 * Sets the boolean value.
	 *
	 * @param booleanValue the boolean value
	 */
	public void setBooleanValue(final boolean booleanValue) {
		this.booleanValue = booleanValue;
	}

	/**
	 * Gets the bytes.
	 *
	 * @return the bytes
	 */
	public byte getBytes() {
		return bytes;
	}

	/**
	 * Sets the bytes.
	 *
	 * @param bytes the bytes
	 */
	public void setBytes(final byte bytes) {
		this.bytes = bytes;
	}

	/**
	 * Gets the Boolean value.
	 *
	 * @return the Boolean value
	 */
	public Boolean getWrappedBooleanValue() {
		return wrappedBooleanValue;
	}

	/**
	 * Sets the Boolean value.
	 *
	 * @param wrappedBooleanValue the Boolean value
	 */
	public void setWrappedBooleanValue(final Boolean wrappedBooleanValue) {
		this.wrappedBooleanValue = wrappedBooleanValue;
	}

	/**
	 * Gets the Byte value.
	 *
	 * @return the Byte value
	 */
	public Byte getWrappedByteValue() {
		return wrappedByteValue;
	}

	/**
	 * Sets the Byte value.
	 *
	 * @param wrappedByteValue the Byte value
	 */
	public void setWrappedByteValue(final Byte wrappedByteValue) {
		this.wrappedByteValue = wrappedByteValue;
	}

	/**
	 * Gets the Double value.
	 *
	 * @return the Double value
	 */
	public Double getDoubleValue() {
		return doubleValue;
	}

	/**
	 * Sets the Double value.
	 *
	 * @param doubleValue the Double value
	 */
	public void setDoubleValue(final Double doubleValue) {
		this.doubleValue = doubleValue;
	}

	/**
	 * Gets the Float value.
	 *
	 * @return the Float value
	 */
	public Float getFloatValue() {
		return floatValue;
	}

	/**
	 * Sets the Float value.
	 *
	 * @param floatValue the Float value
	 */
	public void setFloatValue(final Float floatValue) {
		this.floatValue = floatValue;
	}

	/**
	 * Gets the Integer value.
	 *
	 * @return the Integer value
	 */
	public Integer getIntValue() {
		return intValue;
	}

	/**
	 * Sets the Integer value.
	 *
	 * @param intValue the Integer value
	 */
	public void setIntValue(final Integer intValue) {
		this.intValue = intValue;
	}

	/**
	 * Gets the Long value.
	 *
	 * @return the Long value
	 */
	public Long getLongValue() {
		return longValue;
	}

	/**
	 * Sets the Long value.
	 *
	 * @param longValue the Long value
	 */
	public void setLongValue(final Long longValue) {
		this.longValue = longValue;
	}

	/**
	 * Gets the Short value.
	 *
	 * @return the Short value
	 */
	public Short getShortValue() {
		return shortValue;
	}

	/**
	 * Sets the Short value.
	 *
	 * @param shortValue the Short value
	 */
	public void setShortValue(final Short shortValue) {
		this.shortValue = shortValue;
	}

	/**
	 * Gets the Decimal value.
	 *
	 * @return the Decimal value
	 */
	public BigDecimal getDecimalValue() {
		return decimalValue;
	}

	/**
	 * Sets the Decimal value.
	 *
	 * @param decimalValue the Decimal value
	 */
	public void setDecimalValue(final BigDecimal decimalValue) {
		this.decimalValue = decimalValue;
	}

}
