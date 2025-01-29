/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;

import innowake.lib.core.api.lang.Nullable;

/**
 * Default Postgres type definitions.
 * Custom type definitions are declared in PgMiningType.class
 */
public class PgType {
	
	/**
	 * Binary data of arbitrary size.
	 */
	public static final PgType BLOB = new PgType("bytea");
	/**
	 * Exact numeric value with scale and precision.
	 */
	public static final PgType DECIMAL = new PgType("decimal", p -> "decimal(" + p + ")", (p, s) -> "decimal(" + p + "," + s + ")");
	/**
	 * 64 bit IEEE 754 floating-point value.
	 */
	public static final PgType DOUBLE = new PgType("float8");
	/**
	 * 32 bit IEEE 754 floating-point value.
	 */
	public static final PgType FLOAT = new PgType("float4");
	/**
	 * Absolute (UTC based) point in time, optionally with precision.
	 */
	public static final PgType INSTANT = new PgType("timestamptz", p -> "timestamptz(" + p + ")");
	/**
	 * 32 bit integer.
	 */
	public static final PgType INT = new PgType("int4");
	/**
	 * JSON data stored in binary form.
	 */
	public static final PgType JSONB = new PgType("jsonb");
	/**
	 * 64 bit integer.
	 */
	public static final PgType LONG = new PgType("int8");
	/**
	 * 16 bit integer.
	 */
	public static final PgType SHORT = new PgType("int2");
	/**
	 * Unicode character string of arbitrary or limited length.
	 */
	public static final PgType STRING = new PgType("text", l -> "varchar(" + l + ")");
	/**
	 * Unique identifier.
	 */
	public static final PgType UUID = new PgType("uuid");
	/**
	 * Boolean.
	 */
	public static final PgType BOOLEAN = new PgType("boolean");

	private final String type;
	private final @Nullable Function<Integer, String> typeWithLength;
	private final @Nullable BiFunction<Integer, Integer, String> typeWithScale;
	
	private final @Nullable Integer length;
	private final @Nullable Integer scale;
	
	PgType(final String type) {
		this(type, null, null);
	}
	
	private PgType(final String type, @Nullable final Function<Integer, String> typeWithLength) {
		this(type, typeWithLength, null);
	}
	
	private PgType(final String type, @Nullable final Function<Integer, String> typeWithLength,
			@Nullable final BiFunction<Integer, Integer, String> typeWithScale) {
		this(type, typeWithLength, typeWithScale, null, null);
	}
	
	private PgType(final String type, @Nullable final Function<Integer, String> typeWithLength,
				@Nullable final BiFunction<Integer, Integer, String> typeWithScale,
				@Nullable final Integer length, @Nullable final Integer scale) {
		this.type = type;
		this.typeWithLength = typeWithLength;
		this.typeWithScale = typeWithScale;
		this.length = length;
		this.scale = scale;
	}
	
	/**
	 * Type with length or precision argument. Like {@code type(n)}.
	 * @param lengthOrPrecision Type argument value.
	 * @return Type definition.
	 */
	public PgType withLengthOrPrecision(final int lengthOrPrecision) {
		Objects.requireNonNull(typeWithLength, "Type does not support lenght or precision argument.");
		return new PgType(type, typeWithLength, typeWithScale, lengthOrPrecision, null);
	}
	
	/**
	 * Type with precision and scale argument. Like {@code type(a, b)}.
	 * @param precision Total number of significant digits.
	 * @param scale Number of fractional digits.
	 * @return Type definition.
	 */
	public PgType withPrecisionAndScale(final int precision, final int scale) {
		Objects.requireNonNull(typeWithScale, "Type deos not support precision and scale arguments.");
		return new PgType(type, typeWithLength, typeWithScale, precision, scale);
	}
	
	/**
	 * SQL declaration of the type.
	 * @return SQL fragment.
	 */
	@Override
	public String toString() {
		if (length != null && scale != null) {
			return Objects.requireNonNull(typeWithScale).apply(length, scale);
		} else if (length != null) {
			return Objects.requireNonNull(typeWithLength).apply(length);
		}
		return type;
	}
	
}
