/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;

import org.apache.commons.lang.StringUtils;

/**
 * Base class for generators of a Discovery result file.
 */
abstract class AbstractGenerator {

	/**
	 * Creates a row with the given values.
	 *
	 * @param values the values
	 * @throws IOException if an error occurs
	 */
	abstract void createRow(final Object... values) throws IOException;

	/**
	 * Returns a normalized {@link String} for {@code null}.
	 *
	 * @return normalized {@link String}
	 */
	protected String normalizeNull() {
		return StringUtils.EMPTY;
	}

	/**
	 * Returns the given {@link Double} as normalized {@link BigDecimal}.
	 *
	 * @param value the {@link Double}
	 * @return normalized {@link BigDecimal}
	 */
	protected BigDecimal normalizeDouble(final Double value) {
		return new BigDecimal(value.toString()).setScale(2, RoundingMode.HALF_UP);
	}

	/**
	 * Returns the given {@link Float} as normalized {@link BigDecimal}.
	 *
	 * @param value the {@link Float}
	 * @return normalized {@link BigDecimal}
	 */
	protected BigDecimal normalizeFloat(final Float value) {
		return new BigDecimal(value.toString()).setScale(2, RoundingMode.HALF_UP);
	}
}
