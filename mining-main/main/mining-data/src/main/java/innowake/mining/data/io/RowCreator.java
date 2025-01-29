/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;

/**
 * Creates rows of values.
 */
interface RowCreator {

	/**
	 * Creates rows of values.
	 *
	 * @param values the values
	 * @throws IOException if an error occurs
	 */
	void createRow(final Object... values) throws IOException;
}
