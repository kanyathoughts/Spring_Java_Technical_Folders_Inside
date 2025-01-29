/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Creates the sheet of a {@code Discovery} or {@code Effort Summary} CSV file.
 */
class CsvGenerator extends AbstractGenerator {

	private final OutputStream out;

	/**
	 * Creates a new CsvGenerator.
	 * @param out the output stream where the generated CSV is written to
	 */
	public CsvGenerator(final OutputStream out) {
		this.out = out;
	}

	/**
	 * Creates the header of a {@code Discovery} CSV file.
	 *
	 * @param header the header
	 * @throws IOException if an error occurs
	 */
	void createHeader(final String header) throws IOException {
		CsvExportHelper.addLineHeader(out, header, false);
	}

	/**
	 * Creates the header of a {@code Discovery} or {@code Effort Summary} CSV file.
	 *
	 * @param header the header
	 * @param isEffortSummary {@code true} if the {@code header} of an effort summary is generated. {@code false} for a discovery workbook
	 * @throws IOException if an error occurs
	 */
	void createHeader(final String header, final boolean isEffortSummary) throws IOException {
		CsvExportHelper.addLineHeader(out, header, isEffortSummary);
	}

	@Override
	void createRow(final Object... values) throws IOException {
		for (int i = 0; i < values.length; i++) {
			if (values[i] instanceof final Double d) {
				values[i] = normalizeDouble(d);
			} else if (values[i] instanceof final Float f) {
				values[i] = normalizeFloat(f);
			} else if (values[i] == null) {
				values[i] = normalizeNull();
			}
		}
		CsvExportHelper.addLineEscaped(out, values);
	}
}
