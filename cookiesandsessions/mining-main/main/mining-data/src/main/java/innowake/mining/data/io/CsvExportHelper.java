/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import innowake.mining.shared.io.EffortSummaryWorkbookDefinition;
import innowake.mining.shared.io.WorkbookDefinition;
import innowake.mining.shared.io.WorkbookDefinition.Sheet;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Iterator;


/**
 * Helper methods for exporting a Discovery CSV file as byte array from Mining server.
 */
public class CsvExportHelper {

	public static final Charset WORKSPACE_CHARSET = StandardCharsets.UTF_8;

	private static final byte[] NEW_LINE = getBytes("\r\n");
	private static final byte[] CSV_SEP = getBytes(",");

	private CsvExportHelper() {}

	/**
	 * Replaces single '"' character with '""' and replace single '\' character with '\\'.
	 *
	 * @param string input string
	 * @return escapes string
	 */
	public static String escape(final String string) {
		return string.replace("\"", "\"\"").replace("\\", "\\\\");
	}

	/**
	 * Replaces '""' character with single '"' and replace '\\' character with single '\'.
	 *
	 * @param string input string
	 * @return escapes string
	 */
	public static String unescape(final String string) {
		return string.replace("\"\"", "\"").replace("\\\\", "\\");
	}

	static void addLineHeader(final OutputStream out, final String sheetName, final boolean isEffortSummary) throws IOException {
		final Sheet sheetDefinition = isEffortSummary ? EffortSummaryWorkbookDefinition.SHEETS.get(sheetName) : WorkbookDefinition.SHEETS.get(sheetName);
		addLineUnescaped(out, "# " + sheetDefinition.getSheetName());
		addLineEscaped(out, sheetDefinition.getColumnNames());
	}

	static void addLineUnescaped(final OutputStream out, final Object value) throws IOException {
		out.write(getBytes(value.toString()));
		out.write(NEW_LINE);
	}

	static void addLineEscaped(final OutputStream out, final Object[] values) throws IOException {
		for (final Iterator<Object> iterator = Arrays.asList(values).iterator(); iterator.hasNext();) {
			final Object value = iterator.next();
			if (value instanceof Number) {
				out.write(getBytes(value.toString()));
			} else {
				out.write(getBytes("\"" + escape(value.toString()) + "\""));
			}
			out.write(iterator.hasNext() ? CSV_SEP : NEW_LINE);
		}
	}

	static byte[] getBytes(final String value) {
		return value.getBytes(WORKSPACE_CHARSET);
	}
}