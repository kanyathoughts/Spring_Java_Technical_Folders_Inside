/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.utils;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Arrays;

import org.apache.commons.io.IOUtils;
import org.junit.ComparisonFailure;

import com.google.common.base.Function;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.extensions.MiningExportExtension.ExportValue;

/**
 * Common methods for CSV and Confluence export tests.
 */
public class ExportTestUtils {
	
	/**
	 * Method to read the {@link ExportValue exported value}.
	 *
	 * @param exportValue the {@link ExportValue exported value}
	 * @return the string representation of the exported value
	 * @throws IOException if an I/O error occurs
	 */
	public static String exportValueToString(final ExportValue exportValue) throws IOException {
		return toString(exportValue.getInputStream());
	}

	/**
	 * Read the export output and compare it to an expected result.
	 *
	 * @param export {@link ExportValue exported value}
	 * @param expected String for each expected line in the output.
	 * @throws IOException if an I/O error occurs
	 */
	public static void exportAndCompare(final ExportValue export, final String... expected) throws IOException {
		compare(export.getInputStream(), null, expected);
	}

	/**
	 * Returns the result from the given job {@code result} as string.
	 *
	 * @param result the {@link JobResult}
	 * @return the result as string
	 * @throws IOException if an I/O error occurs
	 */
	public static String jobResultToString(final JobResult result) throws IOException {
		return toString(result.getContent());
	}

	/**
	 * Read the export output and compare it to an expected result.
	 *
	 * @param jobResult {@link JobResult result of the export job}
	 * @param expected String for each expected line in the output.
	 * @throws IOException if an I/O error occurs
	 */
	public static void exportAndCompare(final JobResult jobResult, final String... expected) throws IOException {
		compare(jobResult.getContent(), null, expected);
	}

	/**
	 * Read the export output and compare it to an expected result.
	 *
	 * @param jobResult {@link JobResult result of the export job}
	 * @param expected String for each expected line in the output.
	 * @param valueModifier function that gets the index and value and returns the modified value before the compare is done
	 * @throws IOException if an I/O error occurs
	 */
	public static void exportAndCompare(final JobResult jobResult, final Function<String, String> valueModifier, final String... expected) throws IOException {
		compare(jobResult.getContent(), valueModifier, expected);
	}

	private static String toString(final InputStream inputStream) throws IOException {
		final StringWriter writer = new StringWriter();
		IOUtils.copy(inputStream, writer, "UTF-8");
		return writer.toString();
	}

	private static void compare(final InputStream inputStream, @Nullable final Function<String, String> valueModifier, final String... expected) throws IOException {
		final String[] result = toString(inputStream).split("\n");
		Arrays.sort(result, 1, result.length);
		
		if (valueModifier != null) {
			for (int i = 1; i < result.length; i++) {
				result[i] = valueModifier.apply(result[i]);
			}
		}

		if ( ! Arrays.equals(expected, result)) {
			throw new ComparisonFailure("", String.join("\n", expected), String.join("\n", result));
		}
	}
}
