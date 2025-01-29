/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.input.BOMInputStream;

import com.opencsv.CSVParserBuilder;
import com.opencsv.CSVReader;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.error.TaxonomyImportException;

/**
 * Handy reader when there's insufficient motivation to use the bean binding but
 * the header mapping is still desired. This class is extracted from OpenCSV in order to maintain the order of insertion and handle certain edge cases.
 *
 * @author Andre Rosot
 * @since 4.2
 */
public class CSVReaderHeaderAware extends CSVReader {

	private final Map<String, Integer> headerIndex = new LinkedHashMap<>();

	/**
	 * Constructor with supplied reader.
	 *
	 * @param reader The reader to an underlying CSV source
	 * @param separator the separator being used
	 * @throws IOException If there is an error when reading the header
	 * @throws TaxonomyImportException if there is no content or duplicate header column
	 */
	@SuppressWarnings("deprecation")
	public CSVReaderHeaderAware(final Reader reader, char separator) throws IOException, TaxonomyImportException {
		super(reader, DEFAULT_SKIP_LINES, new CSVParserBuilder().withSeparator(separator).build());

		initializeHeader();
	}

	/**
	 * Retrieves a specific data element from a line based on the value of the header.
	 *
	 * @param headerNames Name of the header element whose data we are trying to find
	 * @return The data element whose position matches that of the header whose value is passed in. Will return null when there are no more data elements.
	 * @throws IOException An error occurred during the read 
	 * @throws TaxonomyImportException there is a mismatch in the number of data items in a row and the number of header items
	 * @throws IllegalArgumentException If headerName does not exist
	 */
	@Nullable
	public String[] readNext(@Nullable final String... headerNames) throws IOException, TaxonomyImportException {
		if (headerNames == null) {
			return super.readNext();
		}
		final String[] strings = readNext();
		if (strings == null) {
			return null;
		}
		if (strings.length != headerIndex.size()) {
			throw new TaxonomyImportException("Header and data column size mismatch");
		}
		final String[] response = new String[headerNames.length];
		for (int i = 0; i < headerNames.length; i++) {
			final String headerName = headerNames[i];
			final Integer index = headerIndex.get(headerName);
			if (index == null) {
				throw new TaxonomyImportException("Header does not exist");
			}
			response[i] = strings[index.intValue()];
		}
		return response;
	}

	/**
	 * Reads the next line and returns a map of header values and data values.
	 *
	 * @return A map whose key is the header row of the data file and the values is the data values. Or null if the line is blank
	 * @throws IOException An error occurred during the read
	 * @throws TaxonomyImportException if there is a mismatch in the number of data items in a row and the number of header items
	 */
	@Nullable
	public Map<String, String> readMap() throws IOException, TaxonomyImportException {
		final String[] strings = readNext();
		if (strings == null) {
			return null;
		}
		if (strings.length != headerIndex.size()) {
			throw new TaxonomyImportException("Header and data column mismatch at line number" + getRecordsRead());
		}
		final Map<String, String> mappedLine = new LinkedHashMap<>();
		for (Map.Entry<String, Integer> entry : headerIndex.entrySet()) {
			if (entry.getValue().intValue() < strings.length) {
				mappedLine.put(entry.getKey(), strings[entry.getValue().intValue()]);
			}
		}
		return mappedLine;
	}

	private void initializeHeader() throws IOException, TaxonomyImportException {
		final String[] headers = super.readNext();
		if (headers == null ||  super.peek() == null) {
			throw new TaxonomyImportException("No content");
		}
		for (int i = 0; i < headers.length; i++) {
			if (Collections.frequency(Arrays.asList(headers), headers[i]) > 1) {
				throw new TaxonomyImportException("Duplicate column " + headers[i]);
			}
			headerIndex.put(headers[i], Integer.valueOf(i));
		}
	}
	
	public static List<Map<String, String>> linesFromCsv(final InputStream inputStream) throws IOException {
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(new BOMInputStream(inputStream), StandardCharsets.UTF_8))) {
			final char separator = detectSeparator(reader);
			try (final CSVReaderHeaderAware csvReader = new CSVReaderHeaderAware(reader, separator)) {
				final List<Map<String, String>> importLines = new ArrayList<>();
				Map<String, String> rowMap;
				while ((rowMap = csvReader.readMap()) != null) {
					importLines.add(rowMap);
				}
				return importLines;
			} catch (final TaxonomyImportException e) {
				throw new IOException(e);
			} 
		}
	}
	
	private static char detectSeparator(final BufferedReader reader) throws IOException {
		String firstLine;
		long commaCount = 0;
		long semicolonCount = 0;
		long tabCount = 0;
		
		try {
			reader.mark(8192);
			do {
				firstLine = reader.readLine();
			} while (firstLine.isEmpty());
			reader.reset();
		} catch (final NullPointerException e) {
			throw new IOException("No lines present in file");
		}
				
		for (final char ch : firstLine.toCharArray()) {
			if ( ch == ',' ) {
				commaCount++;
			} else if ( ch == ';' ) {
				semicolonCount++;
			} else if ( ch == '\t' ) {
				tabCount++;
		}
		}
		
		if (commaCount > semicolonCount) {
			return commaCount > tabCount ? ',' : '\t';
		} else {
			return semicolonCount > tabCount ? ';' : '\t';
		}
	}
}
