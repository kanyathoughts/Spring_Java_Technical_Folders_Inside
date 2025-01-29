/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.io.output.StringBuilderWriter;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.CellValue;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.opencsv.CSVReader;
import com.opencsv.CSVWriterBuilder;
import com.opencsv.ICSVWriter;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.io.DiscoveryUidUtils;

/**
 * Utility methods for tests dealing with Discovery Excel exports.
 */
public class DiscoveryExcelUtil {

	public static Function<String, String> EMPTY_VALUE_CONVERSION = v -> StringUtils.EMPTY;
	public static Function<String, String> MULTIPLE_POSSIBLE_CANDIDATES_FOUND = v -> {
		if (v.startsWith("Multiple possible candidates found")) {
			final int start = v.indexOf('[');
			final int end = v.indexOf(']');
			if (start != -1 && end != -1) {
				final StringBuilder strb = new StringBuilder(v.length());
				strb.append(v.substring(0, start + 1));
				int length = v.substring(start, end).split(",").length;
				for (int i = length; i > 0; i--) {
					strb.append("id").append(i);
					if (i != 1) {
						strb.append(", ");
					}
				}
				strb.append(']');
				return strb.toString();
			}
		}

		return v;
	};

	private static final String DEPENDENCIES_SHEET_MAP_KEY = "Dependencies";
	private static final int ATTRIBUTE_INDEX = 9;

	private static final String SHEET_NAME_PREFIX = "# ";
	private static final Pattern CELL_PATTERN = Pattern.compile("[^\\w\t ]");
	private static final String NEW_LINE = "\r\n";
	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryExcelUtil.class);

	/**
	 * Dump workbook contents to string in CSV format.
	 *
	 * @param workbook the workbook
	 * @return a dump of the entire workbook as string
	 */
	public static String dumpWorkbook(final Workbook workbook) {
		final StringBuilderWriter result = new StringBuilderWriter(4096);
		final FormulaEvaluator evaluator = workbook.getCreationHelper().createFormulaEvaluator();
		final CSVWriterBuilder builder = new CSVWriterBuilder(result)
				.withEscapeChar('\\');

		try (final ICSVWriter csvWriter = builder.build()) {
			for (final Sheet sheet : workbook) {
				csvWriter.writeNext(new String[] {SHEET_NAME_PREFIX + sheet.getSheetName()});
				StreamSupport.stream(sheet.spliterator(), false)
						.map(row -> StreamSupport.stream(row.spliterator(), false)
								.map(evaluator::evaluate)
								.filter(Objects::nonNull)
								.map(CellValue::formatAsString)
								.toArray(String[]::new))
						.forEach(rowCells -> csvWriter.writeNext(rowCells));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}

		return result.toString();
	}


	/**
	 * Parses the given {@code csv} and returns the parse result in map.
	 * 
	 * @param csv the CSV content
	 * @return the parse result 
	 */
	public static Map<String, List<String[]>> getValuesFromCsv(final String csv) {
		return dumpWorkbookToMap(csv, Collections.emptySet(), Collections.emptyMap());
	}

	/**
	 * Compare workbook dumps ignoring the order of lines.
	 * <p>
	 * Column values must match exactly, but order of columns and rows is not important. Columns listed in columns {@code "Uid"} {@code "Target Uid"} and receive
	 * special treatment: for these we will only check for equality if the expected value is {@code -1} (indicating missing Module) or {@code 0}
	 * (indicating Utility Module). Otherwise the value of an uid column is ignored.
	 * 
	 * @param expected expected workbook contents
	 * @param actual actual workbook contents
	 */
	public static void compareIgnoreOrder(final String expected, final String actual) {
		compareIgnoreOrder(expected, actual, Set.of("Uid", "Target Uid", "Parent Uid", "Reached From Uids"), Map.of("Cause", MULTIPLE_POSSIBLE_CANDIDATES_FOUND));
	}

	/**
	 * Compare workbook dumps ignoring the order of lines.
	 * <p>
	 * Column values must match exactly, but order of columns and rows is not important. Columns listed in {@code uidColumns} receive special treatment:
	 * for these we will only check for equality if the expected value is {@code -1} (indicating missing Module) or {@code 0} (indicating Utility Module).
	 * Otherwise the value of an uid column is ignored.
	 * 
	 * @param expected expected workbook contents
	 * @param actual actual workbook contents
	 * @param uidColumns headers of columns that contain uids, which receive special treatment (see above)
	 * @param valueHandlers map to provide functions for cell value conversions for columns headers
	 */
	public static void compareIgnoreOrder(final String expected, final String actual, final Set<String> uidColumns, final Map<String, Function<String, String>> valueHandlers) {
		final Tuple2<String, String> expectedAndActualWorkbookData = getExpectedAndActualWorkbookData(expected, actual, true, uidColumns, valueHandlers);
		assertEquals(expectedAndActualWorkbookData.a, expectedAndActualWorkbookData.b, "Sheets must match");
	}

	/**
	 * Compare workbook dumps preserving the order of lines.
	 * <p>
	 * Column values must match exactly, and order of columns and rows is important. Columns listed in {@code uidColumns} receive special treatment:
	 * for these we will only check for equality if the expected value is {@code -1} (indicating missing Module) or {@code 0} (indicating Utility Module).
	 * Otherwise the value of an uid column is ignored.
	 * 
	 * @param expected workbook contents
	 * @param actual workbook contents
	 * @param uidColumns headers of columns that contain uids, which receive special treatment (see above)
	 * @param valueHandlers map to provide functions for cell value conversions for columns headers
	 * @return true if workbook dumps are exact copy of each other except for uidColumns, false otherwise
	 */
	public static boolean comparePreservingOrder(final String expected, final String actual, final Set<String> uidColumns, final Map<String, Function<String, String>> valueHandlers) {
		final Tuple2<String, String> expectedAndActualWorkbookData = getExpectedAndActualWorkbookData(expected, actual, false, uidColumns, valueHandlers);
		return expectedAndActualWorkbookData.a.equals(expectedAndActualWorkbookData.b);
	}

	private static Tuple2<String, String> getExpectedAndActualWorkbookData(final String expected, final String actual, final boolean ignoreOrder,
			final Set<String> uidColumns, final Map<String, Function<String, String>> valueHandlers) {
		final Map<String, List<String[]>> actualSheets = dumpWorkbookToMap(actual, uidColumns, valueHandlers);
		final Map<String, List<String[]>> expectedSheets = dumpWorkbookToMap(expected, uidColumns, valueHandlers);
		final Set<String> actualSheetNames = actualSheets.keySet();

		assertEquals(expectedSheets.keySet(), actualSheetNames, "Sheet names must match");

		/* Sort the attributes in the dependency section and put it back to the map */
		final List<String[]> actualSheetDependencies = actualSheets.get(DEPENDENCIES_SHEET_MAP_KEY);
		final List<String[]> expectedSheetDependencies = expectedSheets.get(DEPENDENCIES_SHEET_MAP_KEY);
		if (actualSheetDependencies != null && expectedSheetDependencies != null) {
			final List<String[]> sortedActualSheetDependencies = sortAttributeMap(actualSheetDependencies);
			final List<String[]> sortedExpectedSheetDependencies = sortAttributeMap(expectedSheetDependencies);
			/* Put back the sorted values into the dependencies sheet */
			actualSheets.put(DEPENDENCIES_SHEET_MAP_KEY, sortedActualSheetDependencies);
			expectedSheets.put(DEPENDENCIES_SHEET_MAP_KEY, sortedExpectedSheetDependencies);
		}

		final StringBuilder expStrb = new StringBuilder(4096);
		final StringBuilder actStrb = new StringBuilder(4096);
		for (final String currentSheetName : actualSheetNames) {

			/* add sheet title as first row */
			expStrb.append("#").append(currentSheetName).append(NEW_LINE);
			actStrb.append("#").append(currentSheetName).append(NEW_LINE);

			/* get rows for current sheet */
			final List<String[]> expRows = expectedSheets.get(currentSheetName);
			final List<String[]> actRows = actualSheets.get(currentSheetName);

			/* get headers and test if they are equal*/
			final String expHeaders = String.join(",", expRows.get(0));
			assertEquals(expHeaders, String.join(",", actRows.get(0)), "Header names must match");

			/* add header row */
			expStrb.append(expHeaders).append(NEW_LINE);
			actStrb.append(expHeaders).append(NEW_LINE);

			final Stream<String> expSheetRows = expRows.stream()
													   .skip(1) /* skip header */
													   .map(arr -> String.join(",", arr));
			final Stream<String> actSheetRows = actRows.stream()
													   .skip(1) /* skip header */
													   .map(arr -> String.join(",", arr));

			if (ignoreOrder) {
				actSheetRows.sorted()
							.forEach(line -> actStrb.append(line).append(NEW_LINE));
				expSheetRows.sorted()
							.forEach(line -> expStrb.append(line).append(NEW_LINE));
			} else {
				actSheetRows.forEach(line -> actStrb.append(line).append(NEW_LINE));
				expSheetRows.forEach(line -> expStrb.append(line).append(NEW_LINE));
			}
		}

		return new Tuple2<>(expStrb.toString(), actStrb.toString());
	}

	@SuppressWarnings("unchecked")
	private static List<String[]> sortAttributeMap(List<String[]> sheetDependencies) {
		return sheetDependencies.stream().map(sheetDependency -> {
			final String attributes = sheetDependency[ATTRIBUTE_INDEX];
			if ( ! attributes.startsWith("{")) {
				/* value is not a map, return without sorting */
				return sheetDependency;
			}
			try {
				final ObjectMapper mapper = new ObjectMapper();
				final var attributeMap = mapper.readValue(attributes, Map.class);
				final var sortedAttributeMap = sortMap(attributeMap);
				sheetDependency[ATTRIBUTE_INDEX] = sortedAttributeMap;
				return sheetDependency;
			} catch (final IllegalStateException | JsonProcessingException e) {
				LOG.error("Unable to parse/sort the attribute map: " + sheetDependency, e);
				return sheetDependency;
			}
		}).collect(Collectors.toList());
	}

	@SuppressWarnings("unchecked")
	private static String sortMap(final Map<String, Object> map) {
		/* Sort the keys alphabetically */
		final List<String> sortedKeys = new ArrayList<>(map.keySet());
		Collections.sort(sortedKeys);
		final StringBuilder resultBuilder = new StringBuilder();
		resultBuilder.append("{");
		for (String key : sortedKeys) {
			resultBuilder.append(key).append(": ");
			Object value = map.get(key);
			if (value instanceof List<?>) {
				value = sortList((List<?>) value);
			} else if (value instanceof Map) {
				/* If the value is another map, recursively convert it to a string */
				value = sortMap((Map<String, Object>) value);
			}
			resultBuilder.append(value);
			resultBuilder.append(", ");
		}

		/* Remove the trailing comma*/
		if (resultBuilder.length() > 2) {
			resultBuilder.setLength(resultBuilder.length() - 2);
		}
		resultBuilder.append("}");
		return resultBuilder.toString();
	}

	@SuppressWarnings("unchecked")
	private static List<?> sortList(final List<?> attributeList) {
		if (attributeList.isEmpty()) {
			return attributeList;
		}
		if (attributeList.get(0) instanceof String) {
			/* Just sort the list of strings alphabetically */
			 Collections.sort((List<String>) attributeList);
			 return attributeList;
		}
		if (attributeList.get(0) instanceof Map) {
			 return attributeList.stream().map(mapValue -> sortMap((Map<String, Object>) mapValue)).collect(Collectors.toList());
		}
		if (attributeList.get(0) instanceof List<?>) {
			return attributeList.stream().map(listValue -> sortList((List<?>) listValue)).collect(Collectors.toList());
		}
		/* cannot sort, return as it is */
		return attributeList;
	}

	private static String getUidValue(final String columnValue) {
		/* special handling for UId columns: include value only if it is -1 or 0 -- otherwise ignore value by mapping it to empty string */
		try {
			/* the workbook dumps contain the UId in float format (e.g. "-1.0"), but we need to interpret them as integers */
			final int uidValue = (int) Double.parseDouble(columnValue);
			if (uidValue == DiscoveryUidUtils.MISSING_UID || uidValue == DiscoveryUidUtils.UTILITY_UID) {
				/* if UId is -1 or 0 include the actual value in the comparison */
				return columnValue;
			}
		} catch (final NumberFormatException e) {
			LOG.trace("NumberFormatException is being handled", e);
		}

		/* Columns containing an UId must be ignored. required because UId values will be different in each test run */
		return "";
	}
	
	/* create a map based on UIDs (note: this is specific to how we are currently saving excel sheets, so it is a bit risky) */
	private static Map<String, List<String[]>> dumpWorkbookToMap(final String excelDump, final Set<String> uidColumns, final Map<String, Function<String, String>> valueHandlers) {
		final Map<String, List<String[]>> sheetNamesToRows = new LinkedHashMap<>();
		try (final CSVReader csvReader = new CSVReader(new StringReader(excelDump))) {
			String sheet = null;
			List<String[]> lines = new ArrayList<>();
			final Iterator<String[]> it = csvReader.iterator();
			while (it.hasNext()) {
				final String[] cells = it.next();
				if (cells.length == 1 && cells[0].startsWith(SHEET_NAME_PREFIX)) {
					if ( ! lines.isEmpty()) {
						sheetNamesToRows.put(assertNotNull(sheet), handleCellValues(lines, uidColumns, valueHandlers));
						lines = new ArrayList<>();
					}

					sheet = cells[0].substring(2).trim();
				} else if (cells.length != 0) {
					lines.add(cells);
				}
			}

			if ( ! lines.isEmpty()) {
				sheetNamesToRows.put(assertNotNull(sheet), handleCellValues(lines, uidColumns, valueHandlers));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}

		return sheetNamesToRows;
	}

	private static List<String[]> handleCellValues(final List<String[]> lines, final Set<String> uidColumns, final Map<String, Function<String, String>> valueHandlers) {
		for (int column = 0; column < lines.get(0).length; column++) {
			/* get header but without odd non-alpha, non-space characters */
			final String header = CELL_PATTERN.matcher(lines.get(0)[column]).replaceAll("");
			lines.get(0)[column] = header;

			final Function<String, String> valueHandler;
			if (uidColumns.contains(header)) {
				/* Start with row index 1 -> Skip header row at index 0 */
				for (int row = 1; row < lines.size(); row++) {
					if (column < lines.get(row).length) {
						/* Statements (Nodes), SQL Statements, Errors (Nodes) is skipped as it would be removed with WMIN-9622 */
						if (header.equals("Statements Nodes") || header.equals("SQL Statements") || header.equals("Errors Nodes")) {
							lines.get(row)[column] = "";
						} else {
							lines.get(row)[column] = getUidValue(lines.get(row)[column]);
						}
					}
				}
			} else if ((valueHandler = valueHandlers.get(header)) != null) {
				for (int row = 1; row < lines.size(); row++) {
					if (column < lines.get(row).length) {
						lines.get(row)[column] = valueHandler.apply(lines.get(row)[column]);
					}
				}
			}
		}

		return lines;
	}

	public static void main(final String[] args) throws IOException {
		/* Manually replace
		 * 
		 * ReadsWrites => Accesses
		 * SQL_TEMPORARY_TABLE => SQL_TABLE
		 * NATURAL_IW_GDA => NATURAL_GDA
		 * NATURAL_IW_PDA => NATURAL_PDA
		 * NATURAL_IW_LDA => NATURAL_LDA
		 * NATURAL_COPYCODE_REPORTING => NATURAL_COPYCODE
		 * NATURAL_HELP_REPORTING => NATURAL_HELP
		 * "UNKNOWN","UNKNOWN_PROGRAM" => "UNKNOWN","PROGRAM"
		 * "UNKNOWN","UNKNOWN_UTILITY" => "UNKNOWN","UTILITY"
		 * 
		 * in all expected files */
		final String[] folders = {"test-resources/innowake/mining/server/discovery/expected/ims",
								  "test-resources/innowake/mining/server/discovery/expected/incremental",
								  "test-resources/innowake/mining/server/discovery/expected/iris" };

		final Charset charset = Charset.forName("cp1252");
		for (final String folder : folders) {
			final Path resPath = Paths.get(folder);
			Files.walk(resPath)
				.filter(path -> path.getFileName().toString().equals("discovery.xlsx.dump"))
				.forEach(path -> {
					try {
						final List<String> lines = Files.readAllLines(path, charset);
						final Map<String, Integer> statementCount = new HashMap<>();
						boolean modified = false;
						boolean isStatement = false;
						for (int i = 0; i < lines.size(); i++) {
							final String line = lines.get(i);
							if ("# Statements".equals(line)) {
								isStatement = true;
								/* Skip header */
								i++;
							} else if ("# SQL".equals(line)) {
								isStatement = false;
								break;
							} else if (isStatement) {
								final String[] rows = line.split(",");
								int counter = statementCount.computeIfAbsent(rows[0], key -> 0);
								counter++;
								statementCount.put(rows[0], counter);
							}
						}
	
						boolean isModule = false;
						for (int i = 0; i < lines.size(); i++) {
							final String line = lines.get(i);
							if ("# Modules".equals(line)) {
								isModule = true;
								/* Skip header */
								i++;
							} else if ("# Statements".equals(line)) {
								isModule = false;
								break;
							} else if (isModule) {
								final String[] rows = line.split(",");
								if (rows.length > 12) {
									final Integer counter = statementCount.get(rows[0]);
									if (counter != null) {
										rows[12] = counter.toString();
										modified = true;
										lines.set(i, Arrays.stream(rows).collect(Collectors.joining(",")));
									} else if ("-1".equals(rows[12])) {
										rows[12] = "0";
										modified = true;
										lines.set(i, Arrays.stream(rows).collect(Collectors.joining(",")));
									}
								}
							}
						}
						if (modified) {
							Files.write(path, lines, charset);
						}
					} catch (IOException e) {
						throw new IllegalStateException(e);
					}
				});
		}
	}
}
