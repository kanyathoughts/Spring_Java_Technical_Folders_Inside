/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.junit.Test;

import innowake.mining.data.core.annotation.impl.AnnotationMerger;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Tests regarding merging annotations when they are stacked within one another.
 */
public class AnnotationMergerTest {

	private static final Path BASE_PATH = Paths.get("test-resources", "innowake", "mining", "data", "core", "annotation", "merger");
	private static final Charset CHARSET = Charset.forName("cp1252");
	private static final String NEW_LINE = "\n";
	private static final String OFFSET = "offset";
	private static final String LENGTH = "length";
	private static final String PATTERN = String.format("%s=(?<%s>\\d+),%s=(?<%s>\\d+)", OFFSET, OFFSET, LENGTH, LENGTH);
	private static final Pattern INPUT_LINE = Pattern.compile(PATTERN);

	/**
	 * There are SELECT statements within EXEC SQL DECLARE CURSOR, which are treated as a child
	 * of the DECLARE CUSRSOR, but which are itself EXEC SQL nodes. This is something we do not
	 * care about, as we are only interested in the outer EXEC SQL statement.
	 *
	 * @throws IOException when the test file cannot be read
	 */
	@Test
	public void testExecSQL() throws IOException {
		test("execsql", 5);
	}

	/**
	 * Two levels nested should only retain the first level.
	 * <pre>
	 *     +-------------------+
	 *     | Location Level 1  |
	 *     +-------------------+
	 *               |
	 *               |  +-------------------+
	 *               +->| Location Level 2  |
	 *                  +-------------------+
	 *                            |
	 *                            |   +-------------------+
	 *                            +-->| Location Level 3  |
	 *                                +-------------------+
	 * </pre>
	 * 
	 * @throws IOException when the test file cannot be read
	 */
	@Test
	public void testNested() throws IOException {
		test("nested", 2);
	}

	private void test(final String name, final int expectedSize) throws IOException {
		final var input = createAnnotations(name);
		final var actual = AnnotationMerger.merge(input);

		/* assert that the number of Annotations in the merged list is smaller than in the input list */
		assertThatCollectionHasSize(actual, expectedSize);

		/* assert that specific annotations which were merged are missing from the merged list */
		final String actualLocations = actual.stream().map(this::locationString).collect(Collectors.joining(NEW_LINE));
		final String expectedLocations = Files.readAllLines(BASE_PATH.resolve(name + ".expected"), CHARSET).stream().collect(Collectors.joining(NEW_LINE));
		assertEquals(expectedLocations, actualLocations);
	}

	private List<AnnotationPojoTemplate> createAnnotations(final String file) throws IOException {
		return Files.readAllLines(BASE_PATH.resolve(file + ".input"), CHARSET).stream().map(this::createAnnotation).collect(Collectors.toList());
	}

	private AnnotationPojoTemplate createAnnotation(final String inputLine) {
		final var annotation = new AnnotationPojoTemplate();
		annotation.setModule(EntityId.of(1L));
		annotation.setLocation(createLocation(inputLine));
		return annotation;
	}

	private String locationString(final AnnotationPojoPrototype annotation) {
		final ModuleLocation location = annotation.location.getNonNull();
		final Integer offsetValue = location.getOffset();
		final Integer lengthValue = location.getLength();
		return String.format("%s=%d,%s=%d", OFFSET, offsetValue, LENGTH, lengthValue);
	}

	private ModuleLocation createLocation(final String inputLine) {
		final Matcher matcher = INPUT_LINE.matcher(inputLine);
		if (matcher.matches()) {
			return new ModuleLocation(Integer.parseInt(matcher.group(OFFSET)), Integer.parseInt(matcher.group(LENGTH)));
		}
		throw new IllegalArgumentException("The given line must be of format: offset=<OFFSET>,length=<LENGTH>");
	}

	private void assertThatCollectionHasSize(final Collection<?> actual, int expectedActualSize) {
		assertEquals(expectedActualSize, actual.size());
	}

}
