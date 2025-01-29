/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test.discovery.config;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Test;

import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.core.Mapping;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Tests for the {@link Config} class
 */
public class ConfigTest {

	private static final String DEFAULT_UNDISCOVERED_FOLDER = "undiscovered-entities";
	private static final String DEFAULT_OUTPUT_FORMAT = "EXCEL";

	@Test
	public void testDefault() {
		assertEquals("programs", getFolder(ResolveTarget.COBOL_PROGRAM));
		assertEquals("copies", getFolder(ResolveTarget.COBOL_COPYBOOK));
		assertEquals("maps", getFolder(ResolveTarget.CICS_BMS_MAPSET));

		assertEquals("jobs", getFolder(ResolveTarget.JCL_JOB));
		assertEquals("procs", getFolder(ResolveTarget.JCL_PROC));

		assertEquals("", getFolder(ResolveTarget.ASSEMBLER));

		assertEquals("", getFolder(ResolveTarget.XML));

		assertEquals("programs", getFolder(ResolveTarget.PL1_PROGRAM));
		assertEquals("copies", getFolder(ResolveTarget.PL1_COPYBOOK));

		assertEquals("", getFolder(ResolveTarget.NONE));
	}

	/**
	 * Should fall back on default config.
	 */
	@Test
	public void testNoMapping() {
		final Config config = new Config("", Collections.emptyList(), Collections.emptyList(), Boolean.FALSE, true, true, DEFAULT_UNDISCOVERED_FOLDER,
				DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("programs", folder);
	}

	@Test
	public void testOneMapping() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/*", ResolveTarget.COBOL_PROGRAM, "custom")
				), Collections.emptyList(), Boolean.FALSE, true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("custom", folder);
	}

	@Test
	public void testNoneMapping() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/*", ResolveTarget.NONE, "custom")
				), Collections.emptyList(), Boolean.FALSE, true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.NONE);

		assertEquals("custom", folder);
	}

	@Test
	public void testNoneDefaultMapping() {
		final Config config = new Config("", Collections.emptyList(), Collections.emptyList(), Boolean.FALSE, true, true, DEFAULT_UNDISCOVERED_FOLDER,
				DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.NONE);

		assertEquals("", folder);
	}

	@Test
	public void testTwoMapping() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/*", ResolveTarget.COBOL_PROGRAM, "custom"),
				new Mapping("**/*", ResolveTarget.COBOL_PROGRAM, "second")
				), Collections.emptyList(), Boolean.FALSE,   true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("custom", folder);
	}

	@Test
	public void testTwoMappingParent() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/*", ResolveTarget.COBOL, "custom"),
				new Mapping("**/*", ResolveTarget.COBOL_PROGRAM, "second")
				), Collections.emptyList(), Boolean.FALSE,  true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("custom", folder);
	}

	@Test
	public void testOnePath() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/COBOL.SOURCE/*", ResolveTarget.COBOL, "custom")
				), Collections.emptyList(), Boolean.FALSE,  true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("custom", folder);
	}

	@Test
	public void testWrongPath() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/ASSEMBLER.SOURCE/*", ResolveTarget.COBOL, "assembler"),
				new Mapping("**/COBOL.SOURCE/*", ResolveTarget.COBOL, "custom")
				), Collections.emptyList(), Boolean.FALSE,  true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("custom", folder);
	}

	@Test
	public void testTwoPath() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/*", ResolveTarget.COBOL, "custom"),
				new Mapping("**/COBOL.SOURCE/*", ResolveTarget.COBOL_PROGRAM, "wrong")
				), Collections.emptyList(), Boolean.FALSE,  true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("custom", folder);
	}

	@Test
	public void testTwoPathDifferentLanguage() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/*", ResolveTarget.NATURAL, "natural"),
				new Mapping("**/COBOL.SOURCE/*", ResolveTarget.COBOL_PROGRAM, "custom")
				), Collections.emptyList(), Boolean.FALSE,  true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("undiscovered/COBOL.SOURCE/CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("custom", folder);
	}

	@Test(expected=IllegalArgumentException.class)
	public void testEmptyPath() {
		final Config config = new Config("", Collections.emptyList(), Collections.emptyList(), Boolean.FALSE, true, true, DEFAULT_UNDISCOVERED_FOLDER,
				DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		config.getFolder("", ResolveTarget.COBOL_PROGRAM);
	}

	@Test
	public void testRootModule() {
		final Config config = new Config("", Arrays.asList(
				new Mapping("**/*", ResolveTarget.COBOL, "custom")
				), Collections.emptyList(), Boolean.FALSE,  true, true, DEFAULT_UNDISCOVERED_FOLDER, DEFAULT_OUTPUT_FORMAT, Collections.emptyList(), false);
		final String folder = config.getFolder("CSD035", ResolveTarget.COBOL_PROGRAM);

		assertEquals("custom", folder);
	}

	private String getFolder(final ResolveTarget resolveTarget) {
		return Config.getDefaultConfig().getFolder("undiscovered/COBOL.SOURCE/CSD035", resolveTarget);
	}
}
