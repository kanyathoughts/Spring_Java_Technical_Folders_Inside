/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.oracle;

import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.junit.Test;

import innowake.mining.server.discovery.parser.oracle.CDORecordParser.ParseError;

/**
 * Tests for the {@link CDORecordParser}.
 */
public class CDORecordParserTest {
	
	private static final String RESOURCE_PATH = "/test-resources/innowake/mining/server/discovery/parser/oracle/cdo/";
	
	/**
	 * Parsing a CDO file without any records defined.
	 */
	@Test
	public void retrieveNoRecordsIfNoRecordIsDefined() {
		final CDORecordParser parser = new CDORecordParser();
		final String content = getContent("WDIS256B.cdo");
		final List<CDORecord> records = parser.parse(content);
		
		assertEquals("Number of records must match.", 0, records.size());
	}
	
	/**
	 * Parsing a CDO file with a single record defined.
	 */
	@Test
	public void retrieveSingleRecord() {
		final CDORecordParser parser = new CDORecordParser();
		final String content = getContent("WDIS256A.cdo");
		final List<CDORecord> records = parser.parse(content);
		
		assertEquals("Number of records must match.", 1, records.size());
		final List<String> exptectedRecordNames = new ArrayList<>(Arrays.asList("CDD_REC.ACE_BENDATA"));
		assertRecordNames(records, exptectedRecordNames);
	}
	
	/**
	 * Parsing a CDO file with multiple records defined.
	 */
	@Test
	public void retrieveMultipleRecords() {
		final CDORecordParser parser = new CDORecordParser();
		final String content = getContent("WDIS256C.cdo");
		final List<CDORecord> records = parser.parse(content);
		
		assertEquals("Number of records must match.", 2, records.size());
		final List<String> exptectedRecordNames = new ArrayList<>(Arrays.asList("CDD_REC1.ACE_BENDATA", "CDD_REC2.ACE_BENDATA"));
		assertRecordNames(records, exptectedRecordNames);
	}
	
	/**
	 * As for now it's assumed that the record name is on the same line as the "DEFINE RECORD"
	 * there should at least be some kind of indication if this assumption is violated by adding
	 * a parse error.
	 */
	@Test
	public void hasParseErrorWhenRecordNameCannotBeFound() {
		final CDORecordParser parser = new CDORecordParser();
		final String content = getContent("WDIS256D.cdo");
		final List<CDORecord> records = parser.parse(content);
	
		assertTrue("No records must have been identified.", records.isEmpty());
		assertTrue("Parser must have parse errors.", parser.hasParseErrors());
		final List<ParseError> parseErrors = parser.getParseErrors();
		assertEquals("There must be 1 parse error.", 1, parseErrors.size());
		final ParseError parseError = parseErrors.get(0);
		assertTrue("Parse error message should have specific pattern.",
				parseError.getMessage().startsWith("Found 'DEFINE RECORD' without an actual record name"));
	}
	
	/**
	 * There can be multiple whitespace characters separating the record name from the "DEFINE RECORD" command. 
	 */
	@Test
	public void allowsMultipleWhitspaceCharactersForSeparatingTheRecordName() {
		final CDORecordParser parser = new CDORecordParser();
		final String content = getContent("WDIS279.cdo");
		final List<CDORecord> records = parser.parse(content);
	
		assertFalse("There must not be any parse errors.", parser.hasParseErrors());
		assertEquals("Number of records must match.", 1, records.size());
		assertEquals("BEN_WKSP.BENI000_CONTROL_WKSP", records.get(0).getName());
	}
	
	/**
	 * The parse errors should be cleared before any successive runs.
	 */
	@Test
	public void parseErrorsAreClearedOnSuccessiveParseRuns() {
		final CDORecordParser parser = new CDORecordParser();
		final String contentWhichLeadsToParseErrors = getContent("WDIS256D.cdo");
		final List<CDORecord> records = parser.parse(contentWhichLeadsToParseErrors);
	
		assertTrue("No records must have been identified.", records.isEmpty());
		assertTrue("Parser must have parse errors.", parser.hasParseErrors());
		final List<ParseError> parseErrors = parser.getParseErrors();
		assertEquals("There must be 1 parse error.", 1, parseErrors.size());
	
		final String contentWithoutParseErrors = getContent("WDIS256B.cdo");
		parser.parse(contentWithoutParseErrors);
		assertFalse("Parser must not have any parse errors upon successive parse run.", parser.hasParseErrors());
		assertTrue("The list of parse errros must be empty.", parser.getParseErrors().isEmpty());
	}
	
	private void assertRecordNames(final List<CDORecord> records, final List<String> expectedRecordNames) {
		for (final Iterator<String> nameIterator = expectedRecordNames.iterator(); nameIterator.hasNext();) {
			final String recordName = nameIterator.next();
			for (final Iterator<CDORecord> recordIterator = records.iterator(); recordIterator.hasNext();) {
				final CDORecord record = recordIterator.next();
				if (record.getName().equals(recordName)) {
					nameIterator.remove();
					recordIterator.remove();
				}
			}
		}
		
		assertTrue("Not all expected record names were found.", expectedRecordNames.isEmpty());
		assertTrue("More actual records than were expected.", records.isEmpty());
	}

	private String getContent(final String filename) {
		final Path path = Paths.get(System.getProperty("user.dir"), RESOURCE_PATH, filename);
		try {
			return IOUtils.toString(Files.newBufferedReader(path, Charset.forName("Cp1252")));
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			throw new IllegalStateException(e);
		}
	}

}
