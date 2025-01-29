package innowake.mining.server.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.mining.shared.discovery.config.utility.UtilityTaxonomy;
import org.junit.jupiter.api.Test;

import innowake.mining.shared.discovery.config.utility.UtilityEntity;
import innowake.mining.shared.discovery.config.utility.UtilityList;

class DiscoveryConfigSynchUtilTest {
	protected static final Charset CONFIG_CHARSET = Charset.forName("UTF-8");

	private static final String XML_TEST_FILES_ROOT = "./test-resources/innowake/mining/server/discovery/source/utilities-synch/";

	/**
	 * Tests the {@link DiscoveryConfigSynchUtil#readAndParseUtilities(BufferedReader, BufferedReader)} (URL)} ()}
	 * @throws FileNotFoundException 
	 */
	@Test
	void testReadAndParseXML() throws FileNotFoundException {
		final InputStream streamUpdate = new FileInputStream(XML_TEST_FILES_ROOT + "utilitiesToUpdateTest.xml");
		final InputStream streamReference = new FileInputStream(XML_TEST_FILES_ROOT + "utilitiesToReferenceTest.xml");
		final BufferedReader refReader = new BufferedReader(new InputStreamReader(streamReference, CONFIG_CHARSET));
		final BufferedReader updateReader = new BufferedReader(new InputStreamReader(streamUpdate, CONFIG_CHARSET));
		final UtilityList utilityList = DiscoveryConfigSynchUtil.readAndParseUtilities(updateReader, refReader);
		assertNotNull(utilityList);
		assertNotNull(utilityList.getUtilities());
		assertNotNull(utilityList.getUtilities().get(0));
		assertNotNull(utilityList.getUtilities().get(0).getCategories());
		assertNotNull(utilityList.getUtilities().get(0).getCategories().get(0));
		assertEquals("Uncategorized", utilityList.getUtilities().get(0).getCategories().get(0));
		assertEquals(3, utilityList.getUtilities().size());
		assertTrue(utilityList.isUtility("TEST-UTILITY"));
		assertNotNull(utilityList.getUtilities().get(2));
		final UtilityEntity utilityEntity = utilityList.getUtilities().get(2);
		assertEquals("TEST-UTILITY", utilityEntity.getModuleName());
		assertEquals(List.of("None", "All"), utilityEntity.getTaxonomies().stream().map(UtilityTaxonomy::getTaxonomy).collect(Collectors.toList()));
		assertEquals(Set.of("test"), utilityEntity.getTaxonomies().stream().map(UtilityTaxonomy::getType).collect(Collectors.toSet()));
		assertEquals("THIS IS A TEST DESCRIPTION", utilityEntity.getDescription());
		assertEquals("None", utilityEntity.getInfoOrigin());
		assertEquals("true", utilityEntity.getSupported());
		assertEquals("TEST COMMENT", utilityEntity.getComments());
		assertEquals("FTP", utilityEntity.getUtilityInterface().getName());
		assertEquals("GET", utilityEntity.getParameters().get(0).getInbound());
		assertEquals("DIR", utilityEntity.getParameters().get(0).getOutbound().getOutBoundValue());
		assertEquals("INPUT", utilityEntity.getSteps().get(0).getName());
		assertEquals("SYSIN", utilityEntity.getSteps().get(1).getName());
	}
}
