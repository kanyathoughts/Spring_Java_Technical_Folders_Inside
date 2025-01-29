package innowake.mining.moduleextraction;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import innowake.mining.moduleextraction.cobol.CobolBaseRuleSet;
import innowake.mining.moduleextraction.cobol.CobolDescriptionExtractor;


public class ModuleDescriptionsCobolTest extends AbstractModuleDescriptionTest {
	
	public static final CobolDescriptionExtractor BASE_RULESET = new CobolDescriptionExtractor(CobolBaseRuleSet.INSTANCE);
	
	@Test
	public void testCobolBaseRuleSet1() {
		String description = getDescriptionFromFile("src/test/resources/cobol/COBEX001.cbl", BASE_RULESET);
		System.out.println(description);
		// TODO: testing that makes actually sense
		assertTrue(description.contains("  Bls-laus disqualification report program"));
	}
	
	@Test
	public void testCobolBaseRuleSet2() {
		String description = getDescriptionFromFile("src/test/resources/cobol/COBEX002.cbl", BASE_RULESET);
		System.out.println(description);
		// TODO: testing that makes actually sense
//		assertTrue(description.contains("  Bls-laus disqualification report program"));
	}
	
}
