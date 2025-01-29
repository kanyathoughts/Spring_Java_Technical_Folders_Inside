package innowake.mining.moduleextraction;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import innowake.mining.moduleextraction.natural.NaturalBaseRuleSet;
import innowake.mining.moduleextraction.natural.NaturallDescriptionExtractor;


public class ModuleDescriptionsNaturalTest extends AbstractModuleDescriptionTest {
	
	public static final NaturallDescriptionExtractor BASE_RULESET = new NaturallDescriptionExtractor(NaturalBaseRuleSet.INSTANCE);
	
	@Test
	public void testNaturalBaseRuleSet1() {
		String description = getDescriptionFromFile("src/test/resources/natural/NATEX001.nsp", BASE_RULESET);
//		System.out.println(description);
		// TODO: testing that makes actually sense
		assertTrue(description.contains("Fusion/wechsel zweier mitglieder"));
	}
	
}
