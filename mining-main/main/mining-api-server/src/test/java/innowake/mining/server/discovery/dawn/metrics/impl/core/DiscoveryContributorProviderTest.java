/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.core;

import static innowake.mining.shared.model.discovery.ResolveTarget.COBOL;
import static innowake.mining.shared.model.discovery.ResolveTarget.JAVA;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL;
import static org.junit.Assert.assertEquals;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.test.DiscoveryTestContext;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.core.KeyValuePair;
import innowake.mining.shared.discovery.config.core.Property;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/** Test class to check implementation of {@link DiscoveryContributorProvider} */
class DiscoveryContributorProviderTest {

	private static final String LANGUAGE_ENABLE = "ENABLE";
	
	/** Test to check if all no source contributors are returned successfully */
	@Test
	void testProvideContributor() {
		final List<DiscoveryContributor> contributors = ContributorProviderUtility.provideContributors();
		final DiscoveryContributorProviderImpl provider = new DiscoveryContributorProviderImpl(contributors, Collections.emptyList());
		assertEquals(1, provider.getNoSourceContributors().size());
	}

	/** Test to check if the contributors for the source objects are filtered properly based on the language enabled in the DiscoveryConfig */
	@Test
	void testProvideContributorFromSource() {
		final List<DiscoveryContributorFromSource> contributors = ContributorProviderUtility.provideSourceContributors();
		final DiscoveryContributorProviderImpl provider = new DiscoveryContributorProviderImpl(Collections.emptyList(), contributors);
		
		final Config config = new Config("", Collections.emptyList(),
				Arrays.asList(new Property(COBOL, new KeyValuePair(LANGUAGE_ENABLE, Boolean.toString(true))),
						new Property(NATURAL, new KeyValuePair(LANGUAGE_ENABLE, Boolean.toString(true))),
						new Property(JAVA, new KeyValuePair(LANGUAGE_ENABLE, Boolean.toString(false)))),
				Boolean.FALSE, true, true, "", "", Collections.emptyList(), false);

		/* Assert check to test the contributors for technology cobol */
		final SourcePojo sourceObjectCobol = SourcePojoDummy.build(o -> o.setTechnology(Technology.COBOL).setType(Type.PROGRAM));
		final DiscoveryContext contextCobol = new DiscoveryTestContext(Collections.singletonList(sourceObjectCobol), config);
		assertEquals(2, provider.getContributorsForSourceObject(contextCobol, sourceObjectCobol).size());

		/* Assert check to test the contributors for technology natural */
		final SourcePojo sourceObjectNatural = SourcePojoDummy.build(o -> o.setTechnology(Technology.NATURAL).setType(Type.PROGRAM));
		final DiscoveryContext contextNatural = new DiscoveryTestContext(Collections.singletonList(sourceObjectNatural), config);
		assertEquals(1, provider.getContributorsForSourceObject(contextNatural, sourceObjectNatural).size());

		/* Assert check to test the contributors for technology java */
		final SourcePojo sourceObjectJava = SourcePojoDummy.build(o -> o.setTechnology(Technology.JAVA).setType(Type.PROGRAM));
		final DiscoveryContext contextJava = new DiscoveryTestContext(Collections.singletonList(sourceObjectJava), config);
		assertEquals(0, provider.getContributorsForSourceObject(contextJava, sourceObjectJava).size());
	}
}
