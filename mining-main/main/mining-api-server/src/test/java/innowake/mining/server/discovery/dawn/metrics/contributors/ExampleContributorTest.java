/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.test.DiscoveryTestContext;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.Collections;

/**
 * Fake test demonstrating how to test a contributor.
 */
public class ExampleContributorTest {

	@Test
	public void testDeclareRootModule() {
		/* prepare "test source" - in a real test we would load these from a test resources folder */
		final SourcePojo testSource = SourcePojoDummy.build(o -> o
				.setProject(EntityId.of(1l))
				.setName("TEST")
				.setPath("/src/cobol/programs/TEST.cbl")
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setContent(new BinaryString("THERE SHOULD BE COBOL IN HERE")));

		final DiscoveryContext context = new DiscoveryTestContext(Collections.singletonList(testSource));
		/* instead of the Mockito mock, we will probably need to implement some sort of test builder
		 * because otherwise it is difficult to track the usages of the nested builders (ModuleBuilder, DependencyBuilder etc.)
		 *
		 * - the good thing is that the builder interface is "write-only", so the test builder doesn't really need to mock any kind of behavior,
		 * it just has to record method invocations in a structured way and allow to inspect them (i.e. contributor has declared dependency Y on Module X)
		 */
		final DiscoveryBuilderFromSource testBuilder = Mockito.mock(DiscoveryBuilderFromSource.class);

		/* for a real contributor, we need to mock/provide its dependencies here */
		final ExampleContributor contributor = new ExampleContributor();
		contributor.contribute(testBuilder, context, testSource);

		Mockito.verify(testBuilder, Mockito.times(1)).declareRootModule("TEST", ModuleType.COBOL_PROGRAM);
	}
}
