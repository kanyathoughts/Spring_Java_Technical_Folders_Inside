/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.utility;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.discovery.DiscoveryException;

/**
 * Unit test for {@link UtilityList}
 */
class UtilityListTest extends DatabaseRelatedTest {
	
	@Autowired
	private transient ProjectService projectService;
	
	@Test
	void testLoadUtilityList() throws DiscoveryException {
		final UtilityList utilityList = UtilityList.loadUtilityList(projectService, EntityId.of(Long.valueOf(1)));
		assertNotNull(utilityList);
		assertNotNull(utilityList.getUtilities());
		assertNotNull(utilityList.getUtilities().get(0));
		assertNotNull(utilityList.getUtilities().get(0).getCategories());
		assertNotNull(utilityList.getUtilities().get(0).getCategories().get(0));
		assertEquals("File Transfer", utilityList.getUtilities().get(0).getCategories().get(0));
	}
}
