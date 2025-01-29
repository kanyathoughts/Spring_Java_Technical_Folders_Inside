/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SavedSearchService;
import innowake.mining.shared.entities.SavedSearchPojo;

/**
 * Tests for {@link SavedSearchService}
 */
class SavedSearchServiceTest extends DatabaseResettingTest {
	
	@Autowired
	private SavedSearchService savedSearchService;
	
	private static final List<String> SAVED_SEARCHES = Arrays.asList("Missing Source Files", "No Taxonomies Assigned", "Not Referenced", "Requires Review", 
			"Taxonomy Assignments", "Business Related DD Entries", "Copybook Defined DD Entries", "Not Referenced DD entries",  "Business Rule Candidates",
			"Database Candidates", "Ungrouped Annotations", "Field Level Mining Candidates");

	/**
	 * Test for listing all {@link SavedSearch} using ProjectId and Saved searches names
	 */
	@Test
	@Order(9)
	void testfindByName() {
		final List<SavedSearchPojo> savedSearches = savedSearchService.find(q -> q.ofProject(EntityId.of(0l)).withNames(SAVED_SEARCHES));
		assertEquals(SAVED_SEARCHES.size(), savedSearches.size());
	}
}
