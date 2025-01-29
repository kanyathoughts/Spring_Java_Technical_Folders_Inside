/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;

/**
 * Tests for the {@code SavedSearchServiceImpl#convertFiltersToFilterObject(String)}.
 */
@TestInstance(Lifecycle.PER_CLASS)
class SavedSearchServiceImplTest {

	@Test
	void testConvertFiltersToFilterObjectSingleFilter() {
		final String filters = "[{\"key\":\"identificationLink\",\"value\":[\"MISSING\"]}]";
		final Map<String, Object> expectedFilterObj = Map.of("content_identificationLink", Map.of("eq", "MISSING"));
		final Map<String, Object> actualFilterObj = SavedSearchServiceImpl.convertFiltersToFilterObject(filters);
		assertEquals(expectedFilterObj, actualFilterObj);
	}

	@Test
	void testConvertFiltersToFilterObjectMultipleFilters() {
		final String filters = "[{\"key\":\"typeLink\",\"value\":[\"RULE\"]},{\"key\":\"categoryLink.name\",\"value\":[\"Business Rule\"]}]";
		final Map<String, Object> expectedFilterObj = Map.of("content_typeLink", Map.of("eq", "RULE"), "content_categoryLink_name", Map.of("eq", "Business Rule"));
		final Map<String, Object> actualFilterObj = SavedSearchServiceImpl.convertFiltersToFilterObject(filters);
		assertEquals(expectedFilterObj, actualFilterObj);
	}

	@Test
	void testConvertFiltersToFilterObjectWithGteOperator() {
		final String filters = "[{\"key\":\"taxonomyCount\",\"value\":[{\"operator\":\"gte\",\"value\":1}]}]";
		final Map<String, Object> expectedFilterObj = Map.of("content_taxonomyCount", Map.of("gte", 1l));
		final Map<String, Object> actualFilterObj = SavedSearchServiceImpl.convertFiltersToFilterObject(filters);
		assertEquals(expectedFilterObj, actualFilterObj);
	}
	
	@Test
	void testConvertFiltersToFilterObjectWithLteOperator() {
		final String filters = "[{\"key\":\"taxonomyCount\",\"value\":[{\"operator\":\"lte\",\"value\":1}]}]";
		final Map<String, Object> expectedFilterObj = Map.of("content_taxonomyCount", Map.of("lte", 1l));
		final Map<String, Object> actualFilterObj = SavedSearchServiceImpl.convertFiltersToFilterObject(filters);
		assertEquals(expectedFilterObj, actualFilterObj);
	}

	@Test
	void testConvertFiltersToFilterObjectWithBooleanValue() {
		final String filters = "[{\"key\":\"requiresReview\",\"value\":[true]}]";
		final Map<String, Object> expectedFilterObj = Map.of("content_requiresReview", Map.of("is", true));
		final Map<String, Object> actualFilterObj = SavedSearchServiceImpl.convertFiltersToFilterObject(filters);
		assertEquals(expectedFilterObj, actualFilterObj);
	}
	
	@Test
	void testConvertFiltersToFilterObjectWithOrOperator() {
		final String filters = "[{\"key\":\"typeLink\",\"value\":[\"RULE\", \"DATABASE\"]}]";
		final Map<String, Object> expectedFilterObj = Map.of("content_typeLink", Map.of("in", List.of("RULE", "DATABASE")));
		final Map<String, Object> actualFilterObj = SavedSearchServiceImpl.convertFiltersToFilterObject(filters);
		assertEquals(expectedFilterObj, actualFilterObj);
	}
	
	@Test
	void testConvertFiltersToFilterObjectWithEqOperator() {
		final String filters = "[{\"key\":\"inboundDependencyCount\",\"value\":[{\"operator\":\"eq\",\"value\":0}]}]";
		final Map<String, Object> expectedFilterObj = Map.of("content_inboundDependencyCount", Map.of("eq", 0l));
		final Map<String, Object> actualFilterObj = SavedSearchServiceImpl.convertFiltersToFilterObject(filters);
		assertEquals(expectedFilterObj, actualFilterObj);
	}
	
	@Test
	void testConvertFiltersToFilterObjectWithIsOperator() {
		final String filters = "[{\"key\":\"requiresReview\",\"value\":[{\"operator\":\"is\",\"value\":true}]}]";
		final Map<String, Object> expectedFilterObj = Map.of("content_requiresReview", Map.of("is", true));
		final Map<String, Object> actualFilterObj = SavedSearchServiceImpl.convertFiltersToFilterObject(filters);
		assertEquals(expectedFilterObj, actualFilterObj);
	}	
	
	@Test
	void testConvertFiltersToFilterObjectThrowsException() {
		assertThrows(IllegalArgumentException.class, () -> SavedSearchServiceImpl.convertFiltersToFilterObject("[{\"key\":\"identificationLink\"}]"));
	}
}
