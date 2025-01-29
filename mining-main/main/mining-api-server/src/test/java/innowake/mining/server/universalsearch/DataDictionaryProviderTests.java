/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.universalsearch;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import innowake.mining.server.universalsearch.provider.DataDictionaryUniversalSearchProvider;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.testing.DataDictionaryPojoDummy;
import innowake.mining.shared.entities.testing.ModuleLightweightPojoDummy;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.universalsearch.UniversalSearchLink.Type;
import innowake.mining.shared.universalsearch.UniversalSearchResult;

/**
 * Unit tests for {@link DataDictionaryUniversalSearchProvider}
 */
class DataDictionaryProviderTests {

	@InjectMocks
	private DataDictionaryUniversalSearchProvider dataDictionarySearchProvider;

	@Mock
	private DataDictionaryService dataDictionaryService;
	@Mock
	private ModuleService moduleService;

	@BeforeEach
	public void setUp() {
		MockitoAnnotations.openMocks(this);
	}

	@Test
	void testMatchingDataDictionaries() {
		/* Create some mock data for testing */
		final String query = "ame";
		
		final ModuleLightweightPojo testModule1 = createTestModule(1L);
		final ModuleLightweightPojo testModule2 = createTestModule(2L);
		
		final DataDictionaryPojo dataDictionaryEntry1 = createTestDataDictionaryEntry("Name1", "Description1", 112, 6, testModule1.getId());
		final DataDictionaryPojo dataDictionaryEntry2 = createTestDataDictionaryEntry("Name2", "Description2", 236, 15, testModule2.getId());

		/* Create a mock Page */
		final Paged<DataDictionaryPojo> page = new Paged<>(Arrays.asList(dataDictionaryEntry1, dataDictionaryEntry2), 10, 0L, 2L, 1L, 2L) { };

		/* Mock the behavior of dataDictionaryEntryService.findAll */
		when(dataDictionaryService.find(any(), any())).thenReturn(page);
		when(moduleService.getModuleLightweight(testModule1.identity())).thenReturn(testModule1);
		when(moduleService.getModuleLightweight(testModule2.identity())).thenReturn(testModule2);

		final List<UniversalSearchResult> results = dataDictionarySearchProvider.query(EntityId.of(1L), query);
		verify(dataDictionaryService, times(1)).find(any(), any());
		assertEquals(2, results.size());

		final UniversalSearchResult result1 = results.get(0);
		assertEquals("data-dictionary", result1.getProvidedBy());
		assertEquals(0, result1.getRank());
		assertEquals("Data Dictionary", result1.getType());
		assertEquals("Name1", result1.getTitle());
		assertEquals("Path1", result1.getSubTitle());
		assertEquals("Description1", result1.getContext());
		assertEquals(1, result1.getLinks().size());
		assertEquals(Type.CODE_VIEWER, result1.getLinks().get(0).getType());
		assertEquals("112", result1.getLinks().get(0).getProperties().get("length"));
		assertEquals("6", result1.getLinks().get(0).getProperties().get("offset"));

		final UniversalSearchResult result2 = results.get(1);
		assertEquals("data-dictionary", result1.getProvidedBy());
		assertEquals(0, result1.getRank());
		assertEquals("Data Dictionary", result2.getType());
		assertEquals("Name2", result2.getTitle());
		assertEquals("Path2", result2.getSubTitle());
		assertEquals("Description2", result2.getContext());
		assertEquals(1, result1.getLinks().size());
		assertEquals(Type.CODE_VIEWER, result1.getLinks().get(0).getType());
		assertEquals("236", result2.getLinks().get(0).getProperties().get("length"));
		assertEquals("15", result2.getLinks().get(0).getProperties().get("offset"));
	}
	
	@Test
	void testNoMatchingDataDictionaries() {
		final String query = "f42130b3f114a7a8c1c8c2f2bbf984d20e5a035bf56516047b9f38d1a2426fde";
		/* Mock the behavior of dataDictionaryEntryService.findAll */
		when(dataDictionaryService.find(any(), any())).thenReturn(new Paged<DataDictionaryPojo>(Collections.emptyList(), 10, 0L, 0L, null, null) { });
		final List<UniversalSearchResult> results = dataDictionarySearchProvider.query(EntityId.of(1L), query);
		verify(dataDictionaryService, times(1)).find(any(), any());
		assertTrue(results.isEmpty());
	}

	/**
	 * Utility method to create a mock DataDictionaryEntry.
	 */
	private DataDictionaryPojo createTestDataDictionaryEntry(final String name,
			final String description, final Integer length, final Integer offset, final Long moduleId) {
		return new DataDictionaryPojoDummy().prepare(d -> d
				.setName(name)
				.setDescription(description)
				.setLocation(new ModuleLocation(offset, length))
				.setModule(EntityId.of(moduleId)))
			.build();
	}

	/**
	 * Utility method to create a mock ModulePojo.
	 */
	private ModuleLightweightPojo createTestModule(final Long moduleId) {
		return new ModuleLightweightPojoDummy().prepare(m -> m
				.setName("Name" + moduleId)
				.setDescription("Description" + moduleId)
				.setNid(moduleId)
				.setPath("Path" + moduleId))
			.build();
	}

}
