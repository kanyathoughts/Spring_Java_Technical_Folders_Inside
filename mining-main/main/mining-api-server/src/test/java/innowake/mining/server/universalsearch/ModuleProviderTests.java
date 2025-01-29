/* Copyright (c) 2023 Deloitte. All rights reserved. */
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

import innowake.mining.server.universalsearch.provider.ModulesUniversalSearchProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.universalsearch.UniversalSearchLink.Type;
import innowake.mining.shared.universalsearch.UniversalSearchResult;

/**
 * Unit tests for {@link ModulesUniversalSearchProvider}
 */
class ModuleProviderTests {

	@InjectMocks
	private ModulesUniversalSearchProvider moduleSearchProvider;

	@Mock
	private ModuleService moduleService;

	@BeforeEach
	public void setUp() {
		MockitoAnnotations.openMocks(this);
	}

	@Test
	void testMatchingModules() {
		/* Create some mock data for testing */
		final String query = "Module";
		final var module1 = createTestModule("Module1", "Path1", "Description1", 1L);
		final var module2 = createTestModule("Module2", "Path2", "Description2", 2L);

		/* Create a mock Page */
		final Paged<ModulePojo> page = new Paged<>(Arrays.asList(module1, module2), 10, 0L, 2L, 1L, 2L) { };

		/* Mock the behavior of moduleService.findAll */
		when(moduleService.findModules(any(), any())).thenReturn(page);

		final List<UniversalSearchResult> results = moduleSearchProvider.query(EntityId.of(1L), query);
		verify(moduleService, times(1)).findModules(any(), any());
		assertEquals(2, results.size());
		final UniversalSearchResult result1 = results.get(0);
		assertEquals("module-name-path", result1.getProvidedBy());
		assertEquals(0, result1.getRank());
		assertEquals("Module", result1.getType());
		assertEquals("Module1", result1.getTitle());
		assertEquals("Path1", result1.getSubTitle());
		assertEquals("Description1", result1.getContext());
		assertEquals(1, result1.getLinks().size());
		assertEquals(Type.MODULE_DETAILS, result1.getLinks().get(0).getType());
		assertEquals("1", result1.getLinks().get(0).getProperties().get("moduleId"));

		final UniversalSearchResult result2 = results.get(1);
		assertEquals("module-name-path", result1.getProvidedBy());
		assertEquals(0, result1.getRank());
		assertEquals("Module", result2.getType());
		assertEquals("Module2", result2.getTitle());
		assertEquals("Path2", result2.getSubTitle());
		assertEquals("Description2", result2.getContext());
		assertEquals(1, result2.getLinks().size());
		assertEquals(Type.MODULE_DETAILS, result1.getLinks().get(0).getType());
		assertEquals("2", result2.getLinks().get(0).getProperties().get("moduleId"));
	}
	
	@Test
	void testNoMatchingModules() {
		final String query = "f42130b3f114a7a8c1c8c2f2bbf984d20e5a035bf56516047b9f38d1a2426fde";
		/* Mock the behavior of moduleService.findAll */
		when(moduleService.findModules(any(), any())).thenReturn(
				new Paged<ModulePojo>(Collections.emptyList(), 10, 0L, 0L, null, null) { });
		
		final List<UniversalSearchResult> results = moduleSearchProvider.query(EntityId.of(1L), query);
		verify(moduleService, times(1)).findModules(any(), any());
		assertTrue(results.isEmpty());
	}

	/**
	 * Utility method to create a mock Module.
	 */
	private ModulePojo createTestModule(final String name, final String path, final String description, final Long id) {
		return new ModulePojoDummy().prepare(m -> m
				.setName(name)
				.setPath(path)
				.setDescription(description)
				.setNid(id)
			).build();
	}

}
