/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.export.model;

import static innowake.mining.shared.model.EffortSummaryType.PRICING;
import static innowake.mining.shared.model.EffortSummaryType.TYPE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.lang.Nullable;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EffortSummaryService;
import innowake.mining.shared.access.EffortSummaryService.PricingSummaryProperties;
import innowake.mining.shared.access.EffortSummaryService.TypeSummaryProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.EffortSummaryPojo;
import innowake.mining.shared.entities.EffortSummaryPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests if {@link DatabaseRelatedTest} for CRUD operations on {@link EffortSummary}.
 */
class EffortSummaryDbTest extends DatabaseResettingTest {

	private final Long ONE = Long.valueOf(1);
	private final int DEFAULT_VALUE = 1;
	@Autowired
	private EffortSummaryService effortSummaryService;

	/**
	 * Test case to check the project and effort summary creation.
	 * Effort summary requires the typeSummary and pricingSummary which have been added in these methods.
	 */
	@Test
	void createEffortSummary() {
		final var projectId = createProject().identity();
		final List<EffortSummaryPojo> testData = createEffortSummaries(projectId, false);

		for (final var effortSummary : testData) {
			assertEquals(projectId, effortSummary.getProject(), "Project ID must match");
			assertNotNull(effortSummary.getIndex(), "Index must not be null");
		}

		final var summaries = testData.stream().collect(Collectors.partitioningBy(es -> es.getType() == PRICING, Collectors.toList()));
		final var pricingSummaries = summaries.get(Boolean.TRUE);
		assertEquals(2, pricingSummaries.size(), "Number of loaded pricing summaries must be 2");
		final var typeSummaries = summaries.get(Boolean.FALSE);
		assertEquals(2, typeSummaries.size(), "Number of loaded type summaries must be 2");

		List<Long> indexes = pricingSummaries.stream()
												.map(p -> p.getIndex())
												.sorted()
												.collect(Collectors.toList());
		assertEquals(1, indexes.get(0), "Pricing summary index must be 1");
		assertEquals(2, indexes.get(1), "Pricing summary index must be 2");

		indexes = typeSummaries.stream()
				.map(p -> p.getIndex())
				.sorted()
				.collect(Collectors.toList());
		assertEquals(1, indexes.get(0), "Type summary index must be 1");
		assertEquals(2, indexes.get(1), "Type summary index must be 2");

		assertionOnTypeSummary(typeSummaries.stream()
											.map(es -> new TypeSummary(es.getProperties()))
											.collect(Collectors.toList()));
		assertionOnPricingSummary(pricingSummaries.stream()
													.map(es -> new PricingSummary(es.getProperties()))
													.collect(Collectors.toList()));
	}

	/**
	 * Test case to check the project and effort summary creation.
	 * Effort summary requires the typeSummary and pricingSummary which have been added in these methods.
	 */
	@Test
	void updateEffortSummary() {
		final var projectId = createProject().identity();
		final List<EffortSummaryPojo> testData = createEffortSummaries(projectId, false);

		for (final var effortSummary : testData) {
			assertEquals(projectId, effortSummary.getProject(), "Project ID must match");
			assertNotNull(effortSummary.getIndex(), "Index must not be null");
		}

		final var summaries = testData.stream().collect(Collectors.partitioningBy(es -> es.getType() == PRICING, Collectors.toList()));
		final var typeSummaries = summaries.get(Boolean.FALSE);
		assertEquals(2, typeSummaries.size(), "Number of loaded type summaries must be 2");

		final Map<String, Object> properties = new HashMap<>();
		TypeSummaryProperties.LINES_OF_CODE.setIn(properties, 101);
		effortSummaryService.update(new EffortSummaryPojoPrototype()
											.setProject(typeSummaries.get(0).getProject())
											.setIndex(typeSummaries.get(0).getIndex())
											.setType(typeSummaries.get(0).getType())
											.setProperties(properties));

		final var modified = effortSummaryService.findAny(q -> q.ofProject(projectId)
																.byType(TYPE)
																.byIndex(typeSummaries.get(0).getIndex()));
		assertTrue(modified.isPresent(), "Modified effort summary must exist");
		final TypeSummary typeProperties2 = new TypeSummary(modified.get().getProperties());
		assertEquals(101l, typeProperties2.getLinesOfCode(), "Lines of code must have been updated");
	}

	/**
	 * Test case to check the project and effort summary deletion.
	 * Effort summary requires the typeSummary and pricingSummary which have been added in these methods.
	 */
	@Test
	void deleteEffortSummary() {
		final var projectId = createProject().identity();
		final List<EffortSummaryPojo> testData = createEffortSummaries(projectId, true);

		for (final var effortSummary : testData) {
			assertEquals(projectId, effortSummary.getProject(), "Project ID must match");
			assertNotNull(effortSummary.getIndex(), "Index must not be null");
		}

		assertEquals(4, effortSummaryService.delete(q -> q.ofProject(projectId)), "Four effort summaries must have been deleted");
		final var loaded = effortSummaryService.find(q -> q.ofProject(projectId));
		assertTrue(loaded.isEmpty(), "No effort summaries must exist after deletion");
	}

	private void assertionOnPricingSummary(final List<PricingSummary> pricingSummariesList) {
		final PricingSummary pricing_summary_1 = pricingSummariesList.get(0);
		assertEquals(DEFAULT_VALUE, pricing_summary_1.getTotalBatchExecAsmPrograms());
		assertEquals(DEFAULT_VALUE, pricing_summary_1.getTotalBatchExecCobolPrograms());
		assertEquals(0l, pricing_summary_1.getTotalMissingDependencies());
		assertEquals(DEFAULT_VALUE, pricing_summary_1.getTotalDataFiles());
		assertEquals(DEFAULT_VALUE, pricing_summary_1.getTotalErrors());
		assertEquals(DEFAULT_VALUE, pricing_summary_1.getTotalModulesWithErrors());
		assertEquals(DEFAULT_VALUE, pricing_summary_1.getTotalBatchExecPgmStatements());
		assertEquals(DEFAULT_VALUE, pricing_summary_1.getTotalScreens());
		assertEquals(DEFAULT_VALUE, pricing_summary_1.getTotalSQLStatements());

		final PricingSummary pricing_summary_2 = pricingSummariesList.get(1);
		assertEquals(DEFAULT_VALUE, pricing_summary_2.getTotalBatchExecAsmPrograms());
		assertEquals(DEFAULT_VALUE, pricing_summary_2.getTotalBatchExecCobolPrograms());
		assertEquals(1l, pricing_summary_2.getTotalMissingDependencies());
		assertEquals(DEFAULT_VALUE, pricing_summary_2.getTotalDataFiles());
		assertEquals(DEFAULT_VALUE, pricing_summary_2.getTotalErrors());
		assertEquals(DEFAULT_VALUE, pricing_summary_2.getTotalModulesWithErrors());
		assertEquals(DEFAULT_VALUE, pricing_summary_2.getTotalBatchExecPgmStatements());
		assertEquals(DEFAULT_VALUE, pricing_summary_2.getTotalScreens());
		assertEquals(DEFAULT_VALUE, pricing_summary_2.getTotalSQLStatements());
	}

	private void assertionOnTypeSummary(final List<TypeSummary> typeSummariesList) {
		final var type_summary_1 = typeSummariesList.get(0);
		assertEquals(Technology.COBOL.name(), type_summary_1.getTechnology());
		assertEquals(Type.PROGRAM.name(), type_summary_1.getType());
		assertEquals(3l, type_summary_1.getCount());
		assertEquals(DEFAULT_VALUE, type_summary_1.getComplexCount());
		assertEquals(DEFAULT_VALUE, type_summary_1.getEasyCount());
		assertEquals(0l, type_summary_1.getErrorCount());
		assertEquals(0l, type_summary_1.getLinesOfCode());
		assertEquals(0l, type_summary_1.getLinesOfComment());
		assertEquals(DEFAULT_VALUE, type_summary_1.getUnmaintainableCount());
		assertEquals(DEFAULT_VALUE, type_summary_1.getVeryComplexCount());

		final var type_summary_2 = typeSummariesList.get(1);
		assertEquals(Technology.COBOL.name(), type_summary_2.getTechnology());
		assertEquals(Type.PROGRAM.name(), type_summary_2.getType());
		assertEquals(4l, type_summary_2.getCount());
		assertEquals(DEFAULT_VALUE, type_summary_2.getComplexCount());
		assertEquals(DEFAULT_VALUE, type_summary_2.getEasyCount());
		assertEquals(1l, type_summary_2.getErrorCount());
		assertEquals(1l, type_summary_2.getLinesOfCode());
		assertEquals(1l, type_summary_2.getLinesOfComment());
		assertEquals(DEFAULT_VALUE, type_summary_2.getUnmaintainableCount());
		assertEquals(DEFAULT_VALUE, type_summary_2.getVeryComplexCount());
	}

	private List<EffortSummaryPojo> createEffortSummaries(final EntityId projectId, final boolean batchInsert) {
		final Map<String, Object> typeSummary_1 = new HashMap<>();
		TypeSummaryProperties.TECHNOLOGY.setIn(typeSummary_1, Technology.COBOL.name());
		TypeSummaryProperties.TECHNOLOGY_TYPE.setIn(typeSummary_1, Type.PROGRAM.name());
		TypeSummaryProperties.COUNT.setIn(typeSummary_1, 3);
		TypeSummaryProperties.LINES_OF_CODE.setIn(typeSummary_1, 0);
		TypeSummaryProperties.LINES_OF_COMMENT.setIn(typeSummary_1, 0);
		TypeSummaryProperties.ERROR_COUNT.setIn(typeSummary_1, 0);
		TypeSummaryProperties.EASY_COUNT.setIn(typeSummary_1, DEFAULT_VALUE);
		TypeSummaryProperties.COMPLEX_COUNT.setIn(typeSummary_1, DEFAULT_VALUE);
		TypeSummaryProperties.VERY_COMPLEX_COUNT.setIn(typeSummary_1, DEFAULT_VALUE);
		TypeSummaryProperties.UNMAINTAINABLE_COUNT.setIn(typeSummary_1, DEFAULT_VALUE);

		final Map<String, Object> typeSummary_2 = new HashMap<>();
		TypeSummaryProperties.TECHNOLOGY.setIn(typeSummary_2, Technology.COBOL.name());
		TypeSummaryProperties.TECHNOLOGY_TYPE.setIn(typeSummary_2, Type.PROGRAM.name());
		TypeSummaryProperties.COUNT.setIn(typeSummary_2, 4);
		TypeSummaryProperties.COMPLEX_COUNT.setIn(typeSummary_2, DEFAULT_VALUE);
		TypeSummaryProperties.EASY_COUNT.setIn(typeSummary_2, DEFAULT_VALUE);
		TypeSummaryProperties.ERROR_COUNT.setIn(typeSummary_2, 1);
		TypeSummaryProperties.LINES_OF_CODE.setIn(typeSummary_2, 1);
		TypeSummaryProperties.LINES_OF_COMMENT.setIn(typeSummary_2, 1);
		TypeSummaryProperties.UNMAINTAINABLE_COUNT.setIn(typeSummary_2, DEFAULT_VALUE);
		TypeSummaryProperties.VERY_COMPLEX_COUNT.setIn(typeSummary_2, DEFAULT_VALUE);
		
		final Map<String, Object> pricingSummary_1 = new HashMap<>();
		PricingSummaryProperties.TOTAL_BATCH_EXEC_ASM_PROGRAMS.setIn(pricingSummary_1, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_BATCH_EXEC_COBOL_PROGRAMS.setIn(pricingSummary_1, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_MISSING_DEPENDENCIES.setIn(pricingSummary_1, 0);
		PricingSummaryProperties.TOTAL_DATA_FILES.setIn(pricingSummary_1, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_ERRORS.setIn(pricingSummary_1, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_MODULES_WITH_ERRORS.setIn(pricingSummary_1, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_BATCH_EXEC_PGM_STATEMENTS.setIn(pricingSummary_1, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_SCREENS.setIn(pricingSummary_1, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_SQL_STATEMENTS.setIn(pricingSummary_1, DEFAULT_VALUE);
		
		final Map<String, Object> pricingSummary_2 = new HashMap<>();
		PricingSummaryProperties.TOTAL_BATCH_EXEC_ASM_PROGRAMS.setIn(pricingSummary_2, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_BATCH_EXEC_COBOL_PROGRAMS.setIn(pricingSummary_2, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_MISSING_DEPENDENCIES.setIn(pricingSummary_2, 1);
		PricingSummaryProperties.TOTAL_DATA_FILES.setIn(pricingSummary_2, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_ERRORS.setIn(pricingSummary_2, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_MODULES_WITH_ERRORS.setIn(pricingSummary_2, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_BATCH_EXEC_PGM_STATEMENTS.setIn(pricingSummary_2, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_SCREENS.setIn(pricingSummary_2, DEFAULT_VALUE);
		PricingSummaryProperties.TOTAL_SQL_STATEMENTS.setIn(pricingSummary_2, DEFAULT_VALUE);

		final List<EffortSummaryPojoPrototype> effortSummaryList = new ArrayList<>();
		effortSummaryList.add(new EffortSummaryPojoPrototype()
									.setProject(projectId)
									.setType(TYPE)
									.setProperties(typeSummary_1));
		effortSummaryList.add(new EffortSummaryPojoPrototype()
									.setProject(projectId)
									.setType(TYPE)
									.setProperties(typeSummary_2));
		effortSummaryList.add(new EffortSummaryPojoPrototype()
									.setProject(projectId)
									.setType(PRICING)
									.setProperties(pricingSummary_1));
		effortSummaryList.add(new EffortSummaryPojoPrototype()
									.setProject(projectId)
									.setType(PRICING)
									.setProperties(pricingSummary_2));

		if (batchInsert) {
			assertEquals(4, effortSummaryService.create(effortSummaryList), "Four effort summaries must have been created");
		} else {
			for (final var effortSummary : effortSummaryList) {
				effortSummaryService.create(effortSummary);
			}
		}

		final var loaded = effortSummaryService.find(q -> q.ofProject(projectId));
		assertEquals(4, loaded.size(), "Four effort summaries must have been loaded");

		return loaded;
	}
	
	private ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("TEST PROJECT 1")
				.setClient(EntityId.of(ONE))
				.setNatures(Collections.emptySet()));
	}

	private static class PricingSummary {

		private final Map<String, Object> properties;
		
		private PricingSummary(final Map<String, Object> properties) {
			this.properties = properties;
		}

		private long getTotalScreens() {
			return toLong(PricingSummaryProperties.TOTAL_SCREENS.getFrom(properties));
		}

		
		private long getTotalErrors() {
			return toLong(PricingSummaryProperties.TOTAL_ERRORS.getFrom(properties));
		}

		private long getTotalModulesWithErrors() {
			return toLong(PricingSummaryProperties.TOTAL_MODULES_WITH_ERRORS.getFrom(properties));
		}

		
		private long getTotalDataFiles() {
			return toLong(PricingSummaryProperties.TOTAL_DATA_FILES.getFrom(properties));
		}

		
		private long getTotalSQLStatements() {
			return toLong(PricingSummaryProperties.TOTAL_SQL_STATEMENTS.getFrom(properties));
		}

		
		private long getTotalBatchExecPgmStatements() {
			return toLong(PricingSummaryProperties.TOTAL_BATCH_EXEC_PGM_STATEMENTS.getFrom(properties));
		}

		
		private long getTotalBatchExecCobolPrograms() {
			return toLong(PricingSummaryProperties.TOTAL_BATCH_EXEC_COBOL_PROGRAMS.getFrom(properties));
		}

		
		private long getTotalBatchExecAsmPrograms() {
			return toLong(PricingSummaryProperties.TOTAL_BATCH_EXEC_ASM_PROGRAMS.getFrom(properties));
		}

		private long getTotalMissingDependencies() {
			return toLong(PricingSummaryProperties.TOTAL_MISSING_DEPENDENCIES.getFrom(properties));
		}
	}

	private class TypeSummary {

		private final Map<String, Object> properties;

		private TypeSummary(final Map<String, Object> properties) {
			this.properties = properties;
		}

		private String getTechnology() {
			return Optional.ofNullable(TypeSummaryProperties.TECHNOLOGY.getFrom(properties)).orElse("");
		}
		
		/**
		 * @return The type. 
		 */
		private String getType() {
			return Optional.ofNullable(TypeSummaryProperties.TECHNOLOGY_TYPE.getFrom(properties)).orElse("");
		}
		
		/**
		 * @return The number of occurrences of this technology/sub-type. 
		 */
		private long getCount() {
			return toLong(TypeSummaryProperties.COUNT.getFrom(properties));
		}
		
		/**
		 * @return The total lines of code for this technology/sub-type.
		 */
		private long getLinesOfCode() {
			return toLong(TypeSummaryProperties.LINES_OF_CODE.getFrom(properties));
		}
		
		/**
		 * @return The total lines of code comments for this technology/sub-type.
		 */
		private long getLinesOfComment() {
			return toLong(TypeSummaryProperties.LINES_OF_COMMENT.getFrom(properties));
		}
		
		/**
		 * @return The total error count for all modules of this technology/sub-type.
		 */
		private long getErrorCount() {
			return toLong(TypeSummaryProperties.ERROR_COUNT.getFrom(properties));
		}

		/**
		 * @return The number of modules in this technology/sub-type that have a complexity within the 'easy' range.
		 */
		private long getEasyCount() {
			return toLong(TypeSummaryProperties.EASY_COUNT.getFrom(properties));
		}

		/**
		 * @return The number of modules in this technology/sub-type that have a complexity within the 'complex' range.
		 */
		private long getComplexCount() {
			return toLong(TypeSummaryProperties.COMPLEX_COUNT.getFrom(properties));
		}

		private long getVeryComplexCount() {
			return toLong(TypeSummaryProperties.VERY_COMPLEX_COUNT.getFrom(properties));
		}
		
		private long getUnmaintainableCount() {
			return toLong(TypeSummaryProperties.UNMAINTAINABLE_COUNT.getFrom(properties));

		}
	}
	
	private static Long toLong(@Nullable final Object value) {
		if (value == null) {
			return Long.valueOf(0);
		}

		if (value instanceof Long) {
			return (Long) value;
		}

		return Long.valueOf(value.toString());
	}
}
