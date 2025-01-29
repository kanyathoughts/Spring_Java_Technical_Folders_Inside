/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.List;
import java.util.Optional;

import innowake.mining.shared.entities.EffortSummaryPojo;
import innowake.mining.shared.entities.EffortSummaryPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.MapPropertyAccessor;
import innowake.mining.shared.model.EffortSummaryType;

/**
 * Functions for effort summary database entities.
 */
public interface EffortSummaryService {

	interface EffortSummaryOrderBuilder<B extends EffortSummaryOrderBuilder<B>> {

		/**
		 * Sorts effort summary entities by index in the given {@code sortDirection}.
		 * 
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortIndex(final SortDirection sortDirection);
	}

	interface EffortSummaryInquiryBuilder extends EffortSummaryOrderBuilder<EffortSummaryInquiryBuilder> {

		/**
		 * Filters effort summary entities by {@code project}.
		 * 
		 * @param project the id of the project to filter by
		 * @return this instance for method chaining
		 */
		EffortSummaryInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters effort summary entities by {@code EffortSummaryType}.
		 * 
		 * @param type the  {@code EffortSummaryType} to filter by
		 * @return this instance for method chaining
		 */
		EffortSummaryInquiryBuilder byType(EffortSummaryType type); 

		/**
		 * Filters effort summary entities by {@code index}.
		 * 
		 * @param index the  {@code index} to filter by
		 * @return this instance for method chaining
		 */
		EffortSummaryInquiryBuilder byIndex(Long index); 
	}

	/**
	 * Creates a new {@code effort_summary} entity.
	 *
	 * @param effortSummary the {@linkplain EffortSummaryPojoPrototype} to create.
	 * @return the index of the created entity.
	 */
	long create(EffortSummaryPojoPrototype effortSummary);

	/**
	 * Updates the {@code effort_summary} entity. This update method allows you only to change the properties of the entity.
	 *
	 * @param effortSummary the {@linkplain EffortSummaryPojoPrototype} to create.
	 */
	void update(EffortSummaryPojoPrototype effortSummary);

	/**
	 * Creates a new {@code effort_summary} entity for each {@linkplain EffortSummaryPojoPrototype} in {@code effortSummaries}.
	 *
	 * @param effortSummaries the {@linkplain EffortSummaryPojoPrototype EffortSummaryPojoPrototypes} to create.
	 * @return number of created entities.
	 */
	int create(List<EffortSummaryPojoPrototype> effortSummaries);

	/**
	 * Returns the first {@code effort_summary} entitiy that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain EffortSummaryInquiryBuilder} containing the filter criteria and sort options.
	 * @return the {@linkplain EffortSummaryPojo}
	 */
	Optional<EffortSummaryPojo> findAny(BuildingConsumer<EffortSummaryInquiryBuilder> builder);

	/**
	 * Returns all {@code effort_summary} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain EffortSummaryInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain EffortSummaryPojo EffortSummaryPojos}
	 */
	List<EffortSummaryPojo> find(BuildingConsumer<EffortSummaryInquiryBuilder> builder);

	/**
	 * Returns paged subset of {@code effort_summary} entities that match with the filters in the given {@code builder}.
	 *
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain EffortSummaryInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching {@code effort_summary} entities.
	 */
	Paged<EffortSummaryPojo> find(Pagination paging, BuildingConsumer<EffortSummaryInquiryBuilder> builder);

	/**
	 * Deletes all {@code effort_summary} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain EffortSummaryInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code effort_summary} entities
	 */
	int delete(BuildingConsumer<EffortSummaryInquiryBuilder> builder);

	public static class TypeSummaryProperties {

		private TypeSummaryProperties() { }

		public static final MapPropertyAccessor<String> TECHNOLOGY = new MapPropertyAccessor<>("technology");
		public static final MapPropertyAccessor<String> TECHNOLOGY_TYPE = new MapPropertyAccessor<>("type");
		public static final MapPropertyAccessor<Integer> COUNT = new MapPropertyAccessor<>("count");
		public static final MapPropertyAccessor<Integer> LINES_OF_CODE = new MapPropertyAccessor<>("loc");
		public static final MapPropertyAccessor<Integer> LINES_OF_COMMENT = new MapPropertyAccessor<>("locComment");
		public static final MapPropertyAccessor<Integer> ERROR_COUNT = new MapPropertyAccessor<>("errorCount");
		public static final MapPropertyAccessor<Integer> EASY_COUNT = new MapPropertyAccessor<>("easyCount");
		public static final MapPropertyAccessor<Integer> COMPLEX_COUNT = new MapPropertyAccessor<>("complexCount");
		public static final MapPropertyAccessor<Integer> VERY_COMPLEX_COUNT = new MapPropertyAccessor<>("veryComplexCount");
		public static final MapPropertyAccessor<Integer> UNMAINTAINABLE_COUNT = new MapPropertyAccessor<>("unmaintainableCount");
	}

	public static class PricingSummaryProperties {

		private PricingSummaryProperties() { }

		public static final MapPropertyAccessor<Integer> TOTAL_SCREENS = new MapPropertyAccessor<>("totalScreens");
		public static final MapPropertyAccessor<Integer> TOTAL_ERRORS = new MapPropertyAccessor<>("totalErrors");
		public static final MapPropertyAccessor<Integer> TOTAL_MODULES_WITH_ERRORS = new MapPropertyAccessor<>("totalModulesWithErrors");
		public static final MapPropertyAccessor<Integer> TOTAL_DATA_FILES = new MapPropertyAccessor<>("totalDataFiles");
		public static final MapPropertyAccessor<Integer> TOTAL_SQL_STATEMENTS = new MapPropertyAccessor<>("totalSQLStatements");
		public static final MapPropertyAccessor<Integer> TOTAL_BATCH_EXEC_PGM_STATEMENTS = new MapPropertyAccessor<>("totalBatchExecPgmStatements");
		public static final MapPropertyAccessor<Integer> TOTAL_BATCH_EXEC_COBOL_PROGRAMS = new MapPropertyAccessor<>("totalBatchExecCobolPrograms");
		public static final MapPropertyAccessor<Integer> TOTAL_BATCH_EXEC_ASM_PROGRAMS = new MapPropertyAccessor<>("totalBatchExecAsmPrograms");
		public static final MapPropertyAccessor<Integer> TOTAL_BATCH_EXEC_UNKNOWN_MISSING_PROGRAMS = new MapPropertyAccessor<>("totalBatchExecUnknownMissingPrograms");
		public static final MapPropertyAccessor<Integer> TOTAL_MISSING_DEPENDENCIES = new MapPropertyAccessor<>("totalMissingDependencies");

	}
}
