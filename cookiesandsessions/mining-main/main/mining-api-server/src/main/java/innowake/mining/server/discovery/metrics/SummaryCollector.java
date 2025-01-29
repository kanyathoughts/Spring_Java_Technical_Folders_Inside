package innowake.mining.server.discovery.metrics;

import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_GTE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IN;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IS_FALSE;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import innowake.lib.core.lang.Assert;
import innowake.mining.shared.access.EffortSummaryService.PricingSummaryProperties;
import innowake.mining.shared.access.EffortSummaryService.TypeSummaryProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.access.Table.Row;
import innowake.mining.shared.entities.EffortSummaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.EffortSummaryType;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.aggregations.AggregationOperator;

public class SummaryCollector {

	/*
	 * MGO: following bounds for complexity categorization are based on groupings from
	 * Software Engineering Institute at Carnegie Mellon University published 1997 but no longer available online.
	 * These measures are based upon testing risk and the idea that a high complexity score means more test cases
	 * are required to exercise all program branches.
	 * These bounds were used in the original Cobol analysis script and so fit with previous diagnostic results.
	 */
	private static final int EASY_LOWER_BOUND = 0;
	private static final int EASY_UPPER_BOUND = 10;
	private static final int COMPLEX_UPPER_BOUND = 20;
	private static final int VERY_COMPLEX_UPPER_BOUND = 50;

	private static final List<Type> SCREEN_TYPES_LIST = Arrays.asList(Type.BMS_MAP, Type.MAP);
	private static final List<Type> FILE_TYPES_LIST = List.of(Type.LIB, Type.FILE, Type.VSAM_FILE);

	private final ModuleService moduleService;

	/**
	 * Constructor.
	 *
	 * @param moduleService the {@link ModuleService}
	 */
	public SummaryCollector(final ModuleService moduleService) {
		this.moduleService = moduleService;
	}

	/**
	 * Collects the type summaries for all modules of the given {@code projectId}.
	 *
	 * @param projectId the project Id
	 * @return list of {@linkplain EffortSummaryPojoPrototype EffortSummaryPojoPrototypes} with type {@code TYPE}
	 */
	public List<EffortSummaryPojoPrototype> collectModuleSummary(final EntityId projectId) {
		final Map<String, Map<String, List<Integer>>> complexitiesByTechnologyAndType = moduleService.getComplexities(q -> q.ofProject(projectId)
																															.withRepresentation(Representation.PHYSICAL));

		final var aggregations = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.LINES_OF_CODE, AggregationOperator.SUM)
																	 .aggregate(ModuleFieldName.LINES_OF_COMMENT, AggregationOperator.SUM)
																	 .aggregate(ModuleFieldName.ERRORS, AggregationOperator.SUM)
																	 .groupBy(ModuleFieldName.TECHNOLOGY)
																	 .groupBy(ModuleFieldName.TYPE)
																	 .orderBy(ModuleFieldName.TECHNOLOGY)
																	 .ofProject(OPERATOR_EQ, projectId)
																	 .withRepresentation(OPERATOR_EQ, Representation.PHYSICAL));

		final Map<String, TypeSummary> totalSumBuilders = new HashMap<>();
		final Map<String, List<TypeSummary>> sums = new LinkedHashMap<>();
		if (aggregations.isPresent()) {
			final var it = aggregations.get().iterator();
			while (it.hasNext()) {
				final var row = it.next();

				final String technology = getValue(row, ModuleFieldName.TECHNOLOGY);
				final String type = getValue(row, ModuleFieldName.TYPE);

				final TypeSummary totalSumBuilder = totalSumBuilders.computeIfAbsent(technology, k -> new TypeSummary()
																														.setTechnology(technology)
																														.setType("Total"));

				final Map<String, List<Integer>> typeToComplexities = Assert.assertNotNull(complexitiesByTechnologyAndType.get(technology));
				final List<Integer> complexities = Assert.assertNotNull(typeToComplexities.get(type));
				final TypeSummary sumBuilder = new TypeSummary()
						.setTechnology(technology)
						.setType(type);
				sumBuilder.count = complexities.size();
				sumBuilder.linesOfCode = getValue(row, ModuleFieldName.LINES_OF_CODE, 0l).intValue();
				sumBuilder.linesOfComments = getValue(row, ModuleFieldName.LINES_OF_COMMENT, 0l).intValue();
				sumBuilder.errorCount = getValue(row, ModuleFieldName.ERRORS, BigDecimal.ZERO).intValue();

				for (final Integer complexity : complexities) {
					final int cmplxty = complexity.intValue();
					sumBuilder.easyCount = sumBuilder.easyCount + (cmplxty > EASY_LOWER_BOUND && cmplxty <= EASY_UPPER_BOUND ? 1 : 0);
					sumBuilder.complexCount = sumBuilder.complexCount + (cmplxty > EASY_UPPER_BOUND && cmplxty <= COMPLEX_UPPER_BOUND ? 1 : 0);
					sumBuilder.veryComplexCount = sumBuilder.veryComplexCount + (cmplxty > COMPLEX_UPPER_BOUND && cmplxty <= VERY_COMPLEX_UPPER_BOUND ? 1 : 0);
					sumBuilder.unmaintainableCount = sumBuilder.unmaintainableCount + (cmplxty > VERY_COMPLEX_UPPER_BOUND ? 1 : 0);
				}

				sums.computeIfAbsent(technology, k -> new ArrayList<>()).add(sumBuilder);

				totalSumBuilder.count = totalSumBuilder.count + sumBuilder.count;
				totalSumBuilder.linesOfCode = totalSumBuilder.linesOfCode + sumBuilder.linesOfCode;
				totalSumBuilder.linesOfComments = totalSumBuilder.linesOfComments + sumBuilder.linesOfComments;
				totalSumBuilder.errorCount = totalSumBuilder.errorCount + sumBuilder.errorCount;
				totalSumBuilder.easyCount = totalSumBuilder.easyCount + sumBuilder.easyCount;
				totalSumBuilder.complexCount = totalSumBuilder.complexCount + sumBuilder.complexCount;
				totalSumBuilder.veryComplexCount = totalSumBuilder.veryComplexCount + sumBuilder.veryComplexCount;
				totalSumBuilder.unmaintainableCount = totalSumBuilder.unmaintainableCount + sumBuilder.unmaintainableCount;
			}
			
		}

		final List<EffortSummaryPojoPrototype> result = new ArrayList<>(sums.size() + totalSumBuilders.size());
		for (final var sumEntries : sums.entrySet()) {
			for (final var summary : sumEntries.getValue()) {
				result.add(new EffortSummaryPojoPrototype()
								.setProject(projectId)
								.setType(EffortSummaryType.TYPE)
								.setProperties(summary.toProperties()));
			}
			final var totalTypeSummary = Objects.requireNonNull(totalSumBuilders.get(sumEntries.getKey()),
																		"Total type summary must be present for technology: " + sumEntries.getKey());
			result.add(new EffortSummaryPojoPrototype()
								.setProject(projectId)
								.setType(EffortSummaryType.TYPE)
								.setProperties(totalTypeSummary.toProperties()));
		}

		return result;
	}

	/**
	 * Collects the pricing summaries for the given {@code projectId}.
	 *
	 * @param projectId the project Id
	 * @return list of {@linkplain EffortSummaryPojoPrototype EffortSummaryPojoPrototypes} with type {@code PRICING}
	 */
	public List<EffortSummaryPojoPrototype> collectPricingData(final EntityId projectId) {
		/* Collect total SQL statements */
		final var sqlStatements = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.SQL_STATEMENTS, AggregationOperator.SUM)
																		.ofProject(OPERATOR_EQ, projectId));
		final int totalSqlStatements = getValue(sqlStatements, ModuleFieldName.SQL_STATEMENTS, BigDecimal.ZERO).intValue();

		/* Collect total screens */
		final var screens = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.UID, AggregationOperator.COUNT)
																.ofProject(OPERATOR_EQ, projectId)
																.withType(OPERATOR_IN, SCREEN_TYPES_LIST));
		final int totalScreens = getValue(screens, ModuleFieldName.UID, 0l).intValue();

		/* Collect total resource files */
		final var resources = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.UID, AggregationOperator.COUNT)
																	.ofProject(OPERATOR_EQ, projectId)
																	.withRepresentation(OPERATOR_EQ, Representation.VIRTUAL)
																	.withType(OPERATOR_IN, FILE_TYPES_LIST));
		final int totalResourceFiles = getValue(resources, ModuleFieldName.UID, 0l).intValue();

		/* Collect total errors and amount of modules containing errors */
		final var errors = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.UID, AggregationOperator.COUNT)
																.aggregate(ModuleFieldName.ERRORS, AggregationOperator.SUM)
																.ofProject(OPERATOR_EQ, projectId)
																.withErrors(OPERATOR_GTE, Integer.valueOf(1)));
		final int totalErrors = getValue(errors, ModuleFieldName.ERRORS, BigDecimal.ZERO).intValue();
		final int totalModulesWithErrors = getValue(errors, ModuleFieldName.UID, 0l).intValue();

		/* Collect total JCL EXEC PGM occurrences */
		final int totalExecPgms = (int) moduleService.countRelationships(q -> q.ofProject(projectId)
																		.withType(RelationshipType.CALLS)
																		.withSourceType(Type.EXEC_PGM));

		/* Collect total distinct cobol programs called via JCL EXEC PGM */
		final var calledCobolPrograms = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.NAME, AggregationOperator.COUNT)
																			.distinct()
																			.ofProject(OPERATOR_EQ, projectId)
																			.withTechnology(OPERATOR_EQ, ModuleType.COBOL_PROGRAM.getTechnology())
																			.withType(OPERATOR_EQ, ModuleType.COBOL_PROGRAM.getType())
																			.withSourceRelationshipsFrom(b -> b.ofProject(projectId).withType(Type.EXEC_PGM),
																										RelationshipType.CALLS));
		final int execCobolCount = getValue(calledCobolPrograms, ModuleFieldName.NAME, 0l).intValue();

		/* Collect total distinct assembler programs called via JCL EXEC PGM */
		final var calledAssemplerPrograms = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.NAME, AggregationOperator.COUNT)
																				.distinct()
																				.ofProject(OPERATOR_EQ, projectId)
																				.withTechnology(OPERATOR_EQ, ModuleType.ASSEMBLER_PROGRAM.getTechnology())
																				.withType(OPERATOR_EQ, ModuleType.ASSEMBLER_PROGRAM.getType())
																				.withSourceRelationshipsFrom(b -> b.ofProject(projectId).withType(Type.EXEC_PGM),
																											RelationshipType.CALLS));
		final int execAsmCount = getValue(calledAssemplerPrograms, ModuleFieldName.NAME, 0l).intValue();

		/* Collect total distinct missing modules called via JCL EXEC PGM */
		final Optional<Table> missingExcePgms = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.NAME, AggregationOperator.COUNT)
																					.distinct()
																					.ofProject(OPERATOR_EQ, projectId)
																					.withIdentification(OPERATOR_IS_FALSE, false)
																					.withSourceRelationshipsFrom(b -> b.ofProject(projectId)
																													 .withTechnology(Technology.JCL)
																													 .withType(Type.EXEC_PGM),
																												RelationshipType.CALLS));
		final int missingExecCount = getValue(missingExcePgms, ModuleFieldName.NAME, 0l).intValue();

		/* Collect total distinct missing modules */
		final var missingModules = moduleService.getAggregations(q -> q.aggregate(ModuleFieldName.NAME, AggregationOperator.COUNT)
																		.distinct()
																		.ofProject(OPERATOR_EQ, projectId)
																		.withIdentification(OPERATOR_IS_FALSE, false));
		final int missingDependencies = getValue(missingModules, ModuleFieldName.NAME, 0l).intValue();

		final Map<String, Object> pricingSummary = new HashMap<>();
		PricingSummaryProperties.TOTAL_SCREENS.setIn(pricingSummary, totalScreens);
		PricingSummaryProperties.TOTAL_ERRORS.setIn(pricingSummary, totalErrors);
		PricingSummaryProperties.TOTAL_MODULES_WITH_ERRORS.setIn(pricingSummary, totalModulesWithErrors);
		PricingSummaryProperties.TOTAL_DATA_FILES.setIn(pricingSummary, totalResourceFiles);
		PricingSummaryProperties.TOTAL_SQL_STATEMENTS.setIn(pricingSummary, totalSqlStatements);
		PricingSummaryProperties.TOTAL_BATCH_EXEC_PGM_STATEMENTS.setIn(pricingSummary, totalExecPgms);
		PricingSummaryProperties.TOTAL_MISSING_DEPENDENCIES.setIn(pricingSummary, missingDependencies);
		PricingSummaryProperties.TOTAL_BATCH_EXEC_ASM_PROGRAMS.setIn(pricingSummary, execAsmCount);
		PricingSummaryProperties.TOTAL_BATCH_EXEC_COBOL_PROGRAMS.setIn(pricingSummary, execCobolCount);
		PricingSummaryProperties.TOTAL_BATCH_EXEC_UNKNOWN_MISSING_PROGRAMS.setIn(pricingSummary, missingExecCount);

		return List.of(new EffortSummaryPojoPrototype()
							.setProject(projectId)
							.setType(EffortSummaryType.PRICING)
							.setProperties(pricingSummary));
	}

	@SuppressWarnings("unchecked")
	private static <T> T getValue(final Row row, final ModuleFieldName field) {
		return (T) Objects.requireNonNull(row.get(field.name().toLowerCase()));
	}

	@SuppressWarnings("unchecked")
	private static <T> T getValue(final Row row, final ModuleFieldName field, T defaultValue) {
		final var value = row.get(field.name().toLowerCase());
		return value != null ? (T) value : defaultValue;
	}

	@SuppressWarnings("unchecked")
	private static <T> T getValue(final Optional<Table> table, final ModuleFieldName field, T defaultValue) {
		return (T) table.map(data -> data.isEmpty() ? null : data.get(0))
						.map(row -> row.get(field.name().toLowerCase()))
						.orElse(defaultValue);
	}

	private static class TypeSummary {

		private final Map<String, Object> properties = new HashMap<>();
		private int count;
		private int linesOfCode;
		private int linesOfComments;
		private int errorCount;
		private int easyCount;
		private int complexCount;
		private int veryComplexCount;
		private int unmaintainableCount;

		private TypeSummary setTechnology(final String technology) {
			TypeSummaryProperties.TECHNOLOGY.setIn(properties, technology);
			return this;
		}

		private TypeSummary setType(final String type) {
			TypeSummaryProperties.TECHNOLOGY_TYPE.setIn(properties, type);
			return this;
		}

		private Map<String, Object> toProperties() {
			TypeSummaryProperties.COUNT.setIn(properties, count);
			TypeSummaryProperties.LINES_OF_CODE.setIn(properties, linesOfCode);
			TypeSummaryProperties.LINES_OF_COMMENT.setIn(properties, linesOfComments);
			TypeSummaryProperties.ERROR_COUNT.setIn(properties, errorCount);
			TypeSummaryProperties.EASY_COUNT.setIn(properties, easyCount);
			TypeSummaryProperties.COMPLEX_COUNT.setIn(properties, complexCount);
			TypeSummaryProperties.VERY_COMPLEX_COUNT.setIn(properties, veryComplexCount);
			TypeSummaryProperties.UNMAINTAINABLE_COUNT.setIn(properties, unmaintainableCount);
			return properties;
		}
	}
}
