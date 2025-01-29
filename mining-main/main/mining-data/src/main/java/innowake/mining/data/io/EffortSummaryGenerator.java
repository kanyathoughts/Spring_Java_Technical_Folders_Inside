/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.shared.access.EffortSummaryService;
import innowake.mining.shared.access.EffortSummaryService.PricingSummaryProperties;
import innowake.mining.shared.access.EffortSummaryService.TypeSummaryProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.EffortSummaryPojo;
import innowake.mining.shared.model.EffortSummaryType;

/**
 * Creates the effort summary sheets for the Effort Summary workbook.
 */
abstract class EffortSummaryGenerator implements RowCreator {

	@Autowired
	private EffortSummaryService effortSummaryService;

	/**
	 * Processes all {@link EffortSummaryPojo} of the given type {@code type} for a Effort Summary workbook.
	 *
	 * @param projectId the project ID
	 * @param type the {@link EffortSummaryType}
	 * @throws IOException if an error occurs
	 */
	public void process(final EntityId projectId, final EffortSummaryType type) throws IOException {
		final var effortSummaries = effortSummaryService.find(q -> q.ofProject(projectId)
																	.byType(type)
																	.sortIndex(SortDirection.ASCENDING));
		switch (type) {
			case PRICING:
				for (final var effortSummary : effortSummaries) {
					final var properties = effortSummary.getProperties();
					createRow(PricingSummaryProperties.TOTAL_SCREENS.getFrom(properties),
							PricingSummaryProperties.TOTAL_ERRORS.getFrom(properties),
							PricingSummaryProperties.TOTAL_MODULES_WITH_ERRORS.getFrom(properties),
							PricingSummaryProperties.TOTAL_DATA_FILES.getFrom(properties),
							PricingSummaryProperties.TOTAL_BATCH_EXEC_PGM_STATEMENTS.getFrom(properties),
							PricingSummaryProperties.TOTAL_BATCH_EXEC_COBOL_PROGRAMS.getFrom(properties),
							PricingSummaryProperties.TOTAL_BATCH_EXEC_ASM_PROGRAMS.getFrom(properties),
							PricingSummaryProperties.TOTAL_BATCH_EXEC_UNKNOWN_MISSING_PROGRAMS.getFrom(properties),
							Optional.ofNullable(PricingSummaryProperties.TOTAL_SQL_STATEMENTS.getFrom(properties)).orElse(0) > 0 ? Boolean.TRUE : Boolean.FALSE,
							PricingSummaryProperties.TOTAL_MISSING_DEPENDENCIES.getFrom(properties));
				}
				break;
			case TYPE:
				for (final var effortSummary : effortSummaries) {
					final var properties = effortSummary.getProperties();
					createRow(
							TypeSummaryProperties.TECHNOLOGY.getFrom(properties),
							TypeSummaryProperties.TECHNOLOGY_TYPE.getFrom(properties),
							TypeSummaryProperties.COUNT.getFrom(properties),
							TypeSummaryProperties.LINES_OF_CODE.getFrom(properties),
							TypeSummaryProperties.LINES_OF_COMMENT.getFrom(properties),
							TypeSummaryProperties.ERROR_COUNT.getFrom(properties),
							TypeSummaryProperties.EASY_COUNT.getFrom(properties),
							TypeSummaryProperties.COMPLEX_COUNT.getFrom(properties),
							TypeSummaryProperties.VERY_COMPLEX_COUNT.getFrom(properties),
							TypeSummaryProperties.UNMAINTAINABLE_COUNT.getFrom(properties));
				}
				break;
			default:
				throw new IllegalArgumentException("EffortSummaryType is not handled: " + type);
		}
	}
}
