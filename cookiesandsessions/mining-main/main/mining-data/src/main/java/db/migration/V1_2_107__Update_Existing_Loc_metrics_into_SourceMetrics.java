/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package db.migration;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.time.StopWatch;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.model.IdentifiableEntity;

/**
 * Flyway migration script that selects all module's linesOfCode, linesOfComment, complexity, linesOfDeadCode and save them in the new SourceMetrics table
 *  and create a link between Module and SourceMetrics.
 */
public class V1_2_107__Update_Existing_Loc_metrics_into_SourceMetrics extends BaseJavaMigration {

	private static final int LOG_SIZE = 50_000;
	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);
	
	private static final String BASE_QUERY = "SELECT id, @rid, linesOfCode, linesOfComment, complexity, linesOfDeadCode from Module "
	+ "where linesOfCode > 0 or linesOfComment > 0  or complexity > 0 or linesOfDeadCode != -1";
	private static final String CREATE_SOURCE_METRICS_AND_EDGE_QUERY =
			"CREATE EDGE HasAdditionalInfo FROM %s TO (INSERT INTO SourceMetrics SET %s)";

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(assertNotNull(context).getConnection(), true));
		final List<Module> listOfModules = jdbcTemplate.query(BASE_QUERY, new ModuleMapper());
		final StringBuilder columnValues = new StringBuilder(128);
		
		LOG.info(() -> String.format("Migrating %d modules", listOfModules.size()));
		final int[] cnt = { 0 };
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		listOfModules.forEach(module -> {
			final SourceMetrics sourceMetrics = assertNotNull(module.getSourceMetrics());
			/* source metrics values from the module table data */
			columnValues.append("id=sequence('SourceMetrics_Sequence').next(),")
						.append("deadCodeLines=").append(sourceMetrics.getDeadCodeLines());
			if (sourceMetrics.getCodeLines() != null) {
				columnValues.append(",codeLines=").append(sourceMetrics.getCodeLines());
			}
			if (sourceMetrics.getCommentLines() != null) {
				columnValues.append(",commentLines=").append(sourceMetrics.getCommentLines());
			}
			if (sourceMetrics.getComplexityMcCabe() != null) {
				columnValues.append(",complexityMcCabe=").append(sourceMetrics.getComplexityMcCabe());
			}

			/* Creating SourceMetrics vertex and creating edge between Module and created SourceMetrics */
			final String sqlForCreateEdge = String.format(CREATE_SOURCE_METRICS_AND_EDGE_QUERY, module.getRecordId(), columnValues.toString());
			jdbcTemplate.update(sqlForCreateEdge);

			cnt[0]++;
			if (LOG.isInfoEnabled() && cnt[0] % LOG_SIZE == 0) {
				LOG.info(() -> String.format("Migrated %d source metrics so far. Took %s (H:mm:ss.SSS)", cnt[0], stopWatch.toString()));
			}

			columnValues.setLength(0);
		});

		stopWatch.stop();
		LOG.info(() -> String.format("Migration of source metrics took %s (H:mm:ss.SSS)", stopWatch.toString()));
	}
	
	private class ModuleMapper implements RowMapper<Module> {

		@Override
		@Nullable
		public Module mapRow(final ResultSet resultSet, final int rowNum) throws SQLException {
			final Module module = new Module();
			module.setRecordId(resultSet.getString("@rid"));

			final SourceMetrics sourceMetrics = new SourceMetrics();
			module.setSourceMetrics(sourceMetrics);
			sourceMetrics.setCodeLines((Integer) resultSet.getObject("linesOfCode"));
			sourceMetrics.setCommentLines((Integer) resultSet.getObject("linesOfComment"));
			sourceMetrics.setComplexityMcCabe((Integer) resultSet.getObject("complexity"));
			sourceMetrics.setDeadCodeLines((Integer) resultSet.getObject("linesOfDeadCode"));

			return module;
		}
	}

	private static class Module {

		@Nullable
		private SourceMetrics sourceMetrics;
		@Nullable
		private String recordId;

		/**
		 * Sets the record id.
		 *
		 * @param rid the record id
		 */
		private void setRecordId(final String rid) {
			this.recordId = rid;
		}
	
		@Nullable
		private String getRecordId() {
			/* must be not null after all DAOs read the record id */
			return recordId;
		}

		@Nullable
		private SourceMetrics getSourceMetrics() {
			return sourceMetrics;
		}

		private void setSourceMetrics(final SourceMetrics sourceMetrics) {
			this.sourceMetrics = sourceMetrics;
		}
	}

	private static class SourceMetrics extends IdentifiableEntity {

		@Nullable
		private Integer physicalLines;
		@Nullable
		private Integer codeLines;
		@Nullable
		private Integer commentLines;
		@Nullable
		private Integer complexityMcCabe;
		private Integer deadCodeLines = Integer.valueOf(-1);
		
		/**
		 * Get the number of lines of code i.e, SLOC.
		 * If a line contain both, code and comment, it will be added to code.
		 *
		 * @return the number of lines with code
		 */
		@Nullable
		private Integer getCodeLines() {
			return codeLines;
		}
		
		/**
		 * Set the number of lines of code i.e, SLOC.
		 * Must be > -1.
		 *
		 * @param codeLines The number of lines of code
		 * @return the {@link SourceMetrics} instance
		 */
		private SourceMetrics setCodeLines(@Nullable final Integer codeLines) {
			final Integer newValue = positiveIntegerOrNull(codeLines);
			if ( ! Objects.equals(this.codeLines, newValue)) {
				this.codeLines = newValue;
			}
			return this;
		}
		
		/**
		 * Get the number of lines of comment i.e, CLOC.
		 * If a line contain both, code and comment, it will be added to code.
		 *
		 * @return the number of lines with comment
		 */
		@Nullable
		private Integer getCommentLines() {
			return commentLines;
		}
		
		/**
		 * Set the number of lines of comment i.e, CLOC.
		 * Must be > -1.
		 *
		 * @param commentLines The number of lines of comment
		 * @return the {@link SourceMetrics} instance
		 */
		private SourceMetrics setCommentLines(@Nullable final Integer commentLines) {
			final Integer newValue = positiveIntegerOrNull(commentLines);
			if ( ! Objects.equals(this.commentLines, newValue)) {
				this.commentLines = newValue;
			}
			return this;
		}
		
		/**
		 * Get the complexity value by McCabe.
		 *
		 * @return the complexity
		 */
		@Nullable
		private Integer getComplexityMcCabe() {
			return complexityMcCabe;
		}
		
		/**
		 * Set the module complexity calculated by McCabe.
		 * Must be > -1.
		 *
		 * @param complexityMcCabe the complexity value
		 * @return the {@link SourceMetrics} instance
		 */
		private SourceMetrics setComplexityMcCabe(@Nullable final Integer complexityMcCabe) {
			final Integer newValue = positiveIntegerOrNull(complexityMcCabe);
			if ( ! Objects.equals(this.complexityMcCabe, newValue)) {
				this.complexityMcCabe = newValue;
			}
			return this;
		}
		
		/**
		 * Returns the number of code lines, that is never executed.
		 *
		 * @return number of dead code lines
		 */
		private Integer getDeadCodeLines() {
			return deadCodeLines;
		}

		/**
		 * Sets the number of code lines, that is never executed.
		 *
		 * @param deadCodeLines number of dead code lines 
		 * @return the {@link SourceMetrics} instance
		 */
		private SourceMetrics setDeadCodeLines(@Nullable final Integer deadCodeLines) {
			/* Although the default is 0, it can be -1 for virtual modules */
			final Integer newValue = deadCodeLines == null || deadCodeLines.intValue() < -1 ? Integer.valueOf(-1) : deadCodeLines;
			this.deadCodeLines = newValue;
			return this;
		}
		
		@Nullable
		private static Integer positiveIntegerOrNull(@Nullable final Integer value) {
			return value == null || value.intValue() < 0 ? null : value;
		}
	}
}
