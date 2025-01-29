/* Copyright (c) 2022 Deloitte. All rights reserved. */
package db.migration;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.BATCH;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.DELETE;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.LIBRARY;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.MQ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.STORE;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UI;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UPDATE;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.WRITE;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.DB_ACCESS;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.FILE_ACCESS;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.time.StopWatch;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import db.migration.model.legacy.Taxonomy;
import db.migration.model.legacy.TaxonomyCategory;
import db.migration.model.legacy.TaxonomyType;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;

/**
 * Flyway migration which creates all default technical taxonomies for existing project.
 */
@SuppressWarnings({"removal", "deprecation"})
public class V1_2_121__CreateMissingProjectTaxonomies extends BaseJavaMigration {
	
	private static final List<Taxonomy> TECHNICAL_TAXONOMIES;
	
	static {
		final TaxonomyType programType = new TaxonomyType(PROGRAM_TYPE.getDisplayName());
		final TaxonomyType fileAccess = new TaxonomyType(FILE_ACCESS.getDisplayName());
		final TaxonomyType dbAccess = new TaxonomyType(DB_ACCESS.getDisplayName());

		final Taxonomy readFileTaxonomy = new Taxonomy(READ.getDisplayName(), fileAccess);
		final Taxonomy readDBTaxonomy = new Taxonomy(READ.getDisplayName(), dbAccess);
		final Taxonomy writeFileTaxonomy = new Taxonomy(WRITE.getDisplayName(), fileAccess);
		final Taxonomy deleteDBTaxonomy = new Taxonomy(DELETE.getDisplayName(), dbAccess);
		final Taxonomy storeDBTaxonomy = new Taxonomy(STORE.getDisplayName(), dbAccess);
		final Taxonomy updateDBTaxonomy = new Taxonomy(UPDATE.getDisplayName(), dbAccess);
		final Taxonomy batchTaxonomy = new Taxonomy(BATCH.getDisplayName(), programType);
		final Taxonomy libraryTaxonomy = new Taxonomy(LIBRARY.getDisplayName(), programType);
		final Taxonomy uiTaxonomy = new Taxonomy(UI.getDisplayName(), programType);
		final Taxonomy mqTaxonomy = new Taxonomy(MQ.getDisplayName(), programType);

		TECHNICAL_TAXONOMIES = Arrays.asList(readFileTaxonomy, readDBTaxonomy, writeFileTaxonomy, deleteDBTaxonomy, storeDBTaxonomy, updateDBTaxonomy, batchTaxonomy,
				libraryTaxonomy, uiTaxonomy, mqTaxonomy);
	}

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);
	private static final String QUERY_PROJECTS = "SELECT id, @rid,technicalTaxonomyCategory from Project ";
	private static final String QUERY_TAXONOMY_TYPES = "SELECT name, @rid , projectLink.id, categoryLink.@rid,"
			+ " categoryLink.id, categoryLink.name FROM TaxonomyEnum WHERE projectLink.id=? and name=? ORDER BY name ASC ";

	private static final String UPSERT_TAXONOMY = "UPDATE Taxonomy"
			+ " SET id=(SELECT sequence('Taxonomy_Sequence').next()), name=?, typeLink=?, projectLink=? UPSERT RETURN AFTER @rid"
			+ " WHERE name=? AND typeLink=? AND projectLink=?";

	private static final String UPSERT_TAXONOMY_ENUM = "UPDATE TaxonomyEnum SET name=?, projectLink=?, "
			+ "categoryLink=projectLink.technicalTaxonomyCategoryLink UPSERT RETURN AFTER @rid WHERE name=? AND projectLink=?";
	private static final String TECHNICAL_TAXONOMY_CATEGORY = "Technical Taxonomies";
	private static final String TECHNICAL_CATEGORY = "technical";

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(assertNotNull(context).getConnection(), true));
		final List<Object[]> projects = jdbcTemplate.query(QUERY_PROJECTS, new ProjectMapper());
		LOG.info(() -> String.format("creating technical taxonomies for %d projects", projects.size()));

		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();
		projects.forEach(project -> createTaxonomies(TECHNICAL_TAXONOMIES, project, jdbcTemplate));

		stopWatch.stop();
		LOG.info(() -> String.format("creation of technical taxonomies for existing projects took %s (H:mm:ss.SSS)", stopWatch.toString()));

	}

	public void createTaxonomies(final List<Taxonomy> taxonomies, final Object[] project, final JdbcTemplate database) {
		taxonomies.forEach(taxanomy -> createTaxonomy(taxanomy, project, database));

	}

	private void createTaxonomy(final Taxonomy taxonomy, final Object[] project, final JdbcTemplate database) {
		database.update(UPSERT_TAXONOMY_ENUM, taxonomy.getType().getName(), project[1], taxonomy.getType().getName(),
				project[1]);
		final TaxonomyType taxonomyType = database.query(QUERY_TAXONOMY_TYPES, new TaxonomyTypeMapper(), project[0], taxonomy.getType().getName())
				.get(0);

		database.update(UPSERT_TAXONOMY, taxonomy.getName(), taxonomyType.getRecordId(), project[1], taxonomy.getName(),
				taxonomyType.getRecordId(), project[1]);

	}

	private static class ProjectMapper implements RowMapper<Object[]> {
		@Override
		@Nullable
		public Object[] mapRow(final ResultSet resultSet, final int rowNum) throws SQLException {
			return new Object[] {
				resultSet.getObject("id"), /* 0 */
				resultSet.getString("@rid"), /* 1 */
				resultSet.getObject("clientId"), /* 2 */
				resultSet.getObject("technicalTaxonomyCategory"), /* 3 */
				getTaxonomyTechnicalCategory(resultSet) /* 4 */
			};
		}

		private TaxonomyCategory getTaxonomyTechnicalCategory(final ResultSet rs) throws SQLException {
			final TaxonomyCategory taxonomyCategory = new TaxonomyCategory(rs.getString(TECHNICAL_CATEGORY + "TaxonomyCategoryLink.name"), (Long) rs.getObject("id"));
			taxonomyCategory.setId(rs.getLong(TECHNICAL_CATEGORY + "TaxonomyCategoryLink.id"));
			taxonomyCategory.setName(TECHNICAL_TAXONOMY_CATEGORY);
			return taxonomyCategory;
		}
	}

	private static class TaxonomyTypeMapper implements RowMapper<TaxonomyType> {

		@Override
		@Nullable
		public TaxonomyType mapRow(final ResultSet resultSet, final int rowNumber) throws SQLException {
			final TaxonomyType taxonomyType = new TaxonomyType();
			taxonomyType.setProjectId(resultSet.getLong("projectLink.id"));
			taxonomyType.setName(resultSet.getString("name"));
			taxonomyType.setRecordId(resultSet.getString("@rid"));
			final TaxonomyCategory taxonomyCategory = new TaxonomyCategory(resultSet.getString("categoryLink.name"), 
					resultSet.getLong("projectLink.id"));
			taxonomyCategory.setRecordId(resultSet.getString("categoryLink.@rid"));
			taxonomyCategory.setId(resultSet.getLong("categoryLink.id"));

			taxonomyType.setCategory(taxonomyCategory);
			return taxonomyType;
		}
	}

}
