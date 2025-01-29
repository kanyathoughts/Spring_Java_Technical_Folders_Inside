/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package db.migration;

import java.sql.ResultSet;
import java.util.List;
import java.util.Map;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;

/**
 * This class could be extended to add new TaxonomyCategories and set it to the project. Override
 * {@link AbstractUpdateTaxonomyCategoryForProjectAndTaxonomyType#getTaxonomyCategories()} to add the list of TaxonomyCategories
 * and creates Taxonomy Category link in Taxonomy Types(TaxonomyEnum) and updates it.
 */
public abstract class AbstractUpdateTaxonomyCategoryForProjectAndTaxonomyType extends BaseJavaMigration {

	/**
	 * Gets the TaxonomyCategories map.
	 *
	 * @return Map of TaxonomyCategory link & name.
	 */
	protected abstract Map<String, String> getTaxonomyCategories();

	/**
	 * Computes and updates taxonomy category to the TaxonomyEnum
	 * 
	 * @param jdbcTemplate the jdbcTemplate
	 * @param projectRId rid of the project
	 * @param taxonomyTypeName the name of the taxonomyType
	 */
	protected abstract void computeAndUpdateTaxonomyEnum(final JdbcTemplate jdbcTemplate, final String projectRId, final String taxonomyTypeName);

	@Override
	public void migrate(@Nullable final Context context) throws Exception {

		final Map<String, String> taxonomyCategoryLinkAndName = getTaxonomyCategories();

		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));

		/* Creates new project property to link taxonomy category */
		for (final String taxonomyCategoryLinkName : taxonomyCategoryLinkAndName.keySet()) {
			jdbcTemplate.execute("CREATE PROPERTY Project." + taxonomyCategoryLinkName + " IF NOT EXISTS LINK TaxonomyCategory;");
		}

		final List<String> projectRIds = jdbcTemplate.query("SELECT @rid FROM Project;", (final ResultSet rs, final int rowNum) -> rs.getString("@rid"));

		/* Creates each taxonomy category for a project */
		for (final String projectRId : projectRIds) {
			for (final Map.Entry<String, String> taxonomyCategory : taxonomyCategoryLinkAndName.entrySet()) {
				jdbcTemplate.update("INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name = ? , projectLink = ? ; ",
						taxonomyCategory.getValue(), projectRId);
			}
		}

		/* Updates the taxonomy category link of a project with the newly created taxonomy category */
		for (final Map.Entry<String, String> taxonomyCategory : taxonomyCategoryLinkAndName.entrySet()) {
			jdbcTemplate.update(
					"UPDATE PROJECT SET " + taxonomyCategory.getKey()
							+ "= (Select from TaxonomyCategory where name=? and projectLink=(SELECT FROM Project WHERE id=$current.id));",
					taxonomyCategory.getValue());
		}

		/* Creates new TaxonomyEnum property to link taxonomy category */
		jdbcTemplate.execute("CREATE PROPERTY TaxonomyEnum.categoryLink IF NOT EXISTS LINK TaxonomyCategory (NOTNULL, MANDATORY TRUE);");

		/* Updates the taxonomy category link of the TaxonomyEnum with the project's taxonomy category link */
		for (final String projectRId : projectRIds) {
			final List<String> taxonomyTypeNameList = jdbcTemplate.query("SELECT name FROM TaxonomyEnum WHERE projectLink = ?;",
					(final ResultSet rs, final int rowNum) -> rs.getString("name"), projectRId);

			for (final String taxonomyTypeName : taxonomyTypeNameList) {
				computeAndUpdateTaxonomyEnum(jdbcTemplate, projectRId, taxonomyTypeName);
			}
		}

	}

}
