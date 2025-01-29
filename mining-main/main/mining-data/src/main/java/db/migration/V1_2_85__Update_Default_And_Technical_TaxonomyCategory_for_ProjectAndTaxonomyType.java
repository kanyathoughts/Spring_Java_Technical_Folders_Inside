/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package db.migration;

import java.util.HashMap;
import java.util.Map;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Flyway migration script that creates the taxonomy categories for the project, sets it to the new taxonomy category properties(links) of the project
 * and links the Taxonomy Types(TaxonomyEnum) to the Taxonomy Categories of the project.
 */
public class V1_2_85__Update_Default_And_Technical_TaxonomyCategory_for_ProjectAndTaxonomyType extends AbstractUpdateTaxonomyCategoryForProjectAndTaxonomyType {

	private static final String TECHNICAL_TAXONOMIES = "Technical Taxonomies";
	private static final String BUSINESS_TAXONOMIES = "Business Taxonomies";
	private static final String TECHNICAL_TAXONOMY_CATEGORY_LINK = "technicalTaxonomyCategoryLink";
	private static final String DEFAULT_TAXONOMY_CATEGORY_LINK = "defaultTaxonomyCategoryLink";
	private static final Map<String, String> TAXONOMY_CATEGORY_LINK_AND_NAME = new HashMap<>(2);

	@Override
	protected Map<String, String> getTaxonomyCategories() {
		TAXONOMY_CATEGORY_LINK_AND_NAME.put(TECHNICAL_TAXONOMY_CATEGORY_LINK, TECHNICAL_TAXONOMIES);
		TAXONOMY_CATEGORY_LINK_AND_NAME.put(DEFAULT_TAXONOMY_CATEGORY_LINK, BUSINESS_TAXONOMIES);
		return TAXONOMY_CATEGORY_LINK_AND_NAME;
	}
	
	@Override
	protected void computeAndUpdateTaxonomyEnum(final JdbcTemplate jdbcTemplate, final String projectRid, final String taxonomyTypeName) {
		/*
		 * Checks if the taxonomy type is of technical taxonomy against the
		 * TechnicalTaxonomies.TypeName enum and sets the project's technical taxonomy
		 * category if not sets to project's default taxonomy category
		 */
		if (checkTaxonomyTypeName(taxonomyTypeName)) {
			updateTaxonomyEnum(jdbcTemplate, TECHNICAL_TAXONOMY_CATEGORY_LINK, projectRid, taxonomyTypeName);
		} else {				
			updateTaxonomyEnum(jdbcTemplate, DEFAULT_TAXONOMY_CATEGORY_LINK , projectRid, taxonomyTypeName);
		}
	}
	
	private void updateTaxonomyEnum(final JdbcTemplate jdbcTemplate, final String type, final String projectRId,
			final String taxonomyTypeName) {
		jdbcTemplate.update("UPDATE TaxonomyEnum SET categoryLink = projectLink." + type
				+ " WHERE name= ? AND projectLink= ?", taxonomyTypeName, projectRId);
	}

	private static boolean checkTaxonomyTypeName(final String taxonomyTypeName) {
		switch (taxonomyTypeName.toUpperCase()) {
			case "PROGRAM_TYPE":
			case "PROGRAM TYPE":
			case "FILE_ACCESS":
			case "FILE ACCESS":
			case "DB_ACCESS":
			case "DB ACCESS":
				return true;
			default:
				return false;
		}
	}
}

