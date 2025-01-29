/* Copyright (c) 2022 Deloitte. All rights reserved. */
package db.migration;

import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;

import innowake.lib.core.api.lang.Nullable;

/**
 * Flyway migration which creates all default technical taxonomies for existing project.
 */
public class V1_2_115__CreateMissingTaxonomies extends BaseJavaMigration {

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
         /* Had to revert the feature due to the release.
          * The script added the default technical taxonomies to projects where missing.
          * To avoid a gap we keep an empty migration script */
	}
}
