package innowake.mining.data.migration.postgres;

import java.sql.SQLException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import innowake.mining.data.access.postgres.PgArray;
import innowake.mining.data.access.postgres.PgDao;
import innowake.mining.data.access.postgres.PgType;
import innowake.mining.data.access.postgres.PgUtil;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.PojoMapper;

public class SavedSearchMigration extends PostgresSchemaMigrationFromOrient {

	public SavedSearchMigration(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("saved_search_table_creation");

		migrateData("SavedSearches",
			"SELECT id, clientId, projectId, name, savedSearch, scope.name, usage, modifiers, createdByUserId FROM SavedSearch",
			"insert into saved_search values (?, (select uid from client where nid = ?), (select uid from project where nid = ?), ?, ?, ?, ?, ?, ?)",
			1000, (in, out, n) -> {
				out.add(in.getLong(1)); /* id */
				out.add(in.getObject(2) == null ? null : in.getLong(2)); /* clientId */
				out.add(in.getObject(3) == null ? null : in.getLong(3)); /* projectId */
				out.add(in.getString(4)); /* name */
				out.add(in.getString(5)); /* savedSearch */
				out.add(in.getString(6)); /* scope */
				out.add(in.getString(7)); /* usage */

				@SuppressWarnings("unchecked")
				final String[] modifiers = in.getObject(8) == null ? null : ((List<String>) in.getObject(8)).toArray(new String[0]);
				out.add(modifiers == null ? null : new PgArray(PgType.STRING.toString(), modifiers).toJdbcArray(dbPostgres));

				out.add(in.getString(9)); /* createdByUserId */

				return false;
			}, () -> new PgDao(getPgTemplate()) {
					public void run() {
						final var savedSearches = PojoMapper.readList(Map.class, readVersionedResourceFile("saved_search_built_in.json"));
						query("INSERT INTO saved_search (project, name, saved_search, scope, usage, modifiers)"
								+ " VALUES ((SELECT uid FROM project WHERE nid = 0), ?, ?, ?, ?, ?)")
							.updateBatch(savedSearches.stream().map(search -> Stream.of(
										search.get("name"),
										search.get("savedSearch"),
										search.get("scope"),
										search.get("usage"),
										arrayFromCollection(PgType.STRING, collection(search.get("modifiers")))
									)), savedSearches.size());
					}
				}.run()
			);

		updatePgSequence("saved_search_id", "select coalesce(max(id) + 1, 1) from saved_search");
	}
}
