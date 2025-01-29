/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SqlDetailsService.SqlDetailsInquiryBuilder;
import innowake.mining.shared.entities.SqlDetailsPojo;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.StatementType;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.Collection;
import java.util.List;

/**
 * Dao for accessing the SQL details (from dbcutter database).
 * The table name and the column names are decided by the dbcutter team as per their standards.
 */
public class SqlDetailsDbCutterPgDao extends PgDao {

	public SqlDetailsDbCutterPgDao(final JdbcTemplate jdbc) {
		super(jdbc);
	}

	/**
	 * Query builder for SQL details.
	 */
	public class SqlDetailsDbCutterQueryBuilder implements SqlDetailsInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected List<SqlDetailsPojo> build() {
			return query(
					"SELECT id, `miningModuleHash`, query, name, `queryType`, nonconditional, conditional FROM `QueryDetailsView`")
					.with(filters::build)
					.toList((rs, row) -> new SqlDetailsPojo(EntityId.of(Long.valueOf(rs.getInt("id"))),
							rs.getString("miningModuleHash"), rs.getString("query"), rs.getString("name"), rs.getString("queryType"),
							PgUtil.<String> streamArray(rs.getArray("nonconditional")).toList(),
							PgUtil.<String> streamArray(rs.getArray("conditional")).toList()));
		}

		@Override
		public SqlDetailsInquiryBuilder ofProject(final int projectId) {
			filters.accept(q -> q.append("`miningProjectId` = ?", projectId));
			return this;
		}

		@Override
		public SqlDetailsInquiryBuilder withModuleLinkHash(final String moduleLinkHash) {
			filters.accept(q -> q.append("`miningModuleHash` = ?", moduleLinkHash));
			return this;
		}

		@Override
		public SqlDetailsInquiryBuilder withModuleLinkHashes(final Collection<String> moduleLinkHashes) {
			filters.accept(q -> q.append("`miningModuleHash` = any(?)", arrayFromCollection(PgType.STRING, moduleLinkHashes)));
			return this;
		}

		@Override
		public SqlDetailsInquiryBuilder withType(final StatementType type) {
			filters.accept(q -> q.append("`queryType` = ?", type.name()));
			return this;
		}

		@Override
		public SqlDetailsInquiryBuilder withTableName(final String tableName) {
			filters.accept(q -> q.append("name = ?", tableName));
			return this;
		}

		@Override
		public SqlDetailsInquiryBuilder withTypes(final Collection<StatementType> types) {
			filters.accept(q -> q.append("`queryType` = any(?)", arrayFromCollection(PgType.STRING, types)));
			return this;
		}

		@Override
		public SqlDetailsInquiryBuilder notWithTypes(final Collection<StatementType> types) {
			filters.accept(q -> q.append("`queryType` != all(?)", arrayFromCollection(PgType.STRING, types)));
			return this;
		}

		@Override
		public SqlDetailsInquiryBuilder withSqlText(final String text) {
			filters.accept(q -> q.append("query = ?", text));
			return this;
		}
	}

	/**
	 * Find SQL details with the given builder.
	 *
	 * @param builder the builder
	 * @return the list of SQL details
	 */
	public List<SqlDetailsPojo> find(final BuildingConsumer<SqlDetailsInquiryBuilder> builder) {
		return builder.prepare(new SqlDetailsDbCutterQueryBuilder())
				.build();
	}
}
