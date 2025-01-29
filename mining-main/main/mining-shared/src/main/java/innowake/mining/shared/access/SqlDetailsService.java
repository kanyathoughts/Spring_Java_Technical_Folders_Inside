/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.access;

import innowake.mining.shared.entities.SqlDetailsPojo;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.StatementType;

import java.util.Collection;
import java.util.List;

/**
 * Service for accessing SQL details.
 */
public interface SqlDetailsService {

	/**
	 * Returns the list of {@code SqlDetailsPojo} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SqlDetailsService.SqlDetailsInquiryBuilder} containing the filter criteria.
	 * @return list of {@linkplain SqlDetailsPojo}
	 */
	List<SqlDetailsPojo> findSqlDetails(BuildingConsumer<SqlDetailsInquiryBuilder> builder);

	/**
	 * Builder for SQL details view.
	 */
	interface SqlDetailsInquiryBuilder {

		/**
		 * Filters SQL details by {@code project} id.
		 *
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		SqlDetailsInquiryBuilder ofProject(int project);

		/**
		 * Filters SQL details by module's linkhash.
		 *
		 * @param moduleLinkHash the linkhash of the module
		 * @return this instance for method chaining
		 */
		SqlDetailsInquiryBuilder withModuleLinkHash(String moduleLinkHash);

		/**
		 * Filters SQL details by any of the module's linkhashes.
		 *
		 * @param moduleLinkHashes the linkhash of the modules
		 * @return this instance for method chaining
		 */
		SqlDetailsInquiryBuilder withModuleLinkHashes(Collection<String> moduleLinkHashes);

		/**
		 * Filters SQL details by type
		 *
		 * @param type the {@link StatementType} of the SQL
		 * @return this instance for method chaining
		 */
		SqlDetailsInquiryBuilder withType(StatementType type);

		/**
		 * Filters SQL details by table name
		 *
		 * @param tableName the name of the table
		 * @return this instance for method chaining
		 */
		SqlDetailsInquiryBuilder withTableName(String tableName);

		/**
		 * Filters SQL details by types
		 *
		 * @param types the {@link StatementType}s to filter the SQL details for
		 * @return this instance for method chaining
		 */
		SqlDetailsInquiryBuilder withTypes(Collection<StatementType> types);

		/**
		 * Filters SQL details by none of the types
		 *
		 * @param types the {@link StatementType} of the SQL details
		 * @return this instance for method chaining
		 */
		SqlDetailsInquiryBuilder notWithTypes(Collection<StatementType> types);

		/**
		 * Filters SQL details by the sql text
		 *
		 * @param text the text of the SQL details
		 * @return this instance for method chaining
		 */
		SqlDetailsInquiryBuilder withSqlText(String text);
	}
}
