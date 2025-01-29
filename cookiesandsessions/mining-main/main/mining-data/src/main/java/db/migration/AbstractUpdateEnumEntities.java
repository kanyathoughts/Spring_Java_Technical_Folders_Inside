/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package db.migration;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.zip.CRC32;

import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.model.Technology;

/**
 * Flyway migration class for updating the Enum tables such as TechnologyEnum, TypeEnum.
 * 
 * @param <E> Enum type. For e.g., {@link Technology}
 */
public abstract class AbstractUpdateEnumEntities<E extends Enum<?>> extends BaseJavaMigration {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);

	@Nullable
	private Integer checksum;
	
	private static final String BASE_INSERT_QUERY = "INSERT INTO %s SET name = \"%s\"";
	
	/**
	 * Returns the enum to be added if missing.
	 *
	 * @return enum type.
	 */
	protected abstract E[] getEnum();
	
	/**
	 * Arbitrary version of this file.
	 *
	 * @return Arbitrary version.
	 */
	protected abstract Integer arbitraryVersion();
	
	/**
	 * Returns the corresponding Enum entity.
	 *
	 * @return db enum.
	 */
	protected abstract String getTable();

	/**
	 * Computes checksum by combining all {@code E} values.
	 * 
	 * @return checksum.
	 */
	@Nullable
	@Override
	public Integer getChecksum() {
		if (checksum == null) {
			final StringBuilder content = new StringBuilder(Arrays.stream(getEnum())
					.map(Enum::name)
					.collect(Collectors.joining(",")));
			
			content.append(arbitraryVersion());

			final CRC32 crc32 = new CRC32();
			crc32.update(content.toString().getBytes());
			checksum = Integer.valueOf((int) crc32.getValue());
		}
		LOG.trace(() -> "Checksum value : " + checksum);
		return checksum;
	}

	@Override
	public void migrate(@Nullable final Context context) {
		if (context != null) {
			final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(context.getConnection(), true));
			final List<String> dbEnums = jdbcTemplate.queryForList("SELECT name FROM " + getTable(), String.class);
						
			/* checks for missing enums and then performs an INSERT operation */
			Arrays.stream(getEnum())
				.map(Enum::name)
				.filter(enumName -> ! dbEnums.contains(enumName))
				.map(enumName -> {
					final String insertQuery = String.format(BASE_INSERT_QUERY, getTable(), enumName);
					LOG.trace(() -> "Executing the query: " + insertQuery);
					return insertQuery;
				})
				.forEach(jdbcTemplate::execute);				
		}
	}

}
