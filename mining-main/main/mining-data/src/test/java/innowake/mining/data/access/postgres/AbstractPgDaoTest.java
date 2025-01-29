/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;
import java.lang.reflect.Method;
import java.sql.SQLException;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.core.config.Configurator;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.TestInfo;
import org.postgresql.ds.PGSimpleDataSource;
import org.springframework.jdbc.core.JdbcTemplate;

import innowake.mining.data.migration.PostgresMiningSchemaMigrations;

/***
 * Test DAOs and SQL queries without an application context.
 */
public abstract class AbstractPgDaoTest {
	
	private final boolean migrateSchema;
	protected final PGSimpleDataSource ds;
	protected final JdbcTemplate jdbc;
	
	protected AbstractPgDaoTest(final String dbURL, final boolean migrateSchema) {
		Configurator.setLevel(PgDao.class, Level.ALL);
		Configurator.setLevel(innowake.mining.data.Logging.MIGRATION, Level.DEBUG);
		ToStringBuilder.setDefaultStyle(ToStringStyle.MULTI_LINE_STYLE);
		this.migrateSchema = migrateSchema;
		ds = new PGSimpleDataSource();
		ds.setUrl(dbURL);
		jdbc = new JdbcTemplate(ds);
	}
	
	protected AbstractPgDaoTest(final boolean migrateSchema) {
		this("jdbc:postgresql:mining?user=mining", migrateSchema);
	}
	
	protected AbstractPgDaoTest() {
		this(true);
	}
	
	@BeforeEach
	void init(TestInfo test) throws SQLException {
		if (migrateSchema) {
			new PostgresMiningSchemaMigrations().doMigrations(ds, null, -1);
		}
		LogManager.getLogger().always().log(test.getTestMethod().map(Method::getName).orElse("UNKNOWN") + " - BEGIN");
	}
	
	@AfterEach
	void fini(TestInfo test) {
		LogManager.getLogger().always().log(test.getTestMethod().map(Method::getName).orElse("UNKNOWN") + " - END");
	}
	
}
