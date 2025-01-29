/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.mining.data.access.postgres.SqlDetailsDbCutterPgDao;
import innowake.mining.shared.access.SqlDetailsService;
import innowake.mining.shared.entities.SqlDetailsPojo;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Service for accessing SQL details.
 */
@Service
public class SqlDetailsImpl implements SqlDetailsService {

	/* uses different JDBC template and datasource corresponding to dbcutter db for the below dao */
	private final Optional<SqlDetailsDbCutterPgDao> sqlDetailsDbCutterPgDao;

	@Autowired
	public SqlDetailsImpl(@Qualifier("dbcutter-db-postgres") final Optional<JdbcTemplate> jdbcTemplateDbCutter) {
		sqlDetailsDbCutterPgDao = jdbcTemplateDbCutter.map(SqlDetailsDbCutterPgDao::new);
	}

	@Override
	public List<SqlDetailsPojo> findSqlDetails(final BuildingConsumer<SqlDetailsInquiryBuilder> builder) {
		return sqlDetailsDbCutterPgDao
			.orElseThrow(() -> new UnsupportedOperationException("Connection to db cutter is unavailable. Cannot retrieve SQL details."))
			.find(builder);
	}

}
