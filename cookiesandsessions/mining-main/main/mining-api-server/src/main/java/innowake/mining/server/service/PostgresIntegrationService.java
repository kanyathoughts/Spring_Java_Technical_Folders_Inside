/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.orientechnologies.orient.core.id.ORID;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.PgProperties;
import innowake.mining.data.access.postgres.PgPropertiesDao;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.UUIDv5;

/**
 * Basic data access methods for managing the Postgres database
 * and temporary methods for OrientDB compatibility during the transition phase when both databases are in use.
 */
@Service
public class PostgresIntegrationService {
	
	private final DataSource db;

	@Nullable
	private Map<String, String> properties;
	
	@Autowired
	public PostgresIntegrationService(@Qualifier("postgres") final DataSource db) throws SQLException {
		this.db = db;
		cacheProperties();
	}
	
	/**
	 * Loads all properties from the database to the Service cache. 
	 * @throws SQLException In case of a database error.
	 */
	public void cacheProperties() throws SQLException {
		try (final Connection pg = db.getConnection()) {
			properties = new PgPropertiesDao(pg).getAll();
		}
	}
	
	/**
	 * Retrieves the value for a database property from the Service cache.
	 * @param name Property name as defined in {@link PgProperties}.
	 * @return Property value
	 */
	public String getProperty(final String name) {
		return Objects.requireNonNull(properties).get(name);
	}
	
	public EntityFactory createEntityFactory() {
		return new EntityFactory();
	}

	/**
	 * Factory that generates {@link UUID UUIDs} in the namespace of the current database.
	 * <p>Can be used to create dummy {@code module} entities in Postgres for {@code Modules} in OrientDB.</p>
	 * <p>The EntityFactory is not thread-safe</p>
	 */
	public class EntityFactory {

		private final UUIDv5 uuid5;

		private EntityFactory() {
			uuid5 = new UUIDv5(UUID.fromString(getProperty(PgProperties.DATABASE_ID)));
		}
		
		/**
		 * Generate a UUIDv5 for an entity in the namespace of the current database.
		 * @param entity Entity type as defined in {@link MiningEnitityNames}.
		 * @param id One or more identifying elements of the respective entity. For entities in OrientDB this is the String representation of their {@link ORID}.
		 * @return Repeatable generated UUID.
		 */
		public UUID uuid5(final String entity, final String... id) {
			return Objects.requireNonNull(uuid5).generate(entity, id);
		}
		
		/**
		 * Generate a UUIDv5 for an entity in the namespace of the current database.
		 * @param entity Entity type as defined in {@link MiningEnitityNames}.
		 * @param id OrientDB Record ID of the entity. 
		 * @return Repeatable generated UUID.
		 */
		public UUID uuid5(final String entity, final ORID id) {
			return Objects.requireNonNull(uuid5).generate(entity, id.toString());
		}
	}
	
}
