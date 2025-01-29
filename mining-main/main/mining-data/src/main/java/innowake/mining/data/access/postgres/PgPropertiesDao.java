/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.access.postgres;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Manages key/value pairs of strings for basic global configuration purposes in the database.
 */
public class PgPropertiesDao {
	
	protected final Connection db;
	
	/**
	 * Initializes a PropertiesDao instance.
	 * @param db Connection to a Postgres database.
	 */
	public PgPropertiesDao(final Connection db) {
		this.db = db;
	}
	
	/**
	 * Retrieves the specified property.
	 *
	 * @param name Name of the property.
	 * @return Value of the property, if defined.
	 * @throws SQLException If query fails.
	 */
	public Optional<String> get(final String name) throws SQLException {
		try (
			final PreparedStatement st = db.prepareStatement(
				"SELECT value FROM properties WHERE name = ?");
		) {
			st.setString(1, name);
			final ResultSet rs = st.executeQuery();
			if (rs.next()) {
				return Optional.of(rs.getString(1));
			}
		}
		return Optional.empty();
	}
	
	/**
	 * Retrieves the specified property, failing if it is not defined.
	 *
	 * @param name Name of the property.
	 * @return Value of the property.
	 * @throws SQLException If query fails or property is not defined.
	 */
	public String getOrThrow(final String name) throws SQLException {
		return get(name).orElseThrow(() -> new SQLException("Property " + name + " not found"));
	}
	
	/**
	 * Retrieves all defined properties.
	 * @return Map of properties.
	 * @throws SQLException If query fails.
	 */
	public Map<String, String> getAll() throws SQLException {
		final Map<String, String> properties = new HashMap<>();
		try (
			final PreparedStatement st = db.prepareStatement(
				"SELECT name, value FROM properties");
		) {
			final ResultSet rs = st.executeQuery();
			while (rs.next()) {
				properties.put(rs.getString(1), rs.getString(2));
			}
		}
		return properties;
	}
	
	/**
	 * Creates or updates the specified property.
	 *
	 * @param name Name of the property.
	 * @param value Value for the property.
	 * @throws SQLException If query fails.
	 */
	public void set(final String name, final String value) throws SQLException {
		try (
			final PreparedStatement st = db.prepareStatement(
				"INSERT INTO properties AS t values (?, ?) ON CONFLICT (name) DO UPDATE SET value = excluded.value");
		) {
			st.setString(1, name);
			st.setString(2, value);
			st.execute();
		}
	}
	
	/**
	 * Checks if there are any table objects defined in the database.
	 *
	 * @return If any tables are present in the public schema.
	 * @throws SQLException If query fails.
	 */
	public boolean isSchemaPresent() throws SQLException {
		try (
			final PreparedStatement st = db.prepareStatement(
				"SELECT count(*) FROM information_schema.tables WHERE table_schema = 'public'");
			final ResultSet rs = db.createStatement().executeQuery("SELECT count(*) FROM information_schema.tables WHERE table_schema = 'public'");
		) {
			if (rs.next()) {
				return rs.getInt(1) > 0;
			}
			throw new IllegalStateException("Error reading schema information from database.");
		}
	}
	
	public List<String> getEnumLabels(final String name) throws SQLException {
		final Stream.Builder<String> labels = Stream.builder();
		try (
			final PreparedStatement st = db.prepareStatement(
				"SELECT e.enumlabel FROM pg_catalog.pg_enum e"
				+ " INNER JOIN pg_catalog.pg_type t on t.oid = e.enumtypid"
				+ " INNER JOIN pg_catalog.pg_namespace s ON s.oid = t.typnamespace"
				+ " WHERE s.nspname = 'public' AND t.typname = ?");
		) {
			st.setString(1, name);
			final ResultSet rs = st.executeQuery();
			while (rs.next()) {
				labels.accept(rs.getString(1));
			}
		}
		return labels.build().collect(Collectors.toList());
	}
	
}
