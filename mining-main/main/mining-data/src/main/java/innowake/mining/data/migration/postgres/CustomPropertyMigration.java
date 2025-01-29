/*
 * Copyright (c) 2023 Deloitte innoWake GmbH
 */
package innowake.mining.data.migration.postgres;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.IntStream;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;

/**
 * Migrates CustomProperties to Postgres
 */
public class CustomPropertyMigration extends PostgresSchemaMigrationFromOrient {

	private static final ObjectMapper json = new ObjectMapper();
	private static final Set<String> ORIENT_COLUMNS_TO_IGNORE = new HashSet<>(Arrays.asList("in_HasAdditionalInfo", "@rid", "@version", "@class"));

	public CustomPropertyMigration(final SchemaMigrationContext context) {
		super(context);
	}
	
	@Override
	public void migrate() throws SQLException {
		/*
		 * 1. fetch custom prop classes
		 * 2. for each, get the table of the orient db
		 * 3. get all values from that table
		 * 4. extract into JSON object 
		 * 5. insert in respective Table.Column
		 * e.g. Values from ModuleCustomProperties1 -> modules.custom_properties 
		 */
		try (final var st = dbPostgres.createStatement()) {
			final ResultSet rs = st.executeQuery("SELECT custom_property_classes FROM project WHERE custom_property_classes IS NOT NULL");
			while (rs.next()) {
				final Map<String, Object> miningEntitiesToCustomPropClasses = PgJSON.fromPGobject(rs.getObject(1));
				
				long updateSum = 0;
				for (final Entry<String, Object> miningEntityToCustomPropClasses : miningEntitiesToCustomPropClasses.entrySet()) {
					final String miningEntityName = miningEntityToCustomPropClasses.getKey();
					
					final Optional<String> tableName = entityToTableName(miningEntityName);
					if (tableName.isEmpty()) {
						LOG.error("Unmapped MiningEntity Name, could not determine target Table: " + miningEntityName);
						continue;
					}
					
					//map of the entity id to the final object to be inserted into the DB
					final Map<Long, ObjectNode> idToCustomPropertyValueJson = mapToFormattedCustomProperties(miningEntityToCustomPropClasses);
					if (idToCustomPropertyValueJson.isEmpty()) {
						continue;
					}
					
					updateSum += doBatchInsert(tableName, idToCustomPropertyValueJson);
				}
				LOG.info("Inserted " + updateSum + " custom property definitions for ");
			}
		}
	}

	private long doBatchInsert(final Optional<String> tableName, final Map<Long, ObjectNode> idToCustomPropertyValueJson) throws SQLException {
		long updateSum = 0;
		try (final var insert = dbPostgres.prepareStatement("UPDATE " + tableName.get() + " "
				+ "SET custom_properties = ? "
				+ "WHERE nid = ? ")) {
			for (final Entry<Long, ObjectNode> e : idToCustomPropertyValueJson.entrySet()) {
				final var jsonString = e.getValue() == null ? null : e.getValue().toString();
				insert.setObject(1, PgJSON.toPGobjectFromString(jsonString));
				final Long key = e.getKey();
				insert.setLong(2, key);
				insert.addBatch();
			}
			final int[] updates = insert.executeBatch();
			updateSum += IntStream.of(updates).sum();
		}
		return updateSum;
	}

	private Map<Long, ObjectNode> mapToFormattedCustomProperties(final Entry<String, Object> miningEntityToCustomPropClasses) throws SQLException {
		final Map<Long, ObjectNode> idToCustomPropertyValueJson = new LinkedHashMap<>();
		@SuppressWarnings("unchecked")
		final List<String> customPropertyClasses = (List<String>) miningEntityToCustomPropClasses.getValue();
		for (final String customPropertyClass : customPropertyClasses) {
			final Map<Long, ObjectNode> entityIdToCustomProperties = fetchIdToCustomPropertyValues(customPropertyClass);
			
			for (final Entry<Long, ObjectNode> customPropValue : entityIdToCustomProperties.entrySet()) {
				final Long id = customPropValue.getKey();
				final ObjectNode val = customPropValue.getValue();
				final ObjectNode completeCustomPropNode = idToCustomPropertyValueJson.computeIfAbsent(id, k -> json.createObjectNode());
				completeCustomPropNode.set(customPropertyClass, val);
			}
		}
		return idToCustomPropertyValueJson;
	}
	
	/**
	 * @param customPropertyClass the entityClass, e.g. ModuleCustomProperties1
	 * @return mapping of ID to JSON Value, e.g. 
	 * @throws SQLException
	 */
	private Map<Long, ObjectNode> fetchIdToCustomPropertyValues(final String customPropertyClass) throws SQLException {
		final Map<Long, ObjectNode> result = new LinkedHashMap<>();
		try (final var st = getOrientDB().prepareStatement("select in(HasAdditionalInfo).id, * from " + customPropertyClass)) {
			final ResultSet rs = st.executeQuery();
			final ResultSetMetaData meta = rs.getMetaData();

			while (rs.next()) {
				@SuppressWarnings("unchecked")
				final List<Long> idList = (List<Long>) rs.getObject(1);
				if (idList.size() != 1) {
					throw new IllegalStateException("Invalid ID retrieved: " + idList);
				}
				final var id = idList.get(0);
				final var customPropClassNode = json.createObjectNode();
				for (var i = 2; i < meta.getColumnCount(); i++) {
					final String colName = meta.getColumnName(i);
					if (ORIENT_COLUMNS_TO_IGNORE.contains(colName)) {
						continue;
					}
					final var valueNode = extractJsonNode(rs, meta, i);
					customPropClassNode.set(colName, valueNode);
				}
				result.put(id, customPropClassNode);
			}
		}
		
		return result;
	}

	@Nullable
	private JsonNode extractJsonNode(final ResultSet rs, final ResultSetMetaData meta, final int i) throws SQLException {
		final JsonNode valueNode;
		if (meta.getColumnType(i) == Types.ARRAY) {
			final List<?> arrayRs = (List<?>) rs.getObject(i);
			final var arrayNode = json.createArrayNode();
			for (final Object o : arrayRs) {
				arrayNode.add(o.toString());
			}
			valueNode = arrayNode;
		} else {
			final var val = rs.getObject(i);
			valueNode = val == null ? null : TextNode.valueOf(val.toString());
		}
		return valueNode;
	}

	private Optional<String> entityToTableName(final String entiyToApplyTo) {
		switch (entiyToApplyTo) {
			case "Module":
				return Optional.of("module");
			case "Annotation":
				return Optional.of("annotation");
			case "DataDictionaryEntry":
				return Optional.of("data_dictionary");
			case "Client":
				return Optional.of("client");
			case "Project":
				return Optional.of("project");
			case "Statement":
				return Optional.of("statement");
			case "SourceAttachment":
				return Optional.of("source_info");
			case "Taxonomy":
				return Optional.of("taxonomy");
			default:
				return Optional.empty();
		}
	}
}
