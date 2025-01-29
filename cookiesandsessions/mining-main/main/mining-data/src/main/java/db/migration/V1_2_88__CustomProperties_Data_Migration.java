/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package db.migration;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.PreparedStatementCallback;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;
import org.springframework.jdbc.support.JdbcUtils;

import com.orientechnologies.orient.core.id.ORecordId;

import db.migration.model.PropertyMetadata;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.StatementBuilder;

/**
 * Fly-way migration script to migrate custom property data
 */
public class V1_2_88__CustomProperties_Data_Migration extends BaseJavaMigration {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);

	private static final String NEW_CUSTOM_PROPERTY_CLASS_SUFFIX = "CustomProperties";
	private static final String ALTER_PROPERTY_QUERY_PREFIX = "ALTER PROPERTY ";
	private static final String CUSTOM_PROPERTIES_UPDATE_QUERY_PREFIX = "UPDATE CustomProperties SET ";
	private static final String CUSTOM_PROPERTIES_UPDATE_WHERE_CLAUSE = " WHERE @rid IN (SELECT OUT('HasAdditionalInfo')[@class = ?] FROM ?)";
	private static final String CUSTOM_PROPERTIES_CREATE_EDGE_QUERY = "CREATE EDGE HasAdditionalInfo FROM ? TO ?";

	private static final String BASE_SQL_STATEMENT = "select @rid, "
			+ Arrays.stream(Property.values()).map(Property::getPropertyName).collect(Collectors.joining(","))
			+ " from ( select expand(properties) "
			+ "       from ( select expand(classes) "
			+ "           from metadata:schema ) "
			+ "       where name=? ) "
			+ "where not (customFields is null) ";


	private enum Property {
		NAME("name"),
		MIN("min"),
		READONLY("readonly"),
		NOTNULL("notNull"),
		MAX("max"),
		DEFAULTVALUE("defaultValue"),
		DESCRIPTION("description"),
		TYPE("type"),
		MANDATORY("mandatory"),
		LINKEDCLASS("linkedClass"),
		LINKEDTYPE("linkedType"),
		CUSTOMFIELDS("customFields");

		private final String propertyName;

		Property(final String propertyName) {
			this.propertyName = propertyName;
		}

		public String getPropertyName() {
			return propertyName;
		}
	}

	private final List<String> sqlReadOnlyMigrationScript = new ArrayList<>();
	private final List<String> sqlDeletePropertyScript = new ArrayList<>();

	private static final List<String> CLASS_NAMES_TO_BE_MIGRATED = Arrays.asList(
			"Client",
			"Project",
			"SourceAttachment",
			"Module",
			"Statement",
			"Annotation",
			"AnnotationCategory",
			"AnnotationType",
			"DataDictionaryEntry",
			"Taxonomy",
			"TaxonomyEnum"
	);

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));
		/*Migrate custom property for each tables*/
		for (final String entityClassName : CLASS_NAMES_TO_BE_MIGRATED) {
			sqlReadOnlyMigrationScript.clear();
			sqlDeletePropertyScript.clear();
			final List<PropertyMetadata> lsPropertiesWithCustomFields = jdbcTemplate.query(BASE_SQL_STATEMENT, CustomPropertyMetaRowMapper.INSTANCE, entityClassName);
			LOG.info(() -> String.format("Migrating %s class with %d custom fields.", entityClassName, lsPropertiesWithCustomFields.size()));
			if ( ! lsPropertiesWithCustomFields.isEmpty()) {

				final String customPropertyClassName = entityClassName + NEW_CUSTOM_PROPERTY_CLASS_SUFFIX;

				jdbcTemplate.execute("CREATE CLASS " + customPropertyClassName + " IF NOT EXISTS EXTENDS CustomProperties");

				migrateSchema(jdbcTemplate, entityClassName, lsPropertiesWithCustomFields);

				/*Add CustomProperties class to all projects since previously the custom properties were global*/
				jdbcTemplate.execute("UPDATE Project SET customPropertyClasses[\"" + entityClassName + "\"] = [\"" + customPropertyClassName + "\"]");

				migrateData(jdbcTemplate, entityClassName, customPropertyClassName, lsPropertiesWithCustomFields);

				executeScript(jdbcTemplate, sqlReadOnlyMigrationScript);

				executeScript(jdbcTemplate, sqlDeletePropertyScript);
			}

		}

	}

	private void migrateSchema(final JdbcTemplate jdbcTemplate, final String entityClassName, final List<PropertyMetadata> lsPropertiesWithCustomFields ) {
		for (final PropertyMetadata property : lsPropertiesWithCustomFields) {
			final String classPropertyName = entityClassName + NEW_CUSTOM_PROPERTY_CLASS_SUFFIX + '.' + property.getName();
			final String entityClassPropertyName = entityClassName + '.' + property.getName();

			/* Create property*/
			String sqlCreateProperty = "CREATE PROPERTY " + classPropertyName + " IF NOT EXISTS ";
			switch (property.getType()) {
				case REFERENCE: {
					sqlCreateProperty += "LINK " + property.getLinkedClass();
					break;
				}
				case LINKLIST: {
					sqlCreateProperty += "LINKLIST " + property.getLinkedClass();
					break;
				}
				case EMBEDDEDLIST: {
					final String link = (null != property.getLinkedClass()) ? property.getLinkedClass() : property.getLinkedType().name();
					sqlCreateProperty += "EMBEDDEDLIST " + link;
					break;
				}
				default:
					sqlCreateProperty += property.getType().name();
					break;
			}
			jdbcTemplate.execute(sqlCreateProperty);

			/* Migrate property attributes.*/
			final String customPropertyAlterSql = ALTER_PROPERTY_QUERY_PREFIX + classPropertyName;
			jdbcTemplate.execute(customPropertyAlterSql + " MANDATORY " + property.isMandatory());
			jdbcTemplate.execute(ALTER_PROPERTY_QUERY_PREFIX + entityClassPropertyName + " MANDATORY FALSE");

			/* Execute READONLY scripts after migrating data else data migration will fail*/
			sqlReadOnlyMigrationScript.add(customPropertyAlterSql + " READONLY " + property.isReadOnly());
			jdbcTemplate.execute(ALTER_PROPERTY_QUERY_PREFIX + entityClassPropertyName + " READONLY FALSE");

			jdbcTemplate.execute(customPropertyAlterSql + " NOTNULL " + property.isNotNull());
			jdbcTemplate.execute(ALTER_PROPERTY_QUERY_PREFIX + entityClassPropertyName + " NOTNULL FALSE");


			if (null != property.getDefaultValue())
				jdbcTemplate.execute(customPropertyAlterSql + " DEFAULT " + property.getDefaultValue());
			if (null != property.getMin())
				jdbcTemplate.execute(customPropertyAlterSql + " MIN " + property.getMin());
			if (null != property.getMax())
				jdbcTemplate.execute(customPropertyAlterSql + " MAX " + property.getMax());
			if (null != property.getDescription())
				jdbcTemplate.execute(customPropertyAlterSql + " DESCRIPTION \"" + property.getDescription() + "\"");

			/* Migrate customFields.*/
			final Iterator<Entry<Object, Object>> customFieldsIterator = property.getCustomFields().entrySet().iterator();
			while (customFieldsIterator.hasNext()) {
				final Entry<Object, Object> pair = customFieldsIterator.next();
				jdbcTemplate.execute(customPropertyAlterSql + " CUSTOM " + pair.getKey() + "='" + pair.getValue() + "'");
			}
			/* Custom Properties to be deleted after migrating data*/
			sqlDeletePropertyScript.add("UPDATE " + entityClassName + " REMOVE " + property.getName());
			sqlDeletePropertyScript.add("DROP PROPERTY " + entityClassPropertyName + " FORCE");
		}
	}

	private void migrateData(final JdbcTemplate jdbcTemplate, final String entityClassName, final String customPropertyClassName,
							 final List<PropertyMetadata> lsPropertiesWithCustomFields) {
		final List<String> customProperties = lsPropertiesWithCustomFields.stream().map(PropertyMetadata::getName).collect(Collectors.toList());

		final List<Map<String, Object>> customPropertyValues = jdbcTemplate.queryForList(
				"SELECT @Rid,"
						+ StringUtils.join(customProperties, ',')
						+ " FROM "
						+entityClassName
						+" WHERE "
						+ StringUtils.join(customProperties, " IS NOT NULL OR ")
						+" IS NOT NULL");

		for (final Map<String, Object> customPropertyValue : customPropertyValues) {
			final StatementBuilder stmtBuilder = new StatementBuilder();
			for (final Map.Entry<String, Object> entry : customPropertyValue.entrySet()) {
				final String columnName = entry.getKey();
				final Object value = entry.getValue();
				if ( ! columnName.equals("@Rid") && null != value) {
					stmtBuilder.addAttribute(columnName + " = ?", value);
				}
			}

			final StringBuilder sqlUpdate = new StringBuilder(CUSTOM_PROPERTIES_UPDATE_QUERY_PREFIX)
					.append(stmtBuilder.attributes())
					.append(CUSTOM_PROPERTIES_UPDATE_WHERE_CLAUSE);			
			final List<Object> arguments = new ArrayList<>(stmtBuilder.values());
			arguments.add(customPropertyClassName);
			arguments.add(customPropertyValue.get("@Rid"));
			final int updatedCount = jdbcTemplate.update(sqlUpdate.toString(), arguments.toArray(new Object[0]));

			if (updatedCount == 0) {
				final StringBuilder sqlInsert = new StringBuilder("INSERT INTO ")
						.append(customPropertyClassName)
						.append(" SET ")
						.append(stmtBuilder.attributes())
						.append(" RETURN @rid");				
				final String ridCustomProperty = jdbcTemplate.execute(sqlInsert.toString(), new PreparedStatementCallback<String>() {
					@Override
					public @Nullable String doInPreparedStatement(final PreparedStatement ps) throws SQLException, DataAccessException {
						stmtBuilder.init(ps);
						ResultSet rs = null;
						try {
							if (ps.execute()) {
							    rs = ps.getResultSet();
								if (rs.getMetaData().getColumnCount() == 0) {
									throw new IllegalStateException("ResultSet column count must not be 0");
								}
								return rs.getString(1);
							} else {
								throw new IllegalStateException("There must be a ResultSet when inserting CustomProperties.");
							}
						} finally {
							JdbcUtils.closeResultSet(rs);
						}
					}
				});
				jdbcTemplate.update(CUSTOM_PROPERTIES_CREATE_EDGE_QUERY, customPropertyValue.get("@Rid"), new ORecordId(ridCustomProperty));
			}
		}

	}

	private void executeScript(final JdbcTemplate jdbcTemplate,final  List<String> sqlScripts) {
		for (final String sql : sqlScripts) {
			jdbcTemplate.execute(sql);
		}
	}

	private static class CustomPropertyMetaRowMapper implements RowMapper<PropertyMetadata> {

		public static final CustomPropertyMetaRowMapper INSTANCE = new CustomPropertyMetaRowMapper();

		@SuppressWarnings("unchecked")
		@Override
		public @Nullable PropertyMetadata mapRow(@Nullable final ResultSet resultSet, final int rowNum) throws SQLException {
			if (resultSet == null) {
				return null;
			}

			final PropertyMetadata meta = new PropertyMetadata();
			meta.setName(resultSet.getString(Property.NAME.getPropertyName()));
			meta.setType((Integer) resultSet.getObject(Property.TYPE.getPropertyName()));
			meta.setDescription(resultSet.getString(Property.DESCRIPTION.getPropertyName()));
			meta.setMandatory((Boolean) resultSet.getObject(Property.MANDATORY.getPropertyName()));
			meta.setMin(resultSet.getString(Property.MIN.getPropertyName()));
			meta.setMax(resultSet.getString(Property.MAX.getPropertyName()));
			meta.setReadOnly((Boolean) resultSet.getObject(Property.READONLY.getPropertyName()));
			meta.setNotNull((Boolean) resultSet.getObject(Property.NOTNULL.getPropertyName()));
			meta.setLinkedClass(resultSet.getString(Property.LINKEDCLASS.getPropertyName()));
			meta.setDefaultValue(resultSet.getString(Property.DEFAULTVALUE.getPropertyName()));
			meta.setLinkedType((Integer) resultSet.getObject(Property.LINKEDTYPE.getPropertyName()));
			meta.setCustomFields((Map<Object, Object>) resultSet.getObject(Property.CUSTOMFIELDS.getPropertyName()));
			return meta;
		}

	}
}
