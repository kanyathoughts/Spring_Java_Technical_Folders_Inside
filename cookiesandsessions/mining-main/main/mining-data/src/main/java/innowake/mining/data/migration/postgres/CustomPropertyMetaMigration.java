/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.google.gson.Gson;
import com.orientechnologies.orient.core.sql.executor.OResult;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.CustomPropertiesPgDao;
import innowake.mining.data.access.postgres.PgUtil;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.CustomPropertyDataType;

/**
 * Migrates Custom Property metadata (CustomProperties classes in Orient) to Postgres.
 */
public class CustomPropertyMetaMigration extends PostgresSchemaMigrationFromOrient {
	
	private static final String CP_QUERY = "select name, customFields, properties from (select expand(classes) from metadata:schema) where superClass = 'CustomProperties'";

	public CustomPropertyMetaMigration(final SchemaMigrationContext context) {
		super(context);
	}
	
	@Nullable
	private Object tryConvert(@Nullable final String in) {
		if (in == null) {
			return null;
		}
		
		Object value;
		if ("true".equalsIgnoreCase(in)) {
			value = true;
		} else if ("false".equalsIgnoreCase(in)) {
			value = false;
		} else {
			try {
				value = Integer.parseInt(in);
			} catch (final NumberFormatException errInt) {
				try {
					value = Double.parseDouble(in);
				} catch (final NumberFormatException errDbl) {
					value = in; 
				}
			}
		}
		return value;
	}
	
	private Map<String, Object> tryConvert(final Map<String, String> in) {
		final var out = new HashMap<String, Object>();
		for (final var entry : in.entrySet()) {
			out.put(entry.getKey(), tryConvert(entry.getValue()));
		}
		return out;
	}
	
	@Override
	public void migrate() throws SQLException {
		executePgScript("custom_properties_meta_tables_create");
		
		if (isOrientAvailable()) {
			validateProjectIds();

			final CustomPropertiesPgDao cpDao = new CustomPropertiesPgDao(dbPostgres);
			final String[] basicProps = new String[] { "description", "mandatory", "notNull", "readonly", "defaultValue", "min", "max" };
			final Gson gson = new Gson();
			
			try (final var st = getOrientDB().createStatement()) {
				final ResultSet rs = st.executeQuery(CP_QUERY);
				while (rs.next()) {
					/* class */
					final Long projectId = getProjectId(rs);
					final UUID propParent = cpDao.createProperty(EntityId.of(projectId), rs.getString(1), null, 0, Collections.emptyMap());
					
					/* class properties */
					int propIdx = 0;
					@SuppressWarnings("unchecked") final var orientProps = (Collection<OResult>) rs.getObject(3);
					for (final var orientProp : orientProps) {
						final var properties = new HashMap<String, Object>();
						for (final var prop : basicProps) {
							properties.compute(prop, (k, v) -> orientProp.getProperty(k));
						}
						
						final var orientType = CustomPropertyDataType.fromOrientType((Integer) orientProp.getProperty("type"));
						var linkedType = PgUtil.mapNullable(orientProp.getProperty("linkedType"), t -> CustomPropertyDataType.fromOrientType((Integer) t));
						if (linkedType == null && (orientType == CustomPropertyDataType.EMBEDDEDLIST || orientType == CustomPropertyDataType.EMBEDDEDMAP)
								|| linkedType == CustomPropertyDataType.EMBEDDEDLIST || linkedType == CustomPropertyDataType.EMBEDDEDMAP) {
							linkedType = CustomPropertyDataType.STRING;
						}
						properties.put("dataType", linkedType != null ? linkedType.name() : orientType.name());
						if (linkedType != null) {
							properties.put("collectionType", orientType.name());
						}
						
						@SuppressWarnings("unchecked") final var orientFieldProps = (Map<String, String>) orientProp.getProperty("customFields");
						for (final var prop : orientFieldProps.entrySet()) {
							final Object val;
							if ("customViewNames".equals(prop.getKey())) {
								val = gson.fromJson(prop.getValue(), List.class);
					 		} else if ("showWhen".equals(prop.getKey())) {
					 			@SuppressWarnings("unchecked") final Map<String, String> m = gson.fromJson(prop.getValue(), Map.class);
					 			val = tryConvert(m);
							} else if ("pluginVisible".equals(prop.getKey())) {
								val = Boolean.parseBoolean(prop.getValue());
							} else if ("customViewIndex".equals(prop.getKey())) {
								val = Integer.parseInt(prop.getValue());
							} else {
								val = prop.getValue();
							}
							properties.put(prop.getKey(), val);
						}
						
						cpDao.createProperty(EntityId.of(projectId), (String) orientProp.getProperty("name"), propParent, ++propIdx, properties);
					}
				}
			}
		} else {
			LOG.info("No OrientDB available, not migrating custom properties.");
		}
		
		executePgScript("custom_properties_project_assignments_move");
	}

	private void validateProjectIds() throws SQLException {
		try (final ResultSet rsP = dbPostgres.createStatement().executeQuery("SELECT nid FROM project;");
			 final ResultSet rsO = getOrientDB().createStatement().executeQuery(CP_QUERY)) {

			final Set<Long> pgIds = new HashSet<>();
			while (rsP.next()) {
				pgIds.add(Long.valueOf(rsP.getLong(1)));
			}

			final List<String> errors = new ArrayList<>();
			while (rsO.next()) {
				final Long projectId = getProjectId(rsO);

				if ( ! pgIds.contains(projectId)) {
					errors.add( "    Custom property class: " + rsO.getString(1) + " -> project nid: " + projectId.toString());
				}
			}

			if ( ! errors.isEmpty()) {
				throw new SQLException("The Postgres migration can not be continued. There are custom property classes in your OrientDB which have a "
						+ "link to project(s) that don't exist anymore. The problematic cases are:\n\n" 
						+ errors.stream().collect(Collectors.joining("\n"))
						+ "\n\nYou either have to delete the listed custom propertx classes in OrientDB or manually create the project(s) for the listed nids in Postgres."
						+ " When done restart the mining-api-server.");
			}
		}
	}

	private Long getProjectId(final ResultSet rs) throws SQLException {
		@SuppressWarnings("unchecked") final var orientClsProps = (Map<String, String>) rs.getObject(2);
		final String owner = orientClsProps != null ? orientClsProps.get("ownerProject") : null;
		return owner == null ? 0L : Long.parseLong(owner);
	}
}
