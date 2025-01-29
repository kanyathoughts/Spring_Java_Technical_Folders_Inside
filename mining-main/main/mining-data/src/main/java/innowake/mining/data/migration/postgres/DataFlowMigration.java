package innowake.mining.data.migration.postgres;

import com.orientechnologies.orient.core.id.ORecordId;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.UUID;

public class DataFlowMigration extends PostgresSchemaMigrationFromOrient {

	public DataFlowMigration(SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("data_flow_table_create");

		if (Boolean.getBoolean(AstMigration.PROPERTY_AST_MIGRATION)) {
			migrateProxyContainers();
			migrateDataFlowNodes();
			migrateProxyContainerFields();
			migrateDataFlowErrors();
			migrateDataFlowRelationships();
		} else {
			LOG.info("Skipping DataFlow migration as AST was not migrated.");
		}
	}

	private void migrateProxyContainers() {
		migrateData("ProxyContainer",
				"SELECT @rid, moduleId, statement.@rid, type, properties FROM ProxyContainer",
				"INSERT INTO proxy_container VALUES(?, (SELECT uid FROM module WHERE nid = ?), ?, ?, ?)",
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.PROXY_CONTAINER, in.getString(1)));
					out.add(in.getLong(2));
					out.add(getNullableId(MiningEnitityNames.AST_NODE, in.getString(3)));
					out.add(in.getString(4));
					out.add(PgJSON.toPGobject(in.getObject(5)));
					return false;
				}));
	}

	private void migrateDataFlowNodes() {
		migrateData("DataFlowNode",
				"SELECT @rid, moduleId, astNode.@rid, proxyContainer.@rid, name, traced, type FROM DataFlowNode",
				"INSERT INTO data_flow_node VALUES(?, (SELECT uid FROM module WHERE nid = ?), ?, ?, ?, ?, ?)",
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.DATA_FLOW_NODE, in.getString(1)));
					out.add(in.getLong(2));
					out.add(getNullableId(MiningEnitityNames.AST_NODE, in.getString(3)));
					out.add(getNullableId(MiningEnitityNames.PROXY_CONTAINER, in.getString(4)));
					out.add(in.getString(5));
					out.add(in.getBoolean(6));
					out.add(in.getString(7));
					return false;
				}));
	}

	private void migrateProxyContainerFields() {
		migrateData("ProxyContainer",
				"SELECT @rid, fields.@rid as fields FROM ProxyContainer WHERE fields.size() > 0",
				"INSERT INTO proxy_container_field VALUES(?, ?, ?)",
				1000, ((in, out, pass) -> {
					@SuppressWarnings("unchecked")
					final var fields = (List<String>) in.getObject(2);
					if ( ! fields.isEmpty()) {
						out.add(genUUIDv5(MiningEnitityNames.PROXY_CONTAINER, in.getString(1)));	/* proxy container id */
						out.add(genUUIDv5(MiningEnitityNames.DATA_FLOW_NODE, fields.get(pass)));	/* data flow node id */
						out.add(pass + 1);															/* ordinal */
					}
					return pass + 1 < fields.size();
				}));
	}

	private void migrateDataFlowErrors() {
		migrateData("DataFlowNodeErrors", 
				"SELECT @rid, errors.severity as severity, errors.text FROM DataFlowNode WHERE errors != null AND errors.size() != 0 UNWIND severity",
				"INSERT INTO data_flow_error VALUES(?, ?, ?)",
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.DATA_FLOW_NODE, in.getString(1)));
					out.add(in.getString(2));
					out.add(in.getString(3));
					return false;
				}));
	}

	private void migrateDataFlowRelationships() {
		final String insertQuery = "INSERT INTO data_flow_node_relationship VALUES(?, ?, ?::data_flow_node_relationship_type)";

		migrateData("DataFlowNode - ProxyShortcut", 
				"SELECT @rid, out_proxyShortcuts.@rid FROM DataFlowNode WHERE out_proxyShortcuts != null AND out_proxyShortcuts.size() != 0",
				insertQuery,
				1000,
				((in, out, pass) -> migrateRelationship(in, out, pass, DataFlowNodeRelationshipType.PROXY_SHORTCUT)));

		migrateData("DataFlowNode - ReadAccess", 
				"SELECT @rid, readAccesses.@rid FROM DataFlowNode WHERE readAccesses != null AND readAccesses.size() != 0",
				insertQuery,
				1000,
				((in, out, pass) -> migrateRelationship(in, out, pass, DataFlowNodeRelationshipType.READ_ACCESS)));

		migrateData("DataFlowNode - WriteAccess",
				"SELECT @rid, writeAccesses.@rid FROM DataFlowNode WHERE writeAccesses != null AND writeAccesses.size() != 0 ",
				insertQuery,
				1000,
				((in, out, pass) -> migrateRelationship(in, out, pass, DataFlowNodeRelationshipType.WRITE_ACCESS)));

		migrateData("DataFlowNode - RelatedFields",
				"SELECT @rid, relatedFields.@rid FROM DataFlowNode WHERE relatedFields != null AND relatedFields.size() != 0",
				insertQuery,
				1000,
				((in, out, pass) -> migrateRelationship(in, out, pass, DataFlowNodeRelationshipType.RELATED_FIELD)));
	}

	@SuppressWarnings("unchecked")
	private boolean migrateRelationship(final ResultSet in, final List<Object> out, final int pass, final DataFlowNodeRelationshipType type) throws SQLException {
		out.add(genUUIDv5(MiningEnitityNames.DATA_FLOW_NODE, in.getString(1)));

		final List<ORecordId> targets = (List<ORecordId>) in.getObject(2);
		out.add(genUUIDv5(MiningEnitityNames.DATA_FLOW_NODE, targets.get(pass).toString()));
		out.add(type.toString());
		return pass < targets.size();
	}

	private UUID getNullableId(final String entity, @Nullable final String value) {
		return value != null ? genUUIDv5(entity, value) : null;
	}
}