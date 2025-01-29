package innowake.mining.data.migration.postgres;

import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.access.postgres.PgUtil;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;

public class AstMigration extends PostgresSchemaMigrationFromOrient {
	
	public static final String PROPERTY_AST_MIGRATION = "mining.schema.migration.ast";

	public AstMigration(SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("ast_node_location_create");
		executePgScript("ast_node_table_create");

		if (Boolean.getBoolean(PROPERTY_AST_MIGRATION)) {
			migrateAstNodes();
			migrateAstSiblings();
			migrateAstRelationships();
			migrateAstModuleRelationships();
		} else {
			LOG.info("Skipping AST migration.");
		}

		executePgScript("ast_node_post_create");
	}

	@SuppressWarnings("unchecked")
	private void migrateAstNodes() {
		migrateData("AstNode",
				"SELECT @rid, " /* 1 */
					+ "moduleId, " /* 2 */
					+ "advancedModuleLocation.retracedOffset, " /* 3 */
					+ "advancedModuleLocation.retracedLength, " /* 4 */
					+ "advancedModuleLocation.assembledOffset, " /* 5 */
					+ "advancedModuleLocation.assembledLength, " /* 6 */
					+ "advancedModuleLocation.rootRelativeOffset, " /* 7 */
					+ "advancedModuleLocation.rootRelativeLength, " /* 8 */
					+ "advancedModuleLocation.rootRelativeStartLineNumber, " /* 9 */
					+ "advancedModuleLocation.rootRelativeEndLineNumber, " /* 10 */
					+ "parent.@rid, " /* 11 */
					+ "inclusionCalleeModuleId, " /* 12 */
					+ "type, " /* 13 */
					+ "superTypes, " /* 14 */
					+ "label, " /* 15 */
					+ "properties, " /* 16 */
					+ "out_HasAdditionalInfo.in.inputFileIds, " /* 17 JclControlFlowNodeMetadata */
					+ "out_HasAdditionalInfo.in.outputFileIds" /* 18 JclControlFlowNodeMetadata */
					+ " FROM AstNode",
				"INSERT INTO ast_node VALUES (?, " /* id from @rid */
					+ "(SELECT uid FROM module WHERE nid = ?), " /* moduleId */
					+ "CASE WHEN ? THEN (?,?,?,?,?,?,?,?)::ast_node_location ELSE null END, " /* location:: ast_node_location */
					+ "?, " /* uid from parent.@rid */
					+ "(SELECT uid FROM module WHERE nid = ?), null," /* inclusionCalleeModuleId */
					+ "?, " /* type */
					+ "?, " /* super types */
					+ "?, " /* label */
					+ "?)", /* properties */
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(1)));
					out.add(in.getLong(2));
					/* ast_node_location */
					out.add(in.getObject(3) != null);
					out.add(in.getInt(3));
					out.add(in.getInt(4));
					out.add(in.getInt(5));
					out.add(in.getInt(6));
					out.add(in.getInt(7));
					out.add(in.getInt(8));
					out.add(in.getInt(9));
					out.add(in.getInt(10));
					/* end ast_node_location */
					out.add(PgUtil.mapNullable(in.getString(11), id -> genUUIDv5(MiningEnitityNames.AST_NODE, id))); /* parent */
					out.add(in.getLong(12)); /* inclusionCalleeModuleId */
					out.add(in.getString(13)); /* type */
					final Collection<String> superTypes = (Collection<String>) in.getObject(14); /* superTypes */
					out.add(superTypes != null ? superTypes.toArray(new String[0]) : null);
					out.add(in.getString(15)); /* label */

					Map<String, Object> properties = (Map<String, Object>) in.getObject(16);
					final List<List<Long>> inputFileIds = (List<List<Long>>) in.getObject(17);
					if (inputFileIds != null) {
						if (properties == null) {
							properties = new HashMap<>();
						}
						AstService.Properties.FILE_IDS_IN.setIn(properties, inputFileIds.stream().flatMap(List::stream).collect(Collectors.toList()));
					}
					final List<List<Long>> outputFileIds = (List<List<Long>>) in.getObject(18);
					if (outputFileIds != null) {
						if (properties == null) {
							properties = new HashMap<>();
						}
						AstService.Properties.FILE_IDS_OUT.setIn(properties, outputFileIds.stream().flatMap(List::stream).collect(Collectors.toList()));
					}
					out.add(properties == null ? null : PgJSON.toPGobject(properties));

					return false;
				}));
	}
	
	private void migrateAstSiblings() {
		migrateData("AstSiblings",
				"SELECT children FROM AstNode WHERE children.size() > 0",
				"UPDATE ast_node SET sibling = ? WHERE id = ?",
				1000, (in, out, pass) -> {
					@SuppressWarnings("unchecked")
					final var siblings = (List<Object>) in.getObject(1);
					if ( ! siblings.isEmpty()) {
						out.add(pass + 1);
						out.add(genUUIDv5(MiningEnitityNames.AST_NODE, siblings.get(pass).toString()));
					}
					return pass + 1 < siblings.size();
				}
			);
	}

	private void migrateAstRelationships() {
		final String insertQuery = "INSERT INTO ast_relationship VALUES(?, ?, ?, ?::ast_relationship_type, ?)";
		migrateData("Redefines",
				"SELECT @rid, out.@rid, in.@rid FROM Redefines",
				insertQuery,
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.REDEFINES, in.getString(1)));
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(2)));
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(3)));
					out.add(AstRelationshipType.REDEFINES.name());
					out.add(null);
					return false;
				}));

		migrateData("RefersTo",
				"SELECT @rid, out.@rid, in.@rid FROM RefersTo",
				insertQuery,
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.REFERS_TO, in.getString(1)));
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(2)));
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(3)));
					out.add(AstRelationshipType.REFERS.name());
					out.add(null);
					return false;
				}));

		migrateData("FlowsControl", 
				"SELECT @rid, out.@rid, in.@rid, label FROM FlowsControl WHERE out.@class = 'AstNode' AND in.@class = 'AstNode'",
				insertQuery,
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.FLOWS_CONTROL, in.getString(1)));
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(2)));
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(3)));
					out.add(AstRelationshipType.FLOW.name());
					out.add(in.getString(4));
					return false;
				}));

		migrateData("AstBinding",
				"SELECT @rid, out, in, label FROM AstBinding",
				insertQuery,
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.AST_BINDING, in.getString(1)));
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(2)));
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(3)));
					out.add(AstRelationshipType.BINDING.name());
					out.add(in.getString(4));
					return false;
				}));
	}

	private void migrateAstModuleRelationships() {
		final String insertQuery = "INSERT INTO ast_module_relationship VALUES (?, ?::ast_module_relationship_type, (SELECT uid FROM module WHERE nid = ?))";

		migrateData("AstNode - hasAstModuleId",
				"SELECT hasAstModuleId, @rid FROM AstNode WHERE hasAstModuleId IS NOT Null",
				insertQuery,
				1000, ((in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(2)));
					out.add(AstModuleRelationshipType.ROOT.name());
					out.add(in.getLong(1));
					return false;
				}));
		
		migrateData("EntryPoint", 
				"SELECT moduleId, out_FlowsControl.in.@rid as out_FlowsControl FROM EntryPoint WHERE moduleId IS not Null AND out_FlowsControl is NOT Null UNWIND out_FlowsControl",
				insertQuery,
				1000, (in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(2)));
					out.add(AstModuleRelationshipType.ENTRY.name());
					out.add(in.getLong(1));
					return false;
				});
		
		migrateData("HaltPoint",
				"SELECT moduleId, in_FlowsControl.out.@rid as in_FlowsControl FROM HaltPoint WHERE moduleId IS not Null AND in_FlowsControl is NOT Null UNWIND in_FlowsControl",
				insertQuery,
				1000, (in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(2)));
					out.add(AstModuleRelationshipType.HALT.name());
					out.add(in.getLong(1));
					return false;
				});
		
		migrateData("ReturnPoint",
				"SELECT moduleId, in_FlowsControl.out.@rid as in_FlowsControl FROM ReturnPoint WHERE moduleId IS not Null AND in_FlowsControl is NOT Null UNWIND in_FlowsControl",
				insertQuery,
				1000, (in, out, pass) -> {
					out.add(genUUIDv5(MiningEnitityNames.AST_NODE, in.getString(2)));
					out.add(AstModuleRelationshipType.RETURN.name());
					out.add(in.getLong(1));
					return false;
				});
	}
	
}
