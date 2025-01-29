package innowake.mining.shared.entities;

import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;

import java.util.List;

/**
 * Represents the details of a SQL query contained in a module. This data originates from dbcutter.
 */
@MiningDataType(name = MiningEnitityNames.SQL_DETAILS)
public class SqlDetailsPojo extends MiningPojo {
	private final String moduleHash;
	@MiningDataPoint(displayName = "Query", description = "The SQL query")
	@Usage(value = Usages.MINING_UI_REACHABILITY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "SQL Details")
	})
	private final String query;

	@MiningDataPoint(displayName = "Table Name", description = "The name of the table the query is executed on")
	@Usage(value = Usages.MINING_UI_REACHABILITY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "SQL Details")
	})
	private final String tableName;

	@MiningDataPoint(displayName = "Operation", description = "The type of the SQL query")
	@Usage(value = Usages.MINING_UI_REACHABILITY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "SQL Details")
	})
	private final String queryType;

	@MiningDataPoint(displayName = "Select Columns", description = "The columns selected in the query")
	@Usage(value = Usages.MINING_UI_REACHABILITY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "SQL Details")
	})
	private final List<String> nonconditional;

	@MiningDataPoint(displayName = "Conditional Columns", description = "The columns used in the WHERE clause of the query")
	@Usage(value = Usages.MINING_UI_REACHABILITY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "SQL Details")
	})
	@Usage(value = Usages.VIEW_MODE)
	private final List<String> conditional;

	public SqlDetailsPojo(final EntityId identity, final String moduleHash, final String query, final String tableName,
			final String queryType, final List<String> nonconditional, final List<String> conditional) {
		super(identity, CustomPropertiesMap.empty());
		this.moduleHash = moduleHash;
		this.query = query;
		this.tableName = tableName;
		this.queryType = queryType;
		this.nonconditional = nonconditional;
		this.conditional = conditional;
	}

	public String getModuleHash() {
		return moduleHash;
	}

	public String getQuery() {
		return query;
	}

	public String getTableName() {
		return tableName;
	}

	public String getQueryType() {
		return queryType;
	}

	public List<String> getNonconditional() {
		return nonconditional;
	}

	public List<String> getConditional() {
		return conditional;
	}
}
