/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Map;
import java.util.UUID;

import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;

/**
 * {@code statement} entity class.
 */
@MiningDataType(name = MiningEnitityNames.STATEMENT)
public final class StatementPojo extends MiningPojo {

	public static final String PROPERTY_KEY_SQL_LENGTH = "sqlLength";
	public static final String PROPERTY_KEY_TABLES = "tables";
	public static final String PROPERTY_KEY_DISTINCT_TABLES = "distinctTables";
	public static final String PROPERTY_KEY_CUSTOM_COMPLEXITY = "customComplexity";
	public static final String PROPERTY_KEY_HALSTEAD_COMPLEXITY = "halsteadComplexity";
	public static final String PROPERTY_KEY_HALSTEAD_DIFFICULTY = "halsteadDifficulty";
	
	public static final Map<String, String> SQL_PROPERTY_KEYS = Map.of(PROPERTY_KEY_SQL_LENGTH, "Length",
			PROPERTY_KEY_TABLES, "Tables",
			PROPERTY_KEY_DISTINCT_TABLES, "Distinct Tables",
			PROPERTY_KEY_CUSTOM_COMPLEXITY, "Custom Complexity",
			PROPERTY_KEY_HALSTEAD_COMPLEXITY, "Halstead Complexity",
			PROPERTY_KEY_HALSTEAD_DIFFICULTY, "Halstead Difficulty");

	private final Long nid;
	private final UUID module;
	@MiningDataPoint(displayName = "Technology")
	private final Technology technology;
	@MiningDataPoint(displayName = "Statement Type")
	@Usage(Usages.METRICS_CHART_DETAILS_SQL)
	private final StatementType type;
	@MiningDataPoint(displayName = "Text")
	private final String text;
	@Nullable private final Map<String, Object> properties;

	public StatementPojo(
			final UUID uid,
			final Long nid,
			final UUID module,
			final String technology,
			final String type,
			final String text,
			@Nullable final Map<String, Object> properties) {
		super(EntityId.of(uid, nid), CustomPropertiesMap.empty());
		this.nid = nid;
		this.module = module;
		this.technology = Technology.fromName(technology);
		this.type = StatementType.fromName(type);
		this.text = text;
		this.properties = properties;
	}

	/**
	 * @return the {@link UUID} of this statement.
	 */
	public UUID getModule() {
		return module;
	}

	/**
	 * @return the technology of this statement
	 */
	public Technology getTechnology() {
		return technology;
	}

	/**
	 * @return the type of this statement
	 */
	public StatementType getType() {
		return type;
	}

	/**
	 * @return the text of this statement
	 */
	public String getText() {
		return text;
	}

	/**
	 * @return the properties of the statement if available.
	 */
	@Nullable
	public Map<String, Object> getProperties() {
		return properties;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("nid", nid)
				.append("module", module)
				.append("technology", technology)
				.append("type", type)
				.append("text", text)
				.append("properties", properties)
				.toString();
	}
}
