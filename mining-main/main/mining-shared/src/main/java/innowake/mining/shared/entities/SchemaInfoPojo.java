/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * View on {@code module} entities of type 'SCHEMA' with DB schema information.
 */
@MiningDataType(name = MiningEnitityNames.SCHEMA_INFO)
public class SchemaInfoPojo {

	public final UUID moduleUid;
	public final Long moduleId;
	public final String name;
	public final String technology;
	public final Integer tables;
	public final Integer views;
	public final Integer procedures;
	public final Integer triggers;

	public SchemaInfoPojo(
			@JsonProperty("moduleUid") final UUID moduleUid,
			@JsonProperty("moduleId") final Long moduleId,
			@JsonProperty("name") final String name,
			@JsonProperty("technology") final String technology,
			@JsonProperty("tables") final Integer tables,
			@JsonProperty("views") final Integer views,
			@JsonProperty("procedures") final Integer procedures,
			@JsonProperty("triggers") final Integer triggers) {

		this.moduleUid = moduleUid;
		this.moduleId = moduleId;
		this.name = name;
		this.technology = technology;
		this.tables = tables;
		this.views = views;
		this.procedures = procedures;
		this.triggers = triggers;
	}

	public UUID getModuleUid() {
		return moduleUid;
	}

	public Long getModuleId() {
		return moduleId;
	}

	public String getName() {
		return name;
	}

	public String getTechnology() {
		return technology;
	}

	public Integer getTables() {
		return tables;
	}

	public Integer getViews() {
		return views;
	}

	public Integer getProcedures() {
		return procedures;
	}

	public Integer getTriggers() {
		return triggers;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("moduleUid", moduleUid)
				.append("moduleId", moduleId)
				.append("name", name)
				.append("technology", technology)
				.append("tables", tables)
				.append("views", views)
				.append("procedures", procedures)
				.append("triggers", triggers)
				.toString();
	}
}