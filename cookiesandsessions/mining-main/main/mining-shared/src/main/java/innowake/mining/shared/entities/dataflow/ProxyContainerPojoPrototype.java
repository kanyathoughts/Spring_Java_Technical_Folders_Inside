/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dataflow;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import innowake.mining.shared.entities.PojoPrototype;
import innowake.mining.shared.model.datalineage.DataFlowId;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * {@code proxy_container} entity class. Formerly know as {@code ProxyContainer}.
 */
public final class ProxyContainerPojoPrototype implements PojoPrototype {

	public final Definable<UUID> id = new Definable<>(false, "ProxyContainer.id");
	public final Definable<DataFlowId> dataFlowId = new Definable<>(false, "ProxyContainer.dataFlowId");
	public final Definable<EntityId> module = new Definable<>(false, "ProxyContainer.module");
	public final Definable<UUID> statement = new Definable<>(true, "ProxyContainer.statement");
	public final Definable<List<UUID>> fields = new Definable<>(true, "ProxyContainer.fields");
	public final Definable<ProxyContainerPojo.Type> type = new Definable<>(false, "ProxyContainer.type");
	public final Definable<Map<String, Object>> properties = new Definable<>(false, "ProxyContainer.properties");

	public ProxyContainerPojoPrototype setUid(final UUID id) {
		this.id.set(id);
		return this;
	}

	public ProxyContainerPojoPrototype setDataFlowId(final DataFlowId dataFlowId) {
		this.dataFlowId.set(dataFlowId);
		return this;
	}

	@JsonAlias("moduleId")
	public ProxyContainerPojoPrototype setModuleId(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	public ProxyContainerPojoPrototype setStatement(final UUID statement) {
		this.statement.set(statement);
		return this;
	}

	public ProxyContainerPojoPrototype setFields(final List<UUID> fields) {
		this.fields.set(fields);
		return this;
	}

	public ProxyContainerPojoPrototype setType(final ProxyContainerPojo.Type type) {
		this.type.set(type);
		return this;
	}

	public ProxyContainerPojoPrototype setProperties(final Map<String, Object> properties) {
		this.properties.set(properties);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id.orElse(null))
				.append("module", module.orElse(null))
				.append("statement", statement.orElse(null))
				.append("type", type.orElse(null))
				.append("properties", properties.orElse(null))
				.toString();
	}
}
