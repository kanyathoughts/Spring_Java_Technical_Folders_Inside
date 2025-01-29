/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dataflow;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.PojoPrototype;
import innowake.mining.shared.model.datalineage.DataFlowId;

/**
 * {@code proxy_container} entity class. Formerly know as {@code ProxyContainer}.
 */
public final class DataFlowNodePojoPrototype implements PojoPrototype {

	public final Definable<UUID> id = new Definable<>(false, "DataFlowNode.id");
	public final Definable<DataFlowId> dataFlowId = new Definable<>(false, "DataFlowNode.dataFlowId");

	public final Definable<EntityId> module = new Definable<>(false, "DataFlowNode.module");
	public final Definable<UUID> astNode = new Definable<>(true, "DataFlowNode.astNode");
	public final Definable<UUID> proxyContainer = new Definable<>(true, "DataFlowNode.proxyContainer");
	public final Definable<String> name = new Definable<>(false, "DataFlowNode.name");
	public final Definable<Boolean> traced = new Definable<>(false, "DataFlowNode.traced");
	public final Definable<DataFlowNodePojo.Type> type = new Definable<>(false, "DataFlowNode.type");

	public DataFlowNodePojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}

	public DataFlowNodePojoPrototype setDataFlowId(final DataFlowId dataFlowId) {
		this.dataFlowId.set(dataFlowId);
		return this;
	}

	@JsonAlias("moduleId")
	public DataFlowNodePojoPrototype setModuleId(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	public DataFlowNodePojoPrototype setAstNode(final UUID astNode) {
		this.astNode.set(astNode);
		return this;
	}

	public DataFlowNodePojoPrototype setProxyContainer(final UUID proxyContainer) {
		this.proxyContainer.set(proxyContainer);
		return this;
	}

	public DataFlowNodePojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	public DataFlowNodePojoPrototype setTraced(final Boolean traced) {
		this.traced.set(traced);
		return this;
	}

	public DataFlowNodePojoPrototype setType(final DataFlowNodePojo.Type type) {
		this.type.set(type);
		return this;
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("id", id.orElse(null));
		builder.append("dataFlowId", dataFlowId.orElse(null));
		builder.append("module", module.orElse(null));
		builder.append("astNode", astNode.orElse(null));
		builder.append("proxyContainer", proxyContainer.orElse(null));
		builder.append("name", name.orElse(null));
		builder.append("traced", traced.orElse(null));
		builder.append("type", type.orElse(null));
		return builder.toString();
	}
}
