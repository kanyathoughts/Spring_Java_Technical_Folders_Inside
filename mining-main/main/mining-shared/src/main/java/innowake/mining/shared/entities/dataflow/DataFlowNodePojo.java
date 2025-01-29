/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dataflow;

import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.KeyedSupplier;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.datalineage.DataFlowId;

/**
 * {@code data_flow_node} entity class. Formerly know as {@code DataFlowNode}.
 * <p>
 * A data flow node either represents a data field or a statement in a program or an interface that represents a location through which
 * data can flow in and out of a module. A data flow node is linked to other nodes via read or write accesses or a "related field" relationship.
 */
public final class DataFlowNodePojo {

	private final UUID id;
	private final DataFlowId dataFlowId;
	private final EntityId moduleId;
	private final Optional<KeyedSupplier<UUID, AstNodePojo>> astNode;
	private final Optional<ModuleLocation> location;
	private final Optional<KeyedSupplier<UUID, ProxyContainerPojo>> proxyContainer;
	private final String name;
	private final boolean traced;
	private final Type type;

	public DataFlowNodePojo(final UUID id,
							final DataFlowId dataFlowId,
							@JsonProperty("moduleEntity") final EntityId moduleId,
							@JsonProperty("module") final UUID moduleUid,
							@JsonProperty("moduleId") final Long moduleNid,
							@Nullable final Optional<KeyedSupplier<UUID, AstNodePojo>> astNode,
							@Nullable final ModuleLocation location,
							@Nullable final Optional<KeyedSupplier<UUID, ProxyContainerPojo>> proxyContainer,
							final String name,
							final boolean traced,
							final Type type) {
		this.id = id;
		this.moduleId = moduleId != null ? moduleId : EntityId.of(moduleUid, moduleNid);
		this.dataFlowId = dataFlowId;
		this.astNode = astNode;
		this.location = Optional.ofNullable(location);
		this.proxyContainer = proxyContainer;
		this.name = name;
		this.traced = traced;
		this.type = type;
	}

	public UUID getId() {
		return id;
	}

	@JsonIgnore
	public EntityId getModuleId() {
		return moduleId;
	}

	@JsonProperty("module")
	public UUID getModuleUid() {
		return moduleId.getUid();
	}

	@JsonProperty("moduleId")
	public Long getModuleNid() {
		return moduleId.getNid();
	}

	public Optional<UUID> getAstNodeId() {
		return astNode.map(KeyedSupplier::getKey);
	}

	public Optional<AstNodePojo> getAstNode() {
		return astNode.map(KeyedSupplier::get);
	}

	public Optional<ModuleLocation> getLocation() {
		return location;
	}

	public Optional<UUID> getProxyContainerId() {
		return proxyContainer.map(KeyedSupplier::getKey);
	}

	public Optional<ProxyContainerPojo> getProxyContainer() {
		return proxyContainer.map(KeyedSupplier::get);
	}

	public String getName() {
		return name;
	}

	public boolean isTraced() {
		return traced;
	}

	public Type getType() {
		return type;
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("id", id);
		builder.append("dataFlowId", dataFlowId);
		builder.append("module", moduleId);
		builder.append("astNode", astNode);
		builder.append("proxyContainer", proxyContainer);
		builder.append("name", name);
		builder.append("traced", traced);
		builder.append("type", type);
		return builder.toString();
	}

	public DataFlowId getDataFlowId() {
		return dataFlowId;
	}

	/**
	 * The type of data flow node.
	 */
	public enum Type {
		/**
		 * The node represents a data field. Read accesses are outgoing edges, write accesses are incoming edges. May have related fields.
		 */
		FIELD,
		/**
		 * The node represents a statement. Read accesses are incoming edges, write accesses are outgoing edges.
		 */
		STATEMENT,
		/**
		 * A virtual field that represents an input or output interface of a module. May have related fields.
		 */
		PROXY_FIELD
	}
}
