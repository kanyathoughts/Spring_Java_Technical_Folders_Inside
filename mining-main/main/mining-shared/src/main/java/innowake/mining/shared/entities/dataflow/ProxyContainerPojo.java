/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dataflow;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.KeyedSupplier;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.datalineage.DataFlowId;

/**
 * {@code proxy_container} entity class. Formerly known as {@code ProxyContainer}.
 */
public final class ProxyContainerPojo {

	private final UUID id;
	private final EntityId moduleId;
	private final DataFlowId dataFlowId;
	private final Optional<UUID> statement;
	private final Optional<ModuleLocation> statementLocation;
	private final KeyedSupplier<List<UUID>, List<DataFlowNodePojo>> fieldNodes;
	private final Type type;
	private final Map<String, Object> properties;

	public ProxyContainerPojo(final UUID id,
							  @JsonProperty("moduleEntity") final EntityId moduleId,
							  @JsonProperty("module") final UUID moduleUid,
							  @JsonProperty("moduleId") final Long moduleNid,
							  final DataFlowId dataFlowId,
							  @Nullable final UUID statement,
							  @Nullable final ModuleLocation statementLocation,
							  final KeyedSupplier<List<UUID>, List<DataFlowNodePojo>> fieldNodes,
							  final Type type,
							  final Map<String, Object> properties) {
		this.id = id;
		this.moduleId = moduleId != null ? moduleId : EntityId.of(moduleUid, moduleNid);
		this.dataFlowId = dataFlowId;
		this.statement = Optional.ofNullable(statement);
		this.statementLocation = Optional.ofNullable(statementLocation);
		this.type = type;
		this.properties = properties;
		this.fieldNodes = fieldNodes;
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

	public DataFlowId getDataFlowId() {
		return dataFlowId;
	}

	/**
	 * @return the {@link UUID} of the {{@code ast_node} statement this proxy container belongs to
	 */
	public Optional<UUID> getStatement() {
		return statement;
	}

	public Optional<ModuleLocation> getStatementLocation() {
		return statementLocation;
	}

	public List<UUID> getFieldNodesUids() {
		return fieldNodes.getKey();
	}

	public List<DataFlowNodePojo> getFieldNodes() {
		return fieldNodes.get();
	}

	public Type getType() {
		return type;
	}

	public Map<String, Object> getProperties() {
		return properties;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("module", moduleId)
				.append("statement", statement)
				.append("type", type)
				.append("properties", properties)
				.toString();
	}

	/**
	 * Defines generic types for proxy containers. Proxy containers define the "data interface" of Modules.
	 * The generic type is important when linking proxy containers between modules of different technologies.
	 */
	public enum Type {

		/**
		 * Proxy container containing fields that represent the contents of database table.
		 */
		DATABASE_TABLE,
		/**
		 * Proxy container containing fields that represent data that is written to or read from a database table.
		 */
		DATABASE_ACCESS,
		/**
		 * Proxy container representing an entry point into an executable program. The fields in the proxy container
		 * represent parameters that can be received by the entry point.
		 */
		ENTRY_POINT,
		/**
		 * Proxy container representing the invocation of another executable program. The fields in the proxy container
		 * are parameters used in the call.
		 */
		CALL,
		/**
		 * Not an "actual" ProxyContainer. Used to store information on whether we already executed certain methods
		 * on certain modules or nodes.
		 */
		FIELD_TRACING_META_DATA,
		/**
		 * Proxy container containing single field that represent file's binary data.
		 */
		RESOURCE_FILE,
		/**
		 * Proxy container representing file access in a COBOL program either via a file descriptor or EXEC CICS file commands
		 */
		FILE_ACCESS

	}

	/**
	 * Defines enum constants for common proxy container {@linkplain ProxyContainerPojo#getProperties() properties}.
	 */
	public enum Property {
		/**
		 * Property containing the name of the Module that is called by a call-like statement.
		 */
		TARGET_NAME,
		/**
		 * Property mapping the fields of the proxy container to positional arguments of the callee. This property defines which argument of the
		 * CALL is passed to which positional argument of the callee. Contains a list of parameter indexes
		 * that must be equal in length to the number of fields in the proxy container.
		 * <p>
		 * Example: a proxy container of type {@linkplain Type#CALL CALL} contains 3 fields {@code [A, B, C]} and
		 * its {@code CALL_PARAMETER_INDEX} propertyy is set to {@code [0, 2, 3]}. Now we are linking this proxy container to another of type
		 * {@linkplain Type#ENTRY_POINT ENTRY_POINT} which contains 5 fields. We would make the connections like so:
		 * <pre>
		 * Parameter Index | CALL | ENTRY_POINT
		 *          (0)       A   -->   A
		 *          (1)       -         B
		 *          (2)       B   -->   C
		 *          (3)       C   -->   D
		 *          (4)       -         E
		 * </pre>
		 */
		CALL_PARAMETER_INDEX,
		DATABASE_ACCESS_SQL_COLUMN_MAP,
		FIELD_TRACING_META_DATA_TRACE_CALLS_EXECUTED,
		FIELD_TRACING_META_DATA_TRACE_ENTRY_POINT_EXECUTED,
		FIELD_TRACING_META_DATA_TRACE_ALL_EXECUTED,
		FIELD_TRACING_META_DATA_LINK_PROXY_FIELDS_EXECUTED,
		FILE_ACCESS_FILE_NAME,
		FILE_ACCESS_COBOL_FD_NAME,
		FILE_ACCESS_FILE_ALIAS
	}
}
