/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.ModuleLocation;

import java.util.Objects;

/**
 * ID for {@link DataFlowNodePojo}
 */
public class DataFlowId {

	private final String id;

	/**
	 * Creates data flow id from the given id.
	 * @param id the data flow id
	 */
	public DataFlowId(final String id) {
		this.id = id;
	}

	/**
	 * Creates a new {@link DataFlowId} instance for the given {@code id}.
	 * <p>Method must be used only by entity PgDaos when loading DataFlowIds strings from DB.</p>
	 * 
	 * @param id id of the DataFlowNode from DB
	 * @return {@link DataFlowId} for the given {@code id}
	 */
	public static DataFlowId fromDb(final String id) {
		return new DataFlowId(id);
	}

	/**
	 * Creates a {@link DataFlowId} for regular DataFlowNodes, i.e. fields and statements.
	 * <p>
	 * A field or statement is uniquely identified by the containing {@code moduleId}, the {@code type}
	 * of the DataFlowNode and the {@code location} in the module's source code.
	 * The reasoning is that only one DataFlowNode of a given type can exist in a given module at one location
	 * in the source code.
	 * @param moduleId id of the module containing the DataFlowNode
	 * @param location location of the DataFlowNode in the (assembled) source code of the module
	 * @return an id for the DataFlowNode
	 */
	public static Field forField(final Long moduleId, final ModuleLocation location) {
		final var sb = new StringBuilder();
		sb.append("module-");
		sb.append(moduleId);
		sb.append("-field-");
		sb.append(location.getOffset());
		return new Field(sb.toString());
	}

	/**
	 * Creates a {@link DataFlowId} for Unique Field DataFlowNode
	 * 
	 * @param moduleId id of the module containing the DataFlowNode
	 * @param uniqueName name of the DataFlowNode
	 * @return an id for the DataFlowNode
	 */
	public static Field forUniqueField(final Long moduleId, final String uniqueName) {
		final StringBuilder sb = new StringBuilder();
		sb.append("module-");
		sb.append(moduleId);
		sb.append("-field-");
		sb.append(uniqueName);
		return new Field(sb.toString());
	}

	/**
	 * Sub-class for better type checking of specific DataFlowIds.
	 * e.g.:
	 * DataFlowIds generated for a field can't be passe to a method expecting a Statement DataFlowId
	 */
	public static class Field extends DataFlowId {
		protected Field(final String id) {
			super(id);
		}
	}
	
	/**
	 * Creates a {@link DataFlowId} for regular Statement DataFlowNode
	 * 
	 * @param moduleId id of the module containing the DataFlowNode
	 * @param location location of the DataFlowNode in the (assembled) source code of the module
	 * @return an id for the DataFlowNode
	 */
	public static DataFlowId forStatement(final Long moduleId, final ModuleLocation location) {
		final var sb = new StringBuilder();
		sb.append("module-");
		sb.append(moduleId);
		sb.append("-statement-");
		sb.append(location.getOffset());
		return new DataFlowId(sb.toString());
	}

	/**
	 * Creates a {@link DataFlowId} for Unique Statement DataFlowNode
	 * 
	 * @param moduleId id of the module containing the DataFlowNode
	 * @param uniqueName name of the DataFlowNode
	 * @return an id for the DataFlowNode
	 */
	public static DataFlowId forUniqueStatement(final Long moduleId, final String uniqueName) {
		final var sb = new StringBuilder();
		sb.append("module-");
		sb.append(moduleId);
		sb.append("-statement-");
		sb.append(uniqueName);
		return new DataFlowId(sb.toString());
	}

	/**
	 * Creates a {@link DataFlowId} for ProxyContainers.
	 * <p>
	 * A ProxyContainer is uniquely identified by the containing {@code moduleId} and the {@code type} of the proxy container.
	 * If a module has multiple proxy containers of the same type, then each must have a distinct {@code location}.
	 *
	 * @param moduleId id of the module containing the ProxyContainer
	 * @param type type of the ProxyContainer
	 * @param location location of the statement for which the ProxyContainer was created in the (assembled) source code of the module
	 * @return an id for the ProxyContainer
	 */
	public static ProxyContainer forProxyContainer(final Long moduleId, final ProxyContainerPojo.Type type, @Nullable final ModuleLocation location) {
		final var sb = new StringBuilder();
		sb.append("module-");
		sb.append(moduleId);
		sb.append("-container-");
		sb.append(type);
		if (location != null) {
			sb.append("-");
			sb.append(location.getOffset());
		}
		return new ProxyContainer(sb.toString());
	}

	/**
	 * Sub-class for better type checking of specific DataFlowIds.
	 * e.g.:
	 * DataFlowIds generated for a field can't be passe to a method expecting a Statement DataFlowId
	 */
	public static class ProxyContainer extends DataFlowId {
		protected ProxyContainer(final String id) {
			super(id);
		}
	}
	
	/**
	 * Creates a {@link DataFlowId} for ProxyFields.
	 * <p>
	 * A ProxyField is uniquely identified by its ProxyContainer and the index of the field inside the ProxyContainer.
	 * @param proxyContainerId the {@link DataFlowId} of the ProxyContainer containing the field
	 * @param index the index of the field inside the ProxyContainer (0 based index)
	 * @return an id for the ProxyField
	 */
	public static ProxyField forProxyField(final DataFlowId proxyContainerId, final int index) {
		final var sb = new StringBuilder();
		sb.append(proxyContainerId.getId());
		sb.append("-field-");
		sb.append(index);
		return new ProxyField(sb.toString());
	}

	/**
	 * Sub-class for better type checking of specific DataFlowIds.
	 * e.g.:
	 * DataFlowIds generated for a field can't be passe to a method expecting a Statement DataFlowId
	 */
	public static class ProxyField extends DataFlowId {
		protected ProxyField(final String id) {
			super(id);
		}
	}

	/**
	 * Creates a {@link DataFlowId} for Constants.
	 *
	 * @param moduleId  id of the module containing the DataFlowNode
	 * @param constantValue The value of Constant
	 * @return an id for the DataFlowNode
	 */
	public static DataFlowId forConstant(final Long moduleId, final String constantValue) {
		/* we should probably have separate DataFlowIds for constants - but currently we don't persist whether a field is a constant or not,
		 * so contants "look" exactly the same as other fields (without module location), so they must have the same DataFlowId */
		return forUniqueField(moduleId, constantValue);
	}

	/**
	 * Returns Data flow ID
	 *
	 * @return an id for the DataFlowNode
	 */
	public String getId() {
		return id;
	}

	@Override
	public boolean equals(final @Nullable Object o) {
		if (this == o) {
			return true;
		}
		if ( ! (o instanceof DataFlowId)) {
			return false;
		}
		final DataFlowId that = (DataFlowId) o;
		return Objects.equals(id, that.id);
	}

	@Override
	public int hashCode() {
		return Objects.hash(id);
	}

	@Override
	public String toString() {
		return "DataFlowId{" + id + '}';
	}
}
