/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.tuple.Triple;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.CachingFunction;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.datalineage.DataFlowId;

/**
 * Functions for accessing data flow and related database entities.
 */
public interface DataFlowService {

	interface DataFlowOrderBuilder {
		/**
		 * Sorts DataFlows by their name using binary collation.
		 * @param sort Direction.
		 * @return Sorting builder.
		 */
		DataFlowOrderBuilder sortName(SortDirection sort);
	}

	interface DataFlowNodeInquiryBuilder extends DataFlowOrderBuilder {

		/**
		 * Filters a single DataFlowNode by it's ID.
		 * @param id ID of a DataFlow.
		 * @return Filter builder.
		 */
		DataFlowNodeInquiryBuilder byId(UUID id);

		DataFlowNodeInquiryBuilder byIds(Collection<UUID> ids);

		/**
		 * Filters DataFlow Nodes by IDs. The order of the returned nodes corresponds to the order of the IDs in {@code ids}.
		 * <p><b>Sort operations are prohibited</b> when invoking this filter method. Consequently, you must not call any sort method of this
		 * {@link DataFlowNodeInquiryBuilder} neither before or after invoking this method.</p>
		 * 
		 * @param ids {@link UUID UUIDs} of the to be fetched nodes. The order of IDs in this list will determine the order of the nodes in the returned list.
		 * @return this {@link DataFlowNodeInquiryBuilder} instance
		 */
		DataFlowNodeInquiryBuilder byIdsWithOrdinality(List<UUID> ids);

		DataFlowNodeInquiryBuilder ofModule(EntityId id);
		DataFlowNodeInquiryBuilder ofModules(Collection<EntityId> moduleIds);

		/**
		 * Filters DataFlows by Project.
		 * @param projectId ID of the Project.
		 * @return Filter builder.
		 */
		DataFlowNodeInquiryBuilder ofProject(EntityId projectId);

		DataFlowNodeInquiryBuilder ofProxyContainer(UUID id);

		DataFlowNodeInquiryBuilder ofProxyContainer(DataFlowId id);

		DataFlowNodeInquiryBuilder ofAstNode(UUID astNode);

		DataFlowNodeInquiryBuilder withInclusionCalleeModuleId(@Nullable EntityId module);

		/**
		 * Filters module data flow nodes by {@code src_location}.
		 *
		 * @param location the {@link ModuleLocation} of the source
		 * @return this instance for method chaining
		 */
		DataFlowNodeInquiryBuilder withModuleLocation(ModuleLocation location);

		DataFlowNodeInquiryBuilder withName(String key);

		DataFlowNodeInquiryBuilder withType(DataFlowNodePojo.Type type);

		DataFlowNodeInquiryBuilder notWithType(DataFlowNodePojo.Type type);

		DataFlowNodeInquiryBuilder withAssembledOffset(Comperator comperator, int offset);

		DataFlowNodeInquiryBuilder withAssembledEndOffset(Comperator comperator, int offset);

		DataFlowNodeInquiryBuilder withRetracedOffset(Comperator comperator, int offset);

		DataFlowNodeInquiryBuilder withRetracedEndOffset(Comperator comperator, int offset);

		DataFlowNodeInquiryBuilder withDataFlowId(DataFlowId dataFlowId);

		DataFlowNodeInquiryBuilder withRelationshipFrom(UUID src, DataFlowNodeRelationshipType... types);

		DataFlowNodeInquiryBuilder usingAstNodeCache(CachingFunction<UUID, AstNodePojo> cache);

		DataFlowNodeInquiryBuilder usingProxyContainerCache(CachingFunction<UUID, ProxyContainerPojo> cache);
	}

	interface ProxyContainerInquiryBuilder {

		ProxyContainerInquiryBuilder byId(UUID id);

		/**
		 * Filters DataFlows by Project.
		 * @param projectId ID of the Project.
		 * @return Filter builder.
		 */
		ProxyContainerInquiryBuilder ofProject(EntityId projectId);

		ProxyContainerInquiryBuilder ofModule(EntityId moduleId);

		ProxyContainerInquiryBuilder ofModules(List<EntityId> moduleIds);

		ProxyContainerInquiryBuilder withType(ProxyContainerPojo.Type type);

		ProxyContainerInquiryBuilder withDataFlowId(DataFlowId proxyContainerId);

		ProxyContainerInquiryBuilder usingCache(CachingFunction<UUID, DataFlowNodePojo> cache);
	}

	interface DataFlowErrorInquiryBuilder {

		/**
		 * Filters DataFlows by Project.
		 * @param projectId ID of the Project.
		 * @return Filter builder.
		 */
		DataFlowErrorInquiryBuilder ofProject(EntityId projectId);

		DataFlowErrorInquiryBuilder ofNode(UUID uid);
	}

	UUID create(DataFlowNodePojoPrototype node);

	void update(DataFlowNodePojoPrototype node);

	void update(BuildingConsumer<DataFlowNodeInquiryBuilder> builder, DataFlowNodePojoPrototype proto);

	DataFlowNodePojo get(UUID uid);

	List<DataFlowNodePojo> find(BuildingConsumer<DataFlowNodeInquiryBuilder> builder);

	List<UUID> findIds(BuildingConsumer<DataFlowNodeInquiryBuilder> builder);

	Optional<DataFlowNodePojo> findAny(BuildingConsumer<DataFlowNodeInquiryBuilder> builder);

	Optional<UUID> findAnyId(BuildingConsumer<DataFlowNodeInquiryBuilder> builder);

	/**
	 * Batch creation of relationships between nodes.
	 * <p>The {@link Triple} entries in the given {@code relationships} collection must contain the {@code src} data_flow_node id as the 'left',
	 * the {@code dst} data_flow_node id as the 'middle' and the {@link DataFlowNodeRelationshipType} as the 'right' element.</p>
	 *
	 * @param relationships the relationship {@link Triple Triples} to create
	 */
	void createRelationships(Collection<Triple<UUID, UUID, DataFlowNodeRelationshipType>> relationships);

	UUID createProxyContainer(ProxyContainerPojoPrototype proto);

	void updateProxyContainer(ProxyContainerPojoPrototype proto);

	List<ProxyContainerPojo> findProxyContainers(BuildingConsumer<ProxyContainerInquiryBuilder> builder);

	ProxyContainerPojo getProxyContainer(UUID uid);

	Optional<ProxyContainerPojo> findAnyProxyContainer(BuildingConsumer<ProxyContainerInquiryBuilder> builder);

	void createProxyContainerRelationships(UUID proxyContainerUid, List<UUID> nodeIds);

	int deleteProxyContainerRelationships(UUID proxyContainerUid);


	UUID createError(DataFlowErrorPojoPrototype proto);

	void createErrors(Collection<DataFlowErrorPojoPrototype> protos);

	int deleteErrors(BuildingConsumer<DataFlowErrorInquiryBuilder> builder);

	List<DataFlowErrorPojo> findErrors(BuildingConsumer<DataFlowErrorInquiryBuilder> builder);

	/**
	 * Deletes all data flow nodes and proxy containers for the given {@code module}.
	 * <p>Resets the DataLineageTraceExecuted property for the given {@code module}.</p>
	 *
	 * @param module the module to reset
	 */
	void deleteForModule(EntityId module);

	/**
	 * Deletes all data flow nodes and proxy containers for the given {@code project}.
	 * <p>Resets the DataLineageTraceExecuted property for the given {@code project}.</p>
	 *
	 * @param project the project to reset
	 */
	void deleteForProject(EntityId project);
}
