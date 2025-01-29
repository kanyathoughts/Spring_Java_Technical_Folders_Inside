/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.persistence;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Triple;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.server.locking.ModuleLockService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojoPrototype;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.mining.shared.model.datalineage.ProxyContainerPrototype;


/**
 * Persistence service allowing to persist {@link DataLineageResult} into the database, updating the global Data Lineage model for the project.
 */
@Service
public class DataLineagePersistenceService {

	/* intermediate implementation for OrientDB based on DataFlowDao */

	private final DataFlowService dataFlowService;
	private final AstService astService;
	/* currently used to serialize ProxyContainerPojo properties as JSON strings - in postgres we will store the values directly as JSON,
	 * so no longer required there */
	private final ObjectMapper objectMapper;

	private final ExecutorService executorService;

	private final ModuleLockService moduleLockService;

	private final ModuleService moduleService;

	/**
	 * Constructor.
	 * @param dataFlowService DAO for {@link DataFlowNodePojo}
	 * @param astService the {@link AstService} for querying for AST nodes
	 * @param objectMapper the {@link ObjectMapper} to deserialize JSON content from given JSON content String.
	 * @param executorService provides entry points to execute various operations like storeAst
	 * @param moduleLockService service allowing module locking
	 * @param moduleService the {@link ModuleService} for getting UUIDs of modules
	 */
	public DataLineagePersistenceService(final DataFlowService dataFlowService,
										 final AstService astService,
										 final ObjectMapper objectMapper,
										 final ExecutorService executorService,
										 final ModuleLockService moduleLockService,
										 final ModuleService moduleService) {
		this.dataFlowService = dataFlowService;
		this.astService = astService;
		this.objectMapper = objectMapper;
		this.executorService = executorService;
		this.moduleLockService = moduleLockService;
		this.moduleService = moduleService;
	}

	/**
	 * Imports a {@link DataLineageResult} and persists it to the database. This will create or update {@link DataFlowNodePojo}s as needed.
	 * @param context the {@code DataLineageContext} for the import operation
	 * @param moduleId the id of the module on which {@link DataFlowNodePojo}s are created
	 * @param result the {@code DataLineageResult} containing the changes that shall be persisted
	 */
	public void importDataLineageResult(final DataLineageContext context, final EntityId moduleId, final DataLineageResult result) {
		try (final ModuleLockService.Lock lock = moduleLockService.tryLock("importDataLineageResult", context.getProjectId(), moduleId)) {
			doImport(context, moduleId, result);
		}
	}
	
	/**
	 * Marks a module as "resolved", meaning that its internal data flow and all proxy containers have been discovered.
	 * @param context the {@code DataLineageContext} for the operation
	 * @param moduleId the id of the module to mark as resolved
	 */
	public void markModuleResolved(final DataLineageContext context, final EntityId moduleId) {
		dataFlowService.update(q -> q.notWithType(DataFlowNodePojo.Type.PROXY_FIELD)
									 .ofModule(moduleId),
								new DataFlowNodePojoPrototype()
									.setTraced(Boolean.TRUE));
	}
	
	/**
	 * Marks a proxy container as "resolved", meaning that its proxy fields have been connected to all applicable adjacent modules
	 * (e.g. a CALL container has been linked to all applicable ENTRY_POINT containers).
	 * @param context the {@code DataLineageContext} for the operation
	 * @param proxyContainerId the id of the proxy container to mark as resolved
	 */
	public void markProxyContainerResolved(final DataLineageContext context, final DataFlowId proxyContainerId) {
		dataFlowService.update(q -> q.ofProxyContainer(proxyContainerId),
								new DataFlowNodePojoPrototype()
									.setTraced(Boolean.TRUE));
	}

	/**
	 * Ensures AST nodes are stored for the module.
	 * @param context the {@code DataLineageContext} for the operation
	 * @param moduleId the id of the module for which to store AST
	 */
	public void ensureAsts(final DataLineageContext context, final EntityId moduleId) {
		final UUID moduleUid = moduleService.getModuleUid(moduleId);
		if ( ! context.getParseErrorModules().contains(moduleUid) && ! executorService.executeStoreAst(context.getProjectId(), moduleId)) {
			context.addErrorModule(moduleUid);
		}
	}
	
	/**
	 * Returns the {@link ProxyContainerPojo} for the input ID
	 *
	 * @param proxyContainerId proxy container ID
	 * @return the {@link ProxyContainerPojo}
	 */
	public Optional<ProxyContainerPojo> findProxyContainerByDataFlowId(final DataFlowId proxyContainerId) {
		return dataFlowService.findAnyProxyContainer(q -> q.withDataFlowId(proxyContainerId));
	}
	
	/**
	 * Returns a list of {@link ProxyContainerPojo} having the input type
	 *
	 * @param moduleId ID of module
	 * @param type type {@link ProxyContainerPojo}
	 * @return list of {@link ProxyContainerPojo}
	 */
	public List<ProxyContainerPojo> findProxyContainersByModuleAndType(final EntityId moduleId, final ProxyContainerPojo.Type type) {
		return dataFlowService.findProxyContainers(q -> q.ofModule(moduleId).withType(type));
	}

	private void doImport(final DataLineageContext context, final EntityId moduleId, final DataLineageResult result) {
		ensureAsts(context, moduleId);

		final Map<DataFlowId, UUID> nodes = new HashMap<>();
		final Map<DataFlowId, ProxyContainerPojo> containers = new HashMap<>(result.getProxyContainers().size());

		/* ensure all the nodes and containers are created first, before linking anything */
		for (final ProxyContainerPrototype proxyContainerPrototype : result.getProxyContainers()) {
			final ProxyContainerPojo container = containers.computeIfAbsent(proxyContainerPrototype.getDataFlowId(), k ->
			getOrCreateProxyContainer(moduleId, proxyContainerPrototype, nodes));
			if (proxyContainerPrototype.fields.isDefined()) {
				for (final DataFlowNodePrototype field : proxyContainerPrototype.fields.getNonNull()) {
					nodes.computeIfAbsent(field.getDataFlowId(), k ->
							getOrCreateProxyField(moduleId, field, container));
				}
			}
		}
		for (final DataFlowNodePrototype dataFlowNodePrototype : result.getDataFlowNodes()) {
			nodes.computeIfAbsent(dataFlowNodePrototype.getDataFlowId(), k -> getOrCreateDataFlowNode(moduleId, dataFlowNodePrototype, DataFlowNodePojo.Type.FIELD));
		}
		for (final DataFlowNodePrototype statement : result.getStatements()) {
			nodes.computeIfAbsent(statement.getDataFlowId(), k -> getOrCreateDataFlowNode(moduleId, statement, DataFlowNodePojo.Type.STATEMENT));
		}

		/* now that we have ensured everything exists, perform the updates */
		for (final ProxyContainerPrototype proxyContainerPrototype : result.getProxyContainers()) {
			updateProxyContainerPojo(proxyContainerPrototype, nodes, containers);
		}
		for (final DataFlowNodePrototype dataFlowNodePrototype : result.getDataFlowNodes()) {
			updateDataFlowNodePojo(dataFlowNodePrototype, nodes);
		}
		for (final DataFlowNodePrototype statementPrototype : result.getStatements()) {
			updateStatement(statementPrototype, nodes);
		}
	}

	private UUID getOrCreateDataFlowNode(final EntityId moduleId, final DataFlowNodePrototype prototype, final DataFlowNodePojo.Type type) {
		final Optional<UUID> existingNode = dataFlowService.findAnyId(q -> q.withDataFlowId(prototype.getDataFlowId()));
		if (existingNode.isPresent()) {
			return existingNode.get();
		}

		final String name = prototype.name.getNonNull();
		final ModuleLocation location = prototype.location.isDefined() ? prototype.location.get() : null;
		final DataFlowNodePojoPrototype dataFlowNode = new DataFlowNodePojoPrototype()
																.setDataFlowId(prototype.getDataFlowId())
																.setType(type)
																.setName(name)
																.setTraced(Boolean.FALSE)
																.setModuleId(moduleId);

		if (location != null) {
			final Optional<UUID> astNode = astService.findAny(q -> q.ofModule(moduleId)
																	.withAssembledOffset(Comperator.EQUAL, location.getOffset())
																	.withParent(q2 -> q2.ofModule(moduleId)
																						.withAssembledOffset(Comperator.EQUAL, location.getOffset()),
																				false))
														.map(AstNodePojo::getId);

			if (astNode.isEmpty()) {
				/* fail fast in case the location of the DataFlowNodePojo could not be stored */
				throw new IllegalStateException("Failed to persist location for DataFlowNodePojo " + prototype.getDataFlowId()
												+ ": No AST node found at the specified location. The location is invalid: " + location);
			}
			dataFlowNode.setAstNode(astNode.get());
		}

		return dataFlowService.create(dataFlowNode);
	}

	private UUID getOrCreateProxyField(final EntityId moduleId, final DataFlowNodePrototype prototype, final ProxyContainerPojo container) {
		final Optional<UUID> existingNode = dataFlowService.findAnyId(q -> q.withDataFlowId(prototype.getDataFlowId()));
		if (existingNode.isPresent()) {
			return existingNode.get();
		}

		return dataFlowService.create(new DataFlowNodePojoPrototype()
											.setDataFlowId(prototype.getDataFlowId())
											.setType(DataFlowNodePojo.Type.PROXY_FIELD)
											.setName(prototype.name.getNonNull())
											.setModuleId(moduleId)
											.setTraced(Boolean.FALSE)
											.setProxyContainer(container.getId()));
	}

	private ProxyContainerPojo getOrCreateProxyContainer(final EntityId moduleId, final ProxyContainerPrototype prototype, final Map<DataFlowId, UUID> nodes) {
		final Optional<ProxyContainerPojo> existingContainer = dataFlowService.findAnyProxyContainer(q -> q.withDataFlowId(prototype.getDataFlowId()));
		if (existingContainer.isPresent()) {
			for (final DataFlowNodePojo pojo : existingContainer.get().getFieldNodes()) {
				nodes.put(pojo.getDataFlowId(), pojo.getId());
			}
			return existingContainer.get();
		}

		final ProxyContainerPojoPrototype proxyContainer = new ProxyContainerPojoPrototype()
																.setDataFlowId(prototype.getDataFlowId())
																.setType(prototype.type.getNonNull())
																.setModuleId(moduleId)
																.setProperties(prototype.properties.isPresent() ? 
																					getProperties(prototype.properties.getNonNull()) : Collections.emptyMap());

		final ModuleLocation statementLocation = prototype.statementLocation.isDefined() ? prototype.statementLocation.get() : null;
		if (statementLocation != null) {
			final Optional<UUID> statementUid = astService.findAny(q -> q.ofModule(moduleId)
																		 .withAssembledOffset(Comperator.EQUAL, statementLocation.getOffset())
																		 .withParent(q2 -> q2.ofModule(moduleId)
																							 .withAssembledOffset(Comperator.EQUAL, statementLocation.getOffset()),
																					 false))
															.map(AstNodePojo::getId);
			if (statementUid.isEmpty()) {
				throw new IllegalStateException("Failed to persist statement location for ProxyContainerPojo " + prototype.getDataFlowId()
						+ ": this happens when no AST node exists at the specified location. The location is invalid");
			}
			proxyContainer.setStatement(statementUid.get());
		}

		return dataFlowService.getProxyContainer(dataFlowService.createProxyContainer(proxyContainer));
	}

	private UUID updateDataFlowNodePojo(final DataFlowNodePrototype prototype, final Map<DataFlowId, UUID> nodes) {
		final UUID dataFlowNode = nodes.get(prototype.getDataFlowId());
		if (prototype.errors.isDefined()) {
			final var errors = prototype.errors.getNonNull();
			/* Must set the node id into the errors before creating them */
			errors.forEach(e -> e.setNodeId(dataFlowNode));
			dataFlowService.deleteErrors(q -> q.ofNode(dataFlowNode));
			dataFlowService.createErrors(errors);
		}

		final var relatedFields = prototype.relatedFields.orElseNonNull(Collections.emptySet());
		final var dataFlows = prototype.dataFlows.orElseNonNull(Collections.emptySet());
		if ( ! relatedFields.isEmpty() || ! dataFlows.isEmpty()) {
			final List<Triple<UUID, UUID, DataFlowNodeRelationshipType>> relationships = new ArrayList<>(relatedFields.size() * 2 + dataFlows.size() * 6);
			if ( ! relatedFields.isEmpty()) {
				updateRelatedFields(nodes, dataFlowNode, relatedFields, relationships);
			}
			if ( ! dataFlows.isEmpty()) {
				updateDataFlows(nodes, dataFlowNode, dataFlows, relationships);
			}

			dataFlowService.createRelationships(relationships);
		}
		return dataFlowNode;
	}

	private void updateRelatedFields(final Map<DataFlowId, UUID> nodes, final UUID node, final Set<DataFlowId> relatedFields,
			final List<Triple<UUID, UUID, DataFlowNodeRelationshipType>> relationships) {
		for (final DataFlowId relatedField : relatedFields) {
			final UUID relatedFieldNode = getLinkedNode(relatedField, nodes);
			relationships.add(Triple.of(node, relatedFieldNode, DataFlowNodeRelationshipType.RELATED_FIELD));
			relationships.add(Triple.of(relatedFieldNode, node, DataFlowNodeRelationshipType.RELATED_FIELD));
		}
	}

	private void updateDataFlows(final Map<DataFlowId, UUID> nodes, final UUID node, final Set<DataFlow> dataFlows,
			final List<Triple<UUID, UUID, DataFlowNodeRelationshipType>> relationships) {
		for (final DataFlow dataFlow : dataFlows) {
			final DataFlowId destination = dataFlow.getDestination();
			final DataFlowId statement = dataFlow.getStatement();
			if (statement == null) {
				/* data flow directly between fields (not via Statement) is currently expressed as "relatedField" - this will change */
				if (destination != null) {
					final var linkedNode = getLinkedNode(destination, nodes);
					relationships.add(Triple.of(node, linkedNode, DataFlowNodeRelationshipType.RELATED_FIELD));
					relationships.add(Triple.of(linkedNode, node, DataFlowNodeRelationshipType.RELATED_FIELD));
				} else {
					throw new IllegalArgumentException("Dataflow is referencing Statement " + statement + " without a Destination");
				}
			} else {
				final var statementNode = getLinkedNode(statement, nodes);
				relationships.add(Triple.of(node, statementNode, DataFlowNodeRelationshipType.READ_ACCESS));
				relationships.add(Triple.of(statementNode, node, DataFlowNodeRelationshipType.READ_ACCESS));
				if (destination != null) {
					final var destinationNode = getLinkedNode(destination, nodes);
					relationships.add(Triple.of(statementNode, destinationNode, DataFlowNodeRelationshipType.WRITE_ACCESS));
					relationships.add(Triple.of(destinationNode, statementNode, DataFlowNodeRelationshipType.WRITE_ACCESS));
				}
			}
		}
	}

	private UUID getLinkedNode(final DataFlowId dataFlowId, final Map<DataFlowId, UUID> nodes) {
		return nodes.computeIfAbsent(dataFlowId, k -> dataFlowService.findAnyId(q -> q.withDataFlowId(dataFlowId)).orElseThrow(() ->
				new IllegalArgumentException("DataFlow linked node for " + dataFlowId + " does not exist and was not created")));
	}

	private void updateProxyContainerPojo(final ProxyContainerPrototype prototype, final Map<DataFlowId, UUID> nodes,
									  final Map<DataFlowId, ProxyContainerPojo> containers) {
		final ProxyContainerPojo proxyContainer = containers.get(prototype.getDataFlowId());
		boolean reload = false;

		if (prototype.properties.isDefined()) {
			final var newProperties = getProperties(prototype.properties.getNonNull());
			if ( ! newProperties.isEmpty()) {
				if (proxyContainer.getProperties().isEmpty()) {
					dataFlowService.updateProxyContainer(new ProxyContainerPojoPrototype()
															.setUid(proxyContainer.getId())
															.setProperties(newProperties));
					reload = true;
				} else if ( ! proxyContainer.getProperties().equals(newProperties)) {
					final var properties = new HashMap<>(proxyContainer.getProperties());
					properties.putAll(newProperties);
					dataFlowService.updateProxyContainer(new ProxyContainerPojoPrototype()
															.setUid(proxyContainer.getId())
															.setProperties(properties));
					reload = true;
				}
			}
		}

		if (prototype.fields.isDefined()) {
			final List<UUID> oldFieldIds = proxyContainer.getFieldNodes().stream()
															.map(DataFlowNodePojo::getId)
															.collect(Collectors.toList());

			final List<UUID> newFieldIds = prototype.fields.getNonNull().stream()
															.map(nodePrototype -> updateDataFlowNodePojo(nodePrototype, nodes))
															.collect(Collectors.toList());

			if ( ! oldFieldIds.equals(newFieldIds)) {
				/* so far we simply set a new fields list. In Postgres we have to delete the old list then */
				if ( ! oldFieldIds.isEmpty()) {
					dataFlowService.deleteProxyContainerRelationships(proxyContainer.getId());
				}

				dataFlowService.createProxyContainerRelationships(proxyContainer.getId(), newFieldIds);
				reload = true;
			}
		}

		if (reload) {
			containers.put(prototype.getDataFlowId(), dataFlowService.getProxyContainer(proxyContainer.getId()));
		}
	}

	private Map<String, Object> getProperties(final Map<ProxyContainerPojo.Property, Object> properties) {
		if (properties.isEmpty()) {
			return Collections.emptyMap();
		}

		final Map<String, Object> existingProperties = new HashMap<>(properties.size());
		for (final Map.Entry<ProxyContainerPojo.Property, Object> prop : properties.entrySet()) {
			if (prop.getValue() instanceof String) {
				existingProperties.put(prop.getKey().name(), prop.getValue());
			} else {
				try {
					existingProperties.put(prop.getKey().name(), objectMapper.writeValueAsString(prop.getValue()));
				} catch (final JsonProcessingException e) {
					throw new IllegalArgumentException("Unable to store proxy container property " + prop.getKey() + "=" + prop.getValue(), e);
				}
			}
		}

		return existingProperties;
	}

	private void updateStatement(final DataFlowNodePrototype prototype, final Map<DataFlowId, UUID> nodes) {
		final UUID statementNode = nodes.get(prototype.getDataFlowId());
		if (prototype.errors.isDefined()) {
			final Collection<DataFlowErrorPojoPrototype> errors = prototype.errors.getNonNull();
			/* Must set the node id into the errors before creating them */
			errors.forEach(e -> e.setNodeId(statementNode));
			dataFlowService.deleteErrors(q -> q.ofNode(statementNode));
			dataFlowService.createErrors(errors);
		}
		if (prototype.dataFlows.isDefined()) {
			updateDataFlowsForStatement(nodes, statementNode, prototype.dataFlows.getNonNull());
		}
	}

	private void updateDataFlowsForStatement(final Map<DataFlowId, UUID> nodes, final UUID statement, final Set<DataFlow> dataFlows) {
		if ( ! dataFlows.isEmpty()) {
			final List<Triple<UUID, UUID, DataFlowNodeRelationshipType>> relationships = new ArrayList<>(dataFlows.size() * 2);
			for (final DataFlow dataFlow : dataFlows) {
				final UUID fieldNode = getLinkedNode(assertNotNull(dataFlow.getDestination()), nodes);
				relationships.add(Triple.of(statement, fieldNode, DataFlowNodeRelationshipType.WRITE_ACCESS));
				relationships.add(Triple.of(fieldNode, statement, DataFlowNodeRelationshipType.WRITE_ACCESS));
			}
			dataFlowService.createRelationships(relationships);
		}
	}
}
