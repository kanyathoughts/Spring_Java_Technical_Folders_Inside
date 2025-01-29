/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.sql;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import brave.internal.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.server.datalineage.operations.DataLineageResolver;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;

/**
 * Data Lineage Resolver component for linking {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#DATABASE_ACCESS}
 * with {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#DATABASE_TABLE}.
 */
@Component
public class SqlTableResolver implements DataLineageResolver {

	private final ModuleService moduleService;
	private final DataFlowService dataFlowService;

	@Nullable
	private DataLineageCoreService dataLineageCoreService;

	/**
	 * Constructor.
	 * @param moduleService Module data access service.
	 * @param dataFlowService Data flow data access service.
	 */
	public SqlTableResolver(final ModuleService moduleService, final DataFlowService dataFlowService) {
		this.moduleService = moduleService;
		this.dataFlowService = dataFlowService;
	}

	@Override
	public boolean isSupported(final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		return proxyContainer.getType() == ProxyContainerPojo.Type.DATABASE_TABLE || proxyContainer.getType() == ProxyContainerPojo.Type.DATABASE_ACCESS;
	}

	@Override
	public DataLineageResult resolve(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		if (proxyContainer.getType() == ProxyContainerPojo.Type.DATABASE_TABLE) {
			return resolveFromTableToProgram(context, proxyContainer);
		} else {
			return resolveFromProgramToTable(context, proxyContainer);
		}
	}

	@Override
	public void setDataLineageCoreService(final DataLineageCoreService dataLineageCoreService) {
		this.dataLineageCoreService = dataLineageCoreService;
	}

	private DataLineageResult resolveFromTableToProgram(final DataLineageContext context, final ProxyContainerPojo tableContainer) {
		final ModuleLightweightPojo tableModule = moduleService.findAnyModuleLightweight(b -> b.byId(tableContainer.getModuleId()))
				.orElseThrow(() -> new MiningEntityNotFoundException("Unable to load module with numeric id: " + tableContainer.getModuleId()));
		
		final List<ModuleRelationshipPojo> readsWritesEdges = moduleService.findRelationship(q -> q.ofModuleInDirection(tableModule.identity(),
				RelationshipDirection.IN)
				.withType(RelationshipType.ACCESSES));

		if (readsWritesEdges.isEmpty()) {
			/* nothing is accessing the table */
			return DataLineageResult.empty();
		}

		final List<DataLineageResult> results = new ArrayList<>();
		for (final ModuleRelationshipPojo rw : readsWritesEdges) {
			final var optModule = moduleService.findAnyModuleLightweight(q -> q.byUid(rw.getSrcModule()));
			if (optModule.isPresent()) {
				final ModuleBasePojo adjacentModule = optModule.get();
				final Collection<ProxyContainerPojo> accessContainers = assertNotNull(dataLineageCoreService).getProxyContainersOfType(
						context, EntityId.of(adjacentModule.getId()), ProxyContainerPojo.Type.DATABASE_ACCESS);
	
				if ( ! rw.getSrcLocation().isPresent()) {
					continue;
				}
				
				Optional<ModuleLocation> optLocation = rw.getSrcLocation();
				if (optLocation.isPresent()) {
					final ModuleLocation fromLocation = optLocation.get();
					/* match by location */
					matchContainerByLocation(accessContainers, tableContainer, fromLocation, results);
					
				} else {
					/* match by TARGET_NAME */
					//TODO: this property is not set currently
					accessContainers.stream()
							.filter(accessContainer -> {
								final String targetName = (String) accessContainer.getProperties().get(ProxyContainerPojo.Property.TARGET_NAME.name());
								return assertNotNull(tableModule.getName()).equals(targetName);
							})
							.map(accessContainer -> connectContainers(accessContainer, tableContainer))
							.forEach(results::add);
				}
			}
		}
		return DataLineageResult.combine(results);
	}
	
	private void matchContainerByLocation (final Collection<ProxyContainerPojo> accessContainers, final ProxyContainerPojo tableContainer,
			final ModuleLocation fromLocation, final List<DataLineageResult> results) {
		accessContainers.stream()
		.filter(accessContainer -> {
			final var statementLocation = accessContainer.getStatementLocation();
			if (statementLocation.isEmpty()) {
				return false;
			}
			return statementLocation.get().overlapsWith(new ModuleLocation(fromLocation.getOffset(), fromLocation.getLength()));
		})
		.map(accessContainer -> connectContainers(accessContainer, tableContainer))
		.forEach(results::add);
	}

	private DataLineageResult resolveFromProgramToTable(final DataLineageContext context, final ProxyContainerPojo accessContainer) {
		final var statementLocation = accessContainer.getStatementLocation();
		final List<ModuleRelationshipPojo> readsWritesEdges = moduleService.findRelationship(q -> q.ofModuleInDirection(accessContainer.getModuleId(),
				RelationshipDirection.OUT)
				.withType(RelationshipType.ACCESSES));

		if (readsWritesEdges.isEmpty()) {
			/* program doesn't access any tables (or dependency was not resolved) */
			return DataLineageResult.empty();
		}

		Optional<Long> tableModuleId = Optional.empty();
		if (statementLocation.isPresent()) {
			/* match by location */
			tableModuleId = getTableModuleIdByReadsWritesLocation(statementLocation.get(), readsWritesEdges);
		}
		if (tableModuleId.isEmpty()) {
			/* match by TARGET_NAME */
			tableModuleId = getTableModuleIdByTargetName(accessContainer, readsWritesEdges);
		}

		if (tableModuleId.isPresent()) {
			final Optional<ProxyContainerPojo> tableContainer = assertNotNull(dataLineageCoreService).getProxyContainersOfType(
					context, EntityId.of(tableModuleId.get()), ProxyContainerPojo.Type.DATABASE_TABLE)
					.stream()
					.findAny();

			if (tableContainer.isPresent()) {
				return connectContainers(accessContainer, tableContainer.get());
			}
		}
		return DataLineageResult.empty();
	}

	private Optional<Long> getTableModuleIdByReadsWritesLocation(final ModuleLocation statementLocation, final List<ModuleRelationshipPojo> readsWrites) {
		return readsWrites.stream()
				.filter(rw -> {
					final ModuleLocation fromModuleLocation = rw.getSrcLocation().get();
					return fromModuleLocation != null
							&& statementLocation.overlapsWith(new ModuleLocation(fromModuleLocation.getOffset(), fromModuleLocation.getLength()));
				})
				.findFirst()
				.flatMap(rw -> Optional.ofNullable(moduleService.findAnyModuleLightweight(q -> q.byUid(rw.getDstModule()))).get())
				.map(ModuleLightweightPojo::getId);
	}

	private Optional<Long> getTableModuleIdByTargetName(final ProxyContainerPojo accessContainer, final List<ModuleRelationshipPojo> readsWrites) {
		//TODO: this property is currently not set on TABLE_ACCESS containers (so this whole implementation doesn't work)
		final String targetName = (String) accessContainer.getProperties().get(ProxyContainerPojo.Property.TARGET_NAME.name());
		if (targetName == null) {
			return Optional.empty();
		}
		for (final ModuleRelationshipPojo rw : readsWrites) {
			final var optModule = moduleService.findAnyModuleLightweight(q -> q.byUid(rw.getSrcModule()));
			
			if (optModule.isPresent()) {
				final ModuleBasePojo targetModule = optModule.get();
				final String targetModuleName = targetModule.getName();
				if (targetName.equals(targetModuleName)) {
					return Optional.ofNullable(targetModule.getId());
				}
			}
		}
		return Optional.empty();
	}

	@SuppressWarnings("unchecked")
	DataLineageResult connectContainers(final ProxyContainerPojo dbAccess, final ProxyContainerPojo dbTable) {
		/* Note: this was previously in SqlUtil.connectProxyContainers() */
		if (dbTable.getFieldNodesUids().size() == 1
				&& dataFlowService.get(dbTable.getFieldNodesUids().get(0)).getName().equals(SqlTableTracer.PLACEHOLDER_FIELD_NAME)) {
			return connectEverythingToPlaceholder(dbAccess, dbTable);
		}
		final String mapString = (String) dbAccess.getProperties().get(ProxyContainerPojo.Property.DATABASE_ACCESS_SQL_COLUMN_MAP.name());
		if (mapString == null) {
			final List<DataFlowNodePrototype> fields = new ArrayList<>();
			for (final UUID nodeId : dbAccess.getFieldNodesUids()) {
				final DataFlowId flowId = dataFlowService.get(nodeId).getDataFlowId();
				final DataFlowNodePrototype field = new DataFlowNodePrototype(flowId);
				field.addError(new DataFlowErrorPojoPrototype()
						.setSeverity(DataFlowErrorPojo.Severity.WARNING)
						.setText("The columnMap of the ProxyContainerPojo could not be processed"));
				fields.add(field);
			}
			return DataLineageResult.ofNodes(fields);
		}
		final ObjectMapper mapper = new ObjectMapper();
		final Map<String, List<String>> map;
		try {
			map = mapper.readValue(mapString, Map.class);
		} catch (final JsonProcessingException e) {
			throw new IllegalStateException("The columnMap of the ProxyContainerPojo " + dbAccess.getId() + " could not be processed", e);
		}

		final List<DataFlowNodePrototype> changes = new LinkedList<>();
		for (final Map.Entry<String, List<String>> entry : map.entrySet()) {
			final Optional<DataFlowNodePojo> oField = dataFlowService.findAny(q -> q.byIds(dbAccess.getFieldNodesUids())
																					.withName(entry.getKey()));
			if (oField.isEmpty()) {
				continue;
			}
			final DataFlowNodePojo programField = oField.get();
			for (final String column : entry.getValue()) {
				/* find the corresponding proxy fields on the proxy container of the table */
				final Optional<DataFlowNodePojo> columnFieldOptional = dataFlowService.findAny(q -> q.byIds(dbTable.getFieldNodesUids())
																									 .withName(column));
				/* Make the proxy field of the DB access statement and the proxy field(s) of the table related  */
				if (columnFieldOptional.isPresent()) {
					/* for now, data flow is added in both direction - should determine actual direction here */
					final var columnField = columnFieldOptional.get();
					changes.add(new DataFlowNodePrototype(programField.getDataFlowId())
							.addDataFlow(DataFlow.toField(columnField)));
					changes.add(new DataFlowNodePrototype(columnField.getDataFlowId())
							.addDataFlow(DataFlow.toField(programField)));
				}
			}
		}
		return DataLineageResult.ofNodes(changes);
	}

	private DataLineageResult connectEverythingToPlaceholder(final ProxyContainerPojo dbAccess, final ProxyContainerPojo dbTable) {
		final List<DataFlowNodePrototype> changes = new ArrayList<>();
		final DataFlowNodePojo placeholderField = dataFlowService.get(dbTable.getFieldNodesUids().get(0));
		/* connect everything to placeholder field */
		final var dbAccessFields = dataFlowService.find(q -> q.byIds(dbAccess.getFieldNodesUids()));
		for (final DataFlowNodePojo field : dbAccessFields) {
			/* for now, data flow is added in both direction - should determine actual direction here */
			changes.add(new DataFlowNodePrototype(field.getDataFlowId()).addDataFlow(DataFlow.toField(placeholderField)));
			changes.add(new DataFlowNodePrototype(placeholderField.getDataFlowId()).addDataFlow(DataFlow.toField(field)));
		}
		return DataLineageResult.ofNodes(changes);
	}
}
