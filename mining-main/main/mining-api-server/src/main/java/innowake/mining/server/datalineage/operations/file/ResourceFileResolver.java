/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.file;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import org.springframework.stereotype.Component;

import brave.internal.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.server.datalineage.operations.DataLineageResolver;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;

/**
 * Data Lineage Resolver component for linking {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#FILE_ACCESS}
 * with {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#RESOURCE_FILE}.
 */
@Component
public class ResourceFileResolver implements DataLineageResolver {

	private final ModuleService moduleService;
	private final DataFlowService dataFlowService;

	@Nullable
	private DataLineageCoreService dataLineageCoreService;

	/**
	 * Constructor.
	 * @param moduleService Module data access service.
	 * @param dataFlowService Data Flow data access service.
	 */
	public ResourceFileResolver(final ModuleService moduleService, final DataFlowService dataFlowService) {
		this.moduleService = moduleService;
		this.dataFlowService = dataFlowService;
	}

	@Override
	public boolean isSupported(final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		return proxyContainer.getType() == ProxyContainerPojo.Type.RESOURCE_FILE || proxyContainer.getType() == ProxyContainerPojo.Type.FILE_ACCESS;
	}

	@Override
	public DataLineageResult resolve(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		if (proxyContainer.getType() == ProxyContainerPojo.Type.RESOURCE_FILE) {
			return resolveFromFileToProgram(context, module, proxyContainer);
		} else {
			return resolveFromProgramToFile(context, proxyContainer);
		}
	}
	
	@Override
	public void setDataLineageCoreService(final DataLineageCoreService dataLineageCoreService) {
		this.dataLineageCoreService = dataLineageCoreService;
	}

	private DataLineageResult resolveFromFileToProgram(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		final ModuleLightweightPojo moduleV2 = moduleService.findAnyModuleLightweight(b -> b.byNid(module.getId()))
				.orElseThrow(() -> new MiningEntityNotFoundException("Unable to load module with numeric id: " + module.getId()));
		
		final String fileName = moduleV2.getName();
		
		final List<ModuleRelationshipPojo> readsWrites = moduleService.findRelationship(q -> q.ofModuleInDirection(proxyContainer.getModuleId(),
				RelationshipDirection.IN)
				.withType(RelationshipType.ACCESSES));
		
		final DataFlowNodePojo targetField = proxyContainer.getFieldNodes().get(0);


		if (readsWrites.isEmpty()) {
			return DataLineageResult.empty();
		}

		final List<DataFlowNodePrototype> changes = new ArrayList<>();
		for (final ModuleRelationshipPojo rw : readsWrites) {
			final var adjacentModuleOpt = moduleService.findAnyModuleLightweight(q -> q.byUid(rw.getSrcModule()));
			if (adjacentModuleOpt.isPresent()) {
			final ModuleBasePojo adjacentModule = adjacentModuleOpt.get();
				for (final ProxyContainerPojo accessContainer : dataLineageCoreService.getProxyContainersOfType(
						context, EntityId.of(adjacentModule.getId()), ProxyContainerPojo.Type.FILE_ACCESS)) {
	
					final String fileNameProperty = (String) accessContainer.getProperties().get(ProxyContainerPojo.Property.FILE_ACCESS_FILE_NAME.name());
					final List<DataFlowNodePojo> fields = dataFlowService.find(q -> q.byIds(accessContainer.getFieldNodesUids()));
					if (Objects.equals(fileName, fileNameProperty)) {
						addDataFlow(fields, changes, targetField);
					}
				}
			}
		}

		return DataLineageResult.ofNodes(changes);
	}

	private DataLineageResult resolveFromProgramToFile(final DataLineageContext context, final ProxyContainerPojo proxyContainer) {
		final String fileNameProperty = (String) proxyContainer.getProperties().get(ProxyContainerPojo.Property.FILE_ACCESS_FILE_NAME.name());
		
		final List<ModuleRelationshipPojo> readsWrites = moduleService.findRelationship(q -> q.ofModuleInDirection(proxyContainer.getModuleId(),
				RelationshipDirection.OUT)
				.withType(RelationshipType.ACCESSES));

		if (readsWrites.isEmpty()) {
			return DataLineageResult.empty();
		}

		final List<DataFlowNodePrototype> changes = new ArrayList<>();
		for (final ModuleRelationshipPojo rw : readsWrites) {
			final var adjacentModuleOpt = moduleService.findAnyModuleLightweight(q -> q.byUid(rw.getDstModule()));
			if (adjacentModuleOpt.isPresent()) {
				final ModuleBasePojo adjacentModule = adjacentModuleOpt.get();
				final String fileName = adjacentModule.getName();
				if (Objects.equals(fileNameProperty, fileName)) {
					final Optional<ProxyContainerPojo> fileContainer = dataLineageCoreService.getProxyContainersOfType(
							context, EntityId.of(adjacentModule.getId()), ProxyContainerPojo.Type.RESOURCE_FILE).stream().findAny();
					if (fileContainer.isPresent()) {
						final DataFlowNodePojo targetField = dataFlowService.get(fileContainer.get().getFieldNodesUids().get(0));
						final List<DataFlowNodePojo> fields = dataFlowService.find(q -> q.byIds(proxyContainer.getFieldNodesUids()));
						addDataFlow(fields, changes, targetField);
					}
				}
			}
		}

		return DataLineageResult.ofNodes(changes);
	}
	
	private void addDataFlow(final List<DataFlowNodePojo> fields, final List<DataFlowNodePrototype> changes, final DataFlowNodePojo targetField) {
		for (final DataFlowNodePojo field : fields) {
			/* should determine direction of data flow here */
			changes.add(new DataFlowNodePrototype(field.getDataFlowId()).addDataFlow(DataFlow.toField(targetField)));
		}
	}
}
