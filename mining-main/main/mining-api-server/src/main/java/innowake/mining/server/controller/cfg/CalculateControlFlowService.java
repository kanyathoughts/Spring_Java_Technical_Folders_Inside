/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller.cfg;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.mining.data.model.springdata.JclControlFlowNodeMetadata;
import innowake.mining.server.cfg.CfgDatasetDependenciesMetadata;
import innowake.mining.server.cfg.CfgNodeMetadata;
import innowake.mining.server.cfg.JclCfgMetadataProvider;
import innowake.mining.server.cfg.JclNodeBasedModuleResolver;
import innowake.mining.server.cfg.NodeBasedModuleResolver;
import innowake.mining.server.cfg.CfgNodeMetadata.MetadataType;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Service to calculate the control flow graph.
 */
@Service
public class CalculateControlFlowService {
	
	@Autowired
	private ExecutorService executorService;

	@Autowired
	private AstService astService;

	@Autowired
	protected transient ModuleService moduleService;
	
	/**
	 * Calculate the control flow graph for the given module id.
	 *
	 * @param moduleId the id of the module
	 */
	public void calculateControlFlowGraph(final EntityId moduleId) {
		calculateCfg(moduleId);
		/* Get module because we only want to add metadata for JCL Jobs */
		final ModuleLightweightPojo module = moduleService.getModuleLightweight(moduleId);
		if (module.getTechnology() == Technology.JCL && module.getType() == Type.JOB) {
			addMetadata(moduleId);
		}
	}
	
	/**
	 * Calculate the control flow graph for the given module.
	 *
	 * @param module the {@link Module}
	 */
	public void calculateControlFlowGraph(final ModuleLightweightPojo module) {
		final EntityId moduleId = EntityId.of(module.getUid(), module.getId());
		calculateCfg(moduleId);
		if (module.getTechnology().equals(Technology.JCL) && module.getType().equals(Type.JOB)) {
			addMetadata(moduleId);
		}
	}
	
	private void calculateCfg(final EntityId moduleId) {
		final boolean result = executorService.executeControlFlowCalculation(moduleId);
		if ( ! result) {
			throw new IllegalStateException("Unable to calculate control flow for module with Id " + moduleId);
		}
	}
	
	/**
	 * Iterates over JclStepExec-AstNodes of a module with a specified ID and adds metadata to the AstNodes using {@link JclCfgMetadataProvider}.
	 *
	 * @param moduleId the id of the module the metadata is added to
	 */
	private void addMetadata(final EntityId moduleId) {
		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(moduleId).withType("JclStepExec"));
		final NodeBasedModuleResolver moduleResolver = new JclNodeBasedModuleResolver(moduleService);
		final JclCfgMetadataProvider metadataProvider = new JclCfgMetadataProvider(moduleService);
		for (final AstNodePojo astNode : astNodes) {
			moduleResolver.resolveModule(astNode).ifPresent(jclStep -> {
				final List<CfgNodeMetadata> metadata = metadataProvider.getMetadata(jclStep, MetadataType.DATASET_DEPENDENCIES);
				if (metadata.isEmpty()) {
					return;
				}
				/* We get the first entry in the list because currently only CfgDatasetDependenciesMetadata is available */
				final CfgNodeMetadata cfgNodeMetadata = metadata.get(0);
				if (cfgNodeMetadata instanceof CfgDatasetDependenciesMetadata) {
					final CfgDatasetDependenciesMetadata cfgDatasetDependenciesMetadata = (CfgDatasetDependenciesMetadata) cfgNodeMetadata;
					final JclControlFlowNodeMetadata jclMetadata = new JclControlFlowNodeMetadata();
					jclMetadata.setInputFileIds(cfgDatasetDependenciesMetadata.getInputDatasets().stream().map(ModulePojo::getId).collect(Collectors.toList()));
					jclMetadata.setOutputFileIds(cfgDatasetDependenciesMetadata.getOutputDatasets().stream().map(ModulePojo::getId).collect(Collectors.toList()));
					astService.update(new AstNodePojoPrototype().setId(astNode.getId()).setProperties(PojoMapper.convert(jclMetadata)));
				}
			});
		}
	}
}
