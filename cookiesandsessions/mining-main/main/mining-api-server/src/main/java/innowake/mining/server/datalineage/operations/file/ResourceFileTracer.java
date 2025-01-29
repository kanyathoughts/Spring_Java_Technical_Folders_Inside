/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.file;

import java.util.List;

import org.springframework.stereotype.Component;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.operations.DataLineageTracer;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.mining.shared.model.datalineage.ProxyContainerPrototype;

/**
 * Data Lineage Tracer component creating {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#RESOURCE_FILE}
 * proxy containers on file modules.
 */
@Component
public class ResourceFileTracer implements DataLineageTracer {

	/* name for the proxy field that is created to store the file data */
	public static final String PLACEHOLDER_FIELD_NAME = "(file content)";

	@Override
	public boolean isSupported(final ModuleLightweightPojo module) {
		return module.getTechnology() == Technology.RESOURCE
				&& (module.getType() == Type.FILE || module.getType() == Type.VSAM_FILE || module.getType() == Type.GDG_FILE);
	}

	@Override
	public DataLineageResult trace(final DataLineageContext context, final ModuleLightweightPojo module) {
		final ProxyContainerPrototype container = new ProxyContainerPrototype(DataFlowId.forProxyContainer(module.getId(), ProxyContainerPojo.Type.RESOURCE_FILE, null));
		container.setType(ProxyContainerPojo.Type.RESOURCE_FILE);
		final List<DataFlowNodePrototype> fields = List.of(
				new DataFlowNodePrototype(DataFlowId.forProxyField(container.dataFlowId, 0))
						.setName(PLACEHOLDER_FIELD_NAME)
		);
		container.setFields(fields);
		return DataLineageResult.ofProxyContainers(List.of(container));
	}

	@Override
	public DataLineageResult discoverProxyContainersOfType(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo.Type type) {
		return trace(context, module);
	}
}
