/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.unknown;

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
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;

/**
 * Data Lineage Tracer component creating {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#ENTRY_POINT}
 * proxy containers on Unknown modules.
 */
@Component
public class UnknownTracer implements DataLineageTracer {

	/**
	 * Name of the proxy field created for Unknown modules
	 */
	public static final String PLACEHOLDER_FIELD_NAME = "(unknown)";

	protected static final Set<Type> TYPES_SET = Set.of(Type.UNKNOWN, Type.UTILITY, Type.PROGRAM, Type.SUBROUTINE, Type.COPYBOOK);

	/**
	 * Returns true if the given Module has Unknown technology and UNKNOWN/UTILITY/PROGRAM/SUBROUTINE/COPYBOOK type.
	 * @param module the Module
	 * @return  {@code true} if supported
	 */
	@Override
	public boolean isSupported(final ModuleLightweightPojo module) {
		return module.getTechnology() == Technology.UNKNOWN && TYPES_SET.contains(module.getType());
	}

	/**
	 * If the module is of technology {@link Technology#UNKNOWN}, Returns {@link DataLineageResult} result with a ProxyContainer of
	 * type {@link ProxyContainerPojo.Type#ENTRY_POINT} with a single field named {@link #PLACEHOLDER_FIELD_NAME}.
	 * @param context the context for the operation
	 * @param module the module to trace
	 * @return
	 */
	@Override
	public DataLineageResult trace(final DataLineageContext context, final ModuleLightweightPojo module) {
		final ProxyContainerPrototype container = new ProxyContainerPrototype(
				DataFlowId.forProxyContainer(module.getId(), ProxyContainerPojo.Type.ENTRY_POINT, null));
		container.setType(ProxyContainerPojo.Type.ENTRY_POINT);
		final List<DataFlowNodePrototype> fields = List.of(
				new DataFlowNodePrototype(DataFlowId.forProxyField(container.dataFlowId, 0))
						.setName(PLACEHOLDER_FIELD_NAME)
		);
		container.setFields(fields);
		return DataLineageResult.ofProxyContainers(List.of(container));
	}

	@Override
	public DataLineageResult discoverProxyContainersOfType(final DataLineageContext context, final ModuleLightweightPojo module,
			final ProxyContainerPojo.Type type) {
		return trace(context, module);
	}
}
