/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.sql;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import org.springframework.stereotype.Component;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.operations.DataLineageTracer;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.FieldInfoPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.mining.shared.model.datalineage.ProxyContainerPrototype;

/**
 * Data Lineage Tracer component for tracing the data flow "inside of" an SQL table.
 * <p>
 * Since there is no internal data flow inside a table, this component currently only creates an
 * {@linkplain innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#DATABASE_TABLE DATABASE_TABLE} Proxy Container
 * containing Fields for each column of the table.
 * <p>
 * If the schema of the table is unknown then a single Proxy Field is created representing the entire table.
 */
@Component
public class SqlTableTracer implements DataLineageTracer {

	/**
	 * Name of placeholder field created when table schema (i.e. columns) is unknown.
	 */
	public static final String PLACEHOLDER_FIELD_NAME = "(dataTable)";

	private final FieldInfoService fieldInfoService;

	/**
	 * Constructor.
	 * @param fieldInfoService Data access for the database schema import.
	 */
	public SqlTableTracer(final FieldInfoService fieldInfoService) {
		this.fieldInfoService = fieldInfoService;
	}

	@Override
	public boolean isSupported(final ModuleLightweightPojo module) {
		return module.getTechnology() == Technology.SQL && module.getType() == Type.TABLE;
	}

	@Override
	public DataLineageResult trace(final DataLineageContext context, final ModuleLightweightPojo module) {
		final List<FieldInfoPojo> fieldInfos = fieldInfoService.find(q -> q.ofProject(context.getProjectId())
																			.ofModule(EntityId.of(module.getId())));
		final ProxyContainerPrototype container = new ProxyContainerPrototype(DataFlowId.forProxyContainer(module.getId(), ProxyContainerPojo.Type.DATABASE_TABLE, null));
		container.setType(ProxyContainerPojo.Type.DATABASE_TABLE);
		final List<DataFlowNodePrototype> fields = new ArrayList<>();

		if (fieldInfos.isEmpty()) {
			fields.add(new DataFlowNodePrototype(DataFlowId.forProxyField(container.dataFlowId, 0))
					.setName(PLACEHOLDER_FIELD_NAME));
		} else {
			fieldInfos.stream()
					.sorted(Comparator.comparingInt(FieldInfoPojo::getOrdinal))
					.map(fieldInfo -> new DataFlowNodePrototype(DataFlowId.forProxyField(container.dataFlowId, fieldInfo.getOrdinal()))
							.setName(fieldInfo.getName()))
					.forEach(fields::add);
		}
		container.setFields(fields);

		return DataLineageResult.ofProxyContainers(List.of(container));
	}

	@Override
	public DataLineageResult discoverProxyContainersOfType(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo.Type type) {
		return trace(context, module);
	}
}
