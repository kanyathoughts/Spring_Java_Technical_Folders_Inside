/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing.modelimport;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.fieldtracing.service.FieldTracingModelImportService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.ProxyContainerPrototype;
import innowake.ndt.fieldtracing.model.FieldDefinition;
import innowake.ndt.fieldtracing.model.Model;
import innowake.ndt.fieldtracing.model.SourceFile;

import java.util.HashMap;
import java.util.Map;

/**
 * Holds information used during a model import operation.
 * @see FieldTracingModelImportService#importModel(DataLineageContext, Model)
 */
public class ModelImportContext {
	
	private final DataLineageContext dataLineageContext;
	
	private final FieldDefinition<ModuleLightweightPojo> selectedField;
	
	private final Map<DataFlowId, DataFlowNodePrototype> createdNodes = new HashMap<>();
	private final Map<DataFlowId, DataFlowNodePrototype> createdStatements = new HashMap<>();
	private final Map<DataFlowId, ProxyContainerPrototype> createdContainers = new HashMap<>();

	private SourceFile<ModuleLightweightPojo> sourceFile;

	/**
	 * Create new context object with given {@code projectId}.
	 * @param dataLineageContext the data lineage context in which the model import is executed
	 * @param selectedField the initial field that the field tracer was invoked on
	 * @param sourceFile the source file containing the field we are importing
	 */
	public ModelImportContext(final DataLineageContext dataLineageContext, final FieldDefinition<ModuleLightweightPojo> selectedField, final SourceFile<ModuleLightweightPojo> sourceFile) {
		this.dataLineageContext = dataLineageContext;
		this.selectedField = selectedField;
		this.sourceFile = sourceFile;
	}

	/**
	 * Returns the data lineage context in which the model import is executed.
	 * @return the data lineage context
	 */
	public DataLineageContext getDataLineageContext() {
		return dataLineageContext;
	}

	/**
	 * Collects all nodes created during the import operation, to allow bulk-insert (or update)
	 * into the database at the end of the import operation.
	 *
	 * @return the set of collected DataFlowNodes
	 */
	public Map<DataFlowId, DataFlowNodePrototype> getCreatedNodes() {
		return createdNodes;
	}
	
	/**
	 * Collects all proxy containers created during the import operation, to allow bulk-insert (or update)
	 * into the database at the end of the import operation.
	 *
	 * @return the list of collected proxy containers
	 */
	public Map<DataFlowId, ProxyContainerPrototype> getCreatedContainers() {
		return createdContainers;
	}

	/**
	 * Collects all Statement nodes created during the import operation, to allow bulk-insert (or update)
	 * into the database at the end of the import operation.
	 *
	 * @return the list of collected DataFlowNodes
	 */
	public Map<DataFlowId, DataFlowNodePrototype> getCreatedStatements() {
		return createdStatements;
	}

	/**
	 * Gets the "selected field" i.e. the field on which field tracing was invoked.
	 * @return the selected field for which the model was created
	 */
	public FieldDefinition<ModuleLightweightPojo> getSelectedField() {
		return selectedField;
	}
	
	/**
	 * @return the source file the imported field belongs to.
	 */
	public SourceFile<ModuleLightweightPojo> getSourceFile() {
		return sourceFile;
	}
}

