/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.cfg;

import java.util.List;

import innowake.mining.shared.entities.ModulePojo;

/**
 * Metadata for a CfgNodes which contains input and output datasets.
 */
public class CfgDatasetDependenciesMetadata implements CfgNodeMetadata {
	
	private final List<ModulePojo> outputDatasets;
	private final List<ModulePojo> inputDatasets;
	
	public CfgDatasetDependenciesMetadata (final List<ModulePojo> outputDatasets, final List<ModulePojo> inputDatasets) {
		this.outputDatasets = outputDatasets;
		this.inputDatasets = inputDatasets;
	}
	
	@Override
    public MetadataType getType() {
        return MetadataType.DATASET_DEPENDENCIES;
    }
 
	/**
	 * @return list of modules representing the output datasets. The CFG Implementation can decide which information of the module it will actually display
	 */
    public List<ModulePojo> getOutputDatasets() {
    	return outputDatasets;
    }
 
    /**
     * @return list of modules representing the input datasets. The CFG Implementation can decide which information of the module it will actually display
     */
    public List<ModulePojo> getInputDatasets() {
    	return inputDatasets;
    }
}
