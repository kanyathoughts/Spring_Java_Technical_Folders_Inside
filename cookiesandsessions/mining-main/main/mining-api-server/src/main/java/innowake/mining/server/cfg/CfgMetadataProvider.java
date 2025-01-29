/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.cfg;

import java.util.List;

import innowake.mining.server.cfg.CfgNodeMetadata.MetadataType;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Used to obtain metadata for Control-Flow-Graph nodes
 */
public interface CfgMetadataProvider {

	/**
	 * Returns all available metadata for the given module
	 *
	 * @param module module of which the metadata is supposed to be returned
	 * @return metadata of module
	 */
    public List<CfgNodeMetadata> getMetadata(ModulePojo module);
  
    /**
     * Returns available metadata with the given type for the given module
     *
     * @param module module of which the metadata is supposed to be returned
     * @param type {@link MetadataType} of the requested metadata
     * @return metadata of module with given {@link MetadataType}
     */
    public List<CfgNodeMetadata> getMetadata(ModulePojo module, MetadataType type);
    
}
