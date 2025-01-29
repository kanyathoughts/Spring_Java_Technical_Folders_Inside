/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.cfg;

/**
 * Metadata of a Control-Flow-Graph node. Can have language specific implementations.
 */
public interface CfgNodeMetadata {
    public MetadataType getType();
    
    public enum MetadataType {
        DATASET_DEPENDENCIES
    }
}
