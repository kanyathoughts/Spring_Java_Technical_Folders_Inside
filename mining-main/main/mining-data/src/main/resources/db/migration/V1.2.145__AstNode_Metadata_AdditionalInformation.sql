-- in order to use AdditionalInfo on AstNode the class must extend MiningEntity
ALTER CLASS AstNode SUPERCLASS MiningEntity;

-- the metadata for the JCL control flow will be persisted as AdditionalInfo
CREATE CLASS JclControlFlowNodeMetadata IF NOT EXISTS EXTENDS AdditionalInfo;
CREATE PROPERTY JclControlFlowNodeMetadata.inputFiles IF NOT EXISTS LINKLIST Module;
CREATE PROPERTY JclControlFlowNodeMetadata.outputFiles  IF NOT EXISTS LINKLIST Module;