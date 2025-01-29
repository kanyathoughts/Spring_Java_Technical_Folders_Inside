DELETE FROM DataFlowNode;
DELETE FROM ProxyContainer;

CREATE PROPERTY DataFlowNode.dataFlowId STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ProxyContainer.dataFlowId STRING (NOTNULL, MANDATORY TRUE);

CREATE INDEX DataFlowNode_dataFlowId_idx ON DataFlowNode (dataFlowId) UNIQUE;
CREATE INDEX ProxyContainer_dataFlowId_idx ON ProxyContainer (dataFlowId) UNIQUE;