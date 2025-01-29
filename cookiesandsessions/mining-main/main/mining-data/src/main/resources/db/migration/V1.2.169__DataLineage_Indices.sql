CREATE INDEX DataFlowNode_module_idx IF NOT EXISTS ON DataFlowNode (module) NOTUNIQUE;
CREATE INDEX DataFlowNode_module_ast_idx IF NOT EXISTS ON DataFlowNode (module, astNode) UNIQUE;
CREATE INDEX ProxyContainer_module_idx IF NOT EXISTS ON ProxyContainer (module) NOTUNIQUE;
CREATE INDEX ProxyContainer_module_type_idx IF NOT EXISTS ON ProxyContainer (module, type) NOTUNIQUE;
CREATE INDEX DataFlowNode_module_statement_idx IF NOT EXISTS ON ProxyContainer (module, statement) NOTUNIQUE;