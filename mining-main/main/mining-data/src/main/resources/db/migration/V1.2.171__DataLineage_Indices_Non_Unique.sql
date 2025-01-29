DROP INDEX DataFlowNode_module_ast_idx;
DELETE FROM DataFlowNode;
CREATE INDEX DataFlowNode_module_ast_idx IF NOT EXISTS ON DataFlowNode (module, astNode) NOTUNIQUE;