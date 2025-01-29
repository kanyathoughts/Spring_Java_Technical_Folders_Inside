CREATE INDEX RefersTo_out_idx IF NOT EXISTS ON RefersTo (out) NOTUNIQUE;
CREATE INDEX ReDefines_out_idx IF NOT EXISTS ON ReDefines (out) NOTUNIQUE;
CREATE INDEX HasAst_out_idx IF NOT EXISTS ON  HasAst(out) NOTUNIQUE;
CREATE INDEX BelongsToCluster_out_idx IF NOT EXISTS ON  BelongsToCluster(out) NOTUNIQUE;
CREATE INDEX DataFlowNode_module_idx IF NOT EXISTS ON DataFlowNode(module) NOTUNIQUE;