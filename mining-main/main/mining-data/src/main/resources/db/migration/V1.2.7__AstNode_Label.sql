-- additionally property label for display in the control flow graph visualization
CREATE PROPERTY AstNode.label IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);

-- set the label for all existing AstNodes to an empty string
UPDATE AstNode SET label="";

-- create index after data was added speeds up the migration
CREATE INDEX AstNode_label_idx IF NOT EXISTS ON AstNode (label) NOTUNIQUE;