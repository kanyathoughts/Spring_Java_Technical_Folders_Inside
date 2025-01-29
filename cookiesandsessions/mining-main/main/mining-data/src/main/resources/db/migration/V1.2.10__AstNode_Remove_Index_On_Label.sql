-- remove the index on the label property as it's not needed and leads to errors when the value of the indexed field is too large
DROP INDEX AstNode_label_idx IF EXISTS;