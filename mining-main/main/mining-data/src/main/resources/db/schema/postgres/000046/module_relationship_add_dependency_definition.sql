ALTER TABLE module_relationship
	ADD COLUMN dependency_definition uuid REFERENCES dependency_definition ON DELETE SET NULL;

CREATE INDEX module_relationship_dependency_definition_idx ON module_relationship (dependency_definition);
