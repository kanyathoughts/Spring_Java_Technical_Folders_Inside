-- Creates TaxonomyCategory table

CREATE SEQUENCE TaxonomyCategory_Sequence IF NOT EXISTS TYPE ORDERED START 0 INCREMENT 1;

CREATE CLASS TaxonomyCategory IF NOT EXISTS EXTENDS V;
CREATE PROPERTY TaxonomyCategory.id IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TaxonomyCategory.name IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TaxonomyCategory.projectLink IF NOT EXISTS LINK Project (NOTNULL, MANDATORY TRUE);

CREATE INDEX TaxonomyCategory_id_idx IF NOT EXISTS ON TaxonomyCategory (id) UNIQUE;
CREATE INDEX TaxonomyCategory_project_name_idx IF NOT EXISTS ON TaxonomyCategory (projectLink, name) UNIQUE;