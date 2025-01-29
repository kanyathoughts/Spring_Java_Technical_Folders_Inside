
--####################### Delete all Dependency definitions from the DB #######################--
DELETE FROM DependencyDefinition UNSAFE;
--####################### CREATE ID and projectLink property #######################--
CREATE PROPERTY DependencyDefinition.id IF NOT EXISTS Long (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY DependencyDefinition.projectLink IF NOT EXISTS LINK Project (NOTNULL, MANDATORY TRUE);
--####################### CREATE dependencyDefinition sequence #######################--
CREATE SEQUENCE DependencyDefinition_Sequence IF NOT EXISTS TYPE ORDERED START 0 INCREMENT 1;
--####################### CREATE Index on id and projectId #######################--
CREATE INDEX DependencyDefinition_id_idx IF NOT EXISTS ON dependencyDefinition (id) UNIQUE;
