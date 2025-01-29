ALTER PROPERTY DependencyDefinition.moduleFilter NOTNULL FALSE;
ALTER PROPERTY DependencyDefinition.moduleFilter MANDATORY FALSE;
UPDATE DependencyDefinition REMOVE moduleFilter;
DROP PROPERTY DependencyDefinition.moduleFilter IF EXISTS FORCE;
DROP CLASS ModuleFilter IF EXISTS;
CREATE PROPERTY DependencyDefinition.moduleFilter IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
