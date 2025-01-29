CREATE CLASS ConditionalDependency IF NOT EXISTS;
CREATE PROPERTY ConditionalDependency.ifReachedFrom IF NOT EXISTS LINKSET Module (NOTNULL, DEFAULT []);

CREATE PROPERTY Reference.conditionalDependency IF NOT EXISTS EMBEDDED ConditionalDependency;