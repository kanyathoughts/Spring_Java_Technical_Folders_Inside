--################################################################################################
-- Source Object
--################################################################################################
CREATE PROPERTY SourceObject.technologyLink LINK TechnologyEnum (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SourceObject.typeLink LINK TypeEnum (NOTNULL, MANDATORY TRUE);
