--################################################################################################
-- WDIS-473 : Add index on SourceObject.name to improve query performance.
--################################################################################################

CREATE INDEX projectLink_name_idx ON SourceObject (projectLink, name) NOTUNIQUE_HASH_INDEX;
