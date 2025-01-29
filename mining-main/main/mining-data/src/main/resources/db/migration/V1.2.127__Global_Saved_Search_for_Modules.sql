--################################################################################################
-- Global Saved Search for Modules
--################################################################################################

INSERT INTO SavedSearch SET name="Requires Review", usage="miningUi.modulesTable", projectLink=(SELECT FROM Project WHERE id=0), savedSearch='columns=Module.name&columns=Module.requiresReview&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&page=1&sort=name;ASC&filter=[{"key":"requiresReview","value":[true]}]';