--################################################################################################
-- Dna Data
--################################################################################################

CREATE CLASS DnaData EXTENDS V;
CREATE PROPERTY DnaData.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY DnaData.files EMBEDDEDMAP STRING;
ALTER CLASS DnaData STRICTMODE TRUE;
CREATE INDEX DnaData_projectLink_idx ON DnaData (projectLink) UNIQUE;