--Update all existing References from IMS ARCHDEF to UNKNOWN UTILITIES
UPDATE ObjectType SET technologyLink = (SELECT FROM TechnologyEnum WHERE name = 'UNKNOWN'), typeLink = 
	(SELECT FROM TypeEnum WHERE name = 'UTILITY') WHERE typeLink.name = 'ARCHDEF';
	
UPDATE SourceObject SET technologyLink = (SELECT FROM TechnologyEnum WHERE name = 'UNKNOWN'), typeLink = 
	(SELECT FROM TypeEnum WHERE name = 'UTILITY') WHERE typeLink.name = 'ARCHDEF';

--Delete ARCHDEF from TypeEnum
DELETE VERTEX FROM TypeEnum WHERE name = 'ARCHDEF'; 