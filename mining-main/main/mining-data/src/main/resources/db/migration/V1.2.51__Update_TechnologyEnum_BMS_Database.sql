--Update all existing References from BMS & Database to COBOL & Resource respectively in ObjectType and SourceObject
UPDATE ObjectType SET technologyLink = (SELECT FROM TechnologyEnum WHERE name = 'COBOL') WHERE technologyLink.name = 'BMS';
UPDATE ObjectType SET technologyLink = (SELECT FROM TechnologyEnum WHERE name = 'RESOURCE') WHERE technologyLink.name = 'DATABASE';
UPDATE SourceObject SET technologyLink = (SELECT FROM TechnologyEnum WHERE name = 'COBOL') WHERE technologyLink.name = 'BMS';
UPDATE SourceObject SET technologyLink = (SELECT FROM TechnologyEnum WHERE name = 'RESOURCE') WHERE technologyLink.name = 'DATABASE';

--Delete BMS & DATABASE from TechnologyEnum
DELETE VERTEX FROM TechnologyEnum WHERE name IN ['BMS', 'DATABASE']; 