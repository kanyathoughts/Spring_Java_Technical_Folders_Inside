--Update all existing References from Resource to File in ObjectType and SourceObject
UPDATE ObjectType SET typeLink = (SELECT FROM TypeEnum WHERE name = 'FILE') WHERE technologyLink.name = 'RESOURCE' AND typeLink.name = 'RESOURCE';
UPDATE SourceObject SET typeLink = (SELECT FROM TypeEnum WHERE name = 'FILE') WHERE technologyLink.name = 'RESOURCE' AND typeLink.name = 'RESOURCE';

--Delete Resource from TypeEnum
DELETE VERTEX FROM TypeEnum where name = 'RESOURCE'; 
