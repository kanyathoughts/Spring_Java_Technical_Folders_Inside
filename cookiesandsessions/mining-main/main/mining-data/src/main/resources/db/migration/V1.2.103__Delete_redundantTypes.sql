--Update all existing References to UNKNOWN in ObjectType and SourceObject
UPDATE ObjectType SET typeLink = (SELECT FROM TypeEnum WHERE name = 'UNKNOWN') where typeLink.name = technologyLink.name;
UPDATE ObjectType SET typeLink = (SELECT FROM TypeEnum WHERE name = 'UNKNOWN') where typeLink.name = 'UNDISCOVERED';
UPDATE SourceObject SET typeLink = (SELECT FROM TypeEnum WHERE name = 'UNKNOWN') where typeLink.name = technologyLink.name;
UPDATE SourceObject SET typeLink = (SELECT FROM TypeEnum WHERE name = 'UNKNOWN') where typeLink.name = 'UNDISCOVERED';

DELETE VERTEX from typeEnum where name IN (SELECT name from technologyEnum where name != 'UNKNOWN');
DELETE VERTEX from typeEnum where name = 'UNDISCOVERED';