UPDATE ObjectType SET 
technologyLink=(SELECT FROM TechnologyEnum WHERE name="UNKNOWN"),
typeLink=(SELECT FROM TypeEnum WHERE name="UTILITY"),
storageLink=(SELECT FROM StorageEnum WHERE name="UNDEFINED"),
originLink=(SELECT FROM OriginEnum WHERE name="ENVIRONMENT")
UPSERT
WHERE
technologyLink=(SELECT FROM TechnologyEnum WHERE name="UNKNOWN") AND
typeLink=(SELECT FROM TypeEnum WHERE name="UTILITY") AND
storageLink=(SELECT FROM StorageEnum WHERE name="UNDEFINED") AND
originLink=(SELECT FROM OriginEnum WHERE name="ENVIRONMENT");

UPDATE module SET objectTypeLink =
(SELECT FROM ObjectType WHERE technologyLink = (SELECT FROM TechnologyEnum WHERE name="UNKNOWN")
AND
typeLink = (SELECT FROM TypeEnum WHERE name="UTILITY") AND 
storageLink=(SELECT FROM StorageEnum WHERE name="UNDEFINED") AND
originLink=(SELECT FROM OriginEnum WHERE name="ENVIRONMENT")) 
where projectLink.id=0 AND objectTypeLink IN (
SELECT FROM ObjectType WHERE technologyLink = (SELECT FROM TechnologyEnum WHERE name="UNKNOWN")AND
typeLink = (SELECT FROM TypeEnum WHERE name="UNKNOWN") AND 
storageLink=(SELECT FROM StorageEnum WHERE name="UNDEFINED") AND
originLink=(SELECT FROM OriginEnum WHERE name="ENVIRONMENT"));