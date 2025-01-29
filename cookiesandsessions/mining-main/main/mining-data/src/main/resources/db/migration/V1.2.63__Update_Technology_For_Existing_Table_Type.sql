-- Discovery Project technology type update from resource to sql for tables

UPDATE ObjectType SET technologyLink = (SELECT FROM TechnologyEnum WHERE name = 'SQL') WHERE typeLink.name = 'TABLE';
