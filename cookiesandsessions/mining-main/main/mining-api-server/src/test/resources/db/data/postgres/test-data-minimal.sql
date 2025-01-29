--Delete data - Short because of ON DELETE CASCADEs :)

DELETE FROM custom_enum;
DELETE FROM custom_property;
DELETE FROM custom_property_entities;

DELETE FROM project WHERE nid <> 0;
DELETE FROM client WHERE nid <> 0;
DELETE FROM taxonomy_category WHERE project <> (SELECT uid FROM project WHERE nid = 0);

DELETE FROM data_dictionary_other_scope;

DELETE FROM dna_community;
DELETE FROM dna_community_modules;
DELETE FROM dna_snapshot;
DELETE FROM dna_similarity;
DELETE FROM dna_string;
DELETE FROM dna_string_element;

DELETE FROM saved_search WHERE project != (SELECT uid FROM project WHERE nid = 0) OR client != (SELECT uid FROM client WHERE nid = 0);
DELETE FROM job_info;
DELETE FROM field_info;
DELETE FROM module_undiscovered;
DELETE FROM effort_summary;

DELETE FROM taxonomy;
DELETE FROM taxonomy_type;
DELETE FROM taxonomy_category WHERE project NOT IN (SELECT uid FROM project WHERE nid = 0);

DELETE FROM module WHERE project NOT IN (SELECT uid FROM project WHERE nid = 0);

DELETE FROM annotation;
DELETE FROM annotation_category WHERE project NOT IN (SELECT uid FROM project WHERE nid = 0);

DELETE FROM source;

DELETE FROM mining_job_info;

DELETE FROM dependency_definition;

--RESET SEQUENCES
ALTER SEQUENCE client_nid RESTART;
ALTER SEQUENCE project_nid RESTART;
ALTER SEQUENCE source_nid RESTART;
ALTER SEQUENCE module_nid RESTART WITH 2000;
ALTER SEQUENCE statement_nid RESTART;
ALTER SEQUENCE annotation_nid RESTART;
ALTER SEQUENCE annotation_category_id RESTART WITH 1001;
ALTER SEQUENCE data_dictionary_nid RESTART;
ALTER SEQUENCE taxonomy_nid RESTART;
ALTER SEQUENCE taxonomy_category_id RESTART WITH 3;
ALTER SEQUENCE saved_search_id RESTART WITH 1999;


--TEST CLIENTS
INSERT INTO client (uid, name) values (gen_random_uuid(), 'Demo Client 1');
INSERT INTO client (uid, name) values (gen_random_uuid(), 'Demo Client 2');


--TEST PROJECTS
INSERT INTO Project(uid,client,name) select gen_random_uuid(),uid,'Demo Project A' from client where nid=1;
INSERT INTO Project(uid,client,name) select gen_random_uuid(),uid,'Demo Project B' from client where nid=1;
INSERT INTO Project(uid,client,name) select gen_random_uuid(),uid,'Demo Project C' from client where nid=2;
INSERT INTO Project(uid,client,name) select gen_random_uuid(),uid,'Demo Project D' from client where nid=2;

UPDATE PROJECT SET technical_taxonomy_category=1,default_taxonomy_category=2  where nid=0;
UPDATE PROJECT SET technical_taxonomy_category=3,default_taxonomy_category=4  where nid=1;
UPDATE PROJECT SET technical_taxonomy_category=5,default_taxonomy_category=6   where nid=2;
UPDATE PROJECT SET technical_taxonomy_category=7,default_taxonomy_category=8   where nid=3;
UPDATE PROJECT SET technical_taxonomy_category=9,default_taxonomy_category=10  where nid=4;

--TEST TAXONOMY
INSERT INTO taxonomy_category(name, project) VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid=1));
INSERT INTO taxonomy_category(name, project) VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid=1));
INSERT INTO taxonomy_category(name, project) VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid=2));
INSERT INTO taxonomy_category(name, project) VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid=2));
INSERT INTO taxonomy_category(name, project) VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid=3));
INSERT INTO taxonomy_category(name, project) VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid=3));
INSERT INTO taxonomy_category(name, project) VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid=4));
INSERT INTO taxonomy_category(name, project) VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid=4));

INSERT INTO taxonomy_type(id, name, project, category) VALUES (gen_random_uuid(), 'DataDomain', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_category WHERE name='Business Taxonomies' AND project = (SELECT uid FROM project WHERE nid=1)));
INSERT INTO taxonomy_type(id, name, project, category) VALUES (gen_random_uuid(), 'BusinessProcess', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_category WHERE name='Business Taxonomies' AND project = (SELECT uid FROM project WHERE nid=1)));
INSERT INTO taxonomy_type(id, name, project, category) VALUES (gen_random_uuid(), 'BusinessSubsystem', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_category WHERE name='Business Taxonomies' AND project = (SELECT uid FROM project WHERE nid=1)));
