-- DELETE DATA
DELETE FROM project WHERE nid<>0;

-- RESET SEQUENCES
ALTER SEQUENCE project_nid RESTART;
ALTER SEQUENCE module_nid RESTART WITH 2000;
ALTER SEQUENCE taxonomy_category_id RESTART WITH 3;

-- TEST PROJECTS
INSERT INTO Project(uid,client,name) select gen_random_uuid(),uid,'Demo Project E' from client where nid=1;
UPDATE PROJECT SET technical_taxonomy_category=1,default_taxonomy_category=2  where nid=1;

-- TEST TAXONOMY CATEGORY
INSERT INTO taxonomy_category (name, project)
    VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid = 1));
INSERT INTO taxonomy_category (name, project)
    VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid = 1));
-- TEST MODULES

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash1',
        'MMRS711A', 'src/jcl/jobs/MMRS711A.job', (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'JCL', 'JOB', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');
INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash2',
        'MMRS711B', 'src/jcl/jobs/MMRS711B.job', (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'JCL', 'JOB', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');
INSERT INTO module(uid, link_hash,
        name, description, project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash3',
        'STEPDEF.IDCAMS', 'A test description for STEPDEF.IDCAMS', (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'JCL', 'EXEC_PGM', 'FILE_SECTION', 'CUSTOM',
        true, 'DISCOVERY');
INSERT INTO module(uid, link_hash,
        name, description, project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash4',
        'STEPDEF.IDCAMS', 'A test description for STEPDEF.IDCAMS', (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'JCL', 'EXEC_PGM', 'FILE_SECTION', 'CUSTOM',
        true, 'DISCOVERY');

-- TEST CONTAINS MODULE
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE nid=2000),
    (SELECT uid FROM module WHERE nid=2002),
    'CONTAINS');
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE nid=2001),
    (SELECT uid FROM module WHERE nid=2003),
    'CONTAINS');
