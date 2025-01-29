--###################################################################################################################################
--## TEST MODULES
--###################################################################################################################################
-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=1),
	    utf8_to_bytes(E'Some Data')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('/src/MOD 1', 'MOD 1', 'COBOL', 'PROGRAM',
                1, 1, decode('B20025F804F0839F0D7BF462A75ED1B8','hex'), (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator, content_hash)
    VALUES (gen_random_uuid(), 'DummyHash1',
        'MOD 1', '/src/MOD 1', null,
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 1),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', decode('B20025F804F0839F0D7BF462A75ED1B8','hex'));

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid = 2),
	    utf8_to_bytes(E'Some Data')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('/src/MOD 2', 'MOD 2', 'COBOL', 'PROGRAM',
                1, 1, decode('B20025F804F0839F0D7BF462A75ED1B8','hex'), (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator, content_hash)
    VALUES (gen_random_uuid(), 'DummyHash2',
        'MOD 1', '/src/MOD 1', null,
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 2),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', decode('B20025F804F0839F0D7BF462A75ED1B8','hex'));

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid = 3),
	    utf8_to_bytes(E'Some Data')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('/src/MOD 3', 'MOD 3', 'COBOL', 'PROGRAM',
                1, 1, decode('B20025F804F0839F0D7BF462A75ED1B8','hex'), (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator, content_hash)
    VALUES (gen_random_uuid(), 'DummyHash3',
        'MOD 3', '/src/MOD 3', 'A test program',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 3),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', decode('B20025F804F0839F0D7BF462A75ED1B8','hex'));
        
-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid = 4),
	    utf8_to_bytes(E'Some Data')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('/src/MOD 4', 'MOD 4', 'COBOL', 'PROGRAM',
                1, 1, decode('B20025F804F0839F0D7BF462A75ED1B8','hex'), (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator, content_hash)
    VALUES (gen_random_uuid(), 'DummyHash4',
        'MOD 4', '/src/MOD 4', 'A test program',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 4),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', decode('B20025F804F0839F0D7BF462A75ED1B8','hex'));

--##################################################################################################################################§#
--## TEST TAXONOMY
--###################################################################################################################################

INSERT INTO taxonomy_type(id, name, project, category)
    VALUES (gen_random_uuid(), 'DataDomain', (SELECT uid FROM project WHERE nid = 3),
    (SELECT id FROM taxonomy_category WHERE name='Business Taxonomies' AND project = (SELECT uid FROM project WHERE nid = 3)));
INSERT INTO taxonomy_type(id, name, project, category)
    VALUES (gen_random_uuid(), 'BusinessDomain', (SELECT uid FROM project WHERE nid = 4),
    (SELECT id FROM taxonomy_category WHERE name='Technical Taxonomies' AND project = (SELECT uid FROM project WHERE nid = 4)));

INSERT INTO taxonomy(uid, name, project, type)
    VALUES (gen_random_uuid(), 'Taxonomy 1', (SELECT uid FROM project WHERE nid = 3),
    (SELECT id FROM taxonomy_type WHERE name = 'DataDomain' AND project = (SELECT uid FROM project WHERE nid = 3)));

--###################################################################################################################################
--## TEST HAS TAXONOMY
--###################################################################################################################################
INSERT INTO module_taxonomies(module, taxonomy)
    VALUES ((SELECT uid FROM module WHERE nid = 2002), (SELECT uid FROM taxonomy WHERE name = 'Taxonomy 1'));

--###################################################################################################################################
--## TEST ANNOTATION CATEGORY
--###################################################################################################################################
INSERT INTO annotation_category(name, types, project)
    VALUES ('Annotation Category A', array[]::text[], (SELECT uid FROM project WHERE nid=3));

--###################################################################################################################################
--## TEST ANNOTATION
--###################################################################################################################################
INSERT INTO annotation(uid, name, state, type,
    category,
    module, location, created_by, updated_by, source)
VALUES (gen_random_uuid(), 'Annotation 1', 'IN_ANALYSIS', 'RULE',
    (SELECT id FROM annotation_category WHERE name = 'Annotation Category A'),
    (SELECT uid FROM module WHERE nid = 2002), (100, 4), 'admin', 'admin', (utf8_to_bytes(E'abcd')));

--###################################################################################################################################
--## TEST DATA DICTIONARY ENTRY
--###################################################################################################################################
INSERT INTO data_dictionary(uid, name, 
    description, format, 
    created_by, length, scopes, module, 
    location) 
VALUES (gen_random_uuid(), 'MY-PROGRAM-NAME', 
    'This is an english description of the data element name MY-PROGRAM-NAME', 'PICX', 
    'admin', 15, '{"SQL_DATABASE":{"tables":"insert table names here"},"CICS_UI":{"mapname":"mapnome","mapset":"mapsot"}}',
    (SELECT uid FROM module WHERE path='/src/MOD 3'), (1005, 15));
-- Duplicated this entry, as in the source Orient Test Sql
INSERT INTO data_dictionary(uid, name,
    description, format,
    created_by, length, scopes, module,
    location)
VALUES (gen_random_uuid(), 'MY-PROGRAM-NAME',
    'This is an english description of the data element name MY-PROGRAM-NAME', 'PICX',
    'admin', 15, '{"SQL_DATABASE":{"tables":"insert table names here"},"CICS_UI":{"mapname":"mapnome","mapset":"mapsot"}}',
    (SELECT uid FROM module WHERE path='/src/MOD 3'), (1005, 15));

--#####################################
--## FOR DDE-ANNOTATION LINK
--#####################################
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator, content_hash)
    VALUES (gen_random_uuid(), '5b5pa0JRsEZV2NB6HqiGRL',
        'CPYPST', 'src/cobol/CPYPST.cbl', 'null',
        null,
        (SELECT uid FROM project WHERE nid = 1),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', decode('B20025F804F0839F0D7BF462A75ED1B8','hex'));

INSERT INTO data_dictionary(uid, name, 
    description, format, 
    created_by, length, scopes, module, 
    location) 
VALUES ('a5070e40-42d5-4b42-a482-d016a65cdb5d', 'DDEFORANNO', 
    'This is an english description of the data element name MY-PROGRAM-NAME', 'PICX', 
    'admin', 15, '{"SQL_DATABASE":{"tables":"insert table names here"},"CICS_UI":{"mapname":"mapnome","mapset":"mapsot"}}',
    (SELECT uid FROM module WHERE path='src/cobol/CPYPST.cbl'), (1005, 15));

INSERT INTO annotation(uid, name, state, type,
    category,
    module, location, created_by, updated_by, source)
VALUES ('4d5bdb0c-3667-4a5f-b678-fac5195fd30b', 'ANNOFORDDE', 'IN_ANALYSIS', 'RULE',
    (SELECT id FROM annotation_category WHERE name = 'Annotation Category A'),
    (SELECT uid FROM module WHERE path='src/cobol/CPYPST.cbl'), (100, 4), 'admin', 'admin', (utf8_to_bytes(E'abcd')));