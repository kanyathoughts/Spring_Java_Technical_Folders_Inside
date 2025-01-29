--###################################################################################################################################
--## TEST MODULES
--###################################################################################################################################
-- Module with source
ALTER SEQUENCE module_nid RESTART WITH 2000;
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=2),
	    utf8_to_bytes(E'       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  MY-HEX-SHOW-CHARS PIC X(16) VALUE ''0123456789ABCDEF''.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR PIC X(1) OCCURS 16.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR2 PIC X(1) OCCURS 16.\n       PROCEDURE DIVISION\n       .\n')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('src/cobol/programs/PRGE.cbl', 'PRGE', 'COBOL', 'PROGRAM',
                1, 1, '1', (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash1',
        'PRGE', 'src/cobol/programs/PRGE.cbl', 'A test program',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 2),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=1),
	    utf8_to_bytes(E'       PROCEDURE DIVISION.\n           DISPLAY ''1''\n           .\n')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('src/cobol/programs/MMRS7102.cbl', 'MMRS7102', 'COBOL', 'PROGRAM',
                1, 1, '1', (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash2',
        'MMRS7102', 'src/cobol/programs/MMRS7102.cbl', 'A test description for MMRS7102',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 2),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash3',
        'MMRS7103', 'src/cobol/programs/MMRS7103.cbl', (SELECT uid FROM project WHERE nid = 2),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', 'A test description for MMRS7103');

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash4',
        'DUPLICATE', 'src/cobol/programs/DUPLICATE.cbl', (SELECT uid FROM project WHERE nid = 2),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', 'A spring data import taxonomy test program');

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash5',
        'DUPLICATE', 'src/natural/programs/DUPLICATE.nsp', (SELECT uid FROM project WHERE nid = 2),
        'NATURAL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', 'A spring data import taxonomy test program');

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash6',
        'VirtualModule', '', (SELECT uid FROM project WHERE nid = 2),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY', 'A spring data import taxonomy test program');

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash7',
        'PSB4030A', 'src/ims/A/psb/psb/PSBA.psb', (SELECT uid FROM project WHERE nid = 2),
        'IMS', 'PSB', 'FILE_SECTION', 'CUSTOM',
        true, 'DISCOVERY', '');

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash8',
        'DBD4030A', 'src/ims/B/dbd/dbd/DBDA.dbd', (SELECT uid FROM project WHERE nid = 2),
        'IMS', 'DBD', 'FILE_SECTION', 'CUSTOM',
        true, 'DISCOVERY', '');

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash9',
        'DBD4018A', 'src/ims/C/dbd/dbd/DBDB.dbd', (SELECT uid FROM project WHERE nid = 2),
        'IMS', 'PSB', 'FILE_SECTION', 'CUSTOM',
        true, 'DISCOVERY', '');

INSERT INTO module(uid, link_hash,
        name, path, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash10',
        'DBD4019A', 'src/ims/dbd/DBDA.dbd', (SELECT uid FROM project WHERE nid = 2),
        'IMS', 'PSB', 'FILE_SECTION', 'CUSTOM',
        true, 'DISCOVERY', '');

INSERT INTO module(uid, link_hash,
        name, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash11',
        'PSB4030D', (SELECT uid FROM project WHERE nid = 2),
        'IMS', 'PSB', 'FILE_SECTION', 'CUSTOM',
        true, 'DISCOVERY', '');

INSERT INTO module(uid, link_hash,
        name, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (gen_random_uuid(), 'DummyHash12',
        'QRSTCASE', (SELECT uid FROM project WHERE nid = 2),
        'IMS', 'DBD_SEGMENT', 'FILE_SECTION', 'CUSTOM',
        true, 'DISCOVERY', '');

INSERT INTO module(nid, uid, link_hash,
        name, project,
        technology, type, storage, origin,
        identified, creator, description)
    VALUES (-1, gen_random_uuid(), 'DummyHash13',
        'QRSTCASE', (SELECT uid FROM project WHERE nid = 2),
        'IMS', 'DBD_SEGMENT', 'FILE_SECTION', 'CUSTOM',
        false, 'DISCOVERY', '');

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=1),
	    utf8_to_bytes(E'       PROCEDURE DIVISION.\n           DISPLAY ''1''\n           .\n')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('src/cobol/programs/A/mmrs7102.cbl', 'mmrs7102', 'COBOL', 'PROGRAM',
                1, 1, '1', (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash14',
        'mmrs7102', 'src/cobol/programs/A/mmrs7102.cbl', 'A test description for mmrs7102',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 2),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

--###################################################################################################################################
--## TEST TAXONOMY FOR MULTIPLE TAXONOMY ASSIGNMENTS FOR TYPES
--###################################################################################################################################
INSERT INTO taxonomy_type(id, name, project, category)
    VALUES (gen_random_uuid(), 'Program Type', (SELECT uid FROM project WHERE nid=2),
    (SELECT id FROM taxonomy_category WHERE name='Technical Taxonomies' AND project = (SELECT uid FROM project WHERE nid=2)));
INSERT INTO taxonomy_type(id, name, project, category)
    VALUES (gen_random_uuid(), 'File Access', (SELECT uid FROM project WHERE nid=2),
    (SELECT id FROM taxonomy_category WHERE name='Technical Taxonomies' AND project = (SELECT uid FROM project WHERE nid=2)));

INSERT INTO taxonomy(uid, name, project, type)
    VALUES (gen_random_uuid(), 'BATCH', (SELECT uid FROM project WHERE nid=2),
    (SELECT id FROM taxonomy_type WHERE name = 'Program Type' AND project = (SELECT uid FROM project WHERE nid=2)));
INSERT INTO taxonomy(uid, name, project, type)
    VALUES (gen_random_uuid(), 'UI', (SELECT uid FROM project WHERE nid=2),
    (SELECT id FROM taxonomy_type WHERE name = 'Program Type' AND project = (SELECT uid FROM project WHERE nid=2)));
INSERT INTO taxonomy(uid, name, project, type)
    VALUES (gen_random_uuid(), 'READ', (SELECT uid FROM project WHERE nid=2),
    (SELECT id FROM taxonomy_type WHERE name = 'File Access' AND project = (SELECT uid FROM project WHERE nid=2)));
INSERT INTO taxonomy(uid, name, project, type)
    VALUES (gen_random_uuid(), 'WRITE', (SELECT uid FROM project WHERE nid=2),
    (SELECT id FROM taxonomy_type WHERE name = 'File Access' AND project = (SELECT uid FROM project WHERE nid=2)));
INSERT INTO taxonomy(uid, name, project, type)
    VALUES (gen_random_uuid(), 'DATABASE', (SELECT uid FROM project WHERE nid=2),
    (SELECT id FROM taxonomy_type WHERE name = 'Program Type' AND project = (SELECT uid FROM project WHERE nid=2)));

--###################################################################################################################################
--## TEST HAS TAXONOMY
--###################################################################################################################################
INSERT INTO module_taxonomies(module, taxonomy)
    VALUES ((SELECT uid FROM module WHERE nid = 2001), (SELECT uid FROM taxonomy WHERE name = 'BATCH'));
INSERT INTO module_taxonomies(module, taxonomy)
    VALUES ((SELECT uid FROM module WHERE nid = 2001), (SELECT uid FROM taxonomy WHERE name = 'UI'));
INSERT INTO module_taxonomies(module, taxonomy)
    VALUES ((SELECT uid FROM module WHERE nid = 2001), (SELECT uid FROM taxonomy WHERE name = 'READ'));
INSERT INTO module_taxonomies(module, taxonomy)
    VALUES ((SELECT uid FROM module WHERE nid = 2001), (SELECT uid FROM taxonomy WHERE name = 'WRITE'));
INSERT INTO module_taxonomies(module, taxonomy)
    VALUES ((SELECT uid FROM module WHERE nid = 2002), (SELECT uid FROM taxonomy WHERE name = 'BATCH'));
INSERT INTO module_taxonomies(module, taxonomy)
    VALUES ((SELECT uid FROM module WHERE nid = 2002), (SELECT uid FROM taxonomy WHERE name = 'UI'));

--###################################################################################################################################
--# TEST REFERENCES
--###################################################################################################################################
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE name='PSB4030A'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)),
    (SELECT uid FROM module WHERE name='QRSTCASE'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)
	 	AND nid > 0),
    'REFERENCES');
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE name='PSB4030D'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)),
    (SELECT uid FROM module WHERE name='QRSTCASE'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)
	 	AND nid > 0),
    'REFERENCES');
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE name='DBD4030A'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)),
    (SELECT uid FROM module WHERE name='QRSTCASE'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)
	 	AND nid > 0),
    'REFERENCES');
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE name='DBD4018A'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)),
    (SELECT uid FROM module WHERE name='QRSTCASE'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)
	 	AND nid > 0),
    'REFERENCES');
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE name='DBD4019A'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)),
    (SELECT uid FROM module WHERE name='QRSTCASE'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)
	 	AND nid < 0), --special case, with LT 0, instead of GT 0
    'REFERENCES');

--###################################################################################################################################
--## TEST CONTAINS MODULE
--###################################################################################################################################
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE name='DBD4030A'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)),
    (SELECT uid FROM module WHERE name='QRSTCASE'
	 	AND project = (SELECT uid FROM project WHERE nid = 2)
	 	AND nid > 0),
    'CONTAINS');
