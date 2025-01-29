-- DELETE DATA
DELETE FROM project WHERE nid<>0;

-- RESET SEQUENCES
ALTER SEQUENCE project_nid RESTART;
ALTER SEQUENCE module_nid RESTART WITH 1999;

--TEST Project
INSERT INTO project(uid,client,name) select gen_random_uuid(),uid,'Demo Project A' from client where nid=1;

--TEST MODULES
INSERT INTO module(uid, link_hash,
        name, project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash1',
        'QBGPSLP1MMRS710A.STEP01.MMRS7102', (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'JCL', 'EXEC_PGM', 'FILE_SECTION', 'CUSTOM',
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
    VALUES ('src/cobol/programs/PRGA.cbl', 'PRGA', 'COBOL', 'PROGRAM',
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
        'PRGA', 'src/cobol/programs/PRGA.cbl', 'A test program',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=1),
	    utf8_to_bytes(E'       PROCEDURE DIVISION.\n           DISPLAY ''1''\n           COPY CC1\n           DISPLAY ''2''\n           COPY CC2\n           DISPLAY ''3''\n           .\n')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('src/cobol/programs/PRGB.cbl', 'PRGB', 'COBOL', 'PROGRAM',
                1, 1, '1', (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash3',
        'PRGB', 'src/cobol/programs/PRGB.cbl', 'A test program',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=1),
	    utf8_to_bytes(E'           DISPLAY ''CC1''\n           COPY CC2\n')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('src/cobol/programs/CC1.cpy', 'CC1', 'COBOL', 'PROGRAM',
                1, 1, '1', (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash4',
        'CC1', 'src/cobol/programs/CC1.cpy', 'A test copy',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=1),
	    utf8_to_bytes(E'           DISPLAY ''CC2''\n')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('src/cobol/programs/CC2.cpy', 'CC2', 'COBOL', 'PROGRAM',
                1, 1, '1', (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash5',
        'CC2', 'src/cobol/programs/CC2.cpy', 'A test copy',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=1),
	    utf8_to_bytes(E'       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  MY-PROGRAM-NAME PIC X(10) VALUE ''MMRS7101:''.\n       PROCEDURE DIVISION.\n           PERFORM MY-FLOW-01 THRU MY-FLOW-02.\n       MY-FLOW-01.\n           DISPLAY MY-PROGRAM-NAME ''my-flow-01   Label 01    ''.\n       MY-FLOW-02.\n           DISPLAY MY-PROGRAM-NAME ''my-flow-02   Label 02    ''.\n')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('src/cobol/programs/PRGC.cbl', 'PRGC', 'COBOL', 'PROGRAM',
                1, 1, '1', (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash6',
        'PRGC', 'src/cobol/programs/PRGC.cbl', 'A test program',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

-- Module with source
WITH src AS (
	INSERT INTO source(id, project, content)
	VALUES(gen_random_uuid(),
	    (SELECT uid FROM project WHERE nid=1),
	    utf8_to_bytes(E'       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  MY-HEX-SHOW-CHARS PIC X(16) VALUE ''0123456789ABCDEF''.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR PIC X(1) OCCURS 16.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR2 PIC X(1) OCCURS 16.\n       PROCEDURE DIVISION\n       .\n')
    ) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type,
	    meta_data_revision, content_revision, content_hash, uid)
    VALUES ('src/cobol/programs/PRGD.cbl', 'PRGD', 'COBOL', 'PROGRAM',
                1, 1, '1', (SELECT id FROM src))
    RETURNING uid
)
INSERT INTO module(uid, link_hash,
        name, path, description,
        source,
        project,
        technology, type, storage, origin,
        identified, creator)
    VALUES (gen_random_uuid(), 'DummyHash7',
        'PRGD', 'src/cobol/programs/PRGD.cbl', 'A test program',
        (SELECT uid FROM src_info),
        (SELECT uid FROM project WHERE nid = 1 LIMIT 1),
        'COBOL', 'PROGRAM', 'FILE', 'CUSTOM',
        true, 'DISCOVERY');

-- TEST INCLUDES
INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE path='src/cobol/programs/PRGB.cbl'
		AND project = (SELECT uid FROM project WHERE nid = 1)),
    (SELECT uid FROM module WHERE path='src/cobol/programs/CC1.cpy'
		AND project = (SELECT uid FROM project WHERE nid = 1)),
    'INCLUDES');

INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE path='src/cobol/programs/PRGB.cbl'
		AND project = (SELECT uid FROM project WHERE nid = 1)),
    (SELECT uid FROM module WHERE path='src/cobol/programs/CC2.cpy'
		AND project = (SELECT uid FROM project WHERE nid = 1)),
    'INCLUDES');

INSERT INTO module_relationship (id, src, dst, type)
VALUES (gen_random_uuid(),
    (SELECT uid FROM module WHERE path='src/cobol/programs/CC1.cpy'
	 	AND project = (SELECT uid FROM project WHERE nid = 1)),
    (SELECT uid FROM module WHERE path='src/cobol/programs/CC2.cpy'
	 	AND project = (SELECT uid FROM project WHERE nid = 1)),
    'INCLUDES');
