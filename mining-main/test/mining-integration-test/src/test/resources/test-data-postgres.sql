-- DELETE DATA
DELETE FROM taxonomy WHERE project NOT IN (SELECT uid FROM project WHERE nid = 0);
DELETE FROM Project WHERE nid <> 0;
DELETE FROM client WHERE nid <> 0;
DELETE FROM module WHERE project NOT IN (SELECT uid FROM project WHERE nid = 0);
DELETE FROM taxonomy WHERE project NOT IN (SELECT uid FROM project WHERE nid = 0);
DELETE FROM taxonomy_category WHERE project NOT IN (SELECT uid FROM project WHERE nid = 0);
DELETE FROM statement WHERE uid NOT IN (SELECT s.uid FROM statement s JOIN module m ON s.module = m.uid JOIN project p ON m.project = p.uid WHERE p.nid = 0);
DELETE FROM custom_enum;
DELETE FROM custom_property;
DELETE FROM custom_property_entities;

-- CUSTOM PROPERTIES PROJECT
INSERT INTO custom_property (project, name) SELECT uid, 'ProjectCustomProperties' FROM project WHERE nid = 0;
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'customProjectProperty', '{"dataType":"STRING","label":"Custom Project Property","pluginVisible":true,"description":"A custom property for the Project class"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'ProjectCustomProperties';

-- CUSTOM PROPERTIES DataDictionaryEntry
INSERT INTO custom_property (project, name) SELECT uid, 'DataDictionaryEntryCustomProperties' FROM project WHERE nid = 0;
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'customDataDictionaryEntryProperty', '{"dataType":"STRING","label":"Custom DataDictionaryEntry Property","pluginVisible":true,"description":"A custom property for the DataDictionaryEntry class"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'DataDictionaryEntryCustomProperties';

-- CUSTOM PROPERTIES Taxonomy
INSERT INTO custom_property (project, name) SELECT uid, 'TaxonomyCustomProperties' FROM project WHERE nid = 0;
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'customTaxonomyProperty', '{"dataType":"STRING","label":"Custom Taxonomy Property","pluginVisible":true,"description":"A custom property for the Taxonomy class"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'TaxonomyCustomProperties';

-- CUSTOM PROPERTIES ANNOTATION
INSERT INTO custom_property (project, name) SELECT uid, 'AnnotationCustomProperties' FROM project WHERE nid = 0;
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'annotationTags', '{"dataType":"STRING","collectionType":"EMBEDDEDLIST","label":"Annotation Tags","fieldType":"TAG","autoCompletionKey":"annotationTags"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties';
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'colorTags', '{"dataType":"STRING","collectionType":"EMBEDDEDLIST","label":"Annotation Colors","fieldType":"TAG","autoCompletionKey":"colorTags"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties';
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'ruleTags', '{"dataType":"STRING","collectionType":"EMBEDDEDLIST","label":"Annotation Rules","fieldType":"TAG","autoCompletionKey":"ruleTags"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties';
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'customMetaInfo', ('{"dataType":"STRING","mandatory":false,"min":5,"max":30,"readOnly":true,"label":"Some custom meta information","pluginVisible":false'
	|| ',"description":"This is some more custom meta information","dataSource":"Custom datasource URL","showWhen":{"annotationCategoryId":42}'
	|| ',"customViewNames":["First Custom View","Second Custom View"],"customViewIndex":21,"validationRegex":"[a-z]*","validationErrorMessage":"Incorrect Format"}')::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties';
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'customAnnotationProperty', '{"dataType":"STRING","label":"Custom Annotation Property","pluginVisible":true,"description":"A custom property for the Annotation class"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties';

-- CUSTOM PROPERTIES MODULE
INSERT INTO custom_property (project, name) SELECT uid, 'ModuleCustomProperties' FROM project WHERE nid = 0;
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'customMetaInfo1', '{"dataType":"STRING","mandatory":false,"min":5,"max":26,"readOnly":true,"label":"Some custom meta information 1","pluginVisible":false,"description":"This is some more custom meta information 1","dataSource":"Custom datasource URL 1"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'ModuleCustomProperties';
INSERT INTO custom_property (project, parent, name, properties) SELECT project, id,
	'customMetaInfo2', '{"dataType":"STRING","label":"Some custom meta information 2","pluginVisible":false,"description":"This is some more custom meta information 2","dataSource":"Custom datasource URL 2"}'::jsonb
	FROM custom_property WHERE parent IS NULL AND name = 'ModuleCustomProperties';

-- RESET SEQUENCES
ALTER SEQUENCE client_nid RESTART;
ALTER SEQUENCE project_nid RESTART;
ALTER SEQUENCE source_nid RESTART;
ALTER SEQUENCE module_nid RESTART WITH 2000;
ALTER SEQUENCE statement_nid RESTART;
ALTER SEQUENCE annotation_nid RESTART;
ALTER SEQUENCE annotation_category_id RESTART WITH 1001;
ALTER SEQUENCE data_dictionary_nid RESTART;
ALTER SEQUENCE taxonomy_nid RESTART WITH 14;
ALTER SEQUENCE taxonomy_category_id RESTART WITH 3;

--TEST CLIENTS
INSERT INTO client (uid, name) values (gen_random_uuid(), 'Demo Client 1');
INSERT INTO client (uid, name) values (gen_random_uuid(), 'Demo Client 2');

-- TEST PROJECTS
DELETE FROM custom_property_entities;
INSERT INTO project (uid,client,name,metrics_date,custom_properties) select gen_random_uuid(),uid, 'Demo Project A',TO_TIMESTAMP('2020-07-05 15:18:27', 'YYYY-MM-DD HH24:MI:SS'),'{"ProjectCustomProperties":{"customProjectProperty":"A value for the custom project property"}}' from client where nid=1;
INSERT INTO project (uid,client,name) select gen_random_uuid(),uid,'Demo Project B' from client where nid=1;
INSERT INTO project (uid,client,name) select gen_random_uuid(),uid,'Demo Project C' from client where nid=2;
INSERT INTO project (uid,client,name) select gen_random_uuid(),uid,'Demo Project D' from client where nid=2;
UPDATE project SET search_orders = (array['{"source":{"name":null,"type":null,"path":null,"pathPattern":"application/programs/A/*.cbl","containedIn":null},"targets":[{"name":null,"type":null,"path":null,"pathPattern":"application/copies/A/*.cpy","containedIn":null},{"name":null,"type":null,"path":null,"pathPattern":"application/copies/B/*.cpy","containedIn":null}]}'::jsonb,'{"source":{"name":null,"type":null,"path":null,"pathPattern":"application/programs/D/*.cbl","containedIn":null},"targets":[{"name":null,"type":null,"path":null,"pathPattern":"application/copies/E/*.cpy","containedIn":null},{"name":null,"type":null,"path":null,"pathPattern":"application/copies/F/*.cpy","containedIn":null}]}'::jsonb]::jsonb[]) WHERE nid=1;

INSERT INTO custom_property_entities (project, entity, property) VALUES ((SELECT uid FROM project WHERE nid = 1), 'Module', (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'ModuleCustomProperties'));
UPDATE project SET technical_taxonomy_category=1,default_taxonomy_category=2  where nid=0;
UPDATE project SET technical_taxonomy_category=3,default_taxonomy_category=4  where nid=1;
UPDATE project SET technical_taxonomy_category=5,default_taxonomy_category=6   where nid=2;
UPDATE project SET technical_taxonomy_category=7,default_taxonomy_category=8   where nid=3;
UPDATE project SET technical_taxonomy_category=9,default_taxonomy_category=10  where nid=4;

-- TEST MODULES
INSERT INTO module (uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'PRG1', 'src-natural/LibA/PRG1.nsp', 'NATURAL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash1');
INSERT INTO module (uid, project, name, technology, type, storage, origin, identified, creator, link_hash) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'QBGPSLP1MMRS710A.STEP01.MMRS7102', 'JCL', 'EXEC_PGM', 'FILE_SECTION', 'CUSTOM', true, 'DISCOVERY', 'DummyHash2');

-- TEST MODULES for DNA
--the following modules were created without name or path previously
INSERT INTO module (uid, nid, project, name, path, technology, type, storage, origin, identified, creator, link_hash)
	VALUES (gen_random_uuid(), 2, (SELECT uid FROM project WHERE nid=1), 'PRGx', 'src-natural/LibA/PRGx.nsp',
	'NATURAL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHashTestDna2');
INSERT INTO module (uid, nid, project, name, path, technology, type, storage, origin, identified, creator, link_hash) 
	VALUES (gen_random_uuid(), 3, (SELECT uid FROM project WHERE nid=1), 'PRG2', 'src-natural/LibA/PRG2.nsp',
	'NATURAL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHashTestDna2');
INSERT INTO module (uid, nid, project, name, path, technology, type, storage, origin, identified, creator, link_hash) 
	VALUES (gen_random_uuid(), 4, (SELECT uid FROM project WHERE nid=1), 'PRG3', 'src-natural/LibA/PRG3.nsp',
	'NATURAL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHashTestDna2');
INSERT INTO module (uid, nid, project, name, path, technology, type, storage, origin, identified, creator, link_hash) 
	VALUES (gen_random_uuid(), 5, (SELECT uid FROM project WHERE nid=1), 'PRG4', 'src-natural/LibA/PRG4.nsp',
	'NATURAL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHashTestDna2');

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       PROCEDURE DIVISION.\n           DISPLAY ''1''\n           .\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/MMRS71011.cbl', 'MMRS71011', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'MMRS7101', 'src/cobol/programs/MMRS7101.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash3', 'A test description for MMRS7101', uid FROM src_info;
INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/MMRS7101.cbl'), 107, 46, 3);


--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       PROCEDURE DIVISION.\n           DISPLAY ''1''\n           .\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/MMRS7102.cbl', 'MMRS7102', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=2), 'MMRS7102', 'src/cobol/programs/MMRS7102.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash4', 'A test description for MMRS7102', uid FROM src_info;


--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=2), (utf8_to_bytes(E'       PROCEDURE DIVISION.\n           DISPLAY ''1''\n           .\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/MMRS7103.cbl', 'MMRS7103', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, requires_review, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=2), 'MMRS7103', 'src/cobol/programs/MMRS7103.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash5', 'A test description for MMRS7102', true, uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       PROCEDURE DIVISION.\n           DISPLAY ''1''\n           .\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/PRGA.cbl', 'PRGA', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, requires_review, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'PRGA', 'src/cobol/programs/PRGA.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash6', 'A test program', true, uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       PROCEDURE DIVISION.\n           DISPLAY ''1''\n           COPY CC1\n           DISPLAY ''2''\n           COPY CC2\n           DISPLAY ''3''\n           .\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/PRGB.cbl', 'PRGB', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, requires_review, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'PRGB', 'src/cobol/programs/PRGB.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash7', 'A test program', true, uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'           DISPLAY ''CC1''\n           COPY CC2\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/CC1.cpy', 'CC1', 'COBOL', 'COPYBOOK', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'CC1', 'src/cobol/programs/CC1.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash8', 'A test copy', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'           DISPLAY ''CC2''\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/CC2.cpy', 'CC2', 'COBOL', 'COPYBOOK', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'CC2', 'src/cobol/programs/CC2.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash9', 'A test copy', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  MY-PROGRAM-NAME PIC X(10) VALUE ''MMRS7101:''.\n       PROCEDURE DIVISION.\n           PERFORM MY-FLOW-01 THRU MY-FLOW-02.\n       MY-FLOW-01.\n           DISPLAY MY-PROGRAM-NAME ''my-flow-01   Label 01    ''.\n       MY-FLOW-02.\n           DISPLAY MY-PROGRAM-NAME ''my-flow-02   Label 02    ''.\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/PRGC.cbl', 'PRGC', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'PRGC', 'src/cobol/programs/PRGC.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash10', 'A test program', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  MY-HEX-SHOW-CHARS PIC X(16) VALUE ''0123456789ABCDEF''.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR PIC X(1) OCCURS 16.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR2 PIC X(1) OCCURS 16.\n       PROCEDURE DIVISION\n       .\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/PRGD.cbl', 'PRGD', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'PRGD', 'src/cobol/programs/PRGD.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash11', 'A test program', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'******** A                                       \n        IDENTIFICATION DIVISION.                  \n        PROGRAM-ID. A.                            \n        ENVIRONMENT DIVISION.                     \n        CONFIGURATION SECTION.                    \n        INPUT-OUTPUT SECTION.                     \n        FILE-CONTROL.                             \n            SELECT KSDSFILE ASSIGN TO  \"KSD\"    \n                ORGANIZATION  IS  INDEXED         \n                ACCESS  MODE  IS  DYNAMIC         \n                RECORD  KEY   IS  PKEY            \n                FILE STATUS   IS  WS-FS.          \n        DATA DIVISION.                            \n        FILE SECTION.                             \n        FD KSDSFILE RECORD CONTAINS 80 CHARACTERS.\n        01  KSDSFILE-REC.                         \n            05  PKEY                PIC X(6).     \n            05  INFO                PIC X(74).    \n        WORKING-STORAGE SECTION.                  \n        01  MY-PROGRAM-NAME PIC X(10) VALUE ''A''.  \n        LINKAGE SECTION.                          \n        PROCEDURE DIVISION.                       \n                                                  \n                                                  \n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/IOSCOPE.cbl', 'IOSCOPE', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'IOSCOPE', 'src/cobol/programs/IOSCOPE.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash12', 'A test program', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'AI0103M DFHMSD TYPE=&SYSPARM,LANG=COBOL,TIOAPFX=YES,MODE=INOUT,        \n               STORAGE=AUTO,CTRL=FREEKB                                \nAI01003 DFHMDI SIZE=(24,80),LINE=1,COLUMN=1,JUSTIFY=LEFT,              \n               MAPATTS=(COLOR,HILIGHT)                                 \n        DFHMDF POS=(2,3),LENGTH=7,INITIAL=''AI01003'',ATTRB=ASKIP        \n        DFHMDF POS=(2,65),LENGTH=7,INITIAL=''AI01-03'',                  \n               ATTRB=ASKIP                                             \nPRDDS   DFHMDF POS=(4,5),LENGTH=50,ATTRB=PROT                          \n        DFHMDF POS=(4,57),LENGTH=1,ATTRB=ASKIP                         \nISSCO2  DFHMDF POS=(5,18),LENGTH=30,ATTRB=PROT                         \n        DFHMDF POS=(5,51),LENGTH=1,ATTRB=ASKIP                         \nAPPTST2 DFHMDF POS=(6,10),LENGTH=20,ATTRB=PROT                         \n        DFHMDF POS=(6,32),LENGTH=1,ATTRB=ASKIP                         \n        DFHMDF POS=(9,5),LENGTH=15,ATTRB=ASKIP,                        \n               INITIAL=''---------------''                               \n        DFHMDF POS=(9,35),LENGTH=12,ATTRB=ASKIP,                       \n               INITIAL=''------------''                                  \n\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/maps/UISCOPE.map', 'UISCOPE', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'UISCOPE', 'src/cobol/maps/UISCOPE.map', 'CICS', 'BMS_MAPSET', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash13', 'A test map', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'******** A                                                \n        IDENTIFICATION DIVISION.                           \n        PROGRAM-ID. A.                                     \n        ENVIRONMENT DIVISION.                              \n        CONFIGURATION SECTION.                             \n        DATA DIVISION.                                     \n        WORKING-STORAGE SECTION.                           \n        01  DCL.                                           \n            10 COORDINATOR-TYPE     PIC X(1).              \n            10 ANNUAL-DELIV-FEE     PIC S9(4) USAGE COMP.  \n            10 FREE-DELIVERY-IND    PIC X(1).              \n         01  W-SUBJECT-ID           PIC S9(9) COMP.        \n        LINKAGE SECTION.                                   \n        PROCEDURE DIVISION.                                \n        0000-INITIALIZE.                                   \n            EXEC SQL                                       \n                SELECT TABLE1.COORDINATOR_TYPE,            \n                   TABLE1.ANNUAL_DELIV_FEE,                \n                   TABLE1.FREE_DELIVERY_IND                \n                   INTO :DCL.COORDINATOR-TYPE,             \n                        :DCL.ANNUAL-DELIV-FEE,             \n                        :DCL.FREE-DELIVERY-IND             \n                   FROM TABLE1                             \n                   WHERE TABLE1.SUBJECT_ID = :W-SUBJECT-ID \n            END-EXEC.                                      \n                                                           \n ') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DBSCOPE.cbl', 'DBSCOPE', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DBSCOPE', 'src/cobol/programs/DBSCOPE.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash14', 'A test program', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'******** A                                                       \n        IDENTIFICATION DIVISION.                                  \n        PROGRAM-ID. A.                                            \n        ENVIRONMENT DIVISION.                                     \n        CONFIGURATION SECTION.                                    \n        DATA DIVISION.                                            \n        WORKING-STORAGE SECTION.                                  \n         01  W-SUBJECT-ID           PIC S9(9) COMP.               \n        LINKAGE SECTION.                                          \n        01  DFHCOMMAREA.                                          \n            02  LK-FILLER.                                        \n                03  LK-FILLER03                     PIC X(1)      \n                   OCCURS 1 TO 25000 TIMES DEPENDING ON EIBCALEN. \n        PROCEDURE DIVISION.                                       \n        0000-INITIALIZE.                                          \n                                                                  \n ') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/PARAM.cbl', 'PARAM', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'PARAM', 'src/cobol/programs/PARAM.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash15', 'A test program', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       IDENTIFICATION DIVISION.\n       PROGRAM-ID.    EXECSQL.\n       ENVIRONMENT DIVISION.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n           EXEC SQL DECLARE MMRS00C_AWA_VSAMK TABLE\n             (\n               KSDS_PRIMARY_INDEX   VARCHAR(10)\n             )\n           END-EXEC\n      *    ************************************************************\n           EXEC SQL\n             DECLARE   C-VSAMK    CURSOR FOR\n               SELECT    KSDS_PRIMARY_INDEX\n               FROM      MMRS00C_AWA_VSAMK AS VSAMK\n               ORDER BY  VSAMK.KSDS_PRIMARY_INDEX ASC\n               FOR       FETCH ONLY\n           END-EXEC\n       PROCEDURE DIVISION.\n           GOBACK.\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/EXECSQL.cbl', 'EXECSQL', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'EXECSQL', 'src/cobol/programs/EXECSQL.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash16', 'A Cobol program with some EXEC SQL statements.', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       IDENTIFICATION DIVISION.\n       PROGRAM-ID.    DISPLAYPGM.\n        DATA DIVISION.\n        WORKING-STORAGE SECTION.\n        PROCEDURE DIVISION.\n        PARAG-PARA.\n              DISPLAY :''HELLO WORLD''\n              DISPLAY:''MORE TEXT TO CHECK ELLIPSIS WORKING AS EXPECTED''\n              STOP RUN.\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DISPLAYPGM.cbl', 'DISPLAYPGM', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DISPLAYPGM', 'src/cobol/programs/DISPLAYPGM.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash17', 'A Cobol program with some label and diplay stmts', uid FROM src_info;

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM1.cpy', 'DPGM1', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, representation, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM1', 'PHYSICAL', 'src/cobol/programs/DPGM1.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash18', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM1.cpy'), 107, 46, 3);


--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM2.cpy', 'DPGM2', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, representation, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM2', 'PHYSICAL', 'src/cobol/programs/DPGM2.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash19', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM2.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM3.cpy', 'DPGM3', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, representation, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM3', 'PHYSICAL', 'src/cobol/programs/DPGM3.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash20', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM3.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM4.cpy', 'DPGM4', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, representation, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM4', 'PHYSICAL', 'src/cobol/programs/DPGM4.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash21', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM4.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM5.cpy', 'DPGM5', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, representation, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM5', 'PHYSICAL', 'src/cobol/programs/DPGM5.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash22', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM5.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM6.cpy', 'DPGM6', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, representation, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM6', 'VIRTUAL', 'src/cobol/programs/DPGM6.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash23', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM6.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM7.cpy', 'DPGM7', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM7', 'src/cobol/programs/DPGM7.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash24', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM7.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM8.cpy', 'DPGM8', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM8', 'src/cobol/programs/DPGM8.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash25', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM8.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM9.cpy', 'DPGM9', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM9', 'src/cobol/programs/DPGM9.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash26', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM9.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM10.cpy', 'DPGM10', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM10', 'src/cobol/programs/DPGM10.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash27', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM10.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM11.cpy', 'DPGM11', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM11', 'src/cobol/programs/DPGM11.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash28', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM11.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM12.cpy', 'DPGM12', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM12', 'src/cobol/programs/DPGM12.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash29', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM12.cpy'), 107, 46, 3);

--Module with source and source metrics
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/DPGM13.cpy', 'DPGM13', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'DPGM13', 'src/cobol/programs/DPGM13.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash30', 'Same description for test', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/DPGM13.cpy'), 107, 46, 3);

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=2), (utf8_to_bytes(E'       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  MY-HEX-SHOW-CHARS PIC X(16) VALUE ''0123456789ABCDEF''.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR PIC X(1) OCCURS 16.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR2 PIC X(1) OCCURS 16.\n       PROCEDURE DIVISION\n       .\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/PRGE.cbl', 'PRGE', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=2), 'PRGE', 'src/cobol/programs/PRGE.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash31', 'A test program', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'       IDENTIFICATION DIVISION.\n        PROGRAM-ID. IO001A.\n        ENVIRONMENT DIVISION.\n        INPUT-OUTPUT SECTION.\n        FILE-CONTROL.\n            SELECT MYFILE ASSIGN TO IN1\n           ORGANIZATION IS SEQUENTIAL\n           ACCESS MODE IS SEQUENTIAL\n            FILE STATUS IS WS-FS.\n        DATA DIVISION.\n        FILE SECTION.\n        FD MYFILE.\n        01 FS-CONTENT PIC X(80).\n        WORKING-STORAGE SECTION.\n        77 WS-FS  PIC 9(02).\n        01 WS-EOF PIC X(01).\n        PROCEDURE DIVISION.\n            OPEN INPUT MYFILE.\n            PERFORM UNTIL WS-EOF = ''Y''\n                READ MYFILE\n                AT END MOVE ''Y'' TO WS-EOF\n                DISPLAY ''READ : '' FS-CONTENT\n                END-READ\n            END-PERFORM.\n            CLOSE MYFILE.\n            STOP RUN.\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/IO001A.cbl', 'IO001A', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'IO001A', 'src/cobol/programs/IO001A.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash32', 'A Cobol program with some label and diplay stmts', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'CALL ICSCRGET\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/BASICO.bas', 'BASICO', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'BASICO', 'src/cobol/programs/BASICO.bas', 'BASIC', 'OBJECT', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash33', 'A test description for BASICO', uid FROM src_info;
	

--Module
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash, description) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'BASICOFUNC', 'BASIC', 'FUNCTION', 'FILE_SECTION', 'CUSTOM', true, 'DISCOVERY', 'DummyHash34', 'A test description for BASICOFUNC');

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'CALL ICSCRGET\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/BASICOPROG.bas', 'BASICOPROG', 'BASIC', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'BASICOPROG', 'BASIC', 'PROGRAM', 'FILE_SECTION', 'CUSTOM', true, 'DISCOVERY', 'DummyHash35', 'A test description for BASICOPROG', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/V2_API_TEST.cpy', 'V2_API_TEST', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'V2_API_TEST', 'src/cobol/programs/V2_API_TEST.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash36', 'A test description for V2APIDelete', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/V2_API_TEST.cpy'), 107, 46, 3);

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'dummy source content') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/cobol/programs/V2_API_UPDATE_TEST.cpy', 'V2_API_UPDATE_TEST', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'V2_API_UPDATE_TEST', 'src/cobol/programs/V2_API_UPDATE_TEST.cpy', 'COBOL', 'COPYBOOK', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash37', 'A test description for V2APIUpdate', uid FROM src_info;

INSERT INTO source_metrics(module, code_lines, comment_lines, complexity_mc_cabe) VALUES ((SELECT uid FROM module WHERE path='src/cobol/programs/V2_API_UPDATE_TEST.cpy'), 107, 46, 3);

--Modules with no sources
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'PRGTEST', 'src/cobol/programs/PRGTEST.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash38', 'A test program');
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=3), 'DeleteTestSD1', 'src/cobol/programs/DTSD1.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash39', 'A spring data delete test program');
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=3), 'DeleteTestSD2', 'src/cobol/programs/DTSD2.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash40', 'A spring data delete test program');
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=3), 'DeleteTestSD3', 'src/cobol/programs/DTSD3.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash41', 'A spring data delete test program');
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=3), 'DeleteTestSD4', 'src/cobol/programs/DTSD4.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash42', 'A spring data delete test program');

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'DEFINE DATA\nLOCAL\n1 #A (A20)\nEND-DEFINE\nINPUT #A\nWRITE (1) NOTITLE NOHDR ''='' #A\nEND\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/natural/programs/NATPRGA.nsp', 'NATPGRA', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'NATPGRA', 'src/natural/programs/NATPRGA.nsp', 'NATURAL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash43', 'A test natural program', uid FROM src_info;

--Module with source
WITH src AS (
	INSERT INTO source(id, project, content) VALUES(gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (utf8_to_bytes(E'WRITE (1) NOTITLE NOHDR &1& &2& &3&\n') )) RETURNING id
), src_info AS (
	INSERT INTO source_info(path, name, technology, type, meta_data_revision, content_revision, content_hash, uid)
		SELECT 'src/natural/programs/NATCCA.nsc', 'NATCCA', 'COBOL', 'PROGRAM', 1, 1, '1', id FROM src RETURNING uid
)
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description, source) 
	SELECT gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'NATCCA', 'src/natural/programs/NATCCA.nsc', 'NATURAL', 'COPYCODE', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHash44', 'A test natural copycode', uid FROM src_info;

--Modules with no sources
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description) 
VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=3), 
	'BasicModule', 'src/basic/programs/BasicModule.bas', 
	'BASIC', 'OBJECT', 'FILE', 'CUSTOM', 
	true, 'DISCOVERY', 'DummyHash45', 
	'A test program');
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash, description) 
VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=3), 
	'BasicModule2', 'src/basic/programs/BasicModule2.bas', 
	'BASIC', 'OBJECT', 'FILE', 'CUSTOM', 
	true, 'DISCOVERY', 'DummyHash46', 
	'A test program');
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash, description)
VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=3), 
	'BasicFunction',
	'BASIC', 'FUNCTION', 'FILE_SECTION', 'CUSTOM', 
	true, 'DISCOVERY', 'DummyHash47', 
	'A test program');

--Utility modules
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash, info) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'IDCAMS', 'UNKNOWN', 'UTILITY', 'UNDEFINED', 'ENVIRONMENT', true, 'DISCOVERY', 'DummyHash41', '{"comments":"already exists","prefix":"IDC","description":"A utility used to create, delete, load, backup and print VSAM files. It is also used to create alternate indexes.","packageName":"z/OS RMF, DFSMS","taxonomy":"None","infoOrigin":"None","supported":"true"}');
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash, info) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'ABEND', 'UNKNOWN', 'UTILITY', 'UNDEFINED', 'ENVIRONMENT', true, 'DISCOVERY', 'DummyHash41', '{"comments":"","prefix":"ABE","description":"Abnormally terminate a task","packageName":"Enterprise COBOL for z/OS","taxonomy":"Coding Syntax","infoOrigin":"https://www.ibm.com/support/knowledgecenter/SS6SG3_3.4.0/com.ibm.entcobol.doc_3.4/igym1mst.pdf","supported":"false"}');
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash, info) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=2), 'IDCAMS', 'UNKNOWN', 'UTILITY', 'UNDEFINED', 'ENVIRONMENT', true, 'DISCOVERY', 'DummyHash41', '{"comments":"already exists","prefix":"IDC","description":"A utility used to create, delete, load, backup and print VSAM files. It is also used to create alternate indexes.","packageName":"z/OS RMF, DFSMS","taxonomy":"None","infoOrigin":"None","supported":"true"}');
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash, info) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=2), 'ABEND', 'UNKNOWN', 'UTILITY', 'UNDEFINED', 'ENVIRONMENT', true, 'DISCOVERY', 'DummyHash41', '{"comments":"","prefix":"ABE","description":"Abnormally terminate a task","packageName":"Enterprise COBOL for z/OS","taxonomy":"Coding Syntax","infoOrigin":"https://www.ibm.com/support/knowledgecenter/SS6SG3_3.4.0/com.ibm.entcobol.doc_3.4/igym1mst.pdf","supported":"false"}');

-- TEST CUSTOM PROPERTIES MODULE
UPDATE module SET custom_properties = ('{"ModuleCustomProperties":{'
	|| '"customMetaInfo1":"some custom meta 1 value",'
	|| '"customMetaInfo2":"some custom meta 2 value"'
	|| '}}')::jsonb
	WHERE name = 'IDCAMS';
	
UPDATE module SET custom_properties = ('{"ModuleCustomProperties":{'
	|| '"customMetaInfo1":"some custom meta 1 value",'
	|| '"customMetaInfo2":"some custom meta 2 value"'
	|| '}}')::jsonb
	WHERE name = 'PRG1';

--TEST HAS DEPENDENCYGRAPHLINKS
INSERT INTO module_relationship(id, src, dst, type, properties) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid = 2000), (SELECT uid FROM module WHERE nid = 2001), 'CALLS', '{"TEST_PROPERTY": "Hello World"}');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid = 2000), (SELECT uid FROM module WHERE nid = 2002), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid = 2002), (SELECT uid FROM module WHERE name='IDCAMS' AND project = (SELECT uid FROM project WHERE nid = 1)), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid = 2000), (SELECT uid FROM module WHERE name='DPGM1'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM1'), (SELECT uid FROM module WHERE name='DPGM2'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM1'), (SELECT uid FROM module WHERE name='DPGM3'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM2'), (SELECT uid FROM module WHERE name='DPGM4'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM3'), (SELECT uid FROM module WHERE name='DPGM5'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM4'), (SELECT uid FROM module WHERE name='DPGM6'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM5'), (SELECT uid FROM module WHERE name='DPGM7'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM4'), (SELECT uid FROM module WHERE name='DPGM8'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM5'), (SELECT uid FROM module WHERE name='DPGM9'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM6'), (SELECT uid FROM module WHERE name='DPGM10'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM8'), (SELECT uid FROM module WHERE name='DPGM12'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM7'), (SELECT uid FROM module WHERE name='DPGM11'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='DPGM9'), (SELECT uid FROM module WHERE name='DPGM13'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2003), (SELECT uid FROM module WHERE name='IDCAMS' AND project = (SELECT uid FROM project WHERE nid = 2)), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2003), (SELECT uid FROM module WHERE name='ABEND' AND project = (SELECT uid FROM project WHERE nid = 2)), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2032), (SELECT uid FROM module WHERE name='BASICOFUNC' AND project = (SELECT uid FROM project WHERE nid = 1)), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2032), (SELECT uid FROM module WHERE name='BASICOPROG' AND project = (SELECT uid FROM project WHERE nid = 1)), 'CALLS');

--TEST DELETE FROM AND TO MODULE LOCATIONS
INSERT INTO module_relationship(id, src, dst, type, src_location, dst_location) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='EXECSQL'), (SELECT uid FROM module WHERE name='PARAM'), 'CALLS', (100, 4), (200, 5));

-- TEST LINKED MODULE USING CREATE CONTAINS RELATIONSHIP
INSERT INTO module_relationship(id, src, dst, type, src_location, dst_location) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='BasicModule2'), (SELECT uid FROM module WHERE name='BasicFunction'), 'CALLS', (100, 4), (200, 5));
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name = 'BasicModule'), (SELECT uid FROM module WHERE name = 'BasicFunction'), 'CONTAINS');

	
--TEST ANNOTATION CATEGORY
INSERT INTO annotation_category(name, types, project) VALUES ('Annotation Category A', array[]::text[], (SELECT uid FROM project WHERE nid=1));
INSERT INTO annotation_category(name, types, project) VALUES ('Annotation Category B', array[]::text[], (SELECT uid FROM project WHERE nid=1));
INSERT INTO annotation_category(name, types, project) VALUES ('Annotation Category C', array[]::text[], (SELECT uid FROM project WHERE nid=2));
INSERT INTO annotation_category(name, types, project) VALUES ('Annotation Category D', array[]::text[], (SELECT uid FROM project WHERE nid=2));

--TEST ANNOTATION
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, updated_by, source) VALUES (gen_random_uuid(), 'Annotation 1', 'CANDIDATE', 'RULE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category A'), (SELECT uid FROM Module WHERE nid = 2000), (100, 4), 'admin', 'admin', (utf8_to_bytes(E'abcd')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, updated_by, source) VALUES (gen_random_uuid(), 'Annotation 2', 'CANDIDATE', 'RULE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category A'), (SELECT uid FROM Module WHERE nid = 2000), (200, 4), 'admin', null, (utf8_to_bytes(E'1234')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, updated_by, source) VALUES (gen_random_uuid(), 'Annotation 3', 'CANDIDATE', 'RULE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category A'), (SELECT uid FROM Module WHERE nid = 2000), (300, 4), 'admin', null, (utf8_to_bytes(E'efgh')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, updated_by, source) VALUES (gen_random_uuid(), 'Annotation 4', 'CANDIDATE', 'RULE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category A'), (SELECT uid FROM Module WHERE nid = 2000), (400, 4), 'admin', null, (utf8_to_bytes(E'5678')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, updated_by, source) VALUES (gen_random_uuid(), 'Annotation 5', 'IN_ANALYSIS', 'RULE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category B'), (SELECT uid FROM Module WHERE nid = 2001), (500, 5), 'admin', null, (utf8_to_bytes(E'5678')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, updated_by, source) VALUES (gen_random_uuid(), 'Annotation 6', 'REJECTED', 'RULE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category C'), (SELECT uid FROM Module WHERE nid = 2002), (600, 6), 'admin', null, (utf8_to_bytes(E'5678')));

INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, updated_by, source) VALUES (gen_random_uuid(), 'Database Annotation 1', 'CANDIDATE', 'DATABASE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category B'), (SELECT uid FROM module WHERE name='EXECSQL'), (7, 14), 'system_user', 'admin', (utf8_to_bytes(E'IDENTIFICATION')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, updated_by, source) VALUES (gen_random_uuid(), 'Database Annotation 2', 'CANDIDATE', 'DATABASE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category B'), (SELECT uid FROM module WHERE name='EXECSQL'), (677, 7), 'system_user', 'system_user', (utf8_to_bytes(E'GOBACK.')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, source) VALUES (gen_random_uuid(), 'DeleteAnnotation', 'CANDIDATE', 'DATABASE', (SELECT id FROM annotation_category WHERE name = 'Annotation Category B'), (SELECT uid FROM module WHERE name='EXECSQL'), (677, 7), 'admin', (utf8_to_bytes(E'test')));

--TEST DELETE MODULE ALONG WITH DOCUMENT DEPENDENCIES USING SPRING DATA.
INSERT INTO module_dead_code(number_of_lines, module) VALUES (10, (SELECT uid FROM module WHERE name='DeleteTestSD1'));
INSERT INTO module_dead_code(number_of_lines, module) VALUES (11, (SELECT uid FROM module WHERE name='DeleteTestSD1'));

INSERT INTO error_marker(cause, key, severity, project, module, location) VALUES ('TEMP_CAUSE1', 'MODULE_ABORT', 'WARNING', (SELECT uid FROM project WHERE nid = 3), (SELECT uid FROM module WHERE name='DeleteTestSD1'), (-1, 10));
INSERT INTO error_marker(cause, key, severity, project, module, location) VALUES ('TEMP_CAUSE2', 'MODULE_ABORT', 'WARNING', (SELECT uid FROM project WHERE nid = 3), (SELECT uid FROM module WHERE name='DeleteTestSD1'), (-1, 11));
INSERT INTO error_marker(cause, key, severity, project, module, location) VALUES ('TEMP_CAUSE3', 'MODULE_ABORT', 'WARNING', (SELECT uid FROM project WHERE nid = 3), (SELECT uid FROM module WHERE name='DeleteTestSD2'), (-1, 12));
INSERT INTO error_marker(cause, key, severity, project, module, location) VALUES ('TEMP_CAUSE4', 'MODULE_ABORT', 'WARNING', (SELECT uid FROM project WHERE nid = 3), (SELECT uid FROM module WHERE name='DeleteTestSD3'), (-1, 13));

INSERT INTO module_dead_code(number_of_lines, module) VALUES (12, (SELECT uid FROM module WHERE name='DeleteTestSD2'));
INSERT INTO module_dead_code(number_of_lines, module) VALUES (13, (SELECT uid FROM module WHERE name='DeleteTestSD3'));

INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, source) VALUES (gen_random_uuid(), 'DeleteTestAnn1', 'CANDIDATE', 'DATABASE', 2, (SELECT uid FROM Module WHERE name='DeleteTestSD1'), (677, 7), 'admin', (utf8_to_bytes(E'test')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, source) VALUES (gen_random_uuid(), 'DeleteTestAnn2', 'CANDIDATE', 'DATABASE', 2, (SELECT uid FROM Module WHERE name='DeleteTestSD1'), (677, 7), 'admin', (utf8_to_bytes(E'test')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, source) VALUES (gen_random_uuid(), 'DeleteTestAnn3', 'CANDIDATE', 'DATABASE', 2, (SELECT uid FROM Module WHERE name='DeleteTestSD2'), (677, 7), 'admin', (utf8_to_bytes(E'test')));
INSERT INTO annotation(uid, name, state, type, category, module, location, created_by, source) VALUES (gen_random_uuid(), 'DeleteTestAnn4', 'CANDIDATE', 'DATABASE', 2, (SELECT uid FROM Module WHERE name='DeleteTestSD3'), (677, 7), 'admin', (utf8_to_bytes(E'test')));

-- TEST ANNOTATION METADATA
UPDATE annotation SET reasons = ARRAY['IF_ELSE_CONDITION', 'LOOP_CONDITION', 'OTHER_CONDITION'] WHERE name = 'Annotation 1';
UPDATE annotation SET reasons = ARRAY['SELECT_CONDITION', 'OTHER_CONDITION'] WHERE name = 'Annotation 2';
UPDATE annotation SET reasons = ARRAY['LOOP_CONDITION'] WHERE name = 'Annotation 3';

-- TEST CUSTOM PROPERTIES ANNOTATION
UPDATE annotation SET custom_properties = ('{"AnnotationCustomProperties":{'
	|| '"dependentAnnotations":' 
		|| (SELECT to_jsonb(array_agg(uid)) FROM annotation WHERE name='Annotation 2' OR name='Annotation 3') 
		|| ','
	|| '"customMetaInfo":"some custom meta value",'
	|| '"customAnnotationProperty":"A value for the custom Annotation property"'
	|| '}}')::jsonb
	WHERE name = 'Annotation 1';
	
--TEST DATA DICTIONARY ENTRY
INSERT INTO data_dictionary(uid, name, description, format, created_by, length, scopes, module, location) VALUES (gen_random_uuid(), 'MY-PROGRAM-NAME', 'This is an english description of the data element name MY-PROGRAM-NAME', 'PICX', 'admin', 15, '{"SQL_DATABASE":{"tables":"insert table names here"},"CICS_UI":{"mapname":"mapnome","mapset":"mapsot"}}', (SELECT uid FROM module WHERE path='src/cobol/programs/MMRS7101.cbl'), (1005, 15));
INSERT INTO data_dictionary(uid, name, description, format, created_by, length, scopes, module, location) VALUES (gen_random_uuid(), 'MY-BIN-FIELDS', 'This is an english description of the data element name MY-BIN-FIELDS', 'GROUP', 'admin', 13, '{"FILE":{"dataset":"insert your favorite dataset names here"}}', (SELECT uid FROM module WHERE path='src/cobol/programs/MMRS7101.cbl'), (1250, 13));
INSERT INTO data_dictionary(uid, name, description, format, created_by, length, module, location) VALUES (gen_random_uuid(), 'MY-HEX-ORIGIN-LEN', 'This is an english description of the data element name MY-HEX-ORIGIN-LEN', 'PIC9', 'admin', 17, (SELECT uid FROM module WHERE path='src/cobol/programs/MMRS7101.cbl'), (1862, 17));

-- TEST HasBusinessRule / data_dictionary_annotations
insert into data_dictionary_annotations values (
	(SELECT uid FROM data_dictionary WHERE name = 'MY-BIN-FIELDS' LIMIT 1), 
	(SELECT uid FROM annotation WHERE name = 'Annotation 6' LIMIT 1));
insert into data_dictionary_annotations values (
	(SELECT uid FROM data_dictionary WHERE name = 'MY-PROGRAM-NAME' LIMIT 1), 
	(SELECT uid FROM annotation WHERE name = 'Annotation 6' LIMIT 1));
insert into data_dictionary_annotations values (
	(SELECT uid FROM data_dictionary WHERE name = 'MY-HEX-ORIGIN-LEN' LIMIT 1), 
	(SELECT uid FROM annotation WHERE name = 'Annotation 6' LIMIT 1));
insert into data_dictionary_annotations values (
	(SELECT uid FROM data_dictionary WHERE name = 'MY-BIN-FIELDS' LIMIT 1), 
	(SELECT uid FROM annotation WHERE name = 'Annotation 5' LIMIT 1));

--TEST DATA DICTIONARY OTHER SCOPE ENUM
INSERT INTO data_dictionary_other_scope(project, name) VALUES ((SELECT uid FROM project WHERE nid=1), 'SCOPE_1');
INSERT INTO data_dictionary_other_scope(project, name) VALUES ((SELECT uid FROM project WHERE nid=1), 'SCOPE_2');

-- TEST CUSTOM PROPERTIES DataDictionaryEntry
UPDATE data_dictionary SET custom_properties = '{"DataDictionaryEntryCustomProperties":{"customDataDictionaryEntryProperty":"A value for the custom DataDictionaryEntry property"}}'::jsonb
	WHERE name = 'FOO';
UPDATE data_dictionary SET custom_properties = '{"DataDictionaryEntryCustomProperties":{"customDataDictionaryEntryProperty":"A value for the custom DataDictionaryEntry property"}}'::jsonb
	WHERE name = 'MY-PROGRAM-NAME';

-- TEST TAXONOMY CATEGORY
INSERT INTO taxonomy_category(name, project) VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid=1));
INSERT INTO taxonomy_category(name, project) VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid=1));
INSERT INTO taxonomy_category(name, project) VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid=2));
INSERT INTO taxonomy_category(name, project) VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid=2));
INSERT INTO taxonomy_category(name, project) VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid=3));
INSERT INTO taxonomy_category(name, project) VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid=3));
INSERT INTO taxonomy_category(name, project) VALUES ('Technical Taxonomies', (SELECT uid FROM project WHERE nid=4));
INSERT INTO taxonomy_category(name, project) VALUES ('Business Taxonomies', (SELECT uid FROM project WHERE nid=4));

-- TEST TAXONOMY
INSERT INTO taxonomy_type(id, name, project, category) VALUES (gen_random_uuid(), 'DataDomain', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_category WHERE name='Business Taxonomies' AND project = (SELECT uid FROM project WHERE nid=1)));
INSERT INTO taxonomy_type(id, name, project, category) VALUES (gen_random_uuid(), 'BusinessProcess', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_category WHERE name='Business Taxonomies' AND project = (SELECT uid FROM project WHERE nid=1)));
INSERT INTO taxonomy_type(id, name, project, category) VALUES (gen_random_uuid(), 'BusinessSubsystem', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_category WHERE name='Business Taxonomies' AND project = (SELECT uid FROM project WHERE nid=1)));

INSERT INTO taxonomy(uid, name, project, type) VALUES (gen_random_uuid(), 'Employee domain', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_type WHERE name = 'DataDomain' AND project = (SELECT uid FROM project WHERE nid=1)));
INSERT INTO taxonomy(uid, name, project, type) VALUES (gen_random_uuid(), 'Create Invoices', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_type WHERE name = 'BusinessProcess' AND project = (SELECT uid FROM project WHERE nid=1)));
INSERT INTO taxonomy(uid, name, project, type) VALUES (gen_random_uuid(), 'ARB100', (SELECT uid FROM project WHERE nid=1), (SELECT id FROM taxonomy_type WHERE name = 'BusinessSubsystem' AND project = (SELECT uid FROM project WHERE nid=1)));

-- TEST CUSTOM PROPERTIES Taxonomy
UPDATE taxonomy SET custom_properties = '{"TaxonomyCustomProperties":{"customTaxonomyProperty":"A value for the custom Taxonomy property"}}'::jsonb
	WHERE name = 'Employee domain';

--TEST HAS TAXONOMY
INSERT INTO module_taxonomies(module, taxonomy) VALUES ((SELECT uid FROM module WHERE nid = 2000), (SELECT uid FROM taxonomy WHERE name = 'ARB100'));;
INSERT INTO module_taxonomies(module, taxonomy) VALUES ((SELECT uid FROM module WHERE nid = 2000), (SELECT uid FROM taxonomy WHERE name = 'Employee domain'));
INSERT INTO module_taxonomies(module, taxonomy) VALUES ((SELECT uid FROM module WHERE nid = 2001), (SELECT uid FROM taxonomy WHERE name = 'Employee domain'));

-- TEST DNA data

INSERT INTO dna_snapshot (id, project, updated, module_count, dna_config) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid = 1), '2022-07-05 13:18:27Z', 10, '{"similarityThreshold": 0.85, "maxIterations":10, "defaultTolerance":0.0001, "minDNALength":20}');
INSERT INTO dna_snapshot (id, project, updated, module_count, dna_config) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid = 1), '2022-08-05 13:18:27Z', 10, '{"similarityThreshold": 0.85, "maxIterations":10, "defaultTolerance":0.0001, "minDNALength":10}');

INSERT INTO dna_community (id, snapshot, sequencer, similarity_algo, cluster_algo, cluster_index) VALUES (gen_random_uuid(), (SELECT id FROM dna_snapshot ORDER BY updated DESC LIMIT 1), 'COBOL_METHOD_RULE', 'WEIGHTED_LEVENSHTEIN', 'LOUVAIN', 1);
INSERT INTO dna_community (id, snapshot, sequencer, similarity_algo, cluster_algo, cluster_index, title) VALUES (gen_random_uuid(), (SELECT id FROM dna_snapshot ORDER BY updated DESC LIMIT 1), 'COBOL_METHOD_RULE', 'WEIGHTED_LEVENSHTEIN', 'LOUVAIN', -1, 'Unassigned Modules');

INSERT INTO dna_community_modules (community, module) VALUES ((SELECT id FROM dna_community WHERE cluster_index = 1), (SELECT uid FROM module WHERE nid = 2020));
INSERT INTO dna_community_modules (community, module) VALUES ((SELECT id FROM dna_community WHERE cluster_index = 1), (SELECT uid FROM module WHERE nid = 2));
INSERT INTO dna_community_modules (community, module) VALUES ((SELECT id FROM dna_community WHERE cluster_index = 1), (SELECT uid FROM module WHERE nid = 3));
INSERT INTO dna_community_modules (community, module) VALUES ((SELECT id FROM dna_community WHERE cluster_index = 1), (SELECT uid FROM module WHERE nid = 4));
INSERT INTO dna_community_modules (community, module) VALUES ((SELECT id FROM dna_community WHERE cluster_index = 1), (SELECT uid FROM module WHERE nid = 5));
INSERT INTO dna_community_modules (community, module) VALUES ((SELECT id FROM dna_community WHERE cluster_index = -1), (SELECT uid FROM module WHERE nid = 2022));

--TEST for statements
INSERT INTO statement(uid, module, technology, type, text, properties) 
	VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='BasicModule'), 'COBOL', 'SELECT', 'TestStatementA', 
	'{"tables": 15, "customComplexity": 9, "sqlLength": 84, "distinctTables": 14, "customComplexity": 27, "halsteadComplexity": 1.2, "halsteadDifficulty": 99.2}');

INSERT INTO statement(uid, module, technology, type, text, properties) 
	VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE name='BasicModule'), 'COBOL', 'SELECT', 'TestStatementB', 
	'{"customComplexity": 27, "distinctTables": 28, "halsteadComplexity": 3.4, "halsteadDifficulty": 2.8, "sqlLength": 8, "tables": 10}');

INSERT INTO statement(uid, module, technology, type, text, properties) 
	VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2000), 'COBOL', 'SELECT', 'TestStatementC', 
	'{"customComplexity": 9, "distinctTables": 14, "halsteadComplexity": 1.2, "halsteadDifficulty": 99.2, "sqlLength": 84, "tables": 15}');

INSERT INTO statement(uid, module, technology, type, text, properties) 
	VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2001), 'COBOL', 'SELECT', 'TestStatementD', 
	'{"customComplexity": 27, "distinctTables": 28, "halsteadComplexity": 3.4, "halsteadDifficulty": 2.8, "sqlLength": 8, "tables": 10}');

INSERT INTO statement(uid, module, technology, type, text, properties) 
	VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2003), 'COBOL', 'DECLARE_TABLE', 'TestStatementE', 
	'{"customComplexity": 27, "distinctTables": 28, "halsteadComplexity": 3.4, "halsteadDifficulty": 2.8, "sqlLength": 8, "tables": 10}');

INSERT INTO statement(uid, module, technology, type, text, properties) 
	VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2003), 'COBOL', 'ALTER_CHECK_CONSTRAINT', 'TestStatementF', 
	'{"customComplexity": 27, "distinctTables": 28, "halsteadComplexity": 3.4, "halsteadDifficulty": 2.8, "sqlLength": 8, "tables": 10}');

INSERT INTO statement(uid, module, technology, type, text, properties) 
	VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE nid=2003), 'COBOL', 'SELECT', 'TestStatementG', 
	'{"customComplexity": 27, "distinctTables": 28, "halsteadComplexity": 3.4, "halsteadDifficulty": 2.8, "sqlLength": 8, "tables": 10}');
