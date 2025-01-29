-- This script creates the table QueryDetailsView that mimicks the table(view) in the dbcutter database. With this, there is no need of a connection to the dbcutter database to run the tests.
-- The table name and the column names are created by dbcutter team based on their standards.
DROP TABLE IF EXISTS "QueryDetailsView";

CREATE TABLE "QueryDetailsView" (
    id INTEGER,
    "miningProjectId" INTEGER,
    query CHARACTER VARYING,
    "miningModuleHash" CHARACTER VARYING,
    conditional TEXT[],
    nonconditional TEXT[],
    "queryType" TEXT,
    name CHARACTER VARYING
);

INSERT INTO "QueryDetailsView" (id, "miningProjectId", query, "miningModuleHash", nonconditional, conditional, "queryType", name)
VALUES
(7, 1, 'SELECT table7.column14 FROM table7, table8 WHERE table7.column14 = table8.column15 AND table7.column16 > 50', 'DummyHashPgm2SqlDetails', ARRAY['column14', 'column16']::TEXT[], ARRAY['column14']::TEXT[], 'SELECT', 'table7'),
(8, 1, 'SELECT table7.column14 FROM table7, table8 WHERE table7.column14 = table8.column15 AND table7.column16 > 50', 'DummyHashPgm2SqlDetails', ARRAY['column15']::TEXT[], ARRAY[]::TEXT[], 'SELECT', 'table8'),
(9, 1, 'UPDATE table9 SET column17 = ''new_value'' WHERE column18 = ''old_value'' AND column19 = ''another_value''', 'DummyHashPgm2SqlDetails', ARRAY['column18', 'column19']::TEXT[], ARRAY[]::TEXT[], 'UPDATE', 'table9');

INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'SQLPRG1', 'src/cobol/programs/sqlprg1.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHashPgm1SqlDetails');
INSERT INTO module(uid, project, name, path, technology, type, storage, origin, identified, creator, link_hash) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'SQLPRG2', 'src/cobol/programs/sqlprg2.cbl', 'COBOL', 'PROGRAM', 'FILE', 'CUSTOM', true, 'DISCOVERY', 'DummyHashPgm2SqlDetails');
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'table7', 'SQL', 'TABLE', 'FILE_SECTION', 'CUSTOM', true, 'DISCOVERY', 'DummyHashtable7SqlDetails');
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'table8', 'SQL', 'TABLE', 'FILE_SECTION', 'CUSTOM', true, 'DISCOVERY', 'DummyHashtable8SqlDetails');
INSERT INTO module(uid, project, name, technology, type, storage, origin, identified, creator, link_hash) VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), 'table9', 'SQL', 'TABLE', 'FILE_SECTION', 'CUSTOM', true, 'DISCOVERY', 'DummyHashtable9SqlDetails');

INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE link_hash = 'DummyHashPgm1SqlDetails'), (SELECT uid FROM module WHERE link_hash = 'DummyHashPgm2SqlDetails'), 'CALLS');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE link_hash = 'DummyHashPgm2SqlDetails'), (SELECT uid FROM module WHERE link_hash = 'DummyHashtable7SqlDetails'), 'ACCESSES');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE link_hash = 'DummyHashPgm2SqlDetails'), (SELECT uid FROM module WHERE link_hash = 'DummyHashtable8SqlDetails'), 'ACCESSES');
INSERT INTO module_relationship(id, src, dst, type) VALUES (gen_random_uuid(), (SELECT uid FROM module WHERE link_hash = 'DummyHashPgm2SqlDetails'), (SELECT uid FROM module WHERE link_hash = 'DummyHashtable9SqlDetails'), 'ACCESSES');

