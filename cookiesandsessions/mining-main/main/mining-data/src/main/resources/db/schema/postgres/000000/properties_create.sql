CREATE TABLE properties (name text, value text NOT NULL, PRIMARY KEY (name));
INSERT INTO properties (name, value) VALUES ('db.id', gen_random_uuid());
INSERT INTO properties (name, value) VALUES ('schema.version', 0);
