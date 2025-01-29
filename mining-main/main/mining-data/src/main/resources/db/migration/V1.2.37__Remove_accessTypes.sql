UPDATE ReadsWrites REMOVE accessTypes;
DROP PROPERTY ReadsWrites.accessTypes FORCE;
DELETE VERTEX DatabaseAccessEnum;
DROP CLASS DatabaseAccessEnum;

UPDATE TaxonomyEnum SET name = "DB Access" WHERE name = "SQL Access";
UPDATE Taxonomy SET name = "Read" WHERE name = "Select";
UPDATE Taxonomy SET name = "Store" WHERE name = "Insert";