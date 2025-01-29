INSERT INTO custom_property_entities (project, entity, property)
SELECT uid, e.key, p.id FROM project, jsonb_each(custom_property_classes) e, jsonb_array_elements_text(e.value) c
INNER JOIN custom_property p ON p.parent IS NULL AND p.name = c.value;

ALTER TABLE project DROP COLUMN custom_property_classes;
