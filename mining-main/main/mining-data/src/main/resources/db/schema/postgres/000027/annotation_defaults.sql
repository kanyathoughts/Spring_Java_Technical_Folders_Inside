INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['RULE'], 'Business Rule');
INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['RULE'], 'Process Rule');
INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['RULE'], 'Validation Rule');
INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['RULE'], 'Technical Rule');
INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['RULE'], 'Field Computation Rule');
INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['RULE'], 'Error Processing Rule');

INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['DATABASE'], 'Read');
INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['DATABASE'], 'Write');
INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['DATABASE'], 'Declare');
INSERT INTO annotation_category (project, types, name) VALUES ((SELECT uid FROM project WHERE nid = 0), ARRAY['DATABASE'], 'Close');
