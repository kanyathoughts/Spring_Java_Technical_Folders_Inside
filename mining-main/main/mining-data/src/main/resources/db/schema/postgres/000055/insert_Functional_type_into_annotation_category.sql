INSERT INTO annotation_category (project, name, types) 
VALUES ((SELECT uid FROM project WHERE nid =0),'None', '{FUNCTIONAL}');