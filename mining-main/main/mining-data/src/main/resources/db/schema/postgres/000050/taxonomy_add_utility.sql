INSERT INTO taxonomy_type (id, project, category, name)
	SELECT gen_random_uuid(), project.uid, taxonomy_category.id, 'Utility'
	FROM project CROSS JOIN taxonomy_category WHERE project.uid = taxonomy_category.project AND taxonomy_category.name = 'Technical Taxonomies';
INSERT INTO taxonomy (uid, custom_properties, project, type, name)
	SELECT gen_random_uuid(), null, project.uid, taxonomy_type.id, 'Common Interface'
	FROM project CROSS JOIN taxonomy_type WHERE project.uid = taxonomy_type.project AND taxonomy_type.name = 'Utility';
INSERT INTO taxonomy (uid, custom_properties, project, type, name)
	SELECT gen_random_uuid(), null, project.uid, taxonomy_type.id, 'Common Logging'
	FROM project CROSS JOIN taxonomy_type WHERE project.uid = taxonomy_type.project AND taxonomy_type.name = 'Utility';
INSERT INTO taxonomy (uid, custom_properties, project, type, name)
	SELECT gen_random_uuid(), null, project.uid, taxonomy_type.id, 'Common Transaction'
	FROM project CROSS JOIN taxonomy_type WHERE project.uid = taxonomy_type.project AND taxonomy_type.name = 'Utility';