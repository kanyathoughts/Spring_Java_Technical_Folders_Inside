--Delete data

DELETE FROM custom_enum;
DELETE FROM custom_property;
DELETE FROM custom_property_entities;


--###################################################################################################################################
--# CUSTOM PROPERTIES PROJECT
--###################################################################################################################################

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), NULL, 0, 'ProjectCustomProperties', NULL);

INSERT INTO custom_property_entities (project, entity, property) VALUES ((SELECT uid FROM project WHERE nid = 1), 'Project', (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'ProjectCustomProperties'));

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'ProjectCustomProperties'), 1, 'customProjectProperty', '{
  "dataType": "STRING",
  "label": "Custom Project Property",
  "pluginVisible": true,
  "description": "A custom property for the Project class"
}');


--###################################################################################################################################
--# CUSTOM PROPERTIES DataDictionaryEntry
--###################################################################################################################################

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), NULL, 0, 'DataDictionaryEntryCustomProperties', NULL);

INSERT INTO custom_property_entities (project, entity, property) VALUES ((SELECT uid FROM project WHERE nid = 1), 'DataDictionaryEntry', (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'DataDictionaryEntryCustomProperties'));

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'DataDictionaryEntryCustomProperties'), 1, 'customDataDictionaryEntryProperty', '{
  "dataType": "STRING",
  "label": "Custom DataDictionaryEntry Property",
  "pluginVisible": true,
  "description": "A custom property for the DataDictionaryEntry class"
}');


--###################################################################################################################################
--# CUSTOM PROPERTIES Taxonomy
--###################################################################################################################################

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), NULL, 0, 'TaxonomyCustomProperties', NULL);

INSERT INTO custom_property_entities (project, entity, property) VALUES ((SELECT uid FROM project WHERE nid = 1), 'Taxonomy', (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'TaxonomyCustomProperties'));

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'TaxonomyCustomProperties'), 1, 'customTaxonomyProperty', '{
  "dataType": "STRING",
  "label": "Custom Taxonomy Property",
  "pluginVisible": true,
  "description": "A custom property for the Taxonomy class"
}');


--###################################################################################################################################
--# CUSTOM PROPERTIES ANNOTATION
--###################################################################################################################################

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), NULL, 0, 'AnnotationCustomProperties', NULL);

INSERT INTO custom_property_entities (project, entity, property) VALUES ((SELECT uid FROM project WHERE nid = 1), 'Annotation', (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties'));

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties'), 1, 'annotationTags', '{
  "dataType": "EMBEDDEDLIST",
  "label": "Annotation Tags",
  "fieldType": "TAG", 
  "autoCompletionKey": "annotationTags"
}');

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties'), 2, 'colorTags', '{
  "dataType": "EMBEDDEDLIST",
  "label": "Annotation Colors",
  "fieldType": "TAG", 
  "autoCompletionKey": "colorTags"
}');

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties'), 3, 'ruleTags', '{
  "dataType": "EMBEDDEDLIST",
  "label": "Annotation Rules",
  "fieldType": "TAG", 
  "autoCompletionKey": "ruleTags"
}');

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties'), 3, 'customMetaInfo', '{
  "dataType": "STRING",
  "mandatory": false,
  "min": 5,
  "max": 30,
  "readOnly": true,
  "description": "This is some more custom meta information",
  "label": "Some custom meta information",
  "pluginVisible": false,
  "dataSource": "Custom datasource URL",
  "showWhen": {"annotationCategoryId": "42"},
  "customViewNames": ["First Custom View", "Second Custom View"],
  "customViewIndex": 21,
  "validationRegex": "[a-z]*",
  "validationErrorMessage": "Incorrect Format"
}');

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'AnnotationCustomProperties'), 4, 'customAnnotationProperty', '{
  "dataType": "STRING",
  "label": "Custom Annotation Property",
  "pluginVisible": true,
  "description": "A custom property for the Annotation class"
}');


--###################################################################################################################################
--# CUSTOM PROPERTIES MODULE
--###################################################################################################################################

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), NULL, 0, 'ModuleCustomProperties', NULL);

INSERT INTO custom_property_entities (project, entity, property) VALUES ((SELECT uid FROM project WHERE nid = 1), 'Module', (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'ModuleCustomProperties'));

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'ModuleCustomProperties'), 1, 'customMetaInfo1', '{
  "dataType": "STRING",
  "mandatory": false,
  "min": 5,
  "max": 26,
  "readOnly": true,
  "description": "This is some more custom meta information 1",
  "label": "Some custom meta information 1",
  "description": "Test Annotation Custom Property for graph ql test",
  "pluginVisible": false,
  "dataSource": "Custom datasource URL 1"
}');

INSERT INTO custom_property VALUES (gen_random_uuid(), (SELECT uid FROM project WHERE nid=1), (SELECT id FROM custom_property WHERE parent IS NULL AND name = 'ModuleCustomProperties'), 2, 'customMetaInfo2', '{
  "dataType": "STRING",
  "description": "This is some more custom meta information 2",
  "label": "Some custom meta information 2",
  "pluginVisible": false,
  "dataSource": "Custom datasource URL 2"
}');
