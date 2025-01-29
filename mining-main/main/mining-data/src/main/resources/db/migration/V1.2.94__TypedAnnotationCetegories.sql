CREATE PROPERTY AnnotationCategory.typeLink LINKLIST AnnotationTypeEnum (MANDATORY TRUE, DEFAULT "NULL");

INSERT INTO AnnotationCategory SELECT (SELECT @rid FROM project WHERE id = 0) AS projectLink, (SELECT @rid FROM AnnotationTypeEnum WHERE name = 'RULE' OR name = 'DATABASE') AS typeLink, sequence('AnnotationCategory_Sequence').next() AS id, 'Unspecified' AS name;

INSERT INTO AnnotationCategory SELECT (SELECT @rid FROM project WHERE id = 0) AS projectLink, (SELECT @rid FROM AnnotationTypeEnum WHERE name = 'RULE') AS typeLink, sequence('AnnotationCategory_Sequence').next() AS id, 'Business Rule' AS name;
INSERT INTO AnnotationCategory SELECT (SELECT @rid FROM project WHERE id = 0) AS projectLink, (SELECT @rid FROM AnnotationTypeEnum WHERE name = 'RULE') AS typeLink, sequence('AnnotationCategory_Sequence').next() AS id, 'Process Rule' AS name;
INSERT INTO AnnotationCategory SELECT (SELECT @rid FROM project WHERE id = 0) AS projectLink, (SELECT @rid FROM AnnotationTypeEnum WHERE name = 'RULE') AS typeLink, sequence('AnnotationCategory_Sequence').next() AS id, 'Validation Rule' AS name;

INSERT INTO AnnotationCategory SELECT (SELECT @rid FROM project WHERE id = 0) AS projectLink, (SELECT @rid FROM AnnotationTypeEnum WHERE name = 'DATABASE') AS typeLink, sequence('AnnotationCategory_Sequence').next() AS id, 'Read' AS name;
INSERT INTO AnnotationCategory SELECT (SELECT @rid FROM project WHERE id = 0) AS projectLink, (SELECT @rid FROM AnnotationTypeEnum WHERE name = 'DATABASE') AS typeLink, sequence('AnnotationCategory_Sequence').next() AS id, 'Write' AS name;
INSERT INTO AnnotationCategory SELECT (SELECT @rid FROM project WHERE id = 0) AS projectLink, (SELECT @rid FROM AnnotationTypeEnum WHERE name = 'DATABASE') AS typeLink, sequence('AnnotationCategory_Sequence').next() AS id, 'Declare' AS name;
INSERT INTO AnnotationCategory SELECT (SELECT @rid FROM project WHERE id = 0) AS projectLink, (SELECT @rid FROM AnnotationTypeEnum WHERE name = 'DATABASE') AS typeLink, sequence('AnnotationCategory_Sequence').next() AS id, 'Close' AS name;
