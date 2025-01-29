-- Replace the unique index (name, projectLink) with index (name, projectLink, typeLink)
DROP INDEX AnnotationCategory_name_idx IF EXISTS;
CREATE INDEX AnnotationCategory_name_idx ON AnnotationCategory (name, projectLink, typeLink) UNIQUE;