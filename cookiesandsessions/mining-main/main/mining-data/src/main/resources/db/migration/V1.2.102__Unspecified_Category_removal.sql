UPDATE Annotation SET categoryLink = null WHERE categoryLink = (SELECT @rid FROM AnnotationCategory WHERE projectLink = (SELECT @rid FROM project WHERE id = 0) AND name = 'Unspecified');
DELETE VERTEX AnnotationCategory WHERE projectLink = (SELECT @rid FROM project WHERE id = 0) AND name = 'Unspecified';
