UPDATE Module REMOVE sourceAttachmentLink;
DROP PROPERTY Module.sourceAttachmentLink IF EXISTS FORCE;
-- Updates the sourceObjectLink with the newly created source objects from that of the source attachment in the Module
UPDATE Module SET sourceObjectLink = (SELECT FROM SourceObject WHERE projectLink = $current.projectLink AND path = $current.path) 
WHERE path IS NOT NULL and sourceObjectLink IS NULL;