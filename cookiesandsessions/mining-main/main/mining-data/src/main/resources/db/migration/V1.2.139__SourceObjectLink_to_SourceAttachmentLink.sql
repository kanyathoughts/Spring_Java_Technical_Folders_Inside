CREATE PROPERTY Module.sourceAttachmentLink IF NOT EXISTS LINK SourceAttachment;
UPDATE Module SET sourceAttachmentLink=sourceObjectLink where sourceObjectLink is not Null;
UPDATE Module REMOVE sourceObjectLink;
DROP PROPERTY Module.sourceObjectLink IF EXISTS FORCE;
