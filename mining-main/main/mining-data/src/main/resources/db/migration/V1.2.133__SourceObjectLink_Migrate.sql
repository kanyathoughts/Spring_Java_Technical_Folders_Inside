CREATE PROPERTY Module.sourceObjectLink IF NOT EXISTS LINK SourceObject;
UPDATE Module SET sourceObjectLink=sourceAttachmentLink WHERE sourceAttachmentLink IS NOT NULL AND sourceAttachmentLink.@class='SourceObject';
