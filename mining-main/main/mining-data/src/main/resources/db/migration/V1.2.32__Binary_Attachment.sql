CREATE CLASS BinaryAttachmentMimeEnum EXTENDS V;
CREATE PROPERTY BinaryAttachmentMimeEnum.name STRING (NOTNULL, MANDATORY TRUE);
INSERT INTO BinaryAttachmentMimeEnum set name="PNG";
INSERT INTO BinaryAttachmentMimeEnum set name="JPEG";
INSERT INTO BinaryAttachmentMimeEnum set name="SVG";
CREATE INDEX BinaryAttachmentMimeEnum_name_idx ON BinaryAttachmentMimeEnum (name) UNIQUE;

CREATE SEQUENCE BinaryAttachment_Sequence TYPE ORDERED START 0 INCREMENT 1;

CREATE CLASS BinaryAttachment EXTENDS V;
CREATE PROPERTY BinaryAttachment.id LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY BinaryAttachment.name STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY BinaryAttachment.mimetype LINK BinaryAttachmentMimeEnum (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY BinaryAttachment.content BINARY (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY BinaryAttachment.createdByUserLink LINK User (NOTNULL, MANDATORY TRUE);
CREATE INDEX BinaryAttachment_id_idx ON BinaryAttachment (id) UNIQUE;
ALTER CLASS BinaryAttachment STRICTMODE TRUE;

CREATE PROPERTY Client.logo LINK BinaryAttachment;