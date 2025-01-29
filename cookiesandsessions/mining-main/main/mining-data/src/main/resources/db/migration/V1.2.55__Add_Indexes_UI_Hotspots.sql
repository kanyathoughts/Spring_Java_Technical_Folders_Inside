CREATE INDEX Module_projectLinkAndobjectTypeLink_idx ON Module (projectLink, objectTypeLink) NOTUNIQUE;

CREATE INDEX ObjectType_typeLink_idx ON ObjectType (typeLink) NOTUNIQUE;
CREATE INDEX ObjectType_originLink_idx ON ObjectType (originLink) NOTUNIQUE;

