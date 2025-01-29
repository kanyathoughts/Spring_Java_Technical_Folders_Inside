-- These indexes actually break UPSERT on ObjectType, since orient will then sometimes use the wrong index
DROP INDEX ObjectType_typeLink_idx IF EXISTS;
DROP INDEX ObjectType_originLink_idx IF EXISTS;