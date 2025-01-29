
CREATE INDEX Module_projectLinkAndUid_idx IF NOT EXISTS ON Module (projectLink, uid) NOTUNIQUE;