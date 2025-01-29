CREATE PROPERTY DnaCommunity.clusterIndex IF NOT EXISTS INTEGER;
UPDATE DnaCommunity SET clusterIndex=clusterId.asInteger() + 1 WHERE clusterId.asInteger() >= 0 AND clusterIndex IS NULL;
UPDATE DnaCommunity SET clusterIndex=-1 WHERE clusterIndex IS NULL;
ALTER PROPERTY DnaCommunity.clusterIndex NOTNULL TRUE;
ALTER PROPERTY DnaCommunity.clusterId NOTNULL FALSE;
ALTER PROPERTY DnaCommunity.clusterId MANDATORY FALSE;
UPDATE DnaCommunity REMOVE clusterId;
DROP PROPERTY DnaCommunity.clusterId IF EXISTS FORCE;