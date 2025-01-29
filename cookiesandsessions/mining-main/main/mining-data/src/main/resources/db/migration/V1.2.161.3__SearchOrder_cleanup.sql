ALTER PROPERTY Project.searchOrders NOTNULL FALSE;
ALTER PROPERTY Project.searchOrders MANDATORY FALSE;
UPDATE Project REMOVE searchOrders;
DROP PROPERTY Project.searchOrders IF EXISTS FORCE;

CREATE PROPERTY Project.searchOrders IF NOT EXISTS EMBEDDEDLIST STRING (NOTNULL, DEFAULT []);
UPDATE Project SET searchOrders=searchOrdersTemp WHERE searchOrdersTemp IS NOT NULL;

UPDATE Project REMOVE searchOrdersTemp;
DROP PROPERTY Project.searchOrdersTemp IF EXISTS FORCE;
DROP CLASS SearchOrder IF EXISTS;
