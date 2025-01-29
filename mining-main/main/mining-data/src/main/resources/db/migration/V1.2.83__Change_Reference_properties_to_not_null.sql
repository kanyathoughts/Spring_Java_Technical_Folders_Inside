UPDATE Reference SET properties={} WHERE properties IS NULL;
ALTER PROPERTY Reference.properties NOTNULL TRUE;
ALTER PROPERTY Reference.properties DEFAULT {};