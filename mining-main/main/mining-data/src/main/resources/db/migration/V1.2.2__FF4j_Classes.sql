--################################################################################################
-- Schema extension required by FF4j. 
--################################################################################################

CREATE CLASS ff4j_Feature;
CREATE PROPERTY ff4j_Feature.`FEAT_UID` STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ff4j_Feature.`ENABLE` INTEGER (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ff4j_Feature.`DESCRIPTION` STRING;
CREATE PROPERTY ff4j_Feature.`STRATEGY` STRING;
CREATE PROPERTY ff4j_Feature.`EXPRESSION` STRING;
CREATE PROPERTY ff4j_Feature.`GROUPNAME` STRING;
CREATE INDEX ff4j_feature_id_idx ON ff4j_Feature (`FEAT_UID`) UNIQUE;

CREATE CLASS ff4j_Role;
CREATE PROPERTY ff4j_Role.`FEAT_UID` STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ff4j_Role.`ROLE_NAME` STRING (NOTNULL, MANDATORY TRUE);
CREATE INDEX ff4j_role_id_idx ON ff4j_Role (`FEAT_UID`,`ROLE_NAME`) UNIQUE;

CREATE CLASS ff4j_FeatureProperty;
CREATE PROPERTY ff4j_FeatureProperty.`PROPERTY_ID` STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ff4j_FeatureProperty.`CLAZZ` STRING;
CREATE PROPERTY ff4j_FeatureProperty.`CURRENTVALUE` STRING;
CREATE PROPERTY ff4j_FeatureProperty.`FIXEDVALUES` STRING;
CREATE PROPERTY ff4j_FeatureProperty.`DESCRIPTION` STRING;
CREATE PROPERTY ff4j_FeatureProperty.`FEAT_UID` STRING (NOTNULL, MANDATORY TRUE);
CREATE INDEX ff4j_feature_properties_id_idx ON ff4j_FeatureProperty (`PROPERTY_ID`,`FEAT_UID`) UNIQUE;

CREATE CLASS ff4j_Property;
CREATE PROPERTY ff4j_Property.`PROPERTY_ID` STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ff4j_Property.`CLAZZ` STRING;
CREATE PROPERTY ff4j_Property.`CURRENTVALUE` STRING;
CREATE PROPERTY ff4j_Property.`FIXEDVALUES` STRING;
CREATE PROPERTY ff4j_Property.`DESCRIPTION` STRING;
CREATE INDEX ff4j_properties_id_idx ON ff4j_Property (`PROPERTY_ID`) UNIQUE;