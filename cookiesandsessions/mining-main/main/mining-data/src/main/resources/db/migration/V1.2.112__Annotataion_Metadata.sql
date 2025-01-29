CREATE CLASS AnnotationMetaDataReasonEnum IF NOT EXISTS EXTENDS V;
CREATE PROPERTY AnnotationMetaDataReasonEnum.name IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
INSERT INTO AnnotationMetaDataReasonEnum set name="IF_ELSE_CONDITION";
INSERT INTO AnnotationMetaDataReasonEnum set name="MULTI_EXPRESSION_IF_ELSE_CONDITION";
INSERT INTO AnnotationMetaDataReasonEnum set name="LOOP_CONDITION";
INSERT INTO AnnotationMetaDataReasonEnum set name="COBOL_EVALUATE_CONDITION";
INSERT INTO AnnotationMetaDataReasonEnum set name="NATURAL_LOOP_FILE_ACCESS_CONDITION";
INSERT INTO AnnotationMetaDataReasonEnum set name="SELECT_CONDITION";
INSERT INTO AnnotationMetaDataReasonEnum set name="OTHER_CONDITION";
INSERT INTO AnnotationMetaDataReasonEnum set name="COMPUTATION";
INSERT INTO AnnotationMetaDataReasonEnum set name="INPUT_FROM_EXTERNAL_DATA_SOURCE";
INSERT INTO AnnotationMetaDataReasonEnum set name="OUTPUT_TO_EXTERNAL_DATA_SOURCE";
INSERT INTO AnnotationMetaDataReasonEnum set name="NESTED_CONDITION";
CREATE INDEX AnnotationMetaDataReasonEnum_name_idx IF NOT EXISTS ON AnnotationMetaDataReasonEnum (name) UNIQUE;

CREATE CLASS AnnotationMetaData IF NOT EXISTS EXTENDS V;
CREATE PROPERTY AnnotationMetaData.reason IF NOT EXISTS LINK AnnotationMetaDataReasonEnum (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY AnnotationMetaData.annotation IF NOT EXISTS LINK Annotation (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY AnnotationMetaData.projectLink IF NOT EXISTS LINK Project (NOTNULL, MANDATORY TRUE);

CREATE INDEX AnnotationMetaData_project_reason_idx IF NOT EXISTS ON AnnotationMetaData (projectLink, reason) NOTUNIQUE;

CREATE PROPERTY Annotation.metaData IF NOT EXISTS LINKLIST AnnotationMetaData (MANDATORY TRUE, DEFAULT "NULL");