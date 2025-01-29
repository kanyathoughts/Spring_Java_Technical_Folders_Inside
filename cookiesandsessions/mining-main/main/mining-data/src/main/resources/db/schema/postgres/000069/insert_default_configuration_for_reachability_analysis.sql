INSERT INTO project_configuration (project, name, value)
SELECT uid, 'ReachabilityAnalysisConfig', '{
  "upperBoundModuleTypes": ["ECL_JOB", "JCL_JOB", "JAVA_TYPE", "SQL_STORED_PROCEDURE", "SERVICE"],
  "lowerBoundModuleTypes": ["SQL_TABLE", "RESOURCE_FILE", "RESOURCE_VSAM_FILE", "RESOURCE_GDG_FILE", "CICS_TSQ", "CICS_TDQ", "CICS_BMS_MAP", "IMS_DBD", "IMS_MFS", "RESOURCE_TPFDF_DATASET"]
}' FROM project;
