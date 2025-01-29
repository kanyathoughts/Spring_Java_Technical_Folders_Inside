/**
 *Export statement is been used to give TypeName and Usages to dataPointController service.
 */
export enum TypeName {
    MODULE = 'Module',
    PAGEMODULE = 'PAGED_Module',
    PAGEANNOTATION = 'PAGED_Annotation',
    PAGEDATADICTIONARY = 'PAGED_DataDictionaryEntry',
    PAGESQLSTATEMENT = 'PAGED_SqlStatement',
    PAGEDNA = 'PAGED_ModuleInDNACluster',
    DEPENDENCYINFORMATION = 'PAGED_DependencyInformation',
    PAGEREACHABILITY = 'PAGED_ReachabilityData',
    SchedulerImport = 'PAGED_SchedulerImport',
    PAGEERRORMARKER ='PAGED_ErrorMarker'
}

export enum Usages {
    MODULETABLE =
     'miningUi.modulesTable',
    ANNOTATIONTABLE =
      'miningUi.annotationsTable',
    MININGUIGRAPHML =
      'miningUi.graphMlExport',
    DATADICTIONARYTABLE =
      'miningUi.dataDictionaryTable',
    CHARTDETAILSTABLE =
      'miningUi.chartDetailsModule',
    CHARTDETAILSSQL =
      'miningUi.chartDetailsSql',
    DNATABLE =
      'miningUi.modulesInDnaCluster',
    DEPENDENCYTABLE =
      'miningUi.dependenciesTable',
    REACHABILITYTABLE =
      'miningUi.reachabilityTable',
    MODULERRORTABLE =
      'miningUi.moduleErrorMarkerTable',
    SCHEDULERIMPORTTABLE =
      'miningUi.schedulerImportTable'
}
