import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ActivatedRoute, Router } from '@angular/router';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor, HttpService, I18nService } from '@app/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { SharedModule } from '@app/shared';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzTableModule } from 'ng-zorro-antd/table';
import { of, Subject, throwError } from 'rxjs';
import { NODE_CONFIG } from '../graph/utils/node-configurations';
import { ModuleOverViewTaxonomyComponent } from '@app/shared/components/module-overview-taxonomy/module-overview-taxonomy.component';
import { NzBreadCrumbComponent } from 'ng-zorro-antd/breadcrumb';
import { calculateModuleComplexity } from '@app/shared/components/shared-module-details/shared-module-details.util';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { DNAAnalysisService } from '@app/core/services/dna-analysis.service';
import { ModuleDetailsComponent } from './module-details.component';
import { WindowToken } from '@app/core/utils/window';
import { CancelRequestOnNavigationInterceptor } from '@app/core/http/cancel-request-on-navigation.interceptor';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { AnnotationControllerService, DataDictionaryControllerService, DataSchemaControllerService, DiscoveryControllerService, ErrorMarker, FeatureControllerService, IoControllerService, JobControllerService, JobInformation, MetamodelControllerService, ModuleControllerService, ModulePojo, ReferenceControllerService, TaxonomyAssignmentsGetResponse, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { SetSerializationInterceptor } from '@app/core/http/set-serialization.interceptor';
import { CfgSupportedTypeService } from '@app/core/services/cfg-supported-type.service';
import { CFG_SUPPORTED_TYPES } from '@app/core/services/cfg-supported-types.service.spec';
import { getBasePath } from '@app/core/utils/base-path.utils';

describe('ModuleDetailsComponent', () => {
  const moduleValue: ModulePojo = {
    uid: '#136:600',
    customProperties: {
      'Property1': [{
        name: 'customMetaInfo2',
        value: null,
        dataType: 'STRING'
      }, {
        name: 'customMetaInfo1',
        value: null,
        dataType: 'STRING'
      }]
    },
    id: 2007,
    name: 'CC1',
    projectId: 1,
    path: 'src/cobol/programs/CC1.cpy',
    technology: 'COBOL',
    type: 'COPYBOOK',
    storage: 'FILE',
    identification: 'IDENTIFIED',
    origin: 'CUSTOM',
    info: null,
    description: 'notAvailable',
    sourceMetrics: {
      codeLines: null,
      commentLines: null,
      complexityMcCabe: null,
    },
    content: null,
    sourceCodeAvailable: false
  };
  const dnaCardData = {
    clusterings: [{
      clusters: [{ moduleCount: 2, description: '' }],
      algorithm: { Clustering: 'Louvain', Sequencer: 'Cobol Methods', Similarity: 'Weighted Levenshtein' },
      options: [{ name: 'similarity threshold', title: 'Similarity Threshold', value: '0.85' }]
    }]
  } as any;
  const graphQl = {
    "data": {
      "dnaModulesInCluster": [
        {
          module: {
            id: 1,
            name: 'test',
            path: 'test Path'
          },
          clusterIndex: 1
        }
      ]
    }
  };
  const dnaAnalysis = {
    "filterData": {
      "filters": {
        "cluster": 0
      },
      "filterString": "COBOL Methods"
    },
    "chartTableConfig": {
      "columnMap": {
        "ModuleName": {
          "field": "moduleName",
          "header": "moduleName",
          "columnAction": {
            "type": "hyperlink"
          },
          "displayAs": "link"
        },
        "Path": {
          "field": "path",
          "header": "path"
        },
        "Cluster": {
          "field": "cluster",
          "header": "cluster"
        }
      },
      "paginator": true,
      "rows": 30,
      "pageChanger": true,
      "serverSidePagination": false,
      "showTotalCount": true,
      "loading": false
    },
    "requestQuery": {
      "query": "query { dnaModulesInCluster(projectId: 4, algorithm: \"COBOL Methods\", clusterIndex: 0) { module { id name path } clusterIndex } }"
    }
  };
  const dnaCardList = [{
    "chartData": [
      {
        "index": 0,
        "key": "Cluster 0: 34.78% (16 modules):0",
        "value": 16
      },
      {
        "index": 1,
        "key": "Cluster 1: 32.61% (15 modules):1",
        "value": 15
      },
      {
        "index": 2,
        "key": "Cluster 2: 28.26% (13 modules):2",
        "value": 13
      },
      {
        "index": -1,
        "key": "Cluster Unassigned: 4.35% (2 modules):-1",
        "value": 2
      }
    ],
    "options": [
      {
        "title": "Similarity",
        "value": "Weighted Levenshtein"
      },
      {
        "title": "Clustering",
        "value": "Louvain"
      },
      {
        "name": "maxLevels",
        "title": "Maximum Levels",
        "value": "5"
      },
      {
        "name": "maxIterations",
        "title": "Maximum Iterations",
        "value": "10"
      },
      {
        "name": "defaultTolerance",
        "title": "Default Tolerance",
        "value": "0.000100"
      },
      {
        "name": "minDNALength",
        "title": "Minimum DNA Length",
        "value": "5"
      },
      {
        "name": "similarity threshold",
        "title": "Similarity Threshold",
        "value": "0.85"
      }
    ],
    "title": "COBOL Methods",
    "clusterModuleCount": 46,
    "assignedModuleCount": 44,
    "clustersLength": 3,
    "chartFilterData": {
      "filterArgs": [
        "index"
      ]
    }
  }];
  const chartDataValue = [{
    "id": 18931,
    "moduleName": "'BABKREU'",
    "path": "src/cobol/programs/BABKCMP/'BABKREU'.cbl",
    "cluster": 1
  }];
  const errorResponse: ErrorMarker[] = [
    {
      "severity": "ERROR",
      "key": "UNDISCOVERED_DEPENDENCY",
      "cause": "Unable to resolve file MACEXT to actual data set."
    },
    {
      "severity": "WARNING",
      "key": "UNDISCOVERED_DEPENDENCY",
      "cause": "Unable to resolve file CURMTHND to actual data set."
    }
  ];
  moduleValue['projectId'] = 1;
  const jobInfoScheduled: JobInformation = { status: JobInformation.StatusEnum.SCHEDULED };
  const jobInfoCancel: JobInformation = { status: JobInformation.StatusEnum.CANCELED };
  const jobInfoSuccess: JobInformation = { status: JobInformation.StatusEnum.SUCCESS };
  const jobInfoFailure: JobInformation = { status: JobInformation.StatusEnum.FAILURE };
  const jobInfoUnknown: JobInformation = { status: JobInformation.StatusEnum.UNKNOWN };
  const updateParams = { Description: 'new description' };
  const currentClient: ClientProjectRelationship = new ClientProjectRelationship(1, 'test', 1, 'test');
  const references = [{ relationship: "CALLS" }, { relationship: "HAS_DATA_DICTIONARY_ENTRY" }, { relationship: "HAS_ANNOTATION" }]
  const virtualModuleValue = [{ "group": { "CONTAINING_MODULE_ID": 4609, "CONTAINING_MODULE_NAME": "ADD012A" }, "fields": { "NAME": ["ADD012A"], "ID": [4606] } }];
  const schemaInfo = [{ "recordId": "#1178:237", "customProperties": {}, "ordinal": 26, "name": "ONG_TMST", "reference": "", "properties": { "type": "datetime2" } }];
  let component: ModuleDetailsComponent;
  let fixture: ComponentFixture<ModuleDetailsComponent>;

  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
    ['updateModule', 'getAggregatedValues', 'getDataLineageAvailableForModule', 'findErrorMarkers', 'getAggregatedValues2', 'findIncludedModuleIds']);
  const deepLinkServiceSpy: jasmine.SpyObj<DeepLinkService> = jasmine.createSpyObj('DeepLinkService', ['showModuleInEclipse', 'featureIsActive', 'heartbeat']);
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', ['getJobInformation']);
  const relationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj(
    'ClientProjectRelationshipService',
    ['getClientProjectObservable']
  );
  const dnaAnalysisSpy: jasmine.SpyObj<DNAAnalysisService> = jasmine.createSpyObj('DNAAnalysisService',
    ['openChartDetailsTable', 'createChartData', 'formattedDnaTableData']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['destroy', 'getContentComponent']);
  const ioServiceSpy = jasmine.createSpyObj<IoControllerService>('IoControllerService', ['getExportFormats']);
  const referenceControllerServiceSpy: jasmine.SpyObj<ReferenceControllerService> = jasmine.createSpyObj(
    'ReferenceControllerService',
    ['findAllReferencesForModule']
  );
  const dataSchemaControllerServiceSpy: jasmine.SpyObj<DataSchemaControllerService> = jasmine.createSpyObj(
    'DataSchemaControllerService',
    ['findFieldInfos']
  );
  const taxonomyServiceSpy = jasmine.createSpyObj<TaxonomyControllerService>('TaxonomyControllerService', ['getAssignedTaxonomyByModule']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const discoveryControllerService: jasmine.SpyObj<DiscoveryControllerService> = jasmine.createSpyObj<DiscoveryControllerService>
    ('DiscoveryControllerService', ['modelDNAForLatestTimestamp', 'belongsToCluster']);
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService',
    ['graphQl']);
  const childComponent = jasmine.createSpyObj<ModuleOverViewTaxonomyComponent>('ModuleOverViewTaxonomyComponent', ['getTaxonomyData']);
  const customTablesParameterServiceSpy = jasmine.createSpyObj<CustomizableTableParametersService>('CustomizableTableColumnService', ['resetSavedSearchForModuleId'])
  const cfgSupportedTypeServiceSpy = jasmine.createSpyObj<CfgSupportedTypeService>('CfgSupportedTypeService', ['checkIfSupported']);
  const taxonomy: TaxonomyAssignmentsGetResponse = {
    "moduleCount": 3,
    "taxonomies": [
      { "state": "ALL" },
      { "state": "NONE" },
      { "state": "SOME" },
      { "state": "SOME" }
    ]
  };
  const exportFormats = [
    {
      "id": "datapoint-csv",
      "description": "CSV Data",
      "extensionType": "EXPORT_EXTENSION",
      "requiredRole": "VIEWER",
      "requiredNature": "MINING",
      "showOnExportPage": {
        "show": false,
        "category": "",
        "label": ""
      },
      "uploadDescription": {
        "name": "",
        "description": "",
        "required": false,
        "supported": false
      }
    },
    {
      "id": "discovery-effort-summary",
      "description": "Download Effort Summary Excel",
      "extensionType": "EXPORT_EXTENSION",
      "requiredRole": "VIEWER",
      "requiredNature": "DISCOVERY",
      "showOnExportPage": {
        "show": true,
        "category": "Discovery",
        "label": "Download Effort Summary Excel"
      },
      "uploadDescription": {
        "name": "",
        "description": "",
        "required": false,
        "supported": false
      }
    },
    {
      "id": "taxonomy-assignments",
      "description": "list of modules and respective taxonomy assignments",
      "extensionType": "EXPORT_EXTENSION",
      "requiredRole": "VIEWER",
      "requiredNature": "MINING",
      "showOnExportPage": {
        "show": false,
        "category": "",
        "label": "list of modules and respective taxonomy assignments"
      },
      "uploadDescription": {
        "name": "",
        "description": "",
        "required": false,
        "supported": false
      }
    },
    {
      "id": "datalineage-gml",
      "description": "Exports a Data Flow Graph in GML format (for yEd)",
      "extensionType": "JOB_EXTENSION",
      "requiredRole": "VIEWER",
      "requiredNature": "MINING",
      "showOnExportPage": {
        "show": false,
        "category": "",
        "label": "Exports a Data Flow Graph in GML format (for yEd)"
      },
      "uploadDescription": {
        "name": "",
        "description": "",
        "required": false,
        "supported": false
      }
    },
    {
      "id": "datalineage",
      "description": "Data Lineage Graph",
      "extensionType": "JOB_EXTENSION",
      "requiredRole": "VIEWER",
      "requiredNature": "MINING",
      "showOnExportPage": {
        "show": false,
        "category": "",
        "label": "Data Lineage Graph"
      },
      "uploadDescription": {
        "name": "",
        "description": "",
        "required": false,
        "supported": false
      }
    }
  ]
  const i18nServiceSpy = { language: 'en-US' };
  const clusterData = {
    "moduleId": 1,
    "clusters": [{
      "clusterIndex": 1,
      "algorithm": {
        "Sequencer": "COBOL Methods",
        similarity: "Weighted Levenshtein",
        clustering: "Louvain"
      }
    },
    {
      "clusterIndex": 2,
      "algorithm": {
        "Sequencer": "COBOL Skeleton",
        similarity: "Weighted Levenshtein",
        clustering: "Louvain"
      }
    }
    ]
  };
  let mockWindow: any;
  let openedUrl = '';

  beforeEach(waitForAsync(() => {
    mockWindow = {
      get location() {
          return {
              href: 'http://localhost:8081/#/browse-modules/1/1/1/explore',
              hash: '#/browse-modules/1/1/1/explore'
          };
      },
      open: (sUrl: any) => {
          openedUrl = sUrl;
      }
  };
    TestBed.configureTestingModule({
      declarations: [ModuleDetailsComponent, ModuleOverViewTaxonomyComponent],
      imports: [SharedModule,
        NzTableModule,
        FormsModule,
        TranslateModule.forRoot({}),
        HttpClientTestingModule,
        BrowserAnimationsModule
      ],
      providers: [
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: DiscoveryControllerService, useValue: discoveryControllerService },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: NzModalRef, useValue: nzModalRefSpy },
        { provide: DeepLinkService, useValue: deepLinkServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
        { provide: TaxonomyControllerService, useValue: taxonomyServiceSpy },
        { provide: DataSchemaControllerService, useValue: dataSchemaControllerServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        { provide: DNAAnalysisService, useValue: dnaAnalysisSpy },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: IoControllerService, usevalue: ioServiceSpy},
        { provide: WindowToken, useValue: mockWindow },
        { provide: CustomizableTableParametersService, useValues: customTablesParameterServiceSpy },
        { provide: CfgSupportedTypeService, useValue: cfgSupportedTypeServiceSpy },
        NzMessageService,
        NumberFormatter,
        SetSerializationInterceptor,
        NzBreadCrumbComponent,
        TranslateService,
        FeatureToggleService,
        FeatureControllerService,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        CancelRequestOnNavigationInterceptor,
        DataDictionaryControllerService,
        AnnotationControllerService,
        MetamodelControllerService,
        {
          provide: HttpClient,
          useClass: HttpService
        },
        {
          provide: ReferenceControllerService,
          useValue: referenceControllerServiceSpy
        },
        {
          provide: ClientProjectRelationshipService,
          useValue: relationshipServiceSpy
        },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              module: moduleValue
            })
          }
        }
      ]
    }).compileComponents();
    deepLinkServiceSpy.featureIsActive.and.returnValue(of(true));
    deepLinkServiceSpy.heartbeat.and.returnValue(Promise.resolve(true));
    moduleControllerServiceSpy.updateModule.and.returnValue(of(moduleValue as any));
    jobControllerServiceSpy.getJobInformation.and.returnValues(of(jobInfoSuccess as any),
      of(jobInfoFailure as any),
      of(jobInfoCancel as any),
      of(jobInfoUnknown as any),
      of(jobInfoScheduled as any),
      throwError(new Error('Get job status Error')));
    relationshipServiceSpy.getClientProjectObservable.and.returnValue(of(currentClient as any));
    referenceControllerServiceSpy.findAllReferencesForModule.and.returnValue(of(references as any));
    moduleControllerServiceSpy.getAggregatedValues2.and.returnValues(of(virtualModuleValue as any), of([] as any), throwError(new Error('Test Error')));
    taxonomyServiceSpy.getAssignedTaxonomyByModule.and.returnValue(of(taxonomy as any));
    dataSchemaControllerServiceSpy.findFieldInfos.and.returnValue(of(schemaInfo as any));
    labelMappingServiceSpy.mapLabel.and.returnValue('COBOL' as any);
    discoveryControllerService.modelDNAForLatestTimestamp.and.returnValue(of(dnaCardData as any));
    discoveryControllerService.belongsToCluster.and.returnValue(of(clusterData as any));
    dnaAnalysisSpy.openChartDetailsTable.and.returnValue(dnaAnalysis as any);
    dnaAnalysisSpy.createChartData.and.returnValue(dnaCardList as any);
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
    moduleControllerServiceSpy.getDataLineageAvailableForModule.and.returnValue(of(true as any));
    moduleControllerServiceSpy.findErrorMarkers.and.returnValue(of(errorResponse as any));
    moduleControllerServiceSpy.findIncludedModuleIds.and.returnValue(of([1, 2, 3, 4] as any));
    ioServiceSpy.getExportFormats.and.returnValue(of(exportFormats as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleDetailsComponent);
    component = fixture.componentInstance;
    component.selectedModule = moduleValue;
    component.moduleComplexity = calculateModuleComplexity(component.selectedModule);
    nzModalRefSpy.afterClose = new Subject;
    nzModalRefSpy.afterClose.next(null);
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    cfgSupportedTypeServiceSpy.supportedTypes = CFG_SUPPORTED_TYPES;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test ngOnInit', () => {
    component.ngOnInit();
    expect((component as any).dnaAnalysis.createChartData).toHaveBeenCalled();
  });

  it('should test handleControlFlowButton', () => {
    cfgSupportedTypeServiceSpy.checkIfSupported.and.returnValue(false)
    moduleValue.sourceCodeAvailable = false;
    component.selectedModule = moduleValue;
    component.handleControlFlowButton();
    expect(component.viewCFG).toBeFalse();
  });

  it('should update the badge counts fetch by graphql', () => {
    graphQlControllerServiceSpy.graphQl.and.returnValue(of({data:{modules:{content: [{annotationCount: 1, dependencyCount:3}]}}} as any));
    (component as any).getCountsBadges();
    expect(component.badgeObject.annotationCount).toBe(1);
    expect(component.badgeObject.dependencyCount).toBe(3);
    graphQlControllerServiceSpy.graphQl.and.returnValue(of({data:{modules:{content: [{dataDictionaryEntryCount: 1}, {dataDictionaryEntryCount :3}]}}} as any));
    (component as any).getCountsBadges();
    expect(component.badgeObject.dataDictionaryCount).toBe(4);
    expect(component.moduleIdForPreFiltering).toEqual([1, 2, 3, 4]);
  });

  it('should update the badge count for table columns', () => {
    expect(component.badgeObject.schemaFieldCount).toBe(1);
  });

  it('Should check for technology type', () => {
    component.selectedModule.type = ModulePojo.TypeEnum.TABLE;
    labelMappingServiceSpy.mapLabel.and.returnValue('TABLE' as any);
    (component as any).getSchemaTabHeader();
    expect(component.schemaTabHeader).toEqual("Table Columns");

    component.selectedModule.type = ModulePojo.TypeEnum.INDEX;
    labelMappingServiceSpy.mapLabel.and.returnValue('INDEX' as any);
    (component as any).getSchemaTabHeader();
    expect(component.schemaTabHeader).toEqual("Index");

    component.selectedModule.type = ModulePojo.TypeEnum.VIEW;
    labelMappingServiceSpy.mapLabel.and.returnValue('VIEW' as any);
    (component as any).getSchemaTabHeader();
    expect(component.schemaTabHeader).toEqual("View");
  });

  it('should open in eclipse', () => {
    component.openInEclipse();
    expect(deepLinkServiceSpy.showModuleInEclipse).toHaveBeenCalledWith(moduleValue);
  });

  it('should traverse dependencies', () => {
    component.projectId = 1;
    component.moduleId = 1;
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');
  });

  it('should edit value and update', () => {
    component.selectedModule.id = 1;
    component.selectedModule.description = 'Before Editing';
    component.editModuleDetails(new MouseEvent('mousedown'));
    expect(modalServiceSpy.create).toHaveBeenCalled();
    (component as any).updateModuleDesc(updateParams);
    expect(component.selectedModule.description).toBe('new description');
  });

  it('should get generic icon source for empty module', () => {
    component.selectedModule = null;
    expect(component.getIconSource()).toEqual(getBasePath() + NODE_CONFIG.GENERIC.imageUrl);
  });

  it('should view source code', () => {
    component.clientId = 1;
    component.projectId = 1;
    component.moduleId = 1;
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');


    const moduleCopy = Object.assign({}, moduleValue);
    moduleCopy.storage = ModulePojo.StorageEnum.FILE_SECTION;
    moduleCopy.parent = '2';
    component.selectedModule = moduleCopy;
  });

  it('should validate module', () => {
    
    cfgSupportedTypeServiceSpy.checkIfSupported.and.returnValue(false)
    component.selectedModule = {...moduleValue};
    component.handleControlFlowButton();
    expect(component.controlFlowMessage).toEqual('controlFlow.actuallyNotSupported');

    
    cfgSupportedTypeServiceSpy.checkIfSupported.and.returnValue(true)
    component.selectedModule = {...moduleValue, sourceCodeAvailable: false};
    component.handleControlFlowButton();
    expect(component.controlFlowMessage).toEqual('controlFlow.sourceNotAvailable');

  });

  it('should open control flow graph for module', () => {
    component.projectId = 1;
    component.moduleId = 1;
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');
  });

  it('should check if requiresReview is true', () => {
    component.selectedModule.requiresReview = true;
    expect(component.hasRequiresReviewFlag()).toBeTruthy;
  });

  it('should update requiresReview status', () => {
    component.selectedModule.requiresReview = true;
    (component as any).updateRequiresReviewStatus();
    expect(component.selectedModule.requiresReview).toBeFalsy;
  });

  it('should set codeViewer button tooltip text', () => {
    component.clientId = 1;
    component.projectId = 1;
    component.moduleId = 1;
    component.codeViewerTooltip = undefined;
    component.selectedModule.sourceCodeAvailable = true;
    expect(component.codeViewerTooltip).toBeUndefined();
  });

  it('should check the input property', () => {
    component.getTaxonomyListLength(3);
    expect(component.taxonomyListLength).toBe(3);
  });

  it('set showModal flag to true', () => {
    component.moduleOverViewTaxonomyComponent = childComponent;
    component.assignTaxonomies();
    expect(component.showModal).toBeTrue;
  });

  it('should check showModal flag', () => {
    component.getShowModalfromOverviewTaxonomy(false);
    expect(component.showModal).toBeFalse;
  });
});
