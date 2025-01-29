import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { MiningTableConfig, FilterType, FieldTypeEnum, Column, MiningTableRow } from '@app/shared/components/mining-table/mining-table-config.interface';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ActivatedRoute } from '@angular/router';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { StateKey } from '@app/shared/interfaces/state-maintainance.interface';
import { GraphQlControllerService } from '@app/core/services/graphql.service'
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { NzMessageService } from 'ng-zorro-antd/message';
import { SharedModule } from '@app/shared/shared.module';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor } from '@app/core';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { AllowedTableActions, LinkType, StatesType } from '../mining-table/mining-table-action.interface';
import { HttpClient, HttpXsrfTokenExtractor } from '@angular/common/http';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { CustomizableTableCoreComponent } from './customizable-table-core.component';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { DataPointControllerService, FeatureControllerService, JobControllerService, MiningDataPointDefinitionWithPath, SavedSearchControllerService } from '@innowake/mining-api-angular-client';
import { Operators } from '../type-based-filter/type-based-filter.component';
import { TableFilter } from '@app/shared/interfaces/type-based-filter.interface';

describe('CustomizableTableCoreComponent', () => {
  let component: CustomizableTableCoreComponent;
  let fixture: ComponentFixture<CustomizableTableCoreComponent>;
  const i18nServiceSpy = { language: 'en-US' };
  const checkedDataPoint: MiningDataPointDefinitionWithPath = {
    "name": "name",
    "parentTypeName": "Module",
    "referenceTypeName": null,
    "path": "content.name",
    "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
    "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
    "parameters": [],
    "usageAttributes": {
      "general.viewMode": {
        "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
        "displayAs": "link",
        "togetherWith": "linkHash"
      },
      "miningUi.modulesTable": {
        "sortByFieldName": "name.toLowerCase()",
        "rsqlFragment": "name=='${$query}*'",
        "category": "Base Data",
        "defaultColumnIndex": "0"
      },
      "general.searchFilter": {
        "filterMode": "text"
      }
    },
    "displayName": "Module Name",
    "description": "The name of the Module",
    "array": false,
    "id": "Module.name",
    "nullable": true,
    "aliasFor": null,
    "alias": false
  };
  const miningTableConfig: MiningTableConfig = {
    columnMap: {
      language: {
        field: 'target',
        header: 'Target',
        filterProperties: { filterType: FilterType.freeText }
      },
      ModuleName: {
        field: 'name',
        header: 'moduleName',
        hasWarning: (item: any) => true,
        warningMessage: 'This module was modified by the last scan. Please review its meta data (annotations, data dictionary, module description, taxonomies).',
        filterProperties: { filterType: FilterType.freeText },
      },
      type: {
        field: 'type',
        header: 'Type',
        filterProperties: { filterType: FilterType.multiSelect, listOfFilter: [] }
      },
      technology: {
        field: 'technology',
        header: 'Type',
        filterProperties: { filterType: FilterType.multiSelect, listOfFilter: [] }
      },
      lastScan: {
        field: 'metricsDate',
        header: 'lastScan',
        options: [
          {
            value: 'lastScan',
            icon: 'info-circle',
            title: 'Last time this module was updated via code scan.',
            disableItem: () => false
          }
        ]
      }
    },
    bulkSelectionDataPoints: [
      {
        "name": "name",
        "parentTypeName": "Module",
        "referenceTypeName": null,
        "path": "content.name",
        "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
        "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
        "parameters": [],
        "usageAttributes": {
          "general.viewMode": {
            "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
            "displayAs": "link",
            "togetherWith": "linkHash"
          },
          "miningUi.modulesTable": {
            "sortByFieldName": "name.toLowerCase()",
            "rsqlFragment": "name=='${$query}*'",
            "category": "Base Data",
            "defaultColumnIndex": "0"
          },
          "general.searchFilter": {
            "filterMode": "text"
          }
        },
        "displayName": "Module Name",
        "description": "The name of the Module",
        "array": false,
        "id": "Module.name",
        "nullable": true,
        "aliasFor": null,
        "alias": false
      }
    ],
    paginator: true,
    rows: 10,
    serverSidePagination: true,
    loading: true
  };

  const selectedDataPoints: MiningDataPointDefinitionWithPath[] = [
    {
      "name": "name",
      "parentTypeName": "Module",
      "referenceTypeName": null,
      "path": "content.name",
      "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "parameters": [],
      "usageAttributes": {
        "general.viewMode": {
          "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
          "displayAs": "link",
          "togetherWith": "linkHash"
        },
        "miningUi.modulesTable": {
          "sortByFieldName": "name.toLowerCase()",
          "rsqlFragment": "name=='${$query}*'",
          "category": "Base Data",
          "defaultColumnIndex": "0"
        },
        "general.searchFilter": {
          "filterMode": "text"
        }
      },
      "displayName": "Module Name",
      "description": "The name of the Module",
      "array": false,
      "id": "Module.name",
      "nullable": true,
      "aliasFor": null,
      "alias": false
    },
    {
      "name": "name",
      "parentTypeName": "Annotation",
      "referenceTypeName": null,
      "path": "content.module.id",
      "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "parameters": [],
      "usageAttributes": {
        "general.viewMode": {
          "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
          "displayAs": "link",
          "togetherWith": "linkHash"
        },
        "miningUi.annotationsTable": {
          "sortByFieldName": "name.toLowerCase()",
          "rsqlFragment": "name=='${$query}*'",
          "category": "Base Data",
          "defaultColumnIndex": "0"
        },
        "general.searchFilter": {
          "filterMode": "text"
        }
      },
      "displayName": "Annotation Name",
      "description": "The name of the Annotation",
      "array": false,
      "id": "Module.name",
      "nullable": true,
      "aliasFor": null,
      "alias": false
    },
    {
      "name": "technologyLink",
      "parentTypeName": "ObjectType",
      "referenceTypeName": "Technology",
      "path": "content.objectTypeLink.technologyLink",
      "providedBy": new Set(["innowake.mining.data.model.springdata.ObjectTypeV2"]),
      "scalarType": null,
      "parameters": [],
      "usageAttributes": {
        "general.viewMode": {
          "labelMapping": "TECHNOLOGY_LABELS"
        },
        "miningUi.modulesTable": {
          "rsqlFragment": "objectTypeLink.technologyLink.name=in=($'{$query})",
          "multiSelectValueRetrievalFieldName": "TECHNOLOGY",
          "sortByFieldName": "objectTypeLink.technologyLink.name",
          "multiSelectValueRetrievalMode": "moduleControllerDistinctFieldValues",
          "category": "Base Data",
          "defaultColumnIndex": "1"
        },
        "general.searchFilter": {
          "filterMode": "multiSelect"
        }
      },
      "displayName": "Technology",
      "description": "Technology of the Module",
      "array": false,
      "id": "ObjectType.technologyLink",
      "nullable": true,
      "aliasFor": null,
      "alias": false
    }
  ];

  const dataPoints: MiningDataPointDefinitionWithPath[] = [
    {
      "name": "modifiedDate",
      "parentTypeName": "Module",
      "referenceTypeName": null,
      "path": "content.id",
      "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.DateTime,
      "parameters": [],
      "usageAttributes": {
        "general.viewMode": {
          "displayAs": "date"
        },
        "miningUi.modulesTable": {
          "sortByFieldName": "modifiedDate",
          "category": "Modifications"
        },
      },
      "displayName": "Last modified",
      "description": "The date when this Module was last modified (either through code scanning or manually)",
      "array": false,
      "id": "Module.modifiedDate",
      "nullable": true,
      "aliasFor": null,
      "alias": false
    },
    {
      "name": "name",
      "parentTypeName": "Module",
      "referenceTypeName": null,
      "path": "content.name",
      "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "parameters": [],
      "usageAttributes": {
        "general.viewMode": {
          "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
          "displayAs": "link",
          "togetherWith": "linkHash"
        },
        "miningUi.modulesTable": {
          "sortByFieldName": "name.toLowerCase()",
          "rsqlFragment": "name=='${$query}*'",
          "category": "Base Data",
          "defaultColumnIndex": "0"
        },
        "general.searchFilter": {
          "filterMode": "text"
        }
      },
      "displayName": "Module Name",
      "description": "The name of the Module",
      "array": false,
      "id": "Module.name",
      "nullable": true,
      "aliasFor": null,
      "alias": false
    }
  ];

  const updatedDataPoints: { [key: string]: Column; } = {
    "Module Name": {
      "header": "Module Name",
      "field": "name",
      "fieldType": FieldTypeEnum.STRING,
      "filterProperties": {
        "filterType": FilterType.freeText,
        "isFilterActive": false
      },
      "displayAs": "link",
      "sortFn": true,
      "warningMessage": "This module was modified by the last scan. Please review its meta data (annotations, data dictionary, module description, taxonomies).",
      "sortOrder": "ascend"
    },
    "Technology": {
      "header": "Technology",
      "field": "objectTypeLink.technologyLink",
      "fieldType": null,
      "filterProperties": {
        "filterType": FilterType.multiSelect,
        "isFilterActive": false,
        "listOfFilter": [],
        "loadingValues": true
      },
      "sortFn": true
    }
  };
  const graphQl = {
    "data": {
      "modules": {
        "content": [
          {
            "name": "&PARAM1",
            "linkHash": "1EF4567F3E4F4F77D1F29261F57CAD630DA49332522267E913B8CEB3C71DCE4D",
            "objectTypeLink": {
              "technologyLink": "RESOURCE",
              "typeLink": "FILE",
              "storageLink": "FILE"
            },
            "metricsDate": "2022-09-02 10:53",
            "id": 554,
            "requiresReview": false,
            "inCodebase": false,
            "identificationLink": "MISSING",
          },
          {
            "name": "CICS-TC",
            "linkHash": "DC259F8EB2CEDBBE71FC36C22D90E7EFCCEEEF50DA5CF324D0AD91124B955393",
            "objectTypeLink": {
              "technologyLink": "CSD",
              "typeLink": "TRANSACTION",
              "storageLink": "FILE_SECTION"
            },
            "metricsDate": "2022-09-02 10:53",
            "id": 556,
            "requiresReview": false,
            "inCodebase": false,
            "identificationLink": "MISSING",
          }
        ],
        "totalElements": 143,
        "size": 30
      }
    }
  }

  const savedSearches = [
    {
      "id": 73,
      "name": "CICS modules",
      "usage": "miningUi.modulesTable",
      "projectId": 1,
      "savedSearch": "page=1&sort=name.toLowerCase();ASC&filter=[{\"key\":\"objectTypeLink.technologyLink\",\"value\":[\"CICS\"]}]&columns=Module.metricsDate&columns=Module.name&columns=ObjectType.technologyLink&columns=ObjectType.typeLink",
      "createdByUserId": "admin",
      "scope": "PROJECT",
      "customProperties": {},
      "createdByUserName": "admin"
    },
    {
      "id": 3,
      "name": "Not Referenced",
      "usage": "miningUi.modulesTable",
      "projectId": 0,
      "savedSearch": "columns=Module.name&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Module.inboundDependencyCount&page=1&sort=name;ASC&filter=[{\"key\":\"inboundDependencyCount\",\"value\":[{\"operator\":\"eq\",\"value\":0}]}]",
      "createdByUserId": "",
      "scope": "GLOBAL",
      "customProperties": {},
      "createdByUserName": ""
    }
  ];
  const values: any[] = [
    {
      id: 1,
      moduleName: 'PRG1',
      annotationType: 'RULE',
      categoryName: 'Annotation Category A',
      annotationState: 'CANDIDATE',
      children: [
        {
          id: '4_IN_ANALYSIS',
          moduleName: 'PRG2',
          annotationType: 'RULE',
          categoryName: 'Annotation Category D',
          annotationState: 'REVIEW',
          name: 'IN_ANALYSIS',
          children: []
        },
      ],
    },
    {
      id: 9,
      moduleName: 'EXECSQL',
      annotationType: 'DATABASE',
      categoryName: 'Annotation Category B',
      annotationState: 'CANDIDATE',
    },
    {
      id: 3,
      moduleName: 'MMRS',
      annotationType: 'RULE',
      categoryName: 'Annotation Category A',
      annotationState: 'CANDIDATE',
    },
  ];
  const selectedColumns: NzTreeNodeOptions[] = [
    { 'title': 'Module Name', 'name': 'name', 'key': 'name', 'checked': true, 'disableCheckbox': true, 'path': 'content.name' },
    { 'title': 'Metrics Date', 'name': 'metricsDate', 'key': 'metricsDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.metricsDate' },
    { 'title': 'Technology', 'name': 'technology', 'key': 'technology', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.technologyLink' },
    { 'title': 'Type', 'name': 'type', 'key': 'type', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.typeLink' },
    { 'title': 'Modified Date', 'name': 'modifiedDate', 'key': 'modifiedDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.modifiedDate' }
  ];
  const updatedColumns: { [key: string]: Column; } = {
    "Module Name": {
      "field": "inHasAnnotation.out.name",
      "header": "moduleName",
      "warningMessage": "This module was modified by the last scan. Please review its meta data (annotations, data dictionary, module description, taxonomies).",
      "fieldType": FieldTypeEnum.STRING
    },
    "Annotation Type": {
      "field": "typeLink",
      "header": "Annotation Type",
    },
    "Category": {
      "field": "categoryLink.name",
      "header": "Category",
      "fieldType": FieldTypeEnum.STRING
    },
    "Source Code": {
      "field": "inHasAnnotation.out.sourceAttachmentLink.content",
      "header": "Source Code",
      "fieldType": FieldTypeEnum.STRING
    },
    "State": {
      "field": "stateLink",
      "header": "State",
    },
    "Annotation Description": {
      "field": "name",
      "header": "Annotation Description",
      "fieldType": FieldTypeEnum.STRING
    },
    "Modified By": {
      "field": "updatedByUserId",
      "header": "Modified By",
      "fieldType": FieldTypeEnum.STRING
    }
  };
  const sampleData = {
    childSample: {
      id: 1,
    },
  };

  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
    ['getSelectedDataPoints', 'updateTableConfig', 'setDataPointList', 'setColumnIdsSelection', 'checkSelectedColumns', 'resetTableColumnAndDataPoints', 'getTableConfig', 'getDataPoints', 'getDefaultSortBy', 'getCheckedDataPoint']);
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', ['getJobInformation']);
  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['refresh']);
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
    'GraphQlControllerService',
    ['graphQl']
  );
  const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj(
    'DataPointControllerService',
    ['getDataPointsForType']
  );
  const savedSearchControllerServiceSpy: jasmine.SpyObj<SavedSearchControllerService> = jasmine.createSpyObj<SavedSearchControllerService>
    ('SavedSearchControllerService', ['findByUsage']);
  const IAMServiceSpy: jasmine.SpyObj<IdentityAccessManagementService> = jasmine.createSpyObj('IAMServiceSpy', ['getUsername', 'getUserId']);
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'skipErrorHandler', 'get']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [CustomizableTableCoreComponent],
      imports: [
        FormsModule,
        SharedModule,
        BrowserAnimationsModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({}),
        RouterTestingModule.withRoutes([])],
      providers: [
        NzMessageService,
        TranslateService,
        FeatureControllerService,
        NumberFormatter,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        FeatureToggleService,
        HttpXsrfTokenExtractor,
        JobControllerService,
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: NzModalRef, useValue: { destroy: () => true, close: () => true } },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: DataPointControllerService, useValue: dataPointControllerServiceSpy },
        { provide: SavedSearchControllerService, useValue: savedSearchControllerServiceSpy },
        { provide: IdentityAccessManagementService, useValue: IAMServiceSpy },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              project: { id: 1 },
              module: { id : 1}
            }),
            snapshot: {
              queryParams: of({
                filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
                page: 1,
                sort: 'name;ASC',
                savedSearch: 'Business Rule Candidates'
              })
            }
          }
        },
      ]
    }).compileComponents();
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedDataPoints));
    userCustomizableTableServiceSpy.updateTableConfig.and.returnValue(updatedDataPoints);
    userCustomizableTableServiceSpy.getCheckedDataPoint.and.returnValue([checkedDataPoint]);
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
    savedSearchControllerServiceSpy.findByUsage.and.returnValue(of(savedSearches as any));
    httpServiceSpy.disableApiPrefix.and.returnValue(httpServiceSpy);
    IAMServiceSpy.getUsername.and.returnValue('admin');
    jobControllerServiceSpy.getJobInformation.and.returnValue(of({ jobId: 'someJobId', status: 'SUCCESS' }) as any);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CustomizableTableCoreComponent);
    component = fixture.componentInstance;
    component.tableConfig = miningTableConfig;
    component.value = values;
    component.moduleIdForPreFiltering = [1];
    component.pageType = TypeName.PAGEMODULE;
    component.projectId = 1;
    component.switchColumns = { id: 'testId', column: 'testColumn'};
    component.additionalGraphQLPreFilter = {
      content_id: {
        in: [1, 2, 3]
      }
    };
    component.dataPointsToHide = ['Module Name'];
    component.usage = Usages.MODULETABLE;
    component.graphQlType = "modules";
    component.dataChangeEvent = of({
      action: AllowedTableActions.TOOLTIP,
      data: {}
    });
    component.refreshCoreTable = true;
    component.internalDataPoints = [{ name: 'id', path: 'content.id' },
    { name: 'name', path: 'content.inHasAnnotation.out.name' },
    { name: 'id', path: 'content.inHasAnnotation.out.id' },
    { name: 'offset', path: 'content.inHasAnnotation.fromModuleLocation.offset' }];
    component.rowActions = [[{label: 'string',
      value: {},
      styleClass: 'string',
      icon: 'string',
      title: 'string'}], [{label: 'string',
      value: {},
      styleClass: 'string',
      icon: 'string',
      title: 'string'}]];
    localStorage.setItem(
      StateKey.BrowseModuleTableStateKey,
      '{"selection": {}, "filters":{"target": {}, "type":{} } }'
    );
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    component.ngOnInit();
    expect(component).toBeTruthy();
  });

  it('should fetch data for table config', () => {
    spyOn((component as any), 'updateTableConfig').and.callThrough();
    (component as any).fetchTableDataAsPerDataPoints();
    (component as any).updateTableConfig(dataPoints);
    expect((component as any).updateTableConfig).toHaveBeenCalled();
  });

  it('should emit the optionSelected event with correct values', () => {
    const selectedOptionValue = 'your-option-value';
    const selectedRowData: MiningTableRow = { data: 'your-row-data' };
    const expectedEvent = { optionValue: selectedOptionValue, data: selectedRowData?.data };
    spyOn(component.optionSelected, 'emit');
    component.handleShowBusinessVariablesReferenced(selectedOptionValue, selectedRowData);
    expect(component.optionSelected.emit).toHaveBeenCalledWith(expectedEvent);
  });

  it('should cancel the data loading', () => {
    component.cancelDataLoading();
    expect(component.tableConfig.loading).toBe(false);
  });

  it('should check if internal data points are fetched for table', () => {
    component.internalDataPoints = [{
      "name": "id",
      "parentTypeName": "Module",
      "referenceTypeName": null,
      "path": "content.id",
      "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.Long,
      "parameters": [],
      "usages": new Set([
        "miningUi.modulesTable"
      ]),
      "usageAttributes": {
        "miningUi.modulesTable": {
          "category": "Base Data"
        }
      },
      "displayName": "Module Id",
      "description": "The unique id of the Module",
      "array": false,
      "id": "Module.id",
      "nullable": true,
      "aliasFor": null,
      "alias": false
    }];
    const modifiedDataPointslength = (component as any).modifiedDataPoints.length;
    expect(modifiedDataPointslength).toBe(7);
    component.fetchTableDataAsPerDataPoints();
    const internalDataPoint = (component as any).modifiedDataPoints.find((dataPoint: any) => dataPoint.path === component.internalDataPoints[0].path);
    expect(internalDataPoint).toEqual(component.internalDataPoints[0]);
  });

  it('should check if adjusted data dictionary order is fetched for table', () => {
    userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of([
      {name: 'name', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "0"}}},
      {name: 'description', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "1"}}},
      {name: 'translatedFieldValue', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "2"}}},
      {name: 'fieldType', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "3"}}},
      {name: 'format', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "4"}}},
      {name: 'length', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.Long, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "5"}}},
      {name: 'parentGroup', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "6"}}},
      {name: 'fieldLevel', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.Long, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "7"}}},
      {name: 'name', parentTypeName: 'Module', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.moduleTable': {defaultColumnIndex: "8"}}},
      {name: 'createdByUserName', parentTypeName: 'DataDictionaryEntry', scalarType: MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, referenceTypeName: null, aliasFor: null, path: '',
      usageAttributes: {'miningUi.dataDictionaryTable': {defaultColumnIndex: "9"}}},
    ]));
  
  component.fetchTableDataAsPerDataPoints();
  const modifiedDataPointslength = (component as any).modifiedDataPoints.length;
  expect(modifiedDataPointslength).toBe(14);

  const expectedOrder = ['name', 'description', 'translatedFieldValue', 'fieldType', 'format', 'length', 'parentGroup', 'fieldLevel', 'name', 'createdByUserName'];
  expectedOrder.forEach((columnName, index) => {
    expect(component.modifiedDataPoints[index].name).toBe(columnName);
  });
});

  it('Should resolveFieldWarning', () => {
    component.value = [{ value: 1 }, { value: 5 }, { value: 10 }];
    const column: Column = {
      header: 'test', field: 'value', filterProperties: { filterType: FilterType.freeText, filterValue: 'Test value' }, hasWarning: () => false
    };
    const result = component.resolveFieldWarning(component.value, column);
    expect(result).toEqual(false);
  });

  it('Should change the table data', () => {
    component.value = [
      { id: 1, name: 'test 1' },
      { id: 2, name: 'test 2' },
    ];
    component.tooltip = [];

    (component as any).changeTableData({ action: 'delete', id: 1 });
    expect(component.value.length).toEqual(1);

    (component as any).changeTableData({ action: 'update', id: 2, data: { id: 2, name: 'test 2 updated' } });
    expect(component.value[0].name).toEqual('test 2 updated');

    (component as any).changeTableData({ action: 'tooltip', data: ['new tooltip'] });
    expect(component.tooltip).toEqual(['new tooltip']);
  });

  it('Should change the table data test child rows', () => {
    component.childValues = [{
      id: 1,
      moduleName: 'PRG1',
      annotationType: 'RULE',
      categoryName: 'Annotation Category A',
      annotationState: 'CANDIDATE',
      name: 'MMRS_004',
      expand: true,

      children: [
        {
          id: '4_IN_ANALYSIS',
          moduleName: 'PRG2',
          annotationType: 'RULE',
          categoryName: 'Annotation Category D',
          annotationState: 'REVIEW',
          name: 'IN_ANALYSIS',
          expand: true,
          children: []
        },
      ],
    },
    {
      id: 9,
      moduleName: 'EXECSQL',
      annotationType: 'DATABASE',
      categoryName: 'Annotation Category B',
      annotationState: 'CANDIDATE',
    },
    {
      id: 3,
      moduleName: 'MMRS',
      annotationType: 'RULE',
      categoryName: 'Annotation Category A',
      annotationState: 'CANDIDATE',
    }];
    component.expandedRows = [1];
    component.value = [
      { id: 1, name: 'MMRS', expand: true, children: [] },
      { id: 2, name: 'MMRS__004', expand: true, children: [] },
    ];
    (component as any).changeTableData({ action: 'addChild', data: { id: 'MMRS', children: true } });
    (component as any).changeTableData({ action: 'toggleActions', data: { id: 'MMRS', children: true } });
    (component as any).changeTableData({ action: 'restrictEditing', data: { id: 'MMRS', children: true } });
    if (!component.expandedRows.includes(3)) {
      component.expandedRows.push(3)
    }
    expect(component.expandedRows.length).toBe(2);
  });


  it('Should collapse tree table rows', () => {
    const data = { children: [{ id: 3, value: 'four' }] };
    const dataArray = [
      { id: 1, value: 'one' },
      { id: 2, value: 'two', expand: true },
      { id: 3, value: 'three', expand: true },
    ];
    component.collapse(dataArray, {}, false);
    expect(dataArray[1].expand).toEqual(true);

    component.collapse(dataArray, data, false);
    expect(dataArray[2].expand).toEqual(false);

    const value: MiningTableRow = { children: [] };
    spyOn((component as any).optionSelected, 'emit');
    component.collapse(dataArray, value, true);
    expect((component as any).optionSelected.emit).toHaveBeenCalled();
  });

  it('should calculate row indent', () => {
    const rowItemLevelZero = {
      level: 0,
    };
    const levelZeroIndent = component.calculateIndent(rowItemLevelZero);
    expect(levelZeroIndent).toEqual(0);

    const rowItemLevelOne = {
      level: 0,
    };
    const levelOneIndent = component.calculateIndent(rowItemLevelOne);
    expect(levelOneIndent).toEqual(0);

    const rowItemLevelZeroDefaultIndent = {
      level: 0,
      nonExpandableRowIndent: 1,
    };
    const levelZeroDefaultIndent = component.calculateIndent(rowItemLevelZeroDefaultIndent);
    expect(levelZeroDefaultIndent).toEqual(1);

    const rowItemLevelOneDefaultIndent = {
      level: 1,
      nonExpandableRowIndent: 1,
    };
    const levelOneDefaultIndent = component.calculateIndent(rowItemLevelOneDefaultIndent);
    expect(levelOneDefaultIndent).toEqual(25);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
    expect(component.resolveFieldData(sampleData, 'childSample.id')).toBe(1);
  });

  it('should check the refreshCheckedStatus', () => {
    component.totalRecords=1;
    component.setOfSelectedId = new Set([1])
    component.refreshCheckedStatus();
    expect(component.selectedState).toBe(StatesType.ALL);
    component.totalRecords = 2;
    component.refreshCheckedStatus();
    expect(component.selectedState).toBe(StatesType.SOME);
  });

  it('should check the setAndUpdatePageIndexRecord', () => {
    spyOn((component as any).selectedRows, 'emit');
    component.selectedState = StatesType.ALL;
    component.selectDeselectAll();
    expect((component as any).selectedRows.emit).toHaveBeenCalled();
  });

  it('should check the setAndUpdatePageIndexRecord', () => {
    spyOn((component as any).selectedRows, 'emit');
    component['modifiedDataPoints'] = dataPoints;
    component.selectedState = StatesType.NONE;
    component.selectDeselectAll();
    expect((component as any).selectedRows.emit).toHaveBeenCalled();
  });

  it('should close the current loading spinner', () => {
    spyOn((component as any).selectedRows, 'emit');
    spyOn(component, 'updateCheckedSet');
    component.onItemChecked(2032, true);
    expect((component as any).selectedRows.emit).toHaveBeenCalled();

    component.onItemChecked(2032, true, {exampleField: 123, id: 2034});
    expect((component as any).selectedRows.emit).toHaveBeenCalled();

  });

  it('should test sort handling', () => {
    spyOn(component, 'serverSidePaginationCall');
    const sort = [{key: 'content_name', value: 'DESC'}];
    const filter = [{key: 'content_name', value: 'test'}];
    component.disableSortHandling = true;
    component.handleSortChange({sort, pageIndex: 1, pageSize: 30, filter});
    expect(component.serverSidePaginationCall).not.toHaveBeenCalled();
    component.disableSortHandling = false;
    component.handleSortChange({sort, pageIndex: 1, pageSize: 30, filter});
    expect(component.serverSidePaginationCall).toHaveBeenCalled();
  });

  it('should show error message if graphQl request is empty', () => {
    const data = {};
    component.errorMesgId = null;
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(data as any));
    component.usage = Usages.ANNOTATIONTABLE;
    component.serverSidePaginationCall();
    expect(component.errorMesgId).not.toBeNull();

    component.usage = Usages.DATADICTIONARYTABLE;
    component.serverSidePaginationCall();
    expect(component.errorMesgId).not.toBeNull();

    component.usage = Usages.DNATABLE;
    component.serverSidePaginationCall();
    expect(component.errorMesgId).not.toBeNull();
  });

  it('should return "right" if column is of type number and item is not an array', () => {
    const column: Column = {
      header: 'Example Header',
      fieldType: FieldTypeEnum.NUMBER,
      field: 'exampleField'
    };
    const item: MiningTableRow = {
      exampleField: 123
    };
    const alignment = component.getAlign(column, item);
    expect(alignment).toEqual('right');
  });

  it('should return "left" if column is not of type number or item is an array', () => {
    const column: Column = {
      header: 'Example Header',
      fieldType: FieldTypeEnum.STRING,
      field: 'exampleField'
    };
    const item: MiningTableRow = {
      exampleField: ['value1', 'value2']
    };
    const alignment = component.getAlign(column, item);
    expect(alignment).toEqual('left');
  });

  it('should return "left" if column is not provided', () => {
    const item: MiningTableRow = {
      exampleField: 'value'
    };
    const alignment = component.getAlign(undefined, item);
    expect(alignment).toEqual('left');
  });

  it('should return "left" if item is not provided', () => {
    const column: Column = {
      header: 'Example Header',
      fieldType: FieldTypeEnum.STRING,
      field: 'exampleField'
    };
    const alignment = component.getAlign(column, undefined);
    expect(alignment).toEqual('left');
  });

  it('should return correct columns for DEPENDENCYTABLE usage', () => {
    const result = (component as any).getPersistentColumnsBasedOnusage('DEPENDENCYTABLE');
    expect(result).toEqual(['Module.name']);
  });

  it('should return correct columns for DATADICTIONARYTABLE usage', () => {
    const result = (component as any).getPersistentColumnsBasedOnusage(Usages.DATADICTIONARYTABLE);
    expect(result).toEqual(['DataDictionaryEntry.name']);
  });

  it('should return correct columns for REACHABILITYTABLE usage with switchColumns', () => {
    component.switchColumns = { id: 'testId', column: 'testColumn' };
    const result = (component as any).getPersistentColumnsBasedOnusage('REACHABILITYTABLE');
    expect(result).toEqual(['Module.name']);
  });

  it('should return default columns for unknown usage', () => {
    const result = (component as any).getPersistentColumnsBasedOnusage('UNKNOWN');
    expect(result).toEqual(['Module.name']);
  });

  it('should test onPageChange', () => {
    spyOn(component, 'serverSidePaginationCall');
    component.onPageChange(2);
    expect(component.serverSidePaginationCall).toHaveBeenCalled();
  })

  it('should test isActionAvailable', () => {
    const actionAvailable = component.isActionAvailable(['add', 'delete'], 'delete');
    expect(actionAvailable).toBeTrue();
  
    const actionNotAvailable = component.isActionAvailable(null, 'update');
    expect(actionNotAvailable).toBeFalsy();
  });

  it('should test callOptionCallback', () => {
    spyOn((component as any), 'handleTableActions');
    const selectedRowData: MiningTableRow = { data: 'your-row-data' };
    component.callOptionCallback('codeviewer', selectedRowData);
    expect(component.handleTableActions).toHaveBeenCalled();
  });

  it('should test handleSelectedFilter', () => {
    component.parametersService.currentTableParameters.filter = [{key: 'test', value: 'test'}, {key: 'technologyLink', value: 'test'}];
    component.handleSelectedFilter([{text: 'filter1', value: 'filter1', operator: Operators.EQUALS}, {text: 'filter2', value: 'filter2'}], 'technologyLink');
    expect(component.parametersService.currentTableParameters.filter).toBeDefined();
  });

  it('should test resolveFieldData', () => {
    const emptyFields = component.resolveFieldData(null, null);
    expect(emptyFields).toBeNull();

    const data = {
      id: 1
    };
    const fieldWithoutSeparatorNonZero = component.resolveFieldData(data, 'id');
    expect(fieldWithoutSeparatorNonZero).toBe(1);

    const data2 = {
      id: 0
    };
    const fieldWithoutSeparator = component.resolveFieldData(data2, 'id');
    expect(fieldWithoutSeparator).toBe(0);

    const nullFieldWithoutSeparator = component.resolveFieldData({}, 'id');
    expect(nullFieldWithoutSeparator).toBeNull();

    const data3 = {
      childSample: {
        id: 1,
      }
    };
    const fieldWithSeparator = component.resolveFieldData(data3, 'childSample.id');
    expect(fieldWithSeparator).toBeNull;
  });

  describe('getRowSpanValue', () => {
    it('should return 1 if the item or column is undefined', () => {
      const item: MiningTableRow = undefined;
      const column: Column = undefined;
      const result = component.getRowSpanValue(item, column);
      expect(result).toBe(1);
    });

    it('should return 1 if the item does not have rowspan property for the column', () => {
      const item = { field1: 'value1', field2: 'value2' };
      const column = { field: 'field3' , header: 'field3'};
      const result = component.getRowSpanValue(item, column);
      expect(result).toBe(1);
    });

    it('should return the rowspan value for the column if it exists in the item', () => {
      const item = { field1: 'value1', field2: 'value2', rowspan: { field3: 2 } };
      const column = { field: 'field3', header: 'field3' };
      const result = component.getRowSpanValue(item, column);
      expect(result).toBe(2);
    });

    it('should return 1 if the rowspan value for the column is not defined in the item', () => {
      const item = { field1: 'value1', field2: 'value2', rowspan: { id: 2 } };
      const column = { field: 'field3', header: 'field3'};
      const result = component.getRowSpanValue(item, column);
      expect(result).toBe(1);
    });
  });

  it('should update the checked set correctly', () => {
    // Arrange
    const ids = [1, 2, 3];
    const checked = true;
  
    // Act
    component.updateCheckedSet(ids, checked);
  
    // Assert
    expect(component.setOfSelectedId.size).toBe(3);
    expect(component.setOfSelectedId.has(1)).toBe(true);
    expect(component.setOfSelectedId.has(2)).toBe(true);
    expect(component.setOfSelectedId.has(3)).toBe(true);
  });
  
  it('should remove the checked ids correctly', () => {
    // Arrange
    component.setOfSelectedId.add(1);
    component.setOfSelectedId.add(2);
    component.setOfSelectedId.add(3);
    const ids = [2, 3];
    const checked = false;
  
    // Act
    component.updateCheckedSet(ids, checked);
  
    // Assert
    expect(component.setOfSelectedId.size).toBe(1);
    expect(component.setOfSelectedId.has(1)).toBe(true);
    expect(component.setOfSelectedId.has(2)).toBe(false);
    expect(component.setOfSelectedId.has(3)).toBe(false);
  });
  
  it('should update the selectedReachabilityRow correctly', () => {
    // Arrange
    component.usage = Usages.REACHABILITYTABLE;
    const ids = [1];
    const checked = true;
  
    // Act
    component.updateCheckedSet(ids, checked);
  
    // Assert
    expect(component.selectedReachabilityRow.length).toBe(1);
    expect(component.selectedReachabilityRow[0]).toBe('1');
  });
  
  it('should remove the id from selectedReachabilityRow correctly', () => {
    // Arrange
    component.usage = Usages.REACHABILITYTABLE;
    component.selectedReachabilityRow = ['1', '2', '3'];
    const ids = [2];
    const checked = false;
  
    // Act
    component.updateCheckedSet(ids, checked);
  
    // Assert
    expect(component.selectedReachabilityRow.length).toBe(2);
    expect(component.selectedReachabilityRow.includes('1')).toBe(true);
    expect(component.selectedReachabilityRow.includes('2')).toBe(false);
    expect(component.selectedReachabilityRow.includes('3')).toBe(true);
  });
  
  it('should call checkCurrentPageRecord after updating checked set', () => {
    // Arrange
    spyOn(component, 'checkCurrentPageRecord');
    const ids = [1];
    const checked = true;
  
    // Act
    component.updateCheckedSet(ids, checked);
  
    // Assert
    expect(component.checkCurrentPageRecord).toHaveBeenCalled();
  });

  it('should toggle between select current page and deselect current page', () => {
    // Arrange
    component.childValues = { '1': {}, '2': {}, '3': {} };
    component.deselectCurrent = true;
  
    // Act
    (component as any).selectDeselectCurrentIds();
  
    // Assert
    expect(component.deselectCurrent).toBe(false);
  });

  it('should return true if preSelectedModules is not empty and totalRecords is provided and preSelectedModules length is equal to totalRecords', () => {
    component.preSelectedModules = [1, 2, 3];
    const totalRecords = 3;
    const result = component.isPreSelected(1, totalRecords);
    expect(result).toBe(true);
  });

  it('should return false if preSelectedModules is not empty and totalRecords is provided and preSelectedModules length is not equal to totalRecords', () => {
    component.preSelectedModules = [1, 2, 3];
    const totalRecords = 4;
    const result = component.isPreSelected(1, totalRecords);
    expect(result).toBe(false);
  });

  it('should return true if preSelectedModules is not empty and id is included in preSelectedModules', () => {
    component.preSelectedModules = [1, 2, 3];
    const result = component.isPreSelected(1);
    expect(result).toBe(true);
  });

  it('should return false if preSelectedModules is not empty and id is not included in preSelectedModules', () => {
    component.preSelectedModules = [1, 2, 3];
    const result = component.isPreSelected(4);
    expect(result).toBe(false);
  });

  it('should return false if preSelectedModules is empty', () => {
    component.preSelectedModules = [];
    const result = component.isPreSelected(1);
    expect(result).toBe(false);
  });
});