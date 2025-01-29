import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { BehaviorSubject, of } from 'rxjs';
import { MiningTableConfig, FilterType, FieldTypeEnum, Column } from '@app/shared/components/mining-table/mining-table-config.interface';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ActivatedRoute } from '@angular/router';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { StateKey } from '@app/shared/interfaces/state-maintainance.interface';
import { GraphQlControllerService } from '@app/core/services/graphql.service'
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { BaseReportingPageComponent } from './base-reporting-page.component';
import { Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { NzMessageService } from 'ng-zorro-antd/message';
import { SharedModule } from '@app/shared/shared.module';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor } from '@app/core';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { HttpClient, HttpXsrfTokenExtractor } from '@angular/common/http';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { LinkType } from '../mining-table/mining-table-action.interface';
import { MiningTableComponent } from '../mining-table/mining-table.component';
import { SavedSearchComponent } from './saved-search/saved-search.component';
import { DataPointControllerService, FeatureControllerService, JobControllerService, MiningDataPointDefinitionWithPath, SavedSearchControllerService } from '@innowake/mining-api-angular-client';
import { ReachabilityService } from '@app/modules/reachability-ui-product-vision/utils/reachability.service';

describe('BaseReportingPageComponent', () => {
  let component: BaseReportingPageComponent;
  let fixture: ComponentFixture<BaseReportingPageComponent>;
  let route: ActivatedRoute;

  const i18nServiceSpy = { language: 'en-US' };
  const miningTableConfig: MiningTableConfig = {
    columnMap: {
      language: {
        field: 'target',
        header: 'Target',
        filterProperties: { filterType: FilterType.freeText }
      },
      'Module Name': {
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
    paginator: true,
    rows: 10,
    serverSidePagination: true
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
      "path": "content.modifiedDate",
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
      "columnAction": {
        "type": LinkType.HYPERLINK,
        resolveURL: (data: {type: "taxonomyType"}, index: 1) => {
          return 'modules';
         },
      },
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
  ]

  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
    ['getSelectedDataPoints','resetTableColumnAndDataPoints', 'updateTableConfig', 'setDataPointList', 'setColumnIdsSelection', 'checkSelectedColumns', 'getRowSelectionClearanceFlag', 'updateSelectedDataPoints', 'getDefaultSortBy']);
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
  const miningTableComponent: jasmine.SpyObj<MiningTableComponent> = jasmine.createSpyObj('MiningTableComponent', [
    'setCurrentTableFilters',
  ]);

  const savedSearchComponent: jasmine.SpyObj<SavedSearchComponent> = jasmine.createSpyObj('SavedSearchComponent', [
    'setSavedSearchParameter',
    'getTableConfig',
    'setRoute'
  ]);
  let reachabilityServiceSpy: jasmine.SpyObj<ReachabilityService>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [BaseReportingPageComponent],
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
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: ReachabilityService, useValue: reachabilityServiceSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              project: { id: 1 }
            }),
            snapshot: {
              queryParams: of({
                filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
                page: 1,
                sort: 'name;ASC',
                savedSearch: 'Business Rule Candidates',
                preFilter: 'call-chain-69625m3x50'
              })
            }
          }
        },
      ]
    }).compileComponents();
    userCustomizableTableServiceSpy.retainedDataPointId = selectedDataPoints;
    userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedDataPoints));
    userCustomizableTableServiceSpy.updateTableConfig.and.returnValue(updatedDataPoints);
    userCustomizableTableServiceSpy.getDefaultSortBy.and.returnValue('{content_name: ASC}');
    userCustomizableTableServiceSpy.getRowSelectionClearanceFlag.and.returnValue(new BehaviorSubject(true));
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    savedSearchControllerServiceSpy.findByUsage.and.returnValue(of(savedSearches as any));
    httpServiceSpy.disableApiPrefix.and.returnValue(httpServiceSpy);
    httpServiceSpy.get.and.returnValue(of('99.9.99-TRUNK-MINING-SNAPSHOT'));
    IAMServiceSpy.getUsername.and.returnValue('admin');
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
  }));

  beforeEach(() => {
    route = TestBed.get(ActivatedRoute);
    fixture = TestBed.createComponent(BaseReportingPageComponent);
    component = fixture.componentInstance;
    component.tableConfig = miningTableConfig;
    component.selectedSavedSearchName = 'Not Referenced';
    component.usage = Usages.MODULETABLE;
    component.graphQlType = 'modules';
    component.saveSearch = savedSearchComponent;
    localStorage.setItem(
      StateKey.BrowseModuleTableStateKey,
      '{"selection": {}, "filters":{"target": {}, "type":{} } }'
    );
    route.snapshot.queryParams = {
      filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
      page: 1,
      sort: 'name;ASC',
      savedSearch: 'Business Rule Candidates',
      preFilter: 'call-chain-69625m3x50'
    }
    localStorage.setItem('call-chain-69625m3x50', '{"createdTime": "2024-05-08 11:33", "moduleIds": [399, 1811]}');
    component.clientProjectSubscription = component.initialize().subscribe();
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test initialize', () => {
    route.snapshot.queryParams = {
      filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
      page: 1,
      sort: 'name;ASC',
      savedSearch: '',
      preFilter: 'reachabilityModules-69625m3x50',
    }
    localStorage.setItem('reachabilityModules-69625m3x50', '{"createdTime": "2024-05-08 11:33", "moduleIds": [399, 1811], "blockName": "test"}');
    component.clientProjectSubscription = component.initialize().subscribe();
    expect(component.pageTitle).toBe('test');

    route.snapshot.queryParams = {
      filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
      page: 1,
      sort: 'name;ASC',
      savedSearch: 'Business Rule Candidates',
      preFilter: 'test-69625m3x50',
    }
    localStorage.setItem('test-69625m3x50', '{"createdTime": "2024-05-08 11:33", "moduleIds": [399, 1811], "annotationIds": "test-else-case"}');
    sessionStorage.setItem('modulesSavedSearch', 'test-else-case');
    component.clientProjectSubscription = component.initialize().subscribe();
    expect(component.preFilterDetails).toBe('test-else-case');
  })

  it('should test the togglePathDetails with showDetails', () => {
    const currentIncluded = component.additionalGraphQLParams.includeIntermediateModulesPerAccessModule;
    (component as any).togglePathDetails('showDetails');
    expect(component.additionalGraphQLParams.includeIntermediateModulesPerAccessModule).toBe(! currentIncluded);
    expect(component.switchPathDetails).toBe(true);
  })

  it('should test the togglePathDetails with hide details', () => {
    const currentIncluded = component.additionalGraphQLParams.includeIntermediateModulesPerAccessModule;
    (component as any).togglePathDetails('hideDetails');
    expect(component.additionalGraphQLParams.includeIntermediateModulesPerAccessModule).toBe(! currentIncluded);
    expect(component.switchPathDetails).toBe(false);
  })

  it('should navigate to reachability overview', () => {
    const expectedName = component.translateService.instant('reachability.reachabilityBlocks');
    const navigatedUrl = component.navigate(expectedName);
    expect(navigatedUrl).toBe('/project-' + component.projectId + '/reachability/overview');

    const elseCaseName = 'test';
    const noUrl = component.navigate(elseCaseName);
    expect(noUrl).toBeUndefined();
  })

  it('should handle annotations modal cancel', () => {
    component.handleAnnotationsModalCancel();
    expect(component.annotationConfirmVisible).toBeFalsy();
  })

  it('should handle Modules Modal Cancel', () => {
    component.handleModulesModalCancel();
    expect(component.moduleConfirmVisible).toBeFalsy();
  })

  it('should load Customizable Table', () => {
    component.loadCustomizableTable();
    expect(component.loadTable ).toBeTruthy();
  })

  it('should toggle Path Details', () => {
    component.togglePathDetails('showDetails');
    expect(component.switchPathDetails).toBeTruthy();

    component.togglePathDetails('test');
    expect(component.switchPathDetails).toBeFalsy()
  })

  it('should test isFormDirty', () => {
    const formEditorState = !component.sharedAnnotationEditorService.getEditorFormState()
    const formState = component.isFormDirty();
    expect(formState).toBe(formEditorState);
  })

  it('should test onCancel', () => {
    spyOn(component.sharedAnnotationEditorService, 'closeDrawer');
    component.onCancel();
    expect(component.sharedAnnotationEditorService.closeDrawer).toHaveBeenCalled();
  })

  describe('fetchTableDataAsPerDataPoints', () => {
    it('should update initialTableConfig with defaultTableConfig if selectedSavedSearchName is empty', () => {
      const selectedColumns = ['column1', 'column2'];
      const defaultTableConfig = {
        columns: '',
        filter: '[]',
        sort: '{content_name: ASC}'
      };
      const updatedTableConfig = {
        columns: 'column1,column2',
        filter: '[{"key":"filter1","value":"value1"},{"key":"filter2","value":"value2"}]',
        sort: 'column1;ASC'
      };
      component.selectedSavedSearchName = '';
      userCustomizableTableServiceSpy.selectedColumns = selectedColumns;
      savedSearchComponent.getTableConfig.and.returnValue(updatedTableConfig);
      component.fetchTableDataAsPerDataPoints();
      expect(component.initialTableConfig).toEqual(defaultTableConfig);
      expect(userCustomizableTableServiceSpy.updateSelectedDataPoints).toHaveBeenCalledWith(selectedDataPoints);
    });

    it('should update initialTableConfig with saved search table config if selectedSavedSearchName is not empty', () => {
      const selectedColumns = ['column1', 'column2'];
      const savedSearchTableConfig = {
        columns: '',
        filter: '[]',
        sort: '{content_name: ASC}'
      };
      const updatedTableConfig = {
        columns: 'column1,column2',
        filter: '[{"key":"filter1","value":"value1"},{"key":"filter2","value":"value2"}]',
        sort: 'column1;ASC'
      };
      userCustomizableTableServiceSpy.selectedColumns = selectedColumns;
      savedSearchComponent.getTableConfig.and.returnValue(updatedTableConfig);
      component.selectedSavedSearchName = 'Saved Search';
      component.fetchTableDataAsPerDataPoints();
      expect(component.initialTableConfig).toEqual(savedSearchTableConfig);
    });
  });

  it('should get saved search details', () => {
    const initialTableConfig = {
      columns: 'column1,column2',
      filter: '[{"key":"filter1","value":"value1"},{"key":"filter2","value":"value2"}]',
      sort: 'column1;ASC'
    };2
    const route = new ActivatedRoute();
    component.getSavedSearchDetails({savedSearchName: 'test', initialTableConfig, route});
    expect(component.disableSortHandling).toBeTruthy();
  })
});
