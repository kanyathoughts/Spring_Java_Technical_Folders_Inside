import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { BehaviorSubject, Observable, of, throwError } from 'rxjs';
import { MiningTableConfig, FilterType, FieldTypeEnum, Column, BulkAction } from '@app/shared/components/mining-table/mining-table-config.interface';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ActivatedRoute } from '@angular/router';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzTableModule } from 'ng-zorro-antd/table';
import { HttpClient, HttpEvent, HttpXsrfTokenExtractor } from '@angular/common/http';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { StateKey } from '@app/shared/interfaces/state-maintainance.interface';
import { GraphQlControllerService } from '@app/core/services/graphql.service'
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { ModuleReportingComponent } from './module-reporting.component';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor, I18nService } from '@app/core';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { WindowToken } from '@app/core/utils/window';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { KeycloakService } from '@app/core/authentication/keycloak.service';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { HttpService } from '@app/core/http/http.service';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { CancelRequestOnNavigationInterceptor } from '@app/core/http/cancel-request-on-navigation.interceptor';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalService } from 'ng-zorro-antd/modal';
import { CandidateIdentificationControllerService, ControlFlowControllerService, DataPointControllerService, FeatureControllerService, JobControllerService, JobInformation, MiningDataPointDefinitionWithPath, ModuleControllerService, ProjectRole, SavedSearchControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { CfgSupportedTypeService } from '@app/core/services/cfg-supported-type.service';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';

describe('ModuleReportingComponent', () => {
  let component: ModuleReportingComponent;
  let fixture: ComponentFixture<ModuleReportingComponent>;
  let route: ActivatedRoute;
  let mockWindow: any;
  let modalService: NzModalService;
  let keycloakServiceSpy: KeycloakService;
  const IAMServiceSpy: jasmine.SpyObj<IdentityAccessManagementService> = jasmine.createSpyObj('IAMServiceSpy', ['getUsername', 'getUserId', 'logout']);
  const taxonomyServiceSpy = jasmine.createSpyObj<TaxonomyControllerService>('TaxonomyControllerService', ['getAssignedTaxonomyByModule', 'identifyTechnicalTaxonomies']);
  const jobControllerServiceSpy = jasmine.createSpyObj<JobControllerService>('JobControllerService', ['getJobInformation', 'getJobResult', 'cancelJob', 'getJobInformations', 'getJobLog', 'getJobResult', 'submitJobExtension', 'submitJobExtensionV2']);
  const candidateIdentificationServiceSpy = jasmine.createSpyObj<CandidateIdentificationControllerService>('CandidateIdentificationControllerService', ['identifyAllCandidates', 'identifyDeadCode']);
  const jobManagerServiceSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['register']);
  const moduleControllerServiceV1Spy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj<ModuleControllerService>
    ('ModuleControllerService', ['countRequiresReview', 'clearRequiresReview', 'identifyModuleDescriptions']);
  const authorizationServiceSpy = jasmine.createSpyObj<KeycloakAuthorizationService>('KeycloakAuthorizationService', ['hasUserRole', 'isClientAdmin']);
  const featureToggleServiceSpy = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);

  const jobInfoSuccess: JobInformation = { status: JobInformation.StatusEnum.SUCCESS };
  let openedUrl = '';
  const miningTableConfig: MiningTableConfig = {
    columnMap: {
      language: {
        field: 'target',
        header: 'Target',
        filterProperties: { filterType: FilterType.freeText, listOfFilter: [] }
      },
      'Module Name': {
        field: 'name',
        header: 'moduleName',
        hasWarning: (item: any) => true,
        warningMessage: 'This module was modified by the last scan. Please review its meta data (annotations, data dictionary, module description, taxonomies).',
        filterProperties: { filterType: FilterType.freeText, listOfFilter: [] },
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
    serverSidePagination: true,
    bulkActions: [{
      label: 'bulklabel1',
      tooltip: 'bulktooltip1',
      action: (data: Set<number>) => {}
    }]
  };

  const taxonomy =
    [{
      "recordId": "#122:196",
      "customProperties": {
        "TaxonomyCustomProperties": [
          {
            "name": "customTaxonomyProperty",
            "value": "A value for the custom Taxonomy property",
            "dataType": "STRING"
          }
        ]
      },
      "id": 1,
      "name": "Employee domain",
      "projectId": 1,
      "type": {
        "recordId": "#110:192",
        "customProperties": {},
        "name": "DataDomain",
        "projectId": 1,
        "category": {
          "name": "Business Taxonomies",
          "projectId": 1,
          "recordId": "#495:520",
          "customProperties": {},
          "id": 4
        }
      },
      "taxonomyReferenceCount": 2
    },
    {
      "recordId": "#123:196",
      "customProperties": {},
      "id": 2,
      "name": "Create Invoices",
      "projectId": 1,
      "type": {
        "recordId": "#111:192",
        "customProperties": {},
        "name": "BusinessProcess",
        "projectId": 1,
        "category": {
          "name": "Business Taxonomies",
          "projectId": 1,
          "recordId": "#495:520",
          "customProperties": {},
          "id": 4
        }
      },
      "taxonomyReferenceCount": 0
    },
    {
      "recordId": "#124:196",
      "customProperties": {},
      "id": 3,
      "name": "ARB100",
      "projectId": 1,
      "type": {
        "recordId": "#112:192",
        "customProperties": {},
        "name": "BusinessSubsystem",
        "projectId": 1,
        "category": {
          "name": "Business Taxonomies",
          "projectId": 1,
          "recordId": "#495:520",
          "customProperties": {},
          "id": 4
        }
      },
      "taxonomyReferenceCount": 1
    }
    ];
  let status = {
    "status$": of({
      "_isScalar": false,
      "closed": false,
      "isStopped": true,
      "hasError": false,
      "_value": "SUCCESS"
    })
  };

  const dataPoints: MiningDataPointDefinitionWithPath[] = [
    { 'name': 'description', 'displayName': 'Description', 'path': 'content.description', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Description' } } },
    { 'name': 'path', 'displayName': 'Path', 'path': 'content.path', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Base Data' } } },
    { 'name': 'typeLink', 'displayName': 'Type', 'path': 'content.objectTypeLink.typeLink', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Base Data' } } },
    { 'name': 'technologyLink', 'displayName': 'Technology', 'path': 'content.objectTypeLink.technologyLink', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Base Data' } } },
    { 'name': 'dependencyCount', 'displayName': 'Number of All dependencies', 'path': 'content.dependencyCount', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Dependencies' } } },
    { 'name': 'name', 'displayName': 'Module Name', 'path': 'content.name', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Base Data' } } },
    { 'name': 'metricsDate', 'displayName': 'Metrics Date', 'path': 'content.metricsDate', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Base Data' } } },
    { 'name': 'requiresReview', 'displayName': 'Requires Review', 'path': 'content.requiresReview', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Base Data' } } },
    { 'name': 'id', 'displayName': 'Module Id', 'path': 'content.id', 'usageAttributes': { 'miningUi.modulesTable': { category: 'Base Data' } } }]

  const graphQl = {
    data: {
      modules: {
        content: [
          {
            'dependencyCount': 0,
            id: 5425,
            inCodebase: false,
            metricsDate: "2021-09-24T10:55:46.758Z",
            name: "MMRS00C.A.LOADLIB",
            objectTypeLink: { typeLink: "FILE", technologyLink: "RESOURCE" },
            requiresReview: false
          },
          {
            'dependencyCount': 0,
            id: 5425,
            inCodebase: false,
            name: "MMRS00C.A.LOADLIB",
            objectTypeLink: { typeLink: "FILE", technologyLink: "RESOURCE" },
            requiresReview: false
          }
        ],
        size: 30,
        totalElements: 135,
        totalPages: 5
      }
    }
  }

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

  const selectedColumns: NzTreeNodeOptions[] = [
    { 'title': 'Module Name', 'name': 'name', 'key': 'name', 'checked': true, 'disableCheckbox': true, 'path': 'content.name' },
    { 'title': 'Metrics Date', 'name': 'metricsDate', 'key': 'metricsDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.metricsDate' },
    { 'title': 'Technology', 'name': 'technology', 'key': 'technology', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.technologyLink' },
    { 'title': 'Type', 'name': 'type', 'key': 'type', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.typeLink' },
    { 'title': 'Modified Date', 'name': 'modifiedDate', 'key': 'modifiedDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.modifiedDate' }
  ]

  const queryParameters = {
    page: 1,
    filter: 'name=="PRG*"',
    sort: ['name;ASC']
  }

  const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj(
    'DataPointControllerService',
    ['getDataPointsForType']
  );

  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
    'GraphQlControllerService',
    ['graphQl']
  );
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('CustomizableTableColumnService',
    ['getSelectedDataPoints', 'resetTableColumnAndDataPoints', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange', 'handleFilters', 'getGraphQlParam', 'onPageLoad', 'getQueryParams']);
  const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>('NzNotificationService', ['create', 'warning']);
  const i18nServiceSpy = { language: 'en-US' };
  const drawerServiceSpy = jasmine.createSpyObj('NzDrawerService', ['create']);
  const savedSearchControllerServiceSpy: jasmine.SpyObj<SavedSearchControllerService> = jasmine.createSpyObj<SavedSearchControllerService>
    ('SavedSearchControllerService', ['findByUsage']);
  const controlFlowControllerServiceSpy: jasmine.SpyObj<ControlFlowControllerService> = jasmine.createSpyObj<ControlFlowControllerService>
    ('ControlFlowControllerService', ['getControlFlow', 'calculateControlFlowForModule', 'calculateControlFlowGraphs']);
  const cfgSupportedTypes: jasmine.SpyObj<CfgSupportedTypeService> = jasmine.createSpyObj<CfgSupportedTypeService>('CfgSupportedTypeService', ['checkIfSupported']);
  const savedSearch = [{
    customProperties: {},
    name: "Business Rule Candidates",
    projectId: 0,
    savedSearch: "columns=Annotation.typeLink&columns=AnnotationCategory.name&columns=Module.name&columns=Annotation.sourceAttachment&columns=Annotation.stateLink&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Annotation.name&page=1&sort=in_HasAnnotation.out.name;ASC&filter=[{\"key\":\"typeLink\",\"value\":[\"RULE\"]},{\"key\":\"categoryLink.name\",\"value\":[\"Business Rule\"]},{\"key\":\"stateLink\",\"value\":[\"CANDIDATE\"]}]",
    usage: "miningUi.annotationsTable"
  }];

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
    let response = {
      object: {
        className: "innowake.mining.shared.model.TaxonomyImportValidationResult",
        overallResult: "NONE",
        markers: [{ 'test': 'testing' }]
      }
    };
    const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
    mockWindow.open.bind(mockWindow);
    TestBed.configureTestingModule({
      declarations: [ModuleReportingComponent],
      imports: [
        FormsModule,
        NzTableModule,
        TranslateModule.forRoot({}),
        RouterTestingModule.withRoutes([]),
        HttpClientTestingModule,
        BrowserAnimationsModule,
        NzDropDownModule
      ],
      providers: [
        TranslateService,
        NumberFormatter,
        FeatureControllerService,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        CancelRequestOnNavigationInterceptor,
        HttpXsrfTokenExtractor,
        DeepLinkService,
        { provide: NzNotificationService, useValue: notificationSpy },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: ModuleControllerService, useValue: moduleControllerServiceV1Spy },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: SavedSearchControllerService, useValue: savedSearchControllerServiceSpy },
        { provide: TaxonomyControllerService, useValue: taxonomyServiceSpy },
        { provide: CandidateIdentificationControllerService, useValue: candidateIdentificationServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: ControlFlowControllerService, useValue: controlFlowControllerServiceSpy },
        { provide: HttpClient, useClass: HttpService },
        { provide: IdentityAccessManagementService, useValue: IAMServiceSpy },
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
                sort: 'name;ASC'
              })
            }
          }
        },
        { provide: DataPointControllerService, useValue: dataPointControllerServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
        { provide: KeycloakAuthorizationService, useValue: authorizationServiceSpy },
        { provide: FeatureToggleService, useValue: featureToggleServiceSpy },
        { provide: CfgSupportedTypeService, useValue: cfgSupportedTypes },
        NzModalService,
        NzMessageService,
        { provide: NzDrawerService, useValue: drawerServiceSpy }
      ]
    }).compileComponents();
    const percentage = new BehaviorSubject<number>(10);
    taxonomyServiceSpy.getAssignedTaxonomyByModule.and.returnValue(of(taxonomy as any));
    moduleControllerServiceV1Spy.countRequiresReview.and.returnValue(of(1 as any));
    moduleControllerServiceV1Spy.clearRequiresReview.and.returnValue(of('' as any));
    savedSearchControllerServiceSpy.findByUsage.and.returnValue(of(savedSearch as any));
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
    userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedColumns as any));
    userCustomizableTableServiceSpy.updateTableConfig.and.returnValue(updatedColumns as any);
    const identifyRes = ['f7c92b4a-66ff-4b2f-9297-ed575a5a31ed'];
    candidateIdentificationServiceSpy.identifyAllCandidates.and.returnValue(of(identifyRes as any));
    jobManagerServiceSpy.register.and.returnValue(status as any);
    jobManagerServiceSpy.percent = percentage;
    jobControllerServiceSpy.getJobResult.and.returnValues(of(blob) as any);
    jobControllerServiceSpy.getJobInformation.and.returnValues(of(jobInfoSuccess as any));
    moduleControllerServiceV1Spy.identifyModuleDescriptions.and.returnValue(of(identifyRes as any));
    controlFlowControllerServiceSpy.calculateControlFlowGraphs.and.returnValue(of(identifyRes as any));
    taxonomyServiceSpy.identifyTechnicalTaxonomies.and.returnValue(of(identifyRes as any));
    authorizationServiceSpy.isClientAdmin.and.returnValue(true);
    authorizationServiceSpy.hasUserRole.and.returnValue(true);
    IAMServiceSpy.getUsername.and.returnValue('userTest');
    IAMServiceSpy.getUserId.and.returnValue('1');
    featureToggleServiceSpy.isActive.and.returnValue(of(false));
    featureToggleServiceSpy.isActive.withArgs('generativeAiTranslations').and.returnValue(of(true));
    drawerServiceSpy.create.and.returnValue({
      afterClose: of({result: FormResult.Saved}),
      afterOpen: undefined,
      close: undefined,
      open: undefined,
      getContentComponent: undefined,
      getContentComponentRef: undefined
    });
    keycloakServiceSpy = TestBed.inject( KeycloakService);
    spyOn(keycloakServiceSpy, 'getUserRoles').and.returnValue([]);
  }));

  beforeEach(() => {
    route = TestBed.get(ActivatedRoute);
    fixture = TestBed.createComponent(ModuleReportingComponent);
    component = fixture.componentInstance;
    modalService = TestBed.inject(NzModalService);
    component.tableConfig = miningTableConfig;
    component.rowActions = [[{ label: 'btnLabel.edit', value: 'edit' },
      {
        type: LinkType.DROPDOWN,
        icon: 'more',
        options: [
          { label: 'codeviewer', value: 'codeviewer', disableItem: (): boolean => false },
          { label: 'copyAnnotationDetails', value: 'copyAnnotationDetails', disableItem: (): boolean => false},
        ]
      }]];
    localStorage.setItem(
      StateKey.BrowseModuleTableStateKey,
      '{"selection": {}, "filters":{"target": {}, "type":{} } }'
    );
    route.snapshot.queryParams = {
      filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
      page: 1,
      sort: 'name;ASC',
    }
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should show warning when require review count is 2', () => {
    moduleControllerServiceV1Spy.countRequiresReview.and.returnValue(of(2 as any));
    (component as any).hasRequiresReview(clientProjectRelationship);
    expect(component.showWarning).toBeTrue();
  });

  it('should show warning when require review count is 0', () => {
    moduleControllerServiceV1Spy.countRequiresReview.and.returnValue(of(0 as any));
    (component as any).hasRequiresReview(clientProjectRelationship);
    expect(component.showWarning).toBeTrue();
  });

  it('should check updateRequiresReviewStatus', () => {
    component.tableConfig = miningTableConfig;
    component.handleAlertOnConfirm();
    expect(component.showWarning).toBeFalsy();
  });

  it('Empty GraphQL Response', () => {
    const emptyGraphQLResponse = { data: {} };
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(emptyGraphQLResponse as any));
    expect(component.tableConfig.loading).toBeFalsy();
  });

  it('should test getAssignmentData', () => {
    let ids = new Set<number>([1, 23, 4])
    component.getAssignmentData(ids);
    expect(taxonomyServiceSpy.getAssignedTaxonomyByModule).toHaveBeenCalled();
  });

  it('should test handleselected option of codeviwer', () => {
    let value: MiningTableOptionSelected = { optionValue: "code-viewer", data: { id: 100 } };
    component.projectId = 1;
    component.handleSelectedOption(value);
    expect(openedUrl).toContain('code-viewer')
  });

  it('should test handleselected option of controlFlow', () => {
    let value: MiningTableOptionSelected = { optionValue: "control-flow", data: { id: 100 } };
    component.projectId = 1;
    component.handleSelectedOption(value);
    expect(openedUrl).toContain('control-flow')
  });

  it('should test handleselected option of depGraph', () => {
    let value: MiningTableOptionSelected = { optionValue: "dependencies", data: { id: 100 } };
    component.projectId = 1;
    component.handleSelectedOption(value);
    expect(openedUrl).toContain('dependencies')
  });

   it('should test disableTableAction', () => {
     // scenario: module type actually supported but not in codebase
     cfgSupportedTypes.checkIfSupported.and.returnValue(true);
     const disableCodeViewer = (component as any).disableTableAction('code-viewer', {inCodeBase: false, identificationLink: 'MISSING', storage: 'FILE'});
     expect(disableCodeViewer).toEqual({disableButton: true, toolTip: component.translateService.instant('module.codeViewerNotAvailableTooltip')});

     // scenario: module type not supported
     cfgSupportedTypes.checkIfSupported.and.returnValue(false);
     const disableCFG = (component as any).disableTableAction('control-flow', {inCodeBase: false, actuallySupported: false});
     expect(disableCFG).toEqual({disableButton: true, toolTip: component.translateService.instant('cfgNotAvailable')});
   });

  it('should test identifyAllCandidates', () => {
    let ids = new Set<number>([1, 23, 4])
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'identifyCandidates');
    expect(candidateIdentificationServiceSpy.identifyAllCandidates).toHaveBeenCalled();
  });

  it('should trigger identifyDeadCode and handle modal result', () => {
    let ids = new Set<number>([1, 23, 4]);
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'identifyDeadCode');
    expect(candidateIdentificationServiceSpy.identifyDeadCode).toHaveBeenCalled();
  });

  it('should test identifyTechnicalTaxonomies', () => {
    let ids = new Set<number>([1, 23, 4]);
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'identifyTachnicalTaxonomies');
    expect(taxonomyServiceSpy.identifyTechnicalTaxonomies).toHaveBeenCalled();
  });

  it('should test identifyTechnicalTaxonomies', () => {
    let ids = new Set<number>([1, 23, 4]);
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'idetifyModuleDescriptions');
    expect(moduleControllerServiceV1Spy.identifyModuleDescriptions).toHaveBeenCalled();
  });

  it('should test calculateControlFolw', () => {
    let ids = new Set<number>([1, 23, 4]);
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'calculateControlFlowGrapgh');
    expect(controlFlowControllerServiceSpy.calculateControlFlowGraphs).toHaveBeenCalled();
  });

  it('should show confirmation modal', () => {
    let ids = new Set<number>([1, 23, 4]);
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'explainAnnotations');
    expect(component.annotationConfirmVisible).toBeTruthy();
  });

  it('should show module description confirmation modal', () => {
    let ids = new Set<number>([1, 23, 4]);
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'generateModuleDescriptions');
    expect(component.moduleConfirmVisible).toBeTruthy();
  });

  it('should submit generate-annotation-descriptions-from-module job extension', () => {
    component.projectId = 1;
    component.explainAnnotationsOverwrite = false;
    let ids = new Set<number>([1, 23, 4]);
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'explainAnnotations');
    jobControllerServiceSpy.submitJobExtensionV2.and.returnValue(of(throwError('error') as any));
    jobControllerServiceSpy.submitJobExtensionV2.and.returnValue(of('some-job-id') as Observable<any>);
    component.handleAnnotationsModalOk();
    expect(jobControllerServiceSpy.submitJobExtensionV2).toHaveBeenCalledWith(1, 'generate-annotation-descriptions-from-module', { 'ids': [1, 23, 4], 'overwrite': false });
  });

  it('should submit generate-module-descriptions job extension', () => {
    component.projectId = 1;
    component.generateModuleDescriptionsOverwrite = false;
    let ids = new Set<number>([1, 23, 4]);
    localStorage.setItem('reporting_moduleIds', '[1,2]');
    component.identifyWebBasedAnalyses(ids, 'generateModuleDescriptions');
    jobControllerServiceSpy.submitJobExtensionV2.and.returnValue(of('some-job-id') as Observable<any>);
    component.handleModulesModalOk();
    expect(jobControllerServiceSpy.submitJobExtensionV2).toHaveBeenCalledWith(1, 'generate-module-descriptions', { 'ids': [1, 23, 4], 'overwrite': false });
  });

  const containsGenerateAction = (element: BulkAction) => element.label === 'bulkActionButtonLbls.generate';

  it('should contain generate dropdown for manager', () => {
    authorizationServiceSpy.hasUserRole.and.callFake(function(client: ClientProjectRelationship, role: ProjectRole.UserRoleEnum) {
      return role === ProjectRole.UserRoleEnum.MANAGER;
    })
    component.ngOnInit();
    expect(component.tableConfig.bulkActions.some(containsGenerateAction)).toBeTruthy();
  });

  it('should not contain generate dropdown for non-manager', () => {
    authorizationServiceSpy.hasUserRole.and.callFake(function(client: ClientProjectRelationship, role: ProjectRole.UserRoleEnum) {
      return ! (role === ProjectRole.UserRoleEnum.MANAGER);
    })
    component.ngOnInit();
    expect(component.tableConfig.bulkActions.some(containsModulesAction)).toBeFalsy();
  });

  const containsAnnotationAction = (element: BulkAction) => element.subActions && element.subActions.some((element: BulkAction) => element.label === 'bulkActionButtonLbls.explainAnnotations');

  it('should contain explain annotations action for manager', () => {
    authorizationServiceSpy.hasUserRole.and.callFake(function(client: ClientProjectRelationship, role: ProjectRole.UserRoleEnum) {
      return role === ProjectRole.UserRoleEnum.MANAGER;
    })
    component.ngOnInit();
    expect(component.tableConfig.bulkActions.some(containsAnnotationAction)).toBeTruthy();
  });

  it('should not contain explain annotations action for non-manager', () => {
    authorizationServiceSpy.hasUserRole.and.callFake(function(client: ClientProjectRelationship, role: ProjectRole.UserRoleEnum) {
      return ! (role === ProjectRole.UserRoleEnum.MANAGER);
    })
    component.ngOnInit();
    expect(component.tableConfig.bulkActions.some(containsAnnotationAction)).toBeFalsy();
  });

  const containsModulesAction = (element: BulkAction) => element.subActions && element.subActions.some((element: BulkAction) => element.label === 'bulkActionButtonLbls.generateModuleDescriptions');

  it('should contain generate modules descriptions action for manager', () => {
    authorizationServiceSpy.hasUserRole.and.callFake(function(client: ClientProjectRelationship, role: ProjectRole.UserRoleEnum) {
      return role === ProjectRole.UserRoleEnum.MANAGER;
    })
    component.ngOnInit();
    expect(component.tableConfig.bulkActions.some(containsModulesAction)).toBeTruthy();
  });

  it('should test globalCallChain', () => {
    let ids = new Set<number>([1, 23, 4]);
    component.projectId = 1;
    spyOn(localStorage, 'setItem').and.callThrough();
    component.identifyWebBasedAnalyses(ids, 'globalCallChain');
    expect(localStorage.setItem).toHaveBeenCalledWith(`1-reporting_moduleIds`, JSON.stringify(component.moduleIdArray));
    expect(openedUrl).toContain('project-1/reachability/call-chain');
  });

  it('Should test drawer', () => {
    let drawer = drawerServiceSpy.create({});
    drawerServiceSpy.create.and.returnValue(drawer);
    fixture.detectChanges();
    component.taxonomyPropagation(new Set<number>([1]));
    expect(drawerServiceSpy.create).toHaveBeenCalled();
  });
});
