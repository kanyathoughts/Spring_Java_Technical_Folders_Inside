import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor, HttpService, I18nService } from '@app/core';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { SharedModule } from '@app/shared';
import { AllowedTableActions, LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { of, Subject, throwError } from 'rxjs';
import { ModuleAnnotationsComponent } from './module-annotations.component';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { WindowToken } from '@app/core/utils/window';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { CancelRequestOnNavigationInterceptor } from '@app/core/http/cancel-request-on-navigation.interceptor';
import { CustomPropertiesService } from '@app/core/services/custom-properties/custom-properties.service';
import { AnnotationControllerService, AnnotationPojo, AnnotationReport, DataDictionaryControllerService, DataPointControllerService, FeatureControllerService, JobControllerService, MiningDataPointDefinitionWithPath, ModuleControllerService, ProjectControllerService, ReferenceControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { SetSerializationInterceptor } from '@app/core/http/set-serialization.interceptor';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';

describe('ModuleAnnotationsComponent', () => {
  let component: ModuleAnnotationsComponent;
  let fixture: ComponentFixture<ModuleAnnotationsComponent>;
  let oauthServiceSpy: OauthtokenService;
  let drawerServiceSpy: NzDrawerService;
  const annotationControllerServiceSpy: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj<AnnotationControllerService>
  ('AnnotationControllerService',
    ['updateAnnotation', 'findAnnotationById', 'deleteAnnotation']);
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService', ['findAnnotationsForModule' , 'findModuleById']);
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
    'GraphQlControllerService',
    ['graphQl']
  );
  const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj(
    'DataPointControllerService',
    ['getDataPointsForType']
  );
  const dataDictionaryControllerServiceSpy: jasmine.SpyObj<DataDictionaryControllerService> = jasmine.createSpyObj('DataDictionaryControllerService', [
    'updateDataDictionaryEntry', 'deleteDataDictionaryEntry', 'findDataDictionaryEntryByRecordId'
  ]);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
    ['bulkDelete', 'restoreFromSavedSelection']);
  const customPropertyService = jasmine.createSpyObj<CustomPropertiesService>('CustomPropertiesService', ['getCustomPropertiesMetadataForClass', 'getAutoCompletionValues']);

  const authServiceSpy = new NoAuthorizationService();
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose','close']);
  const i18nServiceSpy = { language: 'en-US' };
  let mockWindow: any;
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

  const annotationReport: any = {
    recordId: '#89:1280',
    customProperties: {},
    id: 4,
    name: 'Annotation 4',
    moduleName: 'PRG1',
    annotationType: AnnotationReport.AnnotationTypeEnum.DATABASE,
    categoryName: 'Annotation Category A',
    annotationState: AnnotationReport.AnnotationStateEnum.CANDIDATE,
    sourceCode: '5678',
    taxonomy: '[ARB100, Employee domain]',
    updatedByUserId: 'system',
    createdByUserId: 'system',
    inHasAnnotation: {
      out: {
        id: 1
      }
    }
  };
  const dataDictionaryEntry: any = {
    "recordId": "#220:628",
    "customProperties": {},
    "id": 3695,
    "dataElementName": "MYFXOUT-RECORD",
    "projectId": 6,
    "description": "MYFXOUT-RECORD 1",
    "format": "Group",
    "scopes": [
        "FILE"
    ],
    "length": 0,
    "createdByUserId": "system_user",
    "updatedByUserId": "admin",
    "createdByUserName": "system_user",
    "updatedByUserName": "admin",
    "otherScope": null,
    "otherScopeSource": null,
    "scopeAttributes": {},
    "picClause": null,
    "definedLocation": "Program",
    "state": "APPROVED",
    "isBusiness": true,
    "fieldTransformation": null,
    "sourceInput": "sip1",
    "targetOutput": "tip 12",
    "isReferenced": true,
    "usage": "DISPLAY",
    "reference": {
        "recordId": "#284:628",
        "customProperties": {},
        "id": 12787,
        "relationship": "HAS_DATA_DICTIONARY_ENTRY",
        "fromId": "#148:1228",
        "fromModuleId": 6047,
        "toId": "#220:628",
        "toModuleId": 3695,
        "fromName": "MMRS7111",
        "toName": null,
        "fromModuleLocation": {
            "offset": 4089,
            "length": 14
        },
        "toModuleLocation": {
            "offset": 0,
            "length": 0
        },
        "binding": null,
        "properties": {},
        "conditionalDependency": {
            "ifReachedFromModules": []
        }
    },
    "hasBusinessRules": [
        {
            "recordId": "#867:9",
            "customProperties": {},
            "id": 12854,
            "relationship": "HAS_BUSINESS_RULE",
            "fromId": "#220:628",
            "fromModuleId": 3695,
            "toId": "#189:301",
            "toModuleId": 1470,
            "fromName": null,
            "toName": "Business Rule Candidate [System identified]",
            "fromModuleLocation": {
                "offset": 0,
                "length": 0
            },
            "toModuleLocation": {
                "offset": 0,
                "length": 0
            },
            "binding": null,
            "properties": {},
            "conditionalDependency": {
                "ifReachedFromModules": []
            }
        }
    ],
    "isCandidate": true,
    "fieldLevel": 1,
    "parentGroup": null,
    "groupPath": "MYFXOUT-RECORD",
    "indentation": 0
}
  const codeAnnotationsValue: AnnotationPojo[] = [{
    uid: '#172:234',
    customProperties: {
      'Property1': [{
        name: 'customAnnotationProperty',
        value: 'A value for the custom Annotation property',
        dataType: 'STRING'
      }]
    },
    id: 1,
    name: 'Annotation 1',
    projectId: 1,
    state: 'CANDIDATE',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category A',
    createdByUserId: 'admin',
    updatedByUserId: 'admin',
    sourceAttachment: 'abcd',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {
      'Property1': [{
        name: 'customAnnotationProperty',
        value: 'A value for the custom Annotation property',
        dataType: 'STRING'
      }]
    },
    id: 1,
    name: 'Annotation 2',
    projectId: 1,
    state: 'FOR_REVIEW',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {
      'Property1': [{
        name: 'customAnnotationProperty',
        value: 'A value for the custom Annotation property',
        dataType: 'STRING'
      }]
    },
    id: 1,
    name: 'Annotation 2',
    projectId: 1,
    state: 'REJECTED',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  }];
  const annotationValues: AnnotationPojo[] = [{
    uid: '#172:234',
    customProperties: {'Property1':[{
      name: 'customAnnotationProperty',
      value: 'A value for the custom Annotation property',
      dataType: 'STRING'
    }]},
    id: 1,
    name: 'Annotation 1\nNewLine\nNewLine2',
    projectId: 1,
    state: 'CANDIDATE',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category A',
    createdByUserId: 'admin',
    updatedByUserId: 'admin',
    sourceAttachment: 'abcd',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {'Property1':[{
      name: 'customAnnotationProperty',
      value: 'A value for the custom Annotation property',
      dataType: 'STRING'
    }]},
    id: 1,
    name: 'Annotation 2',
    projectId: 1,
    state: 'IN_ANALYSIS',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {'Property1':[{
      name: 'customAnnotationProperty',
      value: 'A value for the custom Annotation property',
      dataType: 'STRING'
    }]},
    id: 1,
    name: 'Annotation 2',
    projectId: 1,
    state: 'FOR_REVIEW',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {'Property1':[{
      name: 'customAnnotationProperty',
      value: 'A value for the custom Annotation property',
      dataType: 'STRING'
    }]},
    id: 1,
    name: 'Annotation 2',
    projectId: 1,
    state: 'REJECTED',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {'Property1':[{
      name: 'customAnnotationProperty',
      value: 'A value for the custom Annotation property',
      dataType: 'STRING'
    }]},
    id: 1,
    name: 'Annotation 2',
    projectId: 1,
    state: 'APPROVED',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {'Property1':[{
      name: 'customAnnotationProperty',
      value: 'A value for the custom Annotation property',
      dataType: 'STRING'
    }]},
    id: 2,
    name: 'Annotation 2\nNewLine\nNewLine2\nNewLine3',
    projectId: 1,
    state: 'INVALID',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  }];
  const oAuthToken: any = {
    access_token: 'ffb3eff8-53a7-4153-bff1',
    token_type: 'bearer',
    refresh_token: 'a30a484a-b1a1-4580-a437',
    expires_in: 315575999,
    scope: 'read write trust',
    username: 'test-admin'
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ModuleAnnotationsComponent],
      providers: [
        TranslateService,
        SetSerializationInterceptor,
        {provide: ModuleControllerService, useValue: moduleControllerServiceSpy},
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        OauthtokenService,
        AnnotationControllerService,
        FeatureControllerService,
        ProjectControllerService,
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: NzModalRef, useValue: { destroy: () => true, close: () => true } } ,
        { provide: WindowToken, useValue: mockWindow },
        DeepLinkService,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        CancelRequestOnNavigationInterceptor,
        {
          provide: HttpClient,
          useClass: HttpService
        },
        {provide: KeycloakAuthorizationService, useValue: authServiceSpy},
        NumberFormatter,
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: DataPointControllerService, useValue: dataPointControllerServiceSpy },
        { provide: AnnotationControllerService, useValue: annotationControllerServiceSpy },
        { provide: DataDictionaryControllerService, useValue: dataDictionaryControllerServiceSpy },
        { provide: CustomPropertiesService, useValue: customPropertyService },
        TaxonomyControllerService,
        ReferenceControllerService,
        DataDictionaryControllerService,
        JobControllerService
      ],
      imports: [
        SharedModule,
        FormsModule,
        RouterTestingModule,
        TranslateModule.forRoot({}),
        HttpClientTestingModule,
        BrowserAnimationsModule
      ]
    }).compileComponents();
    oauthServiceSpy = TestBed.inject(OauthtokenService);
    drawerServiceSpy = TestBed.inject(NzDrawerService);
    spyOn(oauthServiceSpy, 'getUsername').and.returnValue(oAuthToken.username);
    moduleControllerServiceSpy.findAnnotationsForModule.and.returnValues(of(annotationValues as any), of([] as any), throwError(new Error('Test Error')));
    moduleControllerServiceSpy.findModuleById.and.returnValue(of(annotationValues[0] as any));
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    annotationControllerServiceSpy.findAnnotationById.and.returnValues(of(codeAnnotationsValue[0] as any), throwError('TEST_ERROR'));
    dataDictionaryControllerServiceSpy.findDataDictionaryEntryByRecordId.and.returnValue(of(dataDictionaryEntry as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleAnnotationsComponent);
    component = fixture.componentInstance;
    nzModalRefSpy.afterClose = new Subject;
    nzModalRefSpy.afterClose.next(null);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    component.moduleId = 1;
    component.projectId = 1;
    component.module = { name: 'Test'};
    nzModalRefSpy.afterClose = new Subject;
    nzModalRefSpy.afterClose.next(null);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    component.ngOnInit();
    component.internalDataPoints = [
      { name: 'id', path: 'content.id' },
      { name: 'name', path: 'content.inHasAnnotation.out.name' },
      { name: 'id', path: 'content.inHasAnnotation.out.id' },
      { name: 'offset', path: 'content.inHasAnnotation.fromModuleLocation.offset'}
    ];
    expect(component).toBeTruthy();
  });

  it('user should not have access to edit annotations', () => {
    component.rowActions = [];
    spyOn(authServiceSpy, 'hasUserRole').and.returnValue(false);
    component.ngOnInit();
    expect(component.rowActions).toEqual([]);
  });

  it('should open web annotation editor in edit mode', () => {
    component.rowActions = [[{ label: 'btnLabel.edit', value: 'edit' },
    {
      type: LinkType.DROPDOWN,
      icon: 'more',
      options: [
        { label: 'iconToolTip.codeViewer', value: 'codeviewer', disableItem: (): boolean => false },
      ]
    }]];
      spyOn((component as any).messageService, 'error');
      let drawer = drawerServiceSpy.create({});
      const spy = spyOn(drawerServiceSpy, 'create');
      spy.and.returnValue(drawer);
      fixture.detectChanges();
      component.editCodeAnnotation({optionValue:'edit' , data: annotationReport});
      component.annotationToBeUpdated = codeAnnotationsValue[0];
      drawer.close(AllowedTableActions.UPDATE);
      expect(spy).toHaveBeenCalled();
  });

  it('should check the null check for the afterCloseAnnotationEditor', () => {
    const spy = spyOn(component, 'afterCloseAnnotationEditor').and.callThrough();
    component.afterCloseAnnotationEditor(annotationValues[0],'' as any);
    expect(spy).toHaveBeenCalled();
  });
});
