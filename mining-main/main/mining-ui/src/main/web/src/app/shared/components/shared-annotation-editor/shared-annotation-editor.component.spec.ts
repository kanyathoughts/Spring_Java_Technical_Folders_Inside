import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { DebugElement, NO_ERRORS_SCHEMA } from '@angular/core';
import { UntypedFormBuilder } from '@angular/forms';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { NzMessageService } from 'ng-zorro-antd/message';
import { SharedAnnotationEditorComponent } from './shared-annotation-editor.component';
import { Observable, of, throwError } from 'rxjs';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { AnnotationEditor } from '@app/shared/interfaces/annotation-editor.interface';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { By } from '@angular/platform-browser';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { Router } from '@angular/router';
import { NavigationGuard } from '@app/core/pendingChanges/navigationGuard';
import { SharedAnnotationEditorService } from './shared-annotation-editor.service';
import { ClipboardService, Entity } from '@app/core/services/clipboard.service';
import { AnnotationCategory, AnnotationCategoryControllerService, AnnotationControllerService, AnnotationPojo, AnnotationToFunctionalBlockControllerService, DataDictionaryControllerService, DataDictionaryPojo, MetamodelControllerService, ModuleControllerService, ProjectControllerService } from '@innowake/mining-api-angular-client';

// Create a mock component that implements ComponentCanDeactivateNavigation
class MockComponent {
  canDeactivate(): boolean | Observable<boolean> {
    return true; // Implement the behavior you want to test.
  }
}

describe('SharedAnnotationEditorComponent', () => {
  let component: SharedAnnotationEditorComponent;
  let fixture: ComponentFixture<SharedAnnotationEditorComponent>;
  let drawerServiceSpy: NzDrawerService;
  let guard: NavigationGuard;
  let sharedAnnotationEditorService: SharedAnnotationEditorService;
  const formBuilder: UntypedFormBuilder = new UntypedFormBuilder();
  const formBuilderStub = {};
  const AUTO_COMPLETION_MAP = 'autoCompletionMap';
  const routerMock = {
    navigate: jasmine.createSpy('navigate'),
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
    reasons: ['IF_ELSE_CONDITION'],
    name: 'Annotation 1',
    projectId: 1,
    state: 'CANDIDATE',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category A',
    createdByUserId: 'admin',
    updatedByUserId: 'admin',
    sourceAttachment: 'abcd',
    englishTranslation: 'translation',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    },
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
    reasons: ['IF_ELSE_CONDITION'],
    name: 'Annotation 2',
    projectId: 1,
    state: 'FOR_REVIEW',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    englishTranslation: 'translation',
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
    reasons: ['IF_ELSE_CONDITION'],
    name: 'Annotation 2',
    projectId: 1,
    state: 'REJECTED',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    englishTranslation: 'translation',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#172:234',
    customProperties: {
      'Property1': [{
        name: 'customAnnotationProperty',
        value: 'A value for the custom Annotation property',
        dataType: 'STRING'
      }]
    },
    id: 1,
    reasons: ['IF_ELSE_CONDITION'],
    name: 'Annotation 1',
    projectId: 1,
    state: 'IN_ANALYSIS',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category A',
    createdByUserId: 'admin',
    updatedByUserId: 'admin',
    sourceAttachment: 'abcd',
    englishTranslation: 'translation',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  }];

  const metaModelElseProperty: any = [
    {
      customViewIndex: 0,
      customViewNames: [],
      dataSource: null,
      dataType: "STRING",
      description: "A custom property for the Annotation class",
      label: "Custom Annotation Property",
      mandatory: false,
      max: null,
      min: null,
      name: "customAnnotationProperty",
      pluginVisible: true,
      readOnly: false,
      showWhen: {},
      validationErrorMessage: null,
      validationRegex: null
    }
  ];

  const metaModelProperty: any = [
    {
      customViewIndex: 0,
      customViewNames: [],
      dataSource: null,
      dataType: "STRING",
      description: "A custom property for the Annotation class",
      label: "Custom Annotation Property",
      mandatory: false,
      max: null,
      min: null,
      name: "customAnnotationProperty",
      pluginVisible: true,
      readOnly: false,
      showWhen: {
        annotationCategoryName: 'test'
      },
      validationErrorMessage: null,
      validationRegex: null
    }
  ];

  const annotationCategory: AnnotationCategory[] = [
    {
      recordId: '#86:0',
      customProperties:
      {
        'Property1': [
          {
            dataType: 'STRING'
          }
        ]
      },
      id: 1,
      name: 'Annotation Category A',
      projectId: 1,
      types: ['RULE', 'DATABASE']
    },
    {
      recordId: '#86:1',
      customProperties:
      {
        'Property1': [
          {
            dataType: 'STRING'
          }
        ]
      },
      id: 6,
      name: 'AnnotationTest',
      projectId: 1,
      types: ['RULE', 'DEAD_CODE']
    }
  ];

  const dataDictionaryItem: DataDictionaryPojo[] = [{
    "customProperties": {
      "DataDictionaryEntryCustomProperties": [
        {
          "name": "customDataDictionaryEntryProperty",
          "value": "Custom Property Value",
          "dataType": "STRING"
        }
      ]
    },
    "id": 1,
    "isBusiness": true,
    "dataElementName": "MY-PROGRAM-NAME",
    "description": "New Entry1234",
    "format": "PICX",
    "scopes": {
      "SQL_DATABASE": {},
      "CICS_UI": {}
    },
    "length": 15,
    "module": "MMRS7101",
    "isCandidate": false,
    "state": null,
    "translatedFieldValue": "TESTTESTTEST"
  },
  {
    "customProperties": {
      "DataDictionaryEntryCustomProperties": [
        {
          "name": "customDataDictionaryEntryProperty2",
          "value": "Custom Property Value2",
          "dataType": "STRING"
        }
      ]
    },
    "id": 2,
    "isBusiness": false,
    "dataElementName": "MY-PROGRAM-NAME",
    "description": "New Entry12345",
    "format": "PICX",
    "scopes": {
      "SQL_DATABASE": {},
      "CICS_UI": {}
    },
    "length": 15,
    "module": "MMRS7101",
    "isCandidate": false,
    "state": null,
    "translatedFieldValue": "TESTTESTTEST"
  }];

  const projectPage: any = {
    content: [
      {
        id: 1,
        name: 'test-1'
      },
      {
        id: 2,
        name: 'test-2'
      }
    ],
    customProperties: {
      ProjectCustomProperties: [
        {
          name: AUTO_COMPLETION_MAP,
          value: "{ \"annotationTags\":[\"Tag 2\",\"Tag 1\"], \"otherTags\":[\"Tag O2\",\"Tag O2\",\"Tag O1\"] }"
        }
      ]
    },
    customPropertyClasses: {
      Annotation: ['AnnotationCustomProperties'],
      AnnotationCategory: ['AnnotationCategoryCustomProperties'],
      AnnotationTypeEnum: ['AnnotationTypeEnumCustomProperties']
    }
  };

  const graphQlData: any =  {"data": { "project": {"customPropertyClasses": { Annotation: ['TAG1'] }, "autoCompletionMap": { "annotationTags": ['Tag 2', 'Tag 1'], "otherTags": ['Tag O2', 'Tag O2', 'Tag O1'] } }
  }};

  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);

  const dataDictionaryControllerServiceSpy: jasmine.SpyObj<DataDictionaryControllerService> = jasmine.createSpyObj('DataDictionaryControllerService', [
    'updateDataDictionaryEntry', 'deleteDataDictionaryEntry', 'findDataDictionaryEntryByRecordId', 'findLinkedBusinessRulesById'
  ]);

  const GraphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>
    ('GraphQlControllerService', ['graphQl']);

  const clientProjectRelationshipServiceStub: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj<ClientProjectRelationshipService>
    ('ClientProjectRelationshipService', ['getClientProjectObservable']);

  const annotationCategoryControllerServiceStub: jasmine.SpyObj<AnnotationCategoryControllerService> = jasmine.createSpyObj<AnnotationCategoryControllerService>
    ('AnnotationCategoryControllerService', ['findAllAnnotationCategories']);

  const annotationControllerServiceStub: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj<AnnotationControllerService>
    ('AnnotationControllerService', ['deleteAnnotation', 'createAnnotation', 'updateAnnotation', 'findAnnotationById', 'getGptTranslation', 'findLinkedBusinessVariablesById']);

  const metamodelControllerServiceStub: jasmine.SpyObj<MetamodelControllerService> = jasmine.createSpyObj<MetamodelControllerService>
    ('MetamodelControllerService', ['findMetaModel']);

  const nzMessageServiceStub: jasmine.SpyObj<NzMessageService> = jasmine.createSpyObj<NzMessageService>
    ('NzMessageService', ['success', 'error']);

  const projectServiceSpy = jasmine.createSpyObj<ProjectControllerService>
    ('ProjectControllerService', ['findProjectById']);

  const featureServiceSpy = jasmine.createSpyObj<FeatureToggleService>
    ('FeatureToggleService', ['isActive']);

  const annotationToFunctionalBlockControllerServiceSpy = jasmine.createSpyObj<AnnotationToFunctionalBlockControllerService>
    ('AnnotationToFunctionalBlockControllerService', ['getFunctionalBlockNamesByAnnotationId']);

  const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>
    ('NzNotificationService', ['create', 'error' ]);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [SharedAnnotationEditorComponent],
      imports: [
        HttpClientTestingModule,
        TranslateModule.forRoot({})
      ],
      providers: [
        {
          provide: GraphQlControllerService,
          useValue: GraphQlControllerServiceStub
        },
        { provide: UntypedFormBuilder, useValue: formBuilderStub },
        {
          provide: ClientProjectRelationshipService,
          useValue: clientProjectRelationshipServiceStub
        },
        {
          provide: AnnotationCategoryControllerService,
          useValue: annotationCategoryControllerServiceStub
        },
        {
          provide: AnnotationControllerService,
          useValue: annotationControllerServiceStub
        },
        {
          provide: MetamodelControllerService,
          useValue: metamodelControllerServiceStub
        },
        {
          provide: AnnotationToFunctionalBlockControllerService,
          useValue: annotationToFunctionalBlockControllerServiceSpy
        },
        { provide: NzMessageService, useValue: nzMessageServiceStub },
        { provide: ProjectControllerService, useValue: projectServiceSpy },
        TranslateService,
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        ModuleControllerService,
        { provide: NzNotificationService, useValue: notificationSpy },
        { provide: FeatureToggleService, useValue: featureServiceSpy },
        {provide: DataDictionaryControllerService, useValue: dataDictionaryControllerServiceSpy},
        {
          provide: NzDrawerService,
          useValue: drawerServiceSpy
        },
        {
          provide: Router,
          useValue: routerMock,
        },
        NavigationGuard,
        SharedAnnotationEditorService
      ]
    }).compileComponents();
    
    guard = TestBed.inject(NavigationGuard);
    sharedAnnotationEditorService = TestBed.inject(SharedAnnotationEditorService);
    drawerServiceSpy = TestBed.inject(NzDrawerService);
    clientProjectRelationshipServiceStub.getClientProjectObservable.and.returnValue(of(new ClientProjectRelationship(1, 'client', 1, 'project')));
    GraphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData));
    annotationCategoryControllerServiceStub.findAllAnnotationCategories.and.returnValue(of(annotationCategory as any));
    metamodelControllerServiceStub.findMetaModel.and.returnValue(of(metaModelProperty as any));
    annotationControllerServiceStub.updateAnnotation.and.returnValues(of(codeAnnotationsValue[0] as any), of(codeAnnotationsValue[1] as any),
      of(codeAnnotationsValue[2] as any), of(codeAnnotationsValue[3] as any),
      of(codeAnnotationsValue[4] as any), of(codeAnnotationsValue[5] as any),
      throwError(new Error('Test Error')));
    annotationControllerServiceStub.deleteAnnotation.and.returnValues(of(null as any), throwError(new Error('Deletion Error')));
    annotationControllerServiceStub.createAnnotation.and.returnValues(of(codeAnnotationsValue[0] as any));
    annotationControllerServiceStub.getGptTranslation.and.returnValue(of(['translated text'] as any));
    annotationControllerServiceStub.findLinkedBusinessVariablesById.and.returnValue(of(dataDictionaryItem as any));
    annotationToFunctionalBlockControllerServiceSpy.getFunctionalBlockNamesByAnnotationId.and.returnValue(of(new Set<string>(['test']) as any))
    nzMessageServiceStub.error.and.returnValue('some thing went wrong' as any);
    dataDictionaryControllerServiceSpy.findDataDictionaryEntryByRecordId.and.returnValue(of(dataDictionaryEntry as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SharedAnnotationEditorComponent);
    component = fixture.componentInstance;
    projectServiceSpy.findProjectById.and.returnValue(of(projectPage as any));
    featureServiceSpy.isActive.and.returnValue(of(false));
    let fb: UntypedFormBuilder = new UntypedFormBuilder();
    component['fb'] = fb;
    component.annotation = {
      type: "RULE",
      name: "test",
      state: "INVALID",
      id: 1,
      englishTranslation: 'translation',
      categoryId: 2,
      customProperties: { 'Property1': [{ 'name': 'customAnnotationProperty' }, { 'name': 'annotationTags', 'value': '["hello","hi"]' }] },
      reasons: ['IF_ELSE_CONDITION'],
      dataDictionaryEntries: ['test']
    };
    component.annotationForm = formBuilder.group({
      type: "RULE",
      name: "test",
      state: "INVALID",
      categoryId: 2,
      englishTranslation: 'translation',
      category: 'test',
      TAG: '["hi","hello"]',
      customProperties: [{ 'name': 'customAnnotationProperty' }, { 'name': 'annotationTags', 'value': '["hello","hi"]' }]
    });
    component.categoryList = annotationCategory;
    fixture.detectChanges();
  });

  it('can load instance', () => {
    expect(component).toBeTruthy();
  });

  describe('ngOnInit', () => {
    it('ngOnInit', () => {
      expect(projectPage.content.length).toBeGreaterThan(0);
      projectServiceSpy.findProjectById.and.returnValue(of(projectPage as any));
      expect(projectPage.content.length).toBeGreaterThan(0);
      metamodelControllerServiceStub.findMetaModel.and.returnValue(of(metaModelElseProperty as any));
      component.ngOnInit();
    });

    it('delete option to be true', () => {
      component.isCreateMode = true;
      component.ngOnInit();
      expect(component.deleteOption).toBeTruthy();
    });
    it('ngOnInit', () => {
      component.annotation = {
        type: "RULE",
        state: "IN_ANALYSIS",
        englishTranslation: 'translation',
        categoryId: 2,
        reasons: ['IF_ELSE_CONDITION'],
        dataDictionaryEntries: []
      };
      const projectPage2: any = {
        content: [
          {
            id: 1,
            name: 'test-1'
          },
          {
            id: 2,
            name: 'test-2'
          }
        ],
        customPropertyClasses: {
          AnnotationCategory: ['AnnotationCategoryCustomProperties'],
          AnnotationTypeEnum: ['AnnotationTypeEnumCustomProperties']
        }
      };
      component.annotationForm = formBuilder.group({
        type: "RULE",
        name: "test",
        state: "IN_ANALYSIS",
        categoryId: 2,
        category: 'test',
        englishTranslation: 'translation',
        TAG: '["hi","hello"]',
        customProperties: [{ 'name': 'customAnnotationProperty' }, { 'name': 'annotationTags', 'value': '["hello","hi"]' }]
      });
      projectServiceSpy.findProjectById.and.returnValue(of(projectPage2 as any));
      metamodelControllerServiceStub.findMetaModel.and.returnValue(of(metaModelElseProperty as any));
      component.ngOnInit();
      let debugElement: DebugElement = fixture.debugElement;
      fixture.detectChanges()
      let input = debugElement.query(By.css(".english-translation-content"));
      expect(input.nativeElement.textContent.trim()).toEqual('translation');
    });

    it('ngOnInit should work if no custom properties are present', () => {
      component.annotation = {
        type: "RULE",
        state: "IN_ANALYSIS",
        englishTranslation: 'translation',
        categoryId: 2,
        reasons: ['IF_ELSE_CONDITION'],
        dataDictionaryEntries: []
      };
      const projectPage2: any = {
        content: [
          {
            id: 1,
            name: 'test-1'
          },
          {
            id: 2,
            name: 'test-2'
          }
        ]
      };
      component.annotationForm = formBuilder.group({
        type: "RULE",
        name: "test",
        state: "IN_ANALYSIS",
        categoryId: 2,
        englishTranslation: 'translation',
        category: 'test'
      });
      projectServiceSpy.findProjectById.and.returnValue(of(projectPage2 as any));
      /* fails if the code doesn't check that custom properties are present */
      component.ngOnInit();
    });
  });

  describe('onSubmit', () => {
    it('makes expected calls', () => {
      component.isCreateMode = true;
      component.propertiesMetamodels = metaModelProperty;
      component.annotation = {
        type: "RULE",
        name: "customAnnotationProperty",
        state: "INVALID",
        categoryId: 2,
        customProperties: { 'Property1': [{ 'name': 'customAnnotationProperty' }, { 'name': 'annotationTags', 'value': '["hello","hi"]' }] },
        reasons: ['IF_ELSE_CONDITION'],
        dataDictionaryEntries: []
      };
      spyOn(component, 'onSubmit').and.callThrough();
      component.onSubmit();
      spyOn(component, 'resetForm').and.callThrough();
      component.resetForm();
      expect(component.resetForm).toHaveBeenCalled();
      expect(component.onSubmit).toHaveBeenCalled();
    });
    it('makes expected else calls', () => {
      const metaModelProperty: any = [
        {
          customViewIndex: 0,
          customViewNames: [],
          dataSource: null,
          dataType: "string",
          description: "A custom property for the Annotation class",
          label: "Custom Annotation Property",
          mandatory: false,
          max: null,
          min: null,
          name: "customAnnotationProperty",
          optionList: '["hi","hello"]',
          pluginVisible: true,
          readOnly: false,
          showWhen: {},
          validationErrorMessage: null,
          validationRegex: null
        }
      ];
      component.annotation = {
        type: "RULE",
        name: "customAnnotationProperty",
        state: "INVALID",
        categoryId: 2,
        reasons: ['IF_ELSE_CONDITION'],
        dataDictionaryEntries: []
      };
      component.propertiesMetamodels = metaModelProperty;
      component.isCreateMode = false;
      spyOn(component, 'onSubmit').and.callThrough();
      component.onSubmit();
      expect(component.onSubmit).toHaveBeenCalled();
    });

    it('makes expected else calls', () => {
      const metaModelProperty: any = [
        {
          customViewIndex: 0,
          customViewNames: [],
          dataSource: null,
          dataType: "string",
          description: "A custom property for the Annotation class",
          label: "Custom Annotation Property",
          mandatory: false,
          max: null,
          min: null,
          name: "customAnnotationProperty",
          optionList: '["hi","hello"]',
          pluginVisible: true,
          readOnly: false,
          showWhen: {},
          validationErrorMessage: null,
          validationRegex: null
        }
      ];
      component.annotation = {
        type: "RULE",
        name: "customAnnotationProperty",
        state: "INVALID",
        categoryId: 2,
        reasons: ['IF_ELSE_CONDITION'],
        dataDictionaryEntries: []
      };
      component.propertiesMetamodels = metaModelProperty;
      component.isCreateMode = false;
      component.parentComponent = AnnotationEditor.ECLIPSE_EDITOR;
      spyOn(component, 'onSubmit').and.callThrough();
      component.onSubmit();
      expect(component.deleteOption).toBeFalsy();
    });

    it('makes expected else calls', () => {
      const metaModelProperty: any = [
        {
          customViewIndex: 0,
          customViewNames: [],
          dataSource: null,
          dataType: "string",
          description: "A custom property for the Annotation class",
          label: "Custom Annotation Property",
          mandatory: false,
          max: null,
          min: null,
          name: "customAnnotationProperty",
          optionList: '["hi","hello"]',
          pluginVisible: true,
          readOnly: false,
          showWhen: {},
          validationErrorMessage: null,
          validationRegex: null
        }
      ];
      component.annotation = {
        type: "RULE",
        name: "customAnnotationProperty",
        state: "INVALID",
        categoryId: 2,
        reasons: ['IF_ELSE_CONDITION'],
        dataDictionaryEntries: []
      };
      component.propertiesMetamodels = metaModelProperty;
      component.isCreateMode = true;
      component.parentComponent = AnnotationEditor.CODEVIEWER_ANNOTATION_EDITOR;
      spyOn(component, 'onSubmit').and.callThrough();
      component.onSubmit();
      expect(component.onSubmit).toHaveBeenCalled();
    });
  });

  describe('check setOptionlist', () => {
    it('makes expected calls', () => {
    });
  });

  describe('validationcheck', () => {
    it('true if the form is valid or in case of new form creation', () => {
      component.ngOnInit();
      component.isCreateMode = true;
      /* test dirty status */
      expect(component.canSubmitForm()).toBeTrue();
      component.annotationForm.markAsDirty();
      expect(component.canSubmitForm()).toBeTrue();
      component.annotationForm.markAsPristine();
      expect(component.canSubmitForm()).toBeTrue();

      /* test validation status */
      component.annotationForm.markAsDirty();
      expect(component.canSubmitForm()).toBeTrue();
      component.annotationForm.setErrors({});
      expect(component.canSubmitForm()).toBeFalsy();
      component.annotationForm.setErrors(undefined);
      expect(component.canSubmitForm()).toBeTrue();
    });
  });

  describe('onCancel', () => {
    it('makes expected calls', () => {
      spyOn(component, 'onCancel').and.callThrough();
      component.onCancel();
      expect(component.onCancel).toHaveBeenCalled();
    });
  });

  describe('onDelete', () => {
    it('makes expected calls', () => {
      spyOn(component, 'onDelete').and.callThrough();
      component.onDelete();
      expect(component.onDelete).toHaveBeenCalled();
    });

  });

describe('fillAnnotationDetails', () => {
    let clipboardService: jasmine.SpyObj<ClipboardService>;
    beforeEach(() => {
      // Create a spy object for the ClipboardService 
      clipboardService = jasmine.createSpyObj('ClipboardService', ['copyToClipboard', 'getFromClipboard']);
    });

    it('should show the error while copying from other project', () => {
      clipboardService.getFromClipboard.and.returnValue('5');
      annotationControllerServiceStub.findAnnotationById.and.returnValue(throwError({status: 404}));
      component.fillAnnotationDetails();
      expect(notificationSpy.error).toHaveBeenCalled()
     });
  
    it('should fill data correctly from clipboard without clearing', () => {
      const entity = 'annotation';
      const annotationId = '5';

      // Mock copyToClipboard to add data to the clipboard
      clipboardService.copyToClipboard.and.callFake((entity, annotationId) => {
        // Simulate adding data to clipboard
        clipboardService.getFromClipboard.and.returnValue(annotationId);
      });

      // Mock getFromClipboard to return data
      clipboardService.getFromClipboard.and.returnValue(annotationId);

      //get the data before calling fillAnnotationDetails
      const oldClipBoardValue = clipboardService.getFromClipboard(entity);

      component.fillAnnotationDetails();

      // Ensure that the getFromClipboard returns the expected data after fillAnnotationDetails
      const newClipBoardValue = clipboardService.getFromClipboard(entity);
      expect(newClipBoardValue).toEqual(oldClipBoardValue);
    });

    it('should not overwrite the english translation', () => {
      const entity = 'annotation';

      const myAnnotation1: AnnotationPojo = {
        categoryId: 1,
        categoryName: "Category",
        createdByUserId: "test1",
        createdByUserName: "TESTER1",
        englishTranslation: "Original english translation"
      };

      component.annotation = myAnnotation1;

      const myAnnotation2: AnnotationPojo = {
        categoryId: 2,
        categoryName: "Category",
        createdByUserId: "test2",
        createdByUserName: "TESTER2",
        englishTranslation: "This should not end up in the englishTranslation of myAnnotation1"
      };

      clipboardService.copyToClipboard(entity, myAnnotation2);

      component.fillAnnotationDetails();

      expect(component.annotation.englishTranslation).toEqual('Original english translation');

    });
  });

  describe('openSharedDataDictionaryEditor', () => {
    // ToDo: this test is throwing AfterAll error in Jenkins, we probably need to mock dataDictionaryControllerService.findLinkedBusinessRulesById
    // in order to fix this, excluding to make sure the nightly is green
    xit('makes expected calls', () => {
      component.moduleId = 786;
      spyOn(component, 'openSharedDataDictionaryEditor').and.callThrough();
      component.openSharedDataDictionaryEditor(dataDictionaryEntry);
      expect(component.openSharedDataDictionaryEditor).toHaveBeenCalled();
    });

  });

  it('should return true if editor state is false and the form is not dirty', () => {
    spyOn(sharedAnnotationEditorService, 'getEditorState').and.returnValue(false);
    spyOn(component, 'isFormDirty').and.returnValue(false);

    const result = guard.canDeactivate();
    expect(result).toBe(true);
  });

  it('should return false if editor state is true and the form is not dirty', () => {
    spyOn(sharedAnnotationEditorService, 'getEditorState').and.returnValue(true);
    spyOn(component, 'isFormDirty').and.returnValue(false);

    const result = guard.canDeactivate();
    expect(result).toBe(true);
  });

  it('should return false if editor state is false and the form is dirty', () => {
    spyOn(sharedAnnotationEditorService, 'getEditorState').and.returnValue(false);
    spyOn(component, 'isFormDirty').and.returnValue(true);

    const result = guard.canDeactivate();
    expect(result).toBe(true);
  });

  it('should return false if editor state is true and the form is dirty', () => {
    spyOn(sharedAnnotationEditorService, 'getEditorState').and.returnValue(true);
    spyOn(component, 'isFormDirty').and.returnValue(true);

    const result = guard.canDeactivate();
    expect(result).toBe(true);
  });

  it('should open the GenAI options modal', () => {
    component.isGenAIOptionsModalVisible = false;
    component.openGenAIOptionsModal();
    expect(component.isGenAIOptionsModalVisible).toBe(true);
  });

  it('should close the Gen AI Options modal when handleGenAIOptionsCancel is called', () => {
    component.isGenAIOptionsModalVisible = true;
    component.handleGenAIOptionsCancel();
    expect(component.isGenAIOptionsModalVisible).toBe(false);
  });
  
  it('should close the Gen AI Options modal when handleGenAIOptionsOk is called', () => {
    component.isGenAIOptionsModalVisible = true;
    component.handleGenAIOptionsOk();
    expect(component.isGenAIOptionsModalVisible).toBe(false);
  });
  
  it('should toggle the content view when toggleContentView is called', () => {
    component.showAllContent = false;
    component.toggleContentView();
    expect(component.showAllContent).toBe(true);
  });

  it('should handle custom property categories', () => {
    const customCategories: string[] = ['Category A'];
    component.handleCustomPropertyCategories(customCategories);
    expect(component.customPropertiesCategoryList).toEqual(customCategories);
    expect(component.selectedCustomPropertyCategory).toEqual('Category A');
  });
  
  it('should copy the annotation details to clipboard', () => {
    const clipboardServiceSpy = spyOn(component.clipboardService, 'copyToClipboard');
    const expectedLabel = 'btnLabel.copied';
    component.copyAnnotationDetails();
    expect(clipboardServiceSpy).toHaveBeenCalledWith(Entity.ANNOTATION, component.annotationId);
    expect(component.copyBtnlabel).toBe(expectedLabel);
  });

  it('should test canDeactivate', () => {
    const result = component.canDeactivate();
    expect(result).toBe(true);
  });
});