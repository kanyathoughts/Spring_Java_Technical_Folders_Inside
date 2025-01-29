import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { UntypedFormBuilder } from '@angular/forms';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { of, throwError } from 'rxjs';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { SharedDataDictionaryEditorComponent } from './shared-data-dictionary-editor.component';
import { RouterTestingModule } from '@angular/router/testing';
import { NzMessageService } from 'ng-zorro-antd/message';
import { Overlay } from '@angular/cdk/overlay';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { AnnotationElementData } from '@app/modules/mining-code-viewer/mining-code-viewer';
import { AnnotationControllerService, AnnotationPojo, DataDictionaryControllerService, DataDictionaryPojo, DataPointControllerService, MiningDataPointDefinitionWithPath, ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { WindowToken } from '@app/core/utils/window';
import { CodeAnnotationEditorComponent } from '@app/modules/mining-code-viewer/code-annotation/code-annotation-editor.component';
import { CodeAnnotationEditorMock } from '@app/core/mocks/code-annotation-editor.mock';

describe('SharedDataDictionaryEditorComponent', () => {
  let component: SharedDataDictionaryEditorComponent;
  let fixture: ComponentFixture<SharedDataDictionaryEditorComponent>;
  const formBuilder: UntypedFormBuilder = new UntypedFormBuilder();
  let drawerServiceSpy: NzDrawerService;
  const dataDictionaryValue = {
    recordId: '#203:117',
    id: 2,
    dataElementName: 'MY-PROGRAM-NAME',
    description: 'This is an english description of the data element name MY-PROGRAM-NAME',
    format: 'PICX',
    scopes: {'scope1': {'name': 'SQLDATABASE'}, 'scope2': {'name': 'CICSUI'}},
    length: 15,
    createdByUserId: 'admin',
    reference: {
      recordId: '#267:117',
      id: 7,
      relationship: 'HAS_DATA_DICTIONARY_ENTRY',
      fromId: '#131:600',
      toId: '#203:117',
      fromName: 'MMRS7101',
      fromModuleLocation: {
        recordId: '#140:352',
        customProperties: {},
        offset: 1005,
        length: 15
      },
      toModuleLocation: {
        customProperties: {},
        offset: 0,
        length: 0
      },
      attributes: {},
    }
  };
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
    modulePath: 'PRG1',
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
    categoryName: 'BUSINESS_RULE',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  }];
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
    description: 'A test copy',
    sourceMetrics: {
      codeLines: null,
      commentLines: null,
      complexityMcCabe: null,
    },
    content: null,
    sourceCodeAvailable: false
  };

  const dataDictionaryItem: DataDictionaryPojo = {
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
    "dataElementName": "MY-PROGRAM-NAME",
    "description": "New Entry1234",
    "format": "PICX",
    "scopes": {
      "SQL_DATABASE": {
        "accessType": 'READ'
      },
      "CICS_UI": {}
    },
    "length": 15,
    "module": "MMRS7101",
    "isCandidate": false,
    "state": null,
    "translatedFieldValue": "TESTTESTTEST"
  };

  const graphQlResponse = {
    "data": {
      "taxonomyCategories": {
        "categories": [
          {
            "id": 4,
            "name": "Business Taxonomies",
            "types": [
              {
                "name": "BusinessSubsystem",
                "terms": [
                  {
                    "id": 3,
                    "name": "ARB100"
                  }
                ]
              },
              {
                "name": "DataDomain",
                "terms": [
                  {
                    "id": 1,
                    "name": "Employee domain"
                  }
                ]
              }
            ]
          },
          {
            "id": 3,
            "name": "Technical Taxonomies",
            "types": [
              {
                "name": "DB Access",
                "terms": [
                  {
                    "id": 2,
                    "name": "Read"
                  }
                ]
              },
              {
                "name": "File Access",
                "terms": [
                  {
                    "id": 1,
                    "name": "Write"
                  }
                ]
              }
            ]
          }
        ]
      }
    }
  }
  const linkedAnnotations = [
    {
        "uid": "0ab1d417-52bd-4bd0-894a-132399235f85",
        "project": "ec1dd6e1-6de8-4b17-b0b6-dc25b945a4fe",
        "projectId": 4,
        "name": "Data Validation Rule Candidate [System identified]",
        "state": "CANDIDATE",
        "type": "RULE",
        "categoryId": 4,
        "categoryName": "Validation Rule",
        "createdByUserId": "system_user",
        "updatedByUserId": null as any,
        "module": "642adcf5-3e32-4aee-a215-f76f3c49ba53",
        "moduleId": 3033,
        "moduleName": "COBOL_UNISYS_CLASS-NAME",
        "modulePath": "src/cobol/programs/cobol_unisys_CLASS-NAME.cbl",
        "location": {
            "offset": 2559,
            "length": 172
        },
        "source": "f03f55cd-1535-4e0b-bfed-bd4cc1c0b791",
        "sourceAttachment": "IF ABC-NAME-CHAR-1 IS CLASS-NAME DOLLAR-SIGN\n      *          THEN\n                       NEXT SENTENCE\n                 ELSE\n                   MOVE \"NO\" TO VALID-NAME-IND",
        "englishTranslation": "If\n\tABC-NAME-CHAR-1 IS CLASS-NAME <i>DOLLAR-SIGN</i>\nThen\n\tNEXT SENTENCE\nOtherwise\n\tmove \"NO\" to <i>VALID-NAME-IND</i>",
        "reasons": [] as any,
        "dataDictionaryEntries": [
            "e43d92be-af28-4d5a-88da-0ec78fe3873d"
        ],
        "customProperties": {},
        "updatedByUserName": null as any,
        "createdByUserName": null as any,
        "length": 172,
        "offset": 2559,
        "id": 227
    },
    {
        "uid": "37d32be5-1b82-46e3-a545-f4885240e4f9",
        "project": "ec1dd6e1-6de8-4b17-b0b6-dc25b945a4fe",
        "projectId": 4,
        "name": "Data Validation Rule Candidate [System identified]",
        "state": "CANDIDATE",
        "type": "RULE",
        "categoryId": 4,
        "categoryName": "Business Rule",
        "createdByUserId": "system_user",
        "updatedByUserId": null,
        "module": "642adcf5-3e32-4aee-a215-f76f3c49ba53",
        "moduleId": 3033,
        "moduleName": "COBOL_UNISYS_CLASS-NAME",
        "modulePath": "src/cobol/programs/cobol_unisys_CLASS-NAME.cbl",
        "location": {
            "offset": 2748,
            "length": 176
        },
        "source": "f03f55cd-1535-4e0b-bfed-bd4cc1c0b791",
        "sourceAttachment": "IF ABC-NAME-CHAR-1 IS NOT CLASS-NAME DOLLAR-SIGN\n      *          THEN\n                       NEXT SENTENCE\n                 ELSE\n                   MOVE \"NO\" TO VALID-NAME-IND",
        "englishTranslation": "If\n\tABC-NAME-CHAR-1 IS Not CLASS-NAME <i>DOLLAR-SIGN</i>\nThen\n\tNEXT SENTENCE\nOtherwise\n\tmove \"NO\" to <i>VALID-NAME-IND</i>",
        "reasons": [],
        "dataDictionaryEntries": [
            "e43d92be-af28-4d5a-88da-0ec78fe3873d"
        ],
        "customProperties": {},
        "updatedByUserName": null,
        "createdByUserName": null,
        "length": 176,
        "offset": 2748,
        "id": 228
    }
];

const dataPoints: MiningDataPointDefinitionWithPath[] = [
  {
    "name": "createdByUserId",
    "parentTypeName": "Annotation",
    "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
    "referenceTypeName": null,
    "parameters": [],
    "usages": new Set(["miningUi.annotationsTable"]),
    "usageAttributes": {
      "miningUi.annotationsTable": {
        "category": "Modifications"
      }
    },
    "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationV2"]),
    "displayName": "Created By",
    "description": "Name of the user who created the Annotaion",
    "path": "content.createdByUserId",
    "array": false,
    "id": "Annotation.createdByUserId",
    "nullable": true,
    "alias": false,
    "aliasFor": null
  },
  {
    "name": "content",
    "parentTypeName": "SourceAttachment",
    "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
    "referenceTypeName": null,
    "parameters": [],
    "usages": new Set(["miningUi.annotationsTable"]),
    "usageAttributes": {
      "miningUi.annotationsTable": {
        "category": "Base Data"
      }
    },
    "providedBy": new Set(["innowake.mining.data.model.springdata.SourceAttachmentV2"]),
    "displayName": "Source Code",
    "description": "Source Code attached to the Annotation",
    "path": "content.sourceAttachmentLink.content",
    "array": false,
    "id": "SourceAttachment.content",
    "nullable": true,
    "alias": false,
    "aliasFor": null
  },
  {
    "name": "stateLink",
    "parentTypeName": "Annotation",
    "scalarType": null,
    "referenceTypeName": "AnnotationState",
    "parameters": [],
    "usages": new Set(["miningUi.annotationsTable"]),
    "usageAttributes": {
      "miningUi.annotationsTable": {
        "category": "Base Data"
      }
    },
    "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationV2"]),
    "displayName": "State",
    "description": "State of the Annotation",
    "path": "content.stateLink",
    "array": false,
    "id": "Annotation.stateLink",
    "nullable": true,
    "alias": false,
    "aliasFor": null
  },
  {
    "name": "name",
    "parentTypeName": "AnnotationCategory",
    "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
    "referenceTypeName": null,
    "parameters": [],
    "usages": new Set([
      "miningUi.annotationsTable"
    ]),
    "usageAttributes": {
      "miningUi.annotationsTable": {
        "category": "Base Data"
      }
    },
    "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationCategoryV2"]),
    "displayName": "Category",
    "description": "Category of the Annotation",
    "path": "content.categoryLink.name",
    "array": false,
    "id": "AnnotationCategory.name",
    "nullable": true,
    "alias": false,
    "aliasFor": null
  }];
  const clientProjectRelationshipServiceStub: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj<ClientProjectRelationshipService>
    ('ClientProjectRelationshipService', ['getClientProjectObservable']);
  const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj('DataPointControllerService',
    ['getDataPointsForType']);
  const dataDictionaryControllerServiceSpy: jasmine.SpyObj<DataDictionaryControllerService> = jasmine.createSpyObj('DataDictionaryControllerService', [
    'updateDataDictionaryEntry', 'deleteDataDictionaryEntry', 'findLinkedBusinessVariables', 'createDataDictionaryEntry'
  ]);
  const annotationControllerServiceSpy: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj<AnnotationControllerService>
  ('AnnotationControllerService',
    ['updateAnnotation', 'findAnnotationById', 'deleteAnnotation']);
  const deepLinkServiceSpy: jasmine.SpyObj<DeepLinkService> = jasmine.createSpyObj('DeepLinkService', ['showModuleInEclipse', 'featureIsActive',]);
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
    ['findModuleById']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['destroy', 'updateConfig', 'close', 'getContentComponent']);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
  ['getSelectedDataPoints', 'resetTableColumnAndDataPoints', 'checkSelectedColumns', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange', 'handleFilters', 'getGraphQlParam', 'onPageLoad', 'getQueryParams', 'queryParamsDetails']);
  const graphqlServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService',
    ['graphQl']);
  let mockWindow: any;
  let openUrl = '';

  beforeEach(waitForAsync(() => {
    mockWindow = {
      get location() {
          return {
              href: 'http://localhost:8081/#/browse-modules/1/1/1/explore',
              hash: '#/browse-modules/1/1/1/explore'
          };
      },
      open: (sUrl: any) => {
          openUrl = sUrl;
      }
  };
  mockWindow.open.bind(mockWindow);
    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [SharedDataDictionaryEditorComponent],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({}),
        BrowserAnimationsModule
      ],
      providers: [
        NzMessageService,
        Overlay,
        TranslateService,
        NzNotificationService,
        NzDrawerService,
        {
          provide: ClientProjectRelationshipService,
          useValue: clientProjectRelationshipServiceStub,
        },
        {
          provide: DataDictionaryControllerService,
          useValue: dataDictionaryControllerServiceSpy
        },
        {
          provide: DeepLinkService,
          useValue: deepLinkServiceSpy
        },
        {
          provide: ModuleControllerService,
          useValue: moduleControllerServiceSpy
        },
        {
          provide: DataPointControllerService,
          useValue: dataPointControllerServiceSpy
        },
        { provide: AnnotationControllerService, useValue: annotationControllerServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        { provide: GraphQlControllerService, useValue: graphqlServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
          UntypedFormBuilder,
        { provide: CodeAnnotationEditorComponent, useClass: CodeAnnotationEditorMock }
      ]
    }).compileComponents();
    drawerServiceSpy = TestBed.inject(NzDrawerService);
    clientProjectRelationshipServiceStub.getClientProjectObservable.and.returnValue(of(new ClientProjectRelationship(1, 'client', 1, 'project')));
    dataDictionaryControllerServiceSpy.updateDataDictionaryEntry.and.returnValue(of(dataDictionaryValue as any));
    dataDictionaryControllerServiceSpy.createDataDictionaryEntry.and.returnValue(of(dataDictionaryValue as any));
    dataDictionaryControllerServiceSpy.deleteDataDictionaryEntry.and.returnValues(of(null as any), throwError(new Error('Deletion Error')));
    dataDictionaryControllerServiceSpy.findLinkedBusinessVariables.and.returnValue(of(linkedAnnotations as any));
    deepLinkServiceSpy.featureIsActive.and.returnValue(of(true));
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    annotationControllerServiceSpy.findAnnotationById.and.returnValues(of(codeAnnotationsValue[0] as any), throwError('TEST_ERROR'));
    moduleControllerServiceSpy.findModuleById.and.returnValue(of(moduleValue as any));
    annotationControllerServiceSpy.findAnnotationById.and.returnValues(of(codeAnnotationsValue[0] as any), throwError('TEST_ERROR'));
    graphqlServiceSpy.graphQl.and.returnValue(of(graphQlResponse as any));
    userCustomizableTableServiceSpy.checkSelectedColumns.and.returnValue((true as any));
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SharedDataDictionaryEditorComponent);
    component = fixture.componentInstance;
    let fb: UntypedFormBuilder = new UntypedFormBuilder();
    component['fb'] = fb;
    component.dataDictionaryItem = dataDictionaryItem;
    component.isNewEntry = true;
    component.codeAnnotationEditorItems = codeAnnotationsValue;
    component.dataDictionaryForm = formBuilder.group({
      ddDescription: 'Test Description'
    });
    fixture.detectChanges();
  });

  xit('can load instance', () => {
    expect(component).toBeTruthy();
  });

  describe('ngOnInit', () => {
    it('ngOnInit', () => {
      component.isCodeViewerScreen = true;
      component.ngOnInit();
      component.dataDictionaryItem.description = undefined;
      component.dataDictionaryItem.dataElementName = 'IOSCOPE';
      component.scope = 'SQL_DATABASE,CICS_UI';
      expect(component.showDeleteButton).toBe(false);
    });

    it('Tests onCancel method', () => {
      component.onCancel();
      expect(component.isCancel).toBeTruthy();
    });

    it('Tests onDelete method', () => {
      component.onDelete();
      expect(component.isDelete).toBeTruthy();
    });

    it('Tests onSubmit method', () => {
      component.isNewEntry = true;
      component.onSubmit();
      expect(component.dataDictionaryItem.description).toBeDefined();
    });

    it('Tests getReferenceValue method', () => {
      component.isBusinessRelatedOnInit = false;
      component.isNewEntry = false;
      component.onSubmit();
      component.dataDictionaryItem.isReferenced=true;
      expect(component.getReferenceValue()).toBe('sharedDataDictionaryEditor.yes');
      component.dataDictionaryItem.isReferenced=false;
      expect(component.getReferenceValue()).toBe('sharedDataDictionaryEditor.no');
      component.dataDictionaryItem.isReferenced=undefined;
      expect(component.getReferenceValue()).toBe('notAvailable');
    });

    it('Tests canSubmitForm method', () => {
      component.dataDictionaryForm = formBuilder.group({
        ddDescription: 'Test Description'
      });
      expect(component.canSubmitForm()).toBe(component.dataDictionaryForm.valid && component.dataDictionaryForm.dirty);
    });

    it('makes expected calls for type edit', () => {
      const annotaionElement: AnnotationElementData = {
        moduleId: 777,
        projectId: 1,
        typeLabel:'type',
        stateLabel: 'statelabel',
        modulePath: 'modulepath',
        annotation: codeAnnotationsValue[0],
        moduleName: 'string'
      };
      let drawer = drawerServiceSpy.create({});
      const spy = spyOn(drawerServiceSpy, 'create');
      spy.and.returnValue(drawer);
  
      const data= { type: 'edit', annotation: annotaionElement }
      spyOn(component, 'handleAnnotationSelection').and.callThrough();
      component.handleAnnotationSelection(data);
      expect(component.handleAnnotationSelection).toHaveBeenCalled();
    });
    
    it('makes expected calls for type Delete', () => {
      const annotaionElement: AnnotationElementData = {
        moduleId: 777,
        projectId: 1,
        typeLabel:'type',
        stateLabel: 'statelabel',
        modulePath: 'modulepath',
        annotation: codeAnnotationsValue[0],
        moduleName: 'string'
      };
      const data= { type: 'delete', annotation: annotaionElement }
      spyOn(component, 'handleAnnotationSelection').and.callThrough();
      component.handleAnnotationSelection(data);
      expect(component.handleAnnotationSelection).toHaveBeenCalled();
    });

    it('should open in eclipse', () => {
      //spyOn(component, 'openInEclipse').and.callThrough();
      const moduleValue={
        projectId: 1,
        path: ''
      }
      component.openInEclipse();
      expect(deepLinkServiceSpy.showModuleInEclipse).toHaveBeenCalledWith(moduleValue);
    });

    it('Test scope for creating new DDentry', () => {
      component.dataDictionaryItem.scopes = {};
      component.ngOnInit();
      expect(component.scope).toBe('notAvailable');
    });

    it('should have translated field', () => {
      component.ngOnInit();
      expect(component.dataDictionaryItem.translatedFieldValue).toBe("TESTTESTTEST");
      expect(component.dataDictionaryForm.get('ddTranslatedFieldName').value).toBe("TESTTESTTEST");
    });

    it('should call findLinkedBusinessVariables and handle isBusinessRelated true', () => {
      component.checkIsBusinessRelated(true);
      expect(dataDictionaryControllerServiceSpy.findLinkedBusinessVariables).toHaveBeenCalled();
      expect(modalServiceSpy.create).toHaveBeenCalled();
    });
  
    it('should call findLinkedBusinessVariables and handle isBusinessRelated false', () => {
      component.checkIsBusinessRelated(false);
      expect(dataDictionaryControllerServiceSpy.findLinkedBusinessVariables).toHaveBeenCalled();
      expect(modalServiceSpy.create).toHaveBeenCalled();
    });
  });

  describe('fetchURLParamsFromData', () => {
    it('should return queryParams with columns and filter', () => {
      component.dataPointsList = dataPoints;
      const ids = [1];
      const uniqueIdsKeys = 'linkedAnnotationIds-1m1u4v7318';
      const queryParams = (component as any).fetchURLParamsFromData(ids, uniqueIdsKeys);

      expect(queryParams.columns.length).toBeGreaterThan(0);
      expect(queryParams.filter.length).toBeGreaterThan(0);
      expect(queryParams.preFilter).toEqual(uniqueIdsKeys);
    });
  });
});