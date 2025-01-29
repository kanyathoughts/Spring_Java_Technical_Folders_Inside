import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MiningCodeViewerComponent } from './mining-code-viewer.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { HoverProviderService } from './mining-code-viewer-hover-provider';
import { NzMessageService } from 'ng-zorro-antd/message';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { of, throwError } from 'rxjs';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { BrowserAnimationsModule, NoopAnimationsModule } from '@angular/platform-browser/animations';
import { MonacoEditorMockComponent } from '@app/core/mocks/monaco-editor.mock';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor, HttpService } from '@app/core';
import { HttpClient } from '@angular/common/http';
import { ActivatedRoute } from '@angular/router';
import { LanguageProviderService } from '../../core/services/monaco-editor/language-provider.service';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ModuleBreadcrumbComponent } from '@app/shared/module-breadcrumb/module-breadcrumb.component';
import { LoaderDirective } from '@app/shared/loader/loader.directive';
import { CancelRequestOnNavigationInterceptor } from '@app/core/http/cancel-request-on-navigation.interceptor';
import { AnnotationToFunctionalBlockControllerService, AnnotationCategoryControllerService, AnnotationControllerService, AnnotationPojo, DataDictionaryControllerService, DataDictionaryPojo, DataFieldFormat, FeatureControllerService, IoControllerService, MetamodelControllerService, ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';
import { SetSerializationInterceptor } from '@app/core/http/set-serialization.interceptor';
import { WindowToken } from '@app/core/utils/window';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { SharedAnnotationEditorComponent } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.component';
import { CodeAnnotationEditorComponent } from './code-annotation/code-annotation-editor.component';
import { CodeAnnotationEditorMock } from '@app/core/mocks/code-annotation-editor.mock';

describe('MiningCodeViewerComponent', () => {
  let component: MiningCodeViewerComponent;
  let fixture: ComponentFixture<MiningCodeViewerComponent>;

  const moduleSourceCode = `
        DISPLAY MY-PROGRAM-NAME
                'TRUNC(BIN/STD): read TRUNC-TEST='
                TRUNC-TEST-DISP
        IF TRUNC-TEST = 10
        DISPLAY MY-PROGRAM-NAME 'TRUNC=BIN is active'
        ELSE
        DISPLAY MY-PROGRAM-NAME 'TRUNC=STD is active'
        END-IF`;

  const moduleValue: ModulePojo[] = [
    {
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
      content: moduleSourceCode
    },
    {
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
      technology: 'ASSEMBLER',
      type: 'COPYBOOK',
      storage: 'FILE',
      identification: 'IDENTIFIED',
      origin: 'CUSTOM',
      info: null,
      description: 'A test copy',
      content: moduleSourceCode
    }
  ];

  const codeAnnotationsValue: AnnotationPojo[] = [
    {
      uid: '#172:234',
      customProperties: {
        'Property1': [{
          name: 'customAnnotationProperty',
          value: 'A value for the custom Annotation property',
          dataType: 'STRING'
        }, {
          name: 'referencedTables',
          value: null,
          dataType: 'LINKLIST'
        }, {
          name: 'dependentAnnotations',
          value: '[#173:234, #174:234]',
          dataType: 'LINKLIST'
        }, {
          name: 'customMetaInfo',
          value: 'some custom meta value',
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
      updatedByUserId: null,
      sourceAttachment: 'abcd',
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
        }, {
          name: 'referencedTables',
          value: null,
          dataType: 'LINKLIST'
        }, {
          name: 'dependentAnnotations',
          value: '[#173:234, #174:234]',
          dataType: 'LINKLIST'
        }, {
          name: 'customMetaInfo',
          value: 'some custom meta value',
          dataType: 'STRING'
        }]
      },
      id: 2,
      name: 'Annotation 1',
      projectId: 1,
      state: 'CANDIDATE',
      type: 'RULE',
      categoryId: 1,
      categoryName: 'Annotation Category A',
      createdByUserId: 'admin',
      updatedByUserId: null,
      sourceAttachment: 'abcd',
      moduleName: 'PRG1',
      location: {
        offset: 101,
        length: 5
      }
    }
  ];
  const validationResponse1: DataFieldFormat = {
    "fieldName": "",
    "moduleId": 0,
    "location": {
        "offset": null,
        "length": null
    },
    "languageType": "Group",
    "byteLength": 1,
    "dataDictionaryEntryId": null,
    "group": true
}

  const dataDictionaryValue: DataDictionaryPojo[] = [{
    uid: '#203:117',
    customProperties: {
      'Property1': [{
        name: 'customDataDictionaryEntryProperty',
        value: null,
        dataType: 'STRING'
      }]
    },
    id: 1,
    dataElementName: 'MY-PROGRAM-NAME',
    description: 'This is an english description of the data element name MY-PROGRAM-NAME',
    format: 'PICX',
    scopes: {'scope1': {'name': 'SQLDATABASE'}, 'scope2': {'name': 'CICSUI'}},
    length: 10,
    createdByUserId: 'admin',
    name: 'MMRS7101',
    location: {
      offset: 2,
      length: 15
    }
  }];

  const graphQlResponse = {
    "data": {
      "annotations": {
        "totalElements": 5
      }
    }
  };

  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService', [
    'findModuleById',
    'findAnnotationsForModule',
    'hasAstNodes',
    'storeAstNodes',
    'getDataLineageAvailableForModule'
  ]);

  const annotationControllerServiceSpy: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj('AnnotationControllerService', [
    'createAnnotation',
    'updateAnnotation',
    'deleteAnnotation'
  ]);

  const ioControllerServiceSpy: jasmine.SpyObj<IoControllerService> = jasmine.createSpyObj<IoControllerService>('IoControllerService',
  ['getExportFormats']);

  const dataDictionaryControllerServiceSpy: jasmine.SpyObj<DataDictionaryControllerService> = jasmine.createSpyObj('DataDictionaryControllerService', [
    'findAllDataDictionaryEntries',
    'createDataDictionaryEntry',
    'deleteDataDictionaryEntry',
    'getFormatIfSelectionIsValid',
    'getFormatIfSelectionIsValid1'
  ]);

  const tokenServiceSpy: jasmine.SpyObj<OauthtokenService> = jasmine.createSpyObj('OauthtokenService', [
    'getUsername'
  ]);

  let languageServiceSpy: jasmine.SpyObj<any> = jasmine.createSpyObj('LanguageProviderService', [
    'injectCSS'
  ]);

  const annotationCategoryControllerServiceStub: jasmine.SpyObj<AnnotationCategoryControllerService> = jasmine.createSpyObj<AnnotationCategoryControllerService>
    ('AnnotationCategoryControllerService', ['findAllAnnotationCategories']);

  const metamodelControllerServiceStub: jasmine.SpyObj<MetamodelControllerService> = jasmine.createSpyObj<MetamodelControllerService>
    ('MetamodelControllerService', ['findMetaModel']);

  const GraphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>
    ('GraphQlControllerService', ['graphQl']);


  const typeArr = [
    { key: 'DATABASE', value: 'database' },
    { key: 'DEAD_CODE', value: 'dead-code' },
    { key: 'EXCLUDE', value: 'exclude' },
    { key: 'RULE', value: 'rule' }
  ];

  const childComponent: jasmine.SpyObj<SharedAnnotationEditorComponent> = jasmine.createSpyObj('SharedAnnotationEditorComponent', [
    'isSave',
    'isDelete',
    'resetForm',
    'isClose'
  ]);

  const annotationToFunctionalBlockControllerServiceSpy = jasmine.createSpyObj<AnnotationToFunctionalBlockControllerService>
  ('AnnotationToFunctionalBlockControllerService', ['getFunctionalBlockNamesByAnnotationId']);

  const relationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('ClientProjectRelationshipService', ['getClientProjectObservable']);

  let mockWindow: any;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        NoopAnimationsModule,
        AntDesignImportsModule,
        HttpClientTestingModule,
        FormsModule,
        TranslateModule.forRoot({}),
        ReactiveFormsModule,
        BrowserAnimationsModule
      ],
      declarations: [
        MiningCodeViewerComponent,
        MonacoEditorMockComponent,
        ModuleBreadcrumbComponent,
        LoaderDirective
      ],
      providers: [
        TranslateService,
        SetSerializationInterceptor,
        HoverProviderService,
        AnnotationControllerService,
        NzMessageService,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        CancelRequestOnNavigationInterceptor,
        FeatureControllerService,
        { provide: HttpClient, useClass: HttpService },
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: DataDictionaryControllerService, useValue: dataDictionaryControllerServiceSpy },
        { provide: AnnotationControllerService, useValue: annotationControllerServiceSpy },
        { provide: OauthtokenService, useValue: tokenServiceSpy },
        { provide: IoControllerService, useValue: ioControllerServiceSpy },
        { provide: LanguageProviderService, useValue: languageServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
        { provide: WindowToken, useValue: mockWindow },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              module: moduleValue[0]
            }),
            queryParams: {
              _value:
                { offset: 10 }
            }
          },
        },
        {
          provide: AnnotationCategoryControllerService,
          useValue: annotationCategoryControllerServiceStub
        },
        {
          provide: MetamodelControllerService,
          useValue: metamodelControllerServiceStub
        },
        {
          provide: GraphQlControllerService,
          useValue: GraphQlControllerServiceStub
        },
        {
          provide: AnnotationToFunctionalBlockControllerService,
          useValue: annotationToFunctionalBlockControllerServiceSpy
        },
        { provide: ClientProjectRelationshipService, useValue: relationshipServiceSpy },
        { provide: CodeAnnotationEditorComponent, useClass: CodeAnnotationEditorMock }
      ]
    }).compileComponents();

    moduleControllerServiceSpy.findModuleById.and.returnValue(of(moduleValue[0] as any));
    moduleControllerServiceSpy.findAnnotationsForModule.and.returnValue(of(codeAnnotationsValue as any));
    moduleControllerServiceSpy.hasAstNodes.and.returnValue(of(true as any));
    moduleControllerServiceSpy.storeAstNodes.and.returnValue(of(true as any));
    moduleControllerServiceSpy.getDataLineageAvailableForModule.and.returnValue(of(true as any));

    const mockExportFormats = [
      { id: 'datalineage-gml', description: "Exports a Data Flow Graph in GML format (for yEd)"},];
    ioControllerServiceSpy.getExportFormats.and.returnValue(of(mockExportFormats as any));

    annotationControllerServiceSpy.createAnnotation.and.returnValues(of(codeAnnotationsValue[0] as any));
    annotationControllerServiceSpy.updateAnnotation.and.returnValues(of(codeAnnotationsValue[0] as any));
    annotationControllerServiceSpy.deleteAnnotation.and.returnValues(of('' as any));

    dataDictionaryControllerServiceSpy.findAllDataDictionaryEntries.and.returnValues(of(dataDictionaryValue as any));
    dataDictionaryControllerServiceSpy.createDataDictionaryEntry.and.returnValues(of(dataDictionaryValue[0] as any));
    dataDictionaryControllerServiceSpy.deleteDataDictionaryEntry.and.returnValues(of('' as any));
    dataDictionaryControllerServiceSpy.getFormatIfSelectionIsValid.and.returnValues(of({ languageType: 'PICX', byteLength: 16, group: false } as any));
    dataDictionaryControllerServiceSpy.getFormatIfSelectionIsValid.and.returnValues(of(validationResponse1 as any));
    relationshipServiceSpy.getClientProjectObservable.and.returnValue(of(new ClientProjectRelationship(1, 'Client test', 2, 'Project Test')));
    GraphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlResponse as any));
    tokenServiceSpy.getUsername.and.returnValue('admin');
    languageServiceSpy.injectCSS.and.returnValue(null);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MiningCodeViewerComponent);
    component = fixture.componentInstance;
    component.childComponentRef = {destroy : () => ({})} as any;
    component.sharedAnnotationEditor = childComponent;
    fixture.detectChanges();
    spyOn((component as any), 'getRange').and.callFake(() => {
      return { startLineNumber: 1, startColumn: 1, endLineNumber: 2, endColumn: 10 };
    });
    spyOn(component, 'registerCommands').and.callFake(() => { });
    component.monacoEditor = new MonacoEditorMockComponent() as any;
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('initiate the monaco editor', () => {
    component.hasAstNodes = true;
    component.isDataLineageExtensionAvailable = true;
    moduleControllerServiceSpy.findAnnotationsForModule.and.returnValue(of(codeAnnotationsValue as any));
    dataDictionaryControllerServiceSpy.findAllDataDictionaryEntries.and.returnValues(of(dataDictionaryValue as any));
    spyOn((component as any), 'drawLoadedAnnotations').and.callFake(() => { });
    spyOn(component, 'addActionToEditor').and.callThrough();
    component.onMonacoInit(component.monacoEditor as any);
    expect(component.addActionToEditor).toHaveBeenCalledTimes(4);
  });

  it('Should set the height even if Annotation and DD is empty', () => {
    component.canAddAnnotation = false;
    component.hasAstNodes = false;
    moduleControllerServiceSpy.findAnnotationsForModule.and.returnValue(of([] as any));
    dataDictionaryControllerServiceSpy.findAllDataDictionaryEntries.and.returnValues(of([] as any));
    spyOn((component as any), 'drawLoadedAnnotations').and.callFake(() => { });
    spyOn(component, 'addActionToEditor').and.callThrough();
    component.onMonacoInit(component.monacoEditor as any);
    expect(component.editorDataLoaded).toBeTruthy();
  });

  it('Should show theme as per the technology', () => {
    (component as any).fetchModule(moduleValue[1]);
    expect((component as any).EDITOR_OPTIONS.theme).toEqual('vs');
  });

  it('hover provider service should show edit/remove method', () => {
    const position = {
      lineNumber: 2,
      column: 2
    };
    component.dataDictionaryEntryMap.set(dataDictionaryValue[0].id, dataDictionaryValue[0]);
    component.hasAstNodes = true;
    const hoverProviderService = fixture.componentRef.injector.get(HoverProviderService);
    const resp = hoverProviderService.provideHover(undefined, position);
    expect(resp.contents[2].value).toContain('Edit');
  });

  it('hover provider service should show view method', () => {
    const position = {
      lineNumber: 2,
      column: 2
    };
    component.dataDictionaryEntryMap.set(dataDictionaryValue[0].id, dataDictionaryValue[0]);
    component.hasAstNodes = false;
    const hoverProviderService = fixture.componentRef.injector.get(HoverProviderService);
    const resp = hoverProviderService.provideHover(undefined, position);
    expect(resp.contents[2].value).toContain('View');
  });

  it('create Annotation from monaco editor', () => {
    spyOn((component as any), 'addAnnotationToEditor');
    component.addAnnotation();
    expect(component.isClose).toBeFalsy();
  });

  it('create data dictionary entry from  monaco editor', () => {
    component.hasAstNodes = false;
    spyOn((component as any), 'openDataDictionaryDialog');
    component.addDictionaryRecord();
    expect((component as any).openDataDictionaryDialog).toHaveBeenCalled();
    component.hasAstNodes = true;
  });

  it('Delete data dictionary entry', () => {
    component.currentEntryId = dataDictionaryValue[0].id;
    component.dataDictionaryEntryMap = new Map();
    component.dataDictionaryEntryMap.set(dataDictionaryValue[0].id, dataDictionaryValue[0]);
    component.removeDataDictionaryEntry();
    expect(component.dataDictionaryEntryMap.get(dataDictionaryValue[0].id)).toBeUndefined();
  });

  it('get glyphclass from the annotation type', () => {
    const prefix = 'annotation-editor-container__glyph--';
    for (const item of typeArr) {
      const type = component.getGlyphClassName(item.key as AnnotationPojo.TypeEnum);
      expect(type).toEqual(prefix + item.value);
    }
  });

  describe('errors', () => {

    it('should show module not valid error', () => {
      moduleControllerServiceSpy.findModuleById.and.returnValue(throwError('Test Error'));
      spyOn((component as any), 'loadState');
      (component as any).fetchModule(123, 123);
      expect((component as any).loadState).toEqual('error');
    });

    it('should show error while deleting Data dictionary', () => {
      dataDictionaryControllerServiceSpy.deleteDataDictionaryEntry.and.returnValues(throwError('Test Error'));
      spyOn((component as any).messageService, 'error');
      const expectedParams: any = 'codeViewer.dictionaryDeleteErr';
      component.removeDataDictionaryEntry();
      expect((component as any).messageService.error).toHaveBeenCalledWith(expectedParams);
    });

    it('should handle form result for shared annotation editor', () => {
      component.sharedAnnotationEditor = childComponent;
      component.handleSharedFormResult({result: FormResult.Canceled},{} as any);
      expect(component.isClose).toBeFalsy();
    });

    it('should test onCancel', () => {
      component.sharedAnnotationEditor = childComponent;
      component.onCancel();
      expect(component.sharedAnnotationEditor.isClose).toBeTruthy();
      component.sharedAnnotationEditor = null;
      component.onCancel();
      expect(component.isClose).toBeFalsy();
    });
  });

  describe('Test for AST Nodes', () => {
    beforeEach(() => {
      moduleControllerServiceSpy.hasAstNodes.and.returnValues(of(false as any), of(false as any), of(true as any));
      moduleControllerServiceSpy.storeAstNodes.and.returnValues(of(true as any), of(false as any));
    });

    it('should test for hasAstNodes', () => {
      component.hasAstNodes = false;
      (component as any).checkAstNodesForModule(1, 1);
      expect(component.hasAstNodes).toBeTrue();

      component.hasAstNodes = false;
      (component as any).checkAstNodesForModule(1, 1);
      expect(component.hasAstNodes).toBeFalse();

      component.hasAstNodes = false;
      (component as any).checkAstNodesForModule(1, 1);
      expect(component.hasAstNodes).toBeTrue();
    });
  });

  it('should fetch data dictionary entries and update the data dictionary map', () => {
    // Arrange
    const ddRecords: DataDictionaryPojo[] = [
      {
        uid: '#203:117',
        customProperties: {
          'Property1': [{
            name: 'customDataDictionaryEntryProperty',
            value: null,
            dataType: 'STRING'
          }]
        },
        id: 1,
        dataElementName: 'MY-PROGRAM-NAME',
        description: 'This is an english description of the data element name MY-PROGRAM-NAME',
        format: 'PICX',
        scopes: {'scope1': {'name': 'SQLDATABASE'}, 'scope2': {'name': 'CICSUI'}},
        length: 10,
        createdByUserId: 'admin',
        name: 'MMRS7101',
        location: {
          offset: 2,
          length: 15
        }
      }
    ];
    (component as any).fetchDDEntries(ddRecords);
    expect(component.dataDictionaryEntryMap.size).toBe(1);
    expect(component.dataDictionaryEntryMap.get(1)).toEqual(ddRecords[0]);
  });
});