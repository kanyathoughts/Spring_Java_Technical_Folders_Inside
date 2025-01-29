import { TestBed, ComponentFixture, waitForAsync } from '@angular/core/testing';
import { LanguageProviderService } from '../../../core/services/monaco-editor/language-provider.service';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';
import { CfgMetaDataService } from '@app/core/services/cfg-meta-data.service';
import { ControlFlowMetaDataComponent } from './control-flow-meta-data.component';
import { MonacoEditorMockComponent } from '@app/core/mocks/monaco-editor.mock';
import { HttpClientTestingModule } from "@angular/common/http/testing";
import { DEFAULT_LANGUAGE, MissingTranslationHandler, TranslateCompiler, TranslateLoader, TranslateParser, TranslateService, TranslateStore, USE_DEFAULT_LANG, USE_EXTEND, USE_STORE } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { HttpClient } from '@angular/common/http';
import { CodeAnnotationEditorComponent } from '@app/modules/mining-code-viewer/code-annotation/code-annotation-editor.component';
import { AnnotationElementData } from '@app/modules/mining-code-viewer/mining-code-viewer';
import { AnnotationCategoryControllerService, AnnotationControllerService, AnnotationPojo, ControlFlowNode, ModulePojo } from '@innowake/mining-api-angular-client';
import { CodeAnnotationEditorMock } from '@app/core/mocks/code-annotation-editor.mock';
import { ControlFlowNodeDetails } from '@app/modules/graph/module-control-flow/models/control-flow-node-details';

describe('ControlFlowMetaDataComponent', () => {
    let fixture: ComponentFixture<ControlFlowMetaDataComponent>;
    let component: ControlFlowMetaDataComponent;
    let languageServiceSpy: jasmine.SpyObj<any> = jasmine.createSpyObj('LanguageProviderService', [
        'injectCSS'
    ]);
    let cfgMetaDataServiceSpy: jasmine.SpyObj<any> = jasmine.createSpyObj('CfgMetaDataService', [
        'setState', 'getState'
    ]);
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
            sourceMetrics: {
              codeLines: null,
              commentLines: null,
              complexityMcCabe: null
            },
            content: moduleSourceCode
        }
    ];

    const ifNode: ControlFlowNode = {
        id: '#281:1',
        type: 'CobolIfStmt',
        superTypes: new Set(['BRANCH_STATEMENT']),
        entity: ControlFlowNode.EntityEnum.ANNOTATION,
        parent: '#282:5',
        properties: {},
        offset: 300
    };
    const annotation: AnnotationPojo = {
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
    };
    const translateServiceSpy = jasmine.createSpyObj('TranslateService', ['instant']);
    const messageServiceSpy = jasmine.createSpyObj('NzMessageService', ['error']);
    const annotationControllerServiceSpy: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj<AnnotationControllerService>
    ('AnnotationControllerService', ['findAnnotationById']);
    const annotationCategoryControllerServiceStub: jasmine.SpyObj<AnnotationCategoryControllerService> = jasmine.createSpyObj<AnnotationCategoryControllerService>
    ('AnnotationCategoryControllerService', ['createAnnotationCategory']);
    const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'get', 'skipErrorHandler']);

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [ControlFlowMetaDataComponent, MonacoEditorMockComponent],
            imports: [HttpClientTestingModule],
            providers: [
                { provide: HttpClient, useValue: httpServiceSpy },
                { provide: AnnotationCategoryControllerService, useValue: annotationCategoryControllerServiceStub},
                { provide: TranslateService, useValue: translateServiceSpy },
                TranslateStore, TranslateLoader, TranslateCompiler, TranslateParser, MissingTranslationHandler,
                { provide: USE_DEFAULT_LANG,  useValue: undefined },
                { provide: USE_STORE,  useValue: undefined },
                { provide: USE_EXTEND,  useValue: undefined },
                { provide: DEFAULT_LANGUAGE,  useValue: undefined },     
                { provide: NzMessageService, useValue: messageServiceSpy },      
                { provide: LanguageProviderService, useValue: languageServiceSpy },
                { provide: CfgMetaDataService, useValue: cfgMetaDataServiceSpy },
                { provide: AnnotationControllerService, useValue: annotationControllerServiceSpy },
                { provide: CodeAnnotationEditorComponent, useClass: CodeAnnotationEditorMock },
                {
                    provide: ActivatedRoute,
                    useValue: {
                        data: of({
                            module: moduleValue[0]
                        })
                    },
                },
            ]
        }).compileComponents();
        languageServiceSpy.injectCSS.and.returnValue(null);
        cfgMetaDataServiceSpy.getState.and.returnValue({ inputFiles: false, outputFiles: false });
        annotationControllerServiceSpy.findAnnotationById.and.returnValues(of(annotation as any));
        httpServiceSpy.disableApiPrefix.and.returnValue(httpServiceSpy);
        httpServiceSpy.skipErrorHandler.and.returnValue(httpServiceSpy);
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(ControlFlowMetaDataComponent);
        component = fixture.componentInstance;
        const nodeSelection = of({ controlFlowNode: ifNode as any, graphNodes: [ifNode as any] });
        spyOn((component as any), 'getRange').and.callFake(() => {
          return { startLineNumber: 1, startColumn: 1, endLineNumber: 2, endColumn: 10 };
        });
        component.nodeSelection = nodeSelection;
        ifNode.entity = ControlFlowNode.EntityEnum.ANNOTATION;
        component.controlFlowNode = new ControlFlowNodeDetails(ifNode, null, null);
        component.monacoEditor = new MonacoEditorMockComponent() as any;
        fixture.detectChanges();
    });

    afterEach(() => {
        fixture.destroy();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should close CFG editor', () => {
        spyOn(component.hideShowDrawer, 'emit');
        component.onMonacoInit(component.monacoEditor as any);
        component.closeCfgEditor();
        expect(component.hideShowDrawer.emit).toHaveBeenCalledWith(false);
        expect(component.widePanel).toBe(true);
    });

    it('should toggle widePanel', () => {
        component.widePanel = false;
        component.onToggleClick();
        expect(component.widePanel).toBe(true);
        component.onToggleClick();
        expect(component.widePanel).toBe(false);
    });

    it('should build route for module Id', () => {
      const route = component.buildRouteForModule(2002) as any;
      expect(route).toBe(RouteBuilder.buildModuleRoute(component.projectId, 2002, '/details/overview'));
    });

    it('should maintain selection state', () => {
      component.maintainState['test'] = true;
      component.maintainSectionState('test');
      expect(component.maintainState['test']).toBeFalsy();
      component.maintainState['test'] = false;
      component.maintainSectionState('test');
      expect(component.maintainState['test']).toBeTruthy();
    });

    it('should test annotation change', () => {
      spyOn((component as any).annotationChange, 'emit');
      component.onAnnotationUpdateOrDelete();
      expect((component as any).annotationChange.emit).toHaveBeenCalled();
    });

    it('should test onCancel', () =>{
      component.onCancel();
      expect(component.isClose).toBeFalsy();
      expect(component.openedAnnotationDetails).toBeNull();
    });

    it('should handle form result for shared annotation editor', () => {
      spyOn(component, 'onCancel');
      spyOn(component, 'deleteAnnotation');
      spyOn(component, 'updateAnnotation');
      spyOn((component as any), 'onError');
      (component as any).showError('test', 'test');
      component.handleSharedFormResult({result: FormResult.Deleted}, {editor: component.monacoEditor,
      codeAnnotationComponent: {data: {viewZoneId: 'test'}}} as any);
      expect(component.deleteAnnotation).toHaveBeenCalled();
      component.handleSharedFormResult({result: FormResult.Saved}, {editor: component.monacoEditor,
      codeAnnotationComponent: {data: {viewZoneId: 'test'}}} as any);
      expect(component.updateAnnotation).toHaveBeenCalled();
      component.handleSharedFormResult({result: FormResult.Disabled}, {} as any);
      expect(component.isClose).toBeTruthy();
      component.handleSharedFormResult({result: FormResult.Canceled}, {} as any);
      expect(component.onCancel).toHaveBeenCalled();
      component.handleSharedFormResult({result: FormResult.Error}, {} as any);
      expect((component as any).onError).toHaveBeenCalled();
    });

    it('should test onError', () => {
      spyOn((component as any), 'closeEditor');
      (component as any).onError();
      expect((component as any).closeEditor).toHaveBeenCalled();
    });

    it('should test getDataForAnnotationComponent', () => {
      const annotationData = (component as any).getDataForAnnotationComponent(annotation);
      expect(annotationData.annotation).toBe(annotation);
    });

    it('should test annotation decoration creation', () => {
      const decoration = (component as any).createAnnotationDecoration(2, 1, 16, 'annotation-editor-container__annotation--candidate');
      expect(decoration).toBeDefined();
    });

    it('should update annotation', () => {
      spyOn(component, 'onAnnotationUpdateOrDelete');
      component.updateAnnotation({}, 1, 16, {data: {annotation}} as CodeAnnotationEditorComponent);
      expect(component.onAnnotationUpdateOrDelete).toHaveBeenCalled();
    });

    it('should delete annotation', () => {
      spyOn(component, 'onAnnotationUpdateOrDelete');
      component.deleteAnnotation([], {annotation, viewZoneId: 'test'} as AnnotationElementData);
      expect(component.onAnnotationUpdateOrDelete).toHaveBeenCalled();
    });
});
