import { ComponentFixture, TestBed, waitForAsync, } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { WebAnnotationEditorComponent } from './web-annotation-editor.component';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { AllowedTableActions } from '../mining-table/mining-table-action.interface';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule } from '@ngx-translate/core';
import { of } from 'rxjs';
import { RouterTestingModule } from '@angular/router/testing';
import { Router } from '@angular/router';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';


xdescribe('WebAnnotationEditorComponent', () => {
    let component: WebAnnotationEditorComponent;
    let fixture: ComponentFixture<WebAnnotationEditorComponent>;

    const deepLinkServiceSpy: jasmine.SpyObj<DeepLinkService> = jasmine.createSpyObj('DeepLinkService', ['showModuleInEclipse', 'featureIsActive', 'heartbeat']);
    
    const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
        ['findModuleById']);

    const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(2, 'TestClient', 2, 'TestProject');
    const clientProjectRelationshipServiceStub: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj<ClientProjectRelationshipService>
    ('ClientProjectRelationshipService', ['getClientProjectObservable']);

    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose','close']);

    const currentClient: ClientProjectRelationship = new ClientProjectRelationship(1, 'test', 1, 'test');

    const moduleValue: ModulePojo = {
      uid: '#136:600',
      customProperties: {'Property1':[
        {
        name: 'customMetaInfo2',
        value: null,
        dataType: 'STRING'
      }, {
        name: 'customMetaInfo1',
        value: null,
        dataType: 'STRING'
      }]},
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

    let languageServiceSpy: jasmine.SpyObj<any> = jasmine.createSpyObj('LanguageProviderService', [
      'injectCSS'
    ]);

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            schemas: [NO_ERRORS_SCHEMA],
            declarations: [WebAnnotationEditorComponent],
            imports: [
                RouterTestingModule,
                HttpClientTestingModule,
                TranslateModule.forRoot({}),
              ],
            providers: [{
                provide: ClientProjectRelationshipService,
                useValue: clientProjectRelationshipServiceStub,
              },
              { provide: NzModalRef, useValue: { destroy: () => true, close: () => true } },
              {provide: DeepLinkService, useValue: deepLinkServiceSpy},
              {provide: ModuleControllerService, useValue: moduleControllerServiceSpy},
              { provide: LanguageProviderService, useValue: languageServiceSpy }
            ]
        }).compileComponents();
        clientProjectRelationshipServiceStub.getClientProjectObservable.and.returnValue(of(currentClient as any));
        deepLinkServiceSpy.featureIsActive.and.returnValue(of(true));
        deepLinkServiceSpy.heartbeat.and.returnValue(Promise.resolve(true));
        moduleControllerServiceSpy.findModuleById.and.returnValue(of(moduleValue as any));
        languageServiceSpy.injectCSS.and.returnValue(null);
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(WebAnnotationEditorComponent);
        component = fixture.componentInstance;
        component.annotation = {
            uid: '#172:234',
            customProperties: {'Property1':[{
              name: 'customAnnotationProperty',
              value: 'A value for the custom Annotation property',
              dataType: 'STRING'
            }]},
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
            moduleName: 'PRG1'
          };
    })

    it('should create', () => {
      component.moduleDetails = {
        content: 'test',
        technology: 'technologyTest' as any
      }
      component.showEditor = true;
      expect(component).toBeTruthy();
      component.ngOnInit();
      component.projectId = 1;
      component.moduleId = component.annotation.module;
      component.isCreateMode = false;
      fixture.detectChanges();
    });

    xit('should open in eclipse', () => {
      component.openInEclipse();
      expect(deepLinkServiceSpy.showModuleInEclipse).toHaveBeenCalledWith(component.moduleDetails);
    });

    it('should handle form result for shared annotation editor', () => {
      component.handleSharedFormResult({result: FormResult.Deleted});
      nzModalRefSpy.close(AllowedTableActions.DELETE);
      component.handleSharedFormResult({result: FormResult.Saved});
      nzModalRefSpy.close(AllowedTableActions.UPDATE);
      component.handleSharedFormResult({result: FormResult.Disabled});
      nzModalRefSpy.close();
      component.handleSharedFormResult({result: FormResult.Canceled});
      nzModalRefSpy.close();
    });

    it('should check setLanguage annotation editor', () => {
      component.moduleDetails = {
        content: 'test',
        technology: 'COBOL' as any,
        type: 'BMS_MAP' as any
      }
      component.setLanguage();
      expect(component.editorOptions.language).toBe('bms')
    });

  it('should check calculateLinesForCode method', () => {
    component.startLine = 1;
    (component as any).calculateLinesForCode(2)
    expect((component as any).calculateLinesForCode(2)).toEqual(3);
  });
});
