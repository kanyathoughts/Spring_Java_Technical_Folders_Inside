import { CodeViewerButtonComponent } from "./code-viewer-button.component";
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from "@angular/common/http/testing";
import { BrowserAnimationsModule } from "@angular/platform-browser/animations";
import { RouterTestingModule } from "@angular/router/testing";
import { TranslateModule } from "@ngx-translate/core";
import { WindowToken } from "@app/core/utils/window";
import { ModulePojo } from "@innowake/mining-api-angular-client";

describe('CodeViewerComponent', () => {
    let component: CodeViewerButtonComponent;
    let fixture: ComponentFixture<CodeViewerButtonComponent>;
    let mockWindow: any;
    let openedUrl = '';

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
          complexityMcCabe: null
        },
        content: null,
        sourceCodeAvailable: false,
    };

    const moduleValue1: ModulePojo = {
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
        storage: 'FILE_SECTION',
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
        sourceCodeAvailable: true,
        parentId: 14441
    };

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
            schemas: [NO_ERRORS_SCHEMA],
            declarations: [CodeViewerButtonComponent],
            imports: [
                RouterTestingModule,
                HttpClientTestingModule,
                TranslateModule.forRoot({}),
                BrowserAnimationsModule
            ],
            providers: [{ provide: WindowToken, useValue: mockWindow }]
        }).compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(CodeViewerButtonComponent);
        component = fixture.componentInstance;
        component.moduleDetails = moduleValue;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should open code viewer in new tab', () => {
        component.openInNewBrowserTab('code-viewer');
        expect(openedUrl).toContain('/module-' + moduleValue.id + '/code-viewer');
    });
    
    it('should open code viewer for file section', () => {
        component.moduleDetails = moduleValue1;
        component.openInNewBrowserTab('code-viewer');
        expect(openedUrl).toContain('/module-' + moduleValue1.parentId + '/code-viewer');
    });

    it('should handle code viewer button', () => {
        component.moduleDetails = moduleValue1;
        component.handleCodeViewerButton();
        expect(component.disableCodeViewer).toBeFalse();
    });

    it('should open code viewer in new tab at given offset', () => {
        component.offset = 100;
        component.openInNewBrowserTab('code-viewer');
        expect(openedUrl).toContain('code-viewer?offset=100');
    });
})