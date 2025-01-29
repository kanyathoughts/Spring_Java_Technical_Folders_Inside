import { ComponentFixture, TestBed } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { HoverProviderService } from '@app/modules/mining-code-viewer/mining-code-viewer-hover-provider';
import { MiningMonacoEditorComponent } from './mining-monaco-editor.component';
import { MiningCodeViewerComponent } from '@app/modules/mining-code-viewer/mining-code-viewer.component';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { ModuleControllerService } from '@innowake/mining-api-angular-client';
import { of } from 'rxjs/internal/observable/of';

describe('MiningMonacoEditorComponent', () => {
  let component: MiningMonacoEditorComponent;
  let fixture: ComponentFixture<MiningMonacoEditorComponent>;

  beforeEach(() => {
    const languageProviderServiceStub = () => ({
      grammars: {},
      getTokensProviderForLanguage: (languageId: any, monaco: any, grammar: any) => ({
        then: () => ({})
      }),
      init: () => (of(true))
    });
    const hoverProviderServiceStub = () => ({
      provideHover: { bind: () => ({}) }
    });
    const codeViewerComponentStub = () => ({
      /* empty stub for now */
    });
    const featureToggleServiceStub = () => ({
      /* empty stub for now */
    });
    const moduleControllerServiceStub = () => ({
      /* empty stub for now */
    });
    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [MiningMonacoEditorComponent],
      providers: [
        {
          provide: LanguageProviderService,
          useFactory: languageProviderServiceStub
        },
        { provide: HoverProviderService, useFactory: hoverProviderServiceStub },
        { provide: MiningCodeViewerComponent, useFactory: codeViewerComponentStub },
        { provide: FeatureToggleService, useFactory: featureToggleServiceStub },
        { provide: ModuleControllerService, useFactory: moduleControllerServiceStub }
      ]
    });
    fixture = TestBed.createComponent(MiningMonacoEditorComponent);
    component = fixture.componentInstance;
  });

  it('can load instance', () => {
    expect(component).toBeTruthy();
   
  });

  it(`editorMinHeight has default value`, () => {
    expect(component.editorMinHeight['min-height.px']).toEqual(200);
  });

  it(`editorMinHeight has default value`, () => {
    expect(component.editorMinHeight['min-height.px']).not.toBeGreaterThan(200);
  });

});
