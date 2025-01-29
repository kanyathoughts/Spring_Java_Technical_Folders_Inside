import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { LanguageInfo } from '@app/core/services/monaco-editor/language-info.interface';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { HoverProviderService } from '@app/modules/mining-code-viewer/mining-code-viewer-hover-provider';
import { LinkAndReferenceProvider } from '@app/modules/mining-code-viewer/mining-code-viewer-link-provider';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { CodeLensProvider } from '@app/modules/mining-code-viewer/mining-code-viewer-code-lens-provider';
import { AssembledContent, EntityId, ModuleControllerService } from '@innowake/mining-api-angular-client';
import * as monaco from 'monaco-editor';

const grayBGTheme: monaco.editor.IStandaloneThemeData = {
  base: 'vs',
  inherit: false,
  rules: [
      { background: 'fafafa', token: 'bg-color' }
  ],
  colors: {
    'editor.background': '#fafafa'
  }
};

const MIN_BASE_HEIGHT = 200;
const MIN_LINE_HEIGHT = 20;
const MIN_BUFFER = 25;

@Component({
  selector: 'mining-monaco-editor',
  templateUrl: './mining-monaco-editor.component.html',
})

export class MiningMonacoEditorComponent implements OnInit  {
  @ViewChild('editorContainer', { static: true }) editorContainer: ElementRef;
  @Input() options: monaco.editor.IStandaloneEditorConstructionOptions;
  @Input() dynamicHeight = false;
  @Input() maxHeight = false; // This is to set the max height of the editor, But it does not include the space for view zones.
  @Input() code: string;
  @Input() moduleId: EntityId = null;
  @Input() projectId: number = null;
  @Input() assembledContent: AssembledContent;
  @Input() enableHoveringAndReference = false;
  @Output() init: EventEmitter<any> = new EventEmitter();
  editor: monaco.editor.ICodeEditor;
  constructor(
    private hoverProviderService: HoverProviderService,
    private languageProviderService: LanguageProviderService,
    private featureToggleService: FeatureToggleService,
    private moduleController: ModuleControllerService
    ) {
  }

  get editorMinHeight(): any {
    if (this.dynamicHeight) {
      const lines = (this.code).split(/\r\n|\r|\n/).length;
      const contentHeight = ( lines * MIN_LINE_HEIGHT ) + MIN_BUFFER;
      const height = this.maxHeight ? contentHeight : Math.min((contentHeight), MIN_BASE_HEIGHT);
      return { 'min-height.px' :  height };
    } else {
      return { 'min-height.px' : MIN_BASE_HEIGHT };
    }
  }

  ngOnInit(): void {
    this.initMonaco();
  }

  private initMonaco(): void {
    this.languageProviderService.init().subscribe(() => {
      let grammarKeys: string[] = [];
      let grammarObj: { [scopeName: string]: LanguageInfo } = {};
      if (this.languageProviderService.grammars) {
        grammarKeys = Object.keys(this.languageProviderService.grammars);
        grammarObj = this.languageProviderService.grammars;
      }

      let linkAndReferenceProvider = null;
      let codeLensProvider = null;
      const isModuleAndProjectAvailable = this.moduleId && this.projectId;
      if (isModuleAndProjectAvailable && this.enableHoveringAndReference) {
        linkAndReferenceProvider = new LinkAndReferenceProvider(this.featureToggleService, this.moduleController, this.projectId, this.moduleId,
            !! this.assembledContent);
        codeLensProvider = new CodeLensProvider(this.assembledContent);
      }
      for (const grammar of grammarKeys) {
        monaco.languages.register({ id: grammarObj[grammar].languageId });
        monaco.languages.onLanguage(grammarObj[grammar].languageId, () => {
          void this.languageProviderService.getTokensProviderForLanguage(
            grammarObj[grammar].languageId,
            grammar
          ).then((tokensProvider) => {
            if (tokensProvider != null) {
              monaco.languages.setTokensProvider(grammarObj[grammar].languageId, tokensProvider);
            }
          });
        });

        // Currently we need hovering and reference linking only for the code viewer.
        if (isModuleAndProjectAvailable && this.enableHoveringAndReference) {
          monaco.languages.registerHoverProvider(grammarObj[grammar].languageId, {
            provideHover: this.hoverProviderService.provideHover.bind(this.hoverProviderService),
          });
          monaco.languages.registerLinkProvider(grammarObj[grammar].languageId, linkAndReferenceProvider);
          monaco.languages.registerReferenceProvider(grammarObj[grammar].languageId, linkAndReferenceProvider);
          monaco.languages.registerCodeLensProvider(grammarObj[grammar].languageId, codeLensProvider);
        }
      }
      /** Register theme for code viewer */
      monaco.editor.defineTheme('vs-gray', grayBGTheme);
      this.options['value'] = this.code;
      this.options['scrollBeyondLastLine'] = false;
      this.options.useShadowDOM = true;
      this.editor = monaco.editor.create(
        this.editorContainer.nativeElement as HTMLElement,
        this.options
      );
      this.languageProviderService.injectCSS();
      this.init.emit(this.editor);
    });
  }
}
