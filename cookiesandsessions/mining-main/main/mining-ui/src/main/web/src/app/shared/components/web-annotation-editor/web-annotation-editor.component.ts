import { AfterViewInit, OnDestroy, Optional, Inject } from '@angular/core';
import { Component, Input, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { NzDrawerRef } from 'ng-zorro-antd/drawer';
import { Subscription } from 'rxjs';
import { AllowedTableActions } from '../mining-table/mining-table-action.interface';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { AnnotationEditor, FormResponse, FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { AnnotationPojo, EntityId, ModulePojo } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-web-annotation-editor',
  templateUrl: './web-annotation-editor.component.html',
  providers: [
    LanguageProviderService
  ]
})

export class WebAnnotationEditorComponent implements OnInit, AfterViewInit, OnDestroy {

  @Input() annotation: AnnotationPojo;
  @Input() moduleDetails: ModulePojo;
  @Input() canCopyPasteAnnotation: boolean;

  parentComponent = AnnotationEditor.WEB_ANNOTATION_EDITOR;
  isCreateMode: boolean;
  projectId: number;
  moduleId: EntityId;
  offset: number;
  startLine = 1;
  showEditor = false;

  public editorOptions = {
    theme: 'vs-gray',
    language: 'cobol',
    readOnly: true,
    minimap: { enabled: false },
    lineNumbers: this.calculateLinesForCode.bind(this)
  };

  private clientProjectSubscription: Subscription;
  private currentClient: ClientProjectRelationship;

  constructor(
    @Inject(WindowToken) private $window: Window,
    protected router: Router,
    private deeplinkService: DeepLinkService,
    private clientProjectRelationShip: ClientProjectRelationshipService,
    @Optional() private drawerRef: NzDrawerRef<string>
  ) {
    this.clientProjectSubscription = this.clientProjectRelationShip.getClientProjectObservable().subscribe(currentClient => {
      this.currentClient = currentClient;
    });
  }

  ngOnInit(): void {
    this.projectId = this.currentClient.getProjectId();
    if (this.moduleDetails) {
      this.moduleId = this.moduleDetails.id;
      this.offset = this.annotation.location.offset;
      this.startLine = this.calculateLine(this.moduleDetails?.content);
      this.setLanguage();
    }
    this.isCreateMode = false;
  }

  ngAfterViewInit(): void {
    this.showEditor = true;
  }

  /**
   * Sets the language for Monaco Editor
   */
  setLanguage(): void {
    const moduleTechnology: ModulePojo.TechnologyEnum = this.moduleDetails.technology;
    const tech = moduleTechnology.toLowerCase();
    if (this.moduleDetails.technology === ModulePojo.TechnologyEnum.COBOL &&
      (this.moduleDetails.type === ModulePojo.TypeEnum.BMS_MAP || this.moduleDetails.type === ModulePojo.TypeEnum.BMS_MAPSET)) {
      this.editorOptions.language = 'bms';
    } else {
      this.editorOptions.language = tech;
    }
  }

  /**
   * Call the deeplink service to open the selected module in Eclipse
   */
  openInEclipse(): void {
    this.deeplinkService.showModuleInEclipse(this.moduleDetails);
  }

  /**
   * Handles the output of the shared annotation editor
   * @param result output of the shared annotation editor
   */
  handleSharedFormResult(formResponse: FormResponse<AnnotationPojo>): void {
    switch (formResponse.result) {
      case FormResult.Deleted:
        this.drawerRef?.close(AllowedTableActions.DELETE);
        break;
      case FormResult.Saved:
        this.drawerRef?.close(AllowedTableActions.UPDATE);
        break;
      case FormResult.Disabled:
        break;
      case FormResult.Canceled:
        this.drawerRef?.close();
        break;
      default:
        break;
    }
  }
  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
  }

  /* Method to open CFG in new tab */
  openCFGInNewTab(): void {
    const queryParams = {
      annotationId: this.annotation.uid,
    };
    const queryString = new URLSearchParams(queryParams).toString();
    const newTabUrl = `control-flow?${queryString}`;
    openInNewTab(this.projectId, this.moduleDetails.linkHash, newTabUrl, this.$window);
  }

  /**
   * method to assign  original line numbers and starting for the code annotation code
   * @param originalLineNumber number to add the sequence of number
   * @returns addition of both original number and starting number
   */
  private calculateLinesForCode(originalLineNumber: number): number {
    return originalLineNumber + this.startLine;
  }

  /**
   * method to return the starting line for the annotation code
   * @param  initialSourceCode - annotated code
   * @returns line number from where the code is written
   */
  private calculateLine(initialSourceCode: string): number {
    let line = 0;
    const codeOffset = this.annotation.location.offset;
    if (initialSourceCode?.length) {
      for (let i = 0; i < codeOffset; i++) {
        if (initialSourceCode?.charAt(i) === '\n') {
          line++;
        }
      }
    }
    return line;
  }
}
