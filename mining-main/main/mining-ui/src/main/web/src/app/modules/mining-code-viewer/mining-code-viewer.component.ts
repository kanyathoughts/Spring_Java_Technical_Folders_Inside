/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import {
  Component, OnInit, ViewChild,
  ElementRef,
  OnDestroy,
  ViewContainerRef,
  ComponentFactoryResolver,
  ComponentRef,
  Renderer2,
  Inject
} from '@angular/core';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { NgElement, WithProperties } from '@angular/elements';
import { NzMessageService } from 'ng-zorro-antd/message';
import {
  CodeAnnotationEditorComponent,
  annotationElementIdPrefix,
} from './code-annotation/code-annotation-editor.component';
import { COMMAND_OPEN_DICT, COMMAND_REMOVE_DICT, HoverProviderService } from './mining-code-viewer-hover-provider';
import { TranslateService } from '@ngx-translate/core';
import { Logger } from '@app/core';
import { Subscription } from 'rxjs';
import { monacoEditorStyle } from '../../../theme/monaco-editor-style';
import styles from '../../../theme/style.json';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { AnnotationElementData, AnnotationDetails, AnnotationsFunctionalGroups } from './mining-code-viewer';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { AnnotationEditor, FormResponse, FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { SharedAnnotationEditorComponent } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.component';
import { SharedDataDictionaryEditorComponent } from '@app/shared/components/shared-data-dictionary-editor/shared-data-dictionary-editor.component';
import { HttpErrorResponse } from '@angular/common/http';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { COMMAND_OPEN_CODE_VIEWER_LINK, COMMAND_PEEK_DATAFLOW_REFERENCES, LinkAndReferenceProvider } from './mining-code-viewer-link-provider';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { graphQlQuery } from '@app/core/utils/graphql.util';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import {
  AnnotationControllerService,
  AnnotationPojo,
  AnnotationReport,
  AssembledContent,
  CodeViewerLink,
  DataDictionaryControllerService,
  DataDictionaryPojo,
  DataFieldFormat,
  EntityId,
  IoControllerService,
  ModuleControllerService,
  ModuleLocation,
  ModulePojo,
  ProjectRole } from '@innowake/mining-api-angular-client';
import { WindowToken } from '@app/core/utils/window';
import { editor, IRange, Range } from 'monaco-editor/esm/vs/editor/editor.api';
import { DataLineageExportComponent } from '@app/shared/components/data-lineage-export/data-lineage-export.component';
import { FunctionalGroupService } from '@app/core/services/functional-group.service';
const log = new Logger('MiningCodeViewer');
const sharedAnnotationEditorClass = 'mining__card-drawer shared-side-viewer__drawer-editor shared-side-viewer__drawer-editor--open-';
const virtualScrollBuffer = 200;
const virtualScrollThreshold = 50;

@Component({
  selector: 'app-mining-module-annotation-editor',
  templateUrl: './mining-code-viewer.component.html'
})
/**
 * This component uses the Monaco Editor for displaying the source code For module
 * and provides Web interface to the user for managing the annotations and Data Dictionary records.
 * User can create the Annotation for a selected single or multiple LOC.
 * Data dictionary entry can be added for a single valid word from Module code.
 */
export class MiningCodeViewerComponent implements OnInit, OnDestroy {

  @ViewChild('editorSidebar') editorSidebar: ElementRef;
  @ViewChild(SharedAnnotationEditorComponent) public sharedAnnotationEditor: SharedAnnotationEditorComponent;
  @ViewChild(SharedDataDictionaryEditorComponent) public sharedDDEditor: SharedDataDictionaryEditorComponent;
  @ViewChild('viewContainerRef', { read: ViewContainerRef })
  VCR: ViewContainerRef;
  childComponentRef: ComponentRef<any>;
  editorVisibility = { visibility: 'hidden' };

  public EDITOR_OPTIONS: editor.IStandaloneEditorConstructionOptions = {
    scrollbar: {
      verticalScrollbarSize: 5,
      alwaysConsumeMouseWheel: true
    },
    minimap: { enabled: false },
    theme: 'vs',
    language: 'cobol' || 'natural',
    readOnly: true,
    glyphMargin: false,
    automaticLayout: true,
    renderLineHighlight: 'none',
    contextmenu: true,
    lineHeight: monacoEditorStyle.lineHeight,
    mouseWheelScrollSensitivity: 1
  };

  dataDictionaryEntryMap: Map<number, DataDictionaryPojo> = new Map();

  moduleName = '';
  code = '';
  currentEntryId = -1;

  projectId: number;
  technology: ModulePojo.TechnologyEnum;
  moduleId: number;
  hasAstNodes: boolean;
  loadState: LoaderState = LoaderState.loading;
  loadStateError: string;
  editorDataLoaded = true;

  assembled: boolean;
  assembledViewEnabled: boolean;
  assembledContent: AssembledContent;

  annotationType: any[] = [];
  annotationState: any[] = [];
  isSelectionEmpty = true;
  canAddAnnotation: boolean;
  showHide: string;
  modulePath: string;
  currentModule: ModulePojo;
  newAnnotation: AnnotationPojo;
  parentComponent = AnnotationEditor.CODEVIEWER_ANNOTATION_EDITOR;
  sharedAnnotationEditorStatusClass: string = sharedAnnotationEditorClass;
  editorShowHide: string;
  isSave = false;
  isDelete = false;
  isClose = false;
  showCrossBtn = true;
  viewZoneId: string;
  headerTitle: string;
  isNewDDEntry = false;
  monacoEditor: editor.ICodeEditor;
  loadedAnnotations: number[] = [];
  isDataLineageExtensionAvailable: boolean;
  isDataLineageAvailable: boolean;
  openedAnnotationDetails: AnnotationDetails;
  isEdit = false;
  hashLink: string;
  drawnAnnotationIds: Set<number> = new Set();
  routingSubscription: any;
  listOfFunctionalGroups: AnnotationsFunctionalGroups[]  = [];
  private projectLevelFgs: number;
  private  dataLineageModalInstance: NzModalRef;
  private stateColor = {
    APPROVED: styles.$approvedAnnotation,
    CANDIDATE: styles.$candidateAnnotation,
    FOR_REVIEW: styles.$forReviewAnnotation,
    IN_ANALYSIS: styles.$inAnalysisAnnotation,
    REJECTED: styles.$rejectedAnnotation,
    INVALID: styles.$invalidAnnotation,
    DDENTRY: styles.$dictionaryBackground
  };
  private initialSourceCode: string;
  private MAX_LINE_CHARS = 160; // 160 is the max length we are expecting in the code, it could be more as well.
  private codeAnnotationArray: AnnotationPojo[] = [];
  private dictionaryDecorations: Map<number, any> = new Map();
  /** decoration (background color) for parts included from a different file (in assembled view) */
  private inclusionDecorations: string[] = [];
  private clientProjectSubscription: Subscription;
  private scrollEventSubscription: Subscription;
  private moduleControllerServiceSub: Subscription;

  /**
   * We use a hover provider service since the monaco initialization and customization
   * happens elsewhere. This way we can define the hover provider in this component and 'subscribe'
   * to hovering events.
   */
  constructor(
    public route: ActivatedRoute,
    private moduleControllerService: ModuleControllerService,
    private annotationControllerService: AnnotationControllerService,
    private dataDictionaryController: DataDictionaryControllerService,
    private translateService: TranslateService,
    private messageService: NzMessageService,
    private authService: IdentityAccessManagementService,
    private authorizationService: KeycloakAuthorizationService,
    private relationshipService: ClientProjectRelationshipService,
    private featureToggleService: FeatureToggleService,
    private CFR: ComponentFactoryResolver,
    private hoverProviderService: HoverProviderService,
    private graphQlControllerService: GraphQlControllerService,
    private parametersService: CustomizableTableParametersService,
    private renderer: Renderer2,
    private sharedAnnotationEditorService: SharedAnnotationEditorService,
    private router: Router,
    private modalService: NzModalService,
    private functionalGroupService: FunctionalGroupService,
    private ioService: IoControllerService,
    @Inject(WindowToken) private $window: Window
    ) {
      this.routingSubscription = this.router.events.subscribe((event) => {
        if (event instanceof NavigationEnd) {
          this.router.navigated = false;
        }
      });
      this.hoverProviderService.codeViewer = this;
      const types = AnnotationReport.AnnotationTypeEnum;
      const states = AnnotationReport.AnnotationStateEnum;

      for (const key of Object.keys(types)) {
        this.annotationType.push({
          label: translateService.instant('annotationTypes.' + key.toLowerCase()),
          value: types[key],
        });
      }

      for (const key of Object.keys(states)) {
        this.annotationState.push({
          label: translateService.instant('annotationStates.' + key.toLowerCase()),
          value: states[key],
        });
      }
    }

  ngOnInit(): void {
    this.route.data.subscribe((data: { module: ModulePojo, assembled: boolean }) => {
      this.currentModule = { name: data.module.name, id: data.module.id, projectId: data.module.projectId, parent: data.module.parent };
      this.moduleId = data.module.id;
      this.projectId = data.module.projectId;
      this.technology = data.module.technology;
      this.modulePath = data.module.path;
      this.assembled = data.assembled;
      this.fetchModule(data.module);
      this.hashLink = data.module.linkHash;
      this.clientProjectSubscription = this.relationshipService.getClientProjectObservable().subscribe((currentClient) => {
        this.canAddAnnotation = this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.EDITOR);
      });
    });
    // Disabled as part of WMIN-9363, must be enabked based on editor+sider scroll in syncScroll() method
    // this.scrollEventSubscription =
    //   this.scrollEventService.getScrollObservable().subscribe((scrollPosition) => {
    //     this.showHide = 'mining-fixed-page-header-' + scrollPosition;
    //     this.editorShowHide = 'shared-side-viewer__drawer-editor-header-' + scrollPosition;
    //   });
    this.featureToggleService.isActive('codeViewerAssembledView').subscribe(enabled => this.assembledViewEnabled = enabled);
  }

  /**
   * This method is called when the monaco editor gets ready. When the onInit-method is called the
   * code still might not be displayed completely.
   * @param editor An instance of the monaco editor.
   */
  onMonacoInit(editor: editor.ICodeEditor): void {
    this.monacoEditor = editor;
    this.addActionToEditor('codeViewer.showDataLineage', this.showDataLineage.bind(this, editor));
    /* Add actions in monaco for add annotations and data dictionary elements. */
    if (this.canAddAnnotation) {
      this.addActionToEditor('codeViewer.createNewAnnotation', this.addAnnotation.bind(this, editor));
      this.addActionToEditor('codeViewer.addToDataDictionary', this.addDictionaryRecord.bind(this, editor));
    }
    this.ioService.getExportFormats(this.projectId).subscribe(exportExtensions => {
      const dataLineageExtension = exportExtensions.find(x => x.id === 'datalineage-gml');
      this.isDataLineageExtensionAvailable = dataLineageExtension ? true : false;
      if (this.isDataLineageExtensionAvailable) {
        this.moduleControllerService.getDataLineageAvailableForModule(this.projectId, this.moduleId).subscribe((result: boolean) => {
          if (result) {
            this.isDataLineageAvailable = result;
            this.addActionToEditor('dataLineage.modalTitle', this.exportGraphML.bind(this, editor));
          }
        });
      }
    });

    this.registerCommands();
    /**
     * Gets Data for Annotation and Data dictionary records for the module.
     * Renders data only after we have got the records for both.
     * Disabled in assembled view for now.
     */
    if ( ! this.assembled) {
      // Loads all the Data dictionaries for Module
      this.dataDictionaryController.findAllDataDictionaryEntries(this.projectId, this.moduleId).subscribe((ddRecords: DataDictionaryPojo[]) =>{
        this.fetchDDEntries(ddRecords);
      });

      // Loads all the Annotations for Module
      const annotationCountQuery = this.getAnnotationCountRequestQuery();
      this.graphQlControllerService.graphQl(annotationCountQuery, this.projectId).subscribe((response: { [key: string]: any }) => {
        if (response.data.annotations.totalElements < 200) {
         this.moduleControllerServiceSub?.unsubscribe();
         this.moduleControllerServiceSub = this.moduleControllerService.findAnnotationsForModule(this.projectId, this.moduleId)
         .subscribe((annotationRecords: AnnotationPojo[]) => {
            this.codeAnnotationArray = [];
            this.codeAnnotationArray = annotationRecords;
            this.drawLoadedAnnotations();
            this.scrollToEntryBasedOnOffset();
          });
        } else {
          const startLine = editor.getVisibleRanges()[0].startLineNumber;
          let endLine = editor.getVisibleRanges()[0].endLineNumber;
          if (this.loadedAnnotations.length === 0 && ! this.route.snapshot.queryParamMap.get('offset')) {
            endLine = endLine + virtualScrollBuffer;
            this.loadedAnnotations.push(...Array.from({ length: endLine - startLine + 1 }, (_, index) => startLine + index));
            this.fetchAnnotationByLines(+startLine, +endLine);
          }

          // To load the Annotations on the scroll.
          editor.onDidScrollChange(() => {
            this.fetchAnnotationOnScroll();
          });
          this.scrollToEntryBasedOnOffset();
        }
      });
    } else {
      this.scrollToEntryBasedOnOffset();
    }

    editor.onDidChangeCursorSelection(() => {
      this.isSelectionEmpty = editor?.getModel()?.getValueInRange(editor.getSelection()) === '';
    });

    if (this.assembledContent) {
      this.inclusionDecorations = editor.deltaDecorations(this.inclusionDecorations, this.assembledContent.inclusions.map(inclusion => ({
        range: inclusion.assembledRange as IRange,
        options: {
          className: 'included-range',
          isWholeLine: true
        }
      })));
    }
  }

  openAssembledView() {
    openInNewTab(this.projectId, this.moduleId, 'code-viewer/assembled', window);
  }

  /**
   * Registers commands for the links in the editor and in the data dictionary hover popup.
   */
  registerCommands() {
    editor.registerCommand(
      COMMAND_OPEN_DICT,
      () => this.openDataDictionaryDialog(false)
    );
    editor.registerCommand(
      COMMAND_REMOVE_DICT,
      () => this.removeDataDictionaryEntry()
    );
    editor.registerCommand(
      COMMAND_OPEN_CODE_VIEWER_LINK,
      (accessor, args) => this.openCodeViewerLink(args as CodeViewerLink)
    );
    editor.registerCommand(
      COMMAND_PEEK_DATAFLOW_REFERENCES,
      () => this.peekDataFlowReferences()
    );
    this.addActionToEditor('codeViewer.peekDataFlow', () => this.monacoEditor.trigger(null, COMMAND_PEEK_DATAFLOW_REFERENCES, null));
  }

  openCodeViewerLink(codeViewerLink: CodeViewerLink) {
    if (codeViewerLink.linkTargetType === 'EXTERNAL') {
      /* for external links, open module details for the target module in a new tab */
      openInNewTab(this.projectId, codeViewerLink.toModuleId, 'details', window);
    } else {
      /* for local links, we position the cursor on the first character of the link and then trigger the "peek references" feature */
      this.monacoEditor.setPosition({ lineNumber: codeViewerLink.fromRange.startLineNumber, column: codeViewerLink.fromRange.startColumn });
      this.monacoEditor.trigger(null, 'editor.action.referenceSearch.trigger', null);
    }
  }

  peekDataFlowReferences() {
    LinkAndReferenceProvider.provideDataFlowLinks = true;
    this.monacoEditor.trigger(null, 'editor.action.referenceSearch.trigger', null);
    /* HACK: to be able to navigate to the referenced location from the "peek references" popup, we listen to the next
       * cursor position change (only the next) and scroll the position of the cursor into view when it changes
       * This will not work reliably as the user can otherwise move the cursor. */
    const oneShotListener = this.monacoEditor.onDidChangeCursorPosition((cursorEvent: editor.ICursorPositionChangedEvent) => {
        this.scrollToOffset(this.calculateRealOffset(cursorEvent.position.lineNumber, cursorEvent.position.column));
        oneShotListener.dispose();
    });
  }

  /**
   * Method to create modal for Data Lineage based on validating offset.
   * @param editor The instance of the monaco editor.
   */
  exportGraphML(editor: editor.ICodeEditor): void {
    if (this.isDataLineageAvailable) {
      const lineNumber: number = editor.getSelection().getStartPosition().lineNumber;
      const startColumn: number = editor.getSelection().getStartPosition().column;
      const offset = this.calculateRealOffset(lineNumber, startColumn);
      this.dataDictionaryController.getFormatIfSelectionIsValid(this.projectId, this.moduleId, offset).subscribe(
        (response: DataFieldFormat) => {
          if (response) {
            this.dataLineageModalInstance = this.modalService.create<DataLineageExportComponent>({
              nzTitle: this.translateService.instant('dataLineage.modalTitle'),
              nzClosable: true,
              nzMaskClosable: false,
              nzWrapClassName: 'vertical-center-modal',
              nzKeyboard: true,
              nzWidth: '50vw',
              nzContent: DataLineageExportComponent
            });
            const modalComponent = this.dataLineageModalInstance.getContentComponent();
            modalComponent.projectId = this.projectId;
            modalComponent.moduleId = this.moduleId;
            modalComponent.fieldOffset = offset;
          }
        },
        (err: HttpErrorResponse) => {
          this.errorHandlerForTextSelection(err);
        }
      );
    } else {
      this.messageService.info(this.translateService.instant('module.dataLineageTooltip') as string);
    }
  }

  /**
   * Adds new annotation for module and renders new widget in the UI for the same.
   */
  addAnnotation() {
    if (this.sharedAnnotationEditorService.getEditorState() && ! this.isFormDirty()) {
      this.modalService.confirm({
        nzTitle: this.translateService.instant('sharedAnnotationEditorComponent.confirmModalTitle'),
        nzContent: this.translateService.instant('sharedAnnotationEditorComponent.confirmModalDesc'),
        nzOkText: this.translateService.instant('sharedAnnotationEditorComponent.confirmModalOk'),
        nzOkType: 'primary',
        nzOnOk: () => this.addNewAnnotation(),
        nzCancelText: this.translateService.instant('sharedAnnotationEditorComponent.confirmModalCancel'),
      });
    } else {
      this.addNewAnnotation();
    }
  }

  /**
   * Method is to open the shared-annotation-editor dynamically.
   * @param entity instance of the Editor.
   * @param isCreateMode to check if the Annotation is in create or edit mode..
   * @param annotationDetails data contained by the Annotation element
   */
  openSharedAnnotationEditor(entity: any, isCreateMode: boolean, annotationDetails?: any) {
    this.sharedAnnotationEditorService.setEditorState(true);
    this.headerTitle = this.translateService.instant('annotationReporting.sharedAnnotationEditorTitle', { moduleName: this.moduleName });
    if (this.childComponentRef) {
      this.childComponentRef.destroy();
      if (this.dictionaryDecorations.get(-1)) {
        this.monacoEditor.deltaDecorations(this.dictionaryDecorations.get(-1) as string[], []);
      }
    }
    this.showCrossBtn = true;
    this.sharedAnnotationEditorStatusClass = sharedAnnotationEditorClass + entity.state.toLowerCase().replace('_', '-');
    this.editorVisibility.visibility = 'visible';
    const componentFactory = this.CFR.resolveComponentFactory(SharedAnnotationEditorComponent);
    if (annotationDetails) {
      this.openedAnnotationDetails = annotationDetails;
    }
    this.childComponentRef = this.VCR.createComponent(componentFactory);
    this.childComponentRef.instance.annotation = entity;
    this.childComponentRef.instance.moduleId = this.moduleId;
    this.childComponentRef.instance.isCreateMode = isCreateMode;
    this.childComponentRef.instance.parentComponent = this.parentComponent;
    this.childComponentRef.instance.canCopyPasteAnnotation = this.canAddAnnotation;
    this.childComponentRef.instance.formResult.subscribe((event: FormResponse<AnnotationPojo>) => {
      this.handleSharedFormResult(event, annotationDetails as AnnotationDetails);
    });
  }

  /**
   * Deletes the Annotation and updates the UI accordingly.
   * @param initialDecorators the initial decorator of the Annotation to delete.
   * @param annotationElement data contained by the Annotation element
   */
  deleteAnnotation(initialDecorators: string[], annotationElement: AnnotationElementData): void {
    this.monacoEditor.deltaDecorations(initialDecorators, []);
    if (annotationElement.viewZoneId != null) {
      this.monacoEditor.changeViewZones((changeAccessor: any) => {
        changeAccessor.removeZone(annotationElement.viewZoneId);
        const zone = document.getElementById('line' + annotationElement.annotation.id);
        if (zone) {
          const zoneId = zone.getAttribute('monaco-view-zone');
          changeAccessor.removeZone(zoneId);
        }
      });
    }
    this.closeEditor();
  }

  /**
   * Updates the annotation and change the style fo the annotation accordingly.
   * removes the viewZone for editor and adds new one for state Change and adjust
   * the position of annotation again.
   * @param initialDecorators initial decoration for the Annotation .
   * @param lineNumberStart starting line no in the code for Annotation.
   * @param lineNumberEnd ending line no in the code for Annotation.
   * @param codeAnnotationComponent component containing the Annotation.
   */
  updateAnnotation(
    initialDecorators: any,
    lineNumberStart: number,
    lineNumberEnd: number,
    codeAnnotationComponent: CodeAnnotationEditorComponent
  ): void {
    const annotation = codeAnnotationComponent.data.annotation;
    const functionalBlockDetails = codeAnnotationComponent.data.functionalBlockDetails;
    const additionalClass =
      'annotation-editor-container__annotation--' + annotation.state.toLowerCase().replace('_', '-');
    const deltaDeco: editor.IModelDeltaDecoration = this.createAnnotationDecoration(
      annotation.id,
      lineNumberStart,
      lineNumberEnd,
      additionalClass
    );
    initialDecorators = this.monacoEditor.deltaDecorations([initialDecorators] as string[], [deltaDeco]);
    codeAnnotationComponent.data = this.getDataForAnnotationComponent(annotation, codeAnnotationComponent.data.viewZoneId);
    codeAnnotationComponent.projectLevelFgs = this.projectLevelFgs;
    codeAnnotationComponent.data.functionalBlockDetails = functionalBlockDetails;
    this.closeEditor();
  }

  /**
   * Adds a new right click action to the monaco editor.
   * @param idLabel The label which to show on right click.
   * @param fn The function which should be called on click.
   */
  addActionToEditor(idLabel: string, fn: any): void {
    (this.monacoEditor as any).addAction({
      id: idLabel,
      label: this.translateService.instant(idLabel),
      precondition: null,
      keybindingContext: null,
      contextMenuGroupId: 'navigation',
      contextMenuOrder: 1.5,
      run: fn,
    });
  }

  /**
   * Delta decoration for annotations.
   * @param id id of the data dictionary record.
   * @param lineNumberStart starting line no in the code for Annotation.
   * @param lineNumberEnd ending line no in the code for Annotation
   * @param additionalClass class for the Annotation code highlight
   */
  createAnnotationDecoration(id: number, lineNumberStart: number, lineNumberEnd: number, additionalClass: string): any {
    return {
      range: this.getRange(lineNumberStart, 1, lineNumberEnd, this.MAX_LINE_CHARS),
      options: {
        id: 'annotation-code-lines-' + id,
        className: additionalClass,
      },
    };
  }

  /**
   * Delta decoration for Data dictionary records.
   * @param id id of the data dictionary record.
   * @param lineNumberStart starting line no in the code for DD record.
   * @param columnStart starting column no in the code for DD record.
   * @param lineNumberEnd ending line no in the code for DD record
   * @param columnEnd ending column no in the code for DD record.
   */
  createDictionaryDecoration(
    id: number,
    lineNumberStart: number,
    columnStart: number,
    lineNumberEnd: number,
    columnEnd: number
  ): { range: any; options: { id: string; className: string } } {
    const additionalClass = 'annotation-editor-container__data-dictionary';
    return {
      range: this.getRange(lineNumberStart, columnStart, lineNumberEnd, columnEnd),
      options: {
        id: 'general-delta-decoration-' + id,
        className: additionalClass,
      },
    };
  }

  /**
   * Calculates the line number in the original source code with given offset and length.
   * @param offset from original source code.
   * @param length from original source code.
   */
  calculateLineAndColumn(offset: number, length: number): any {
    let i: number;
    /* Initial line is 1! */
    let line = 1;
    let currentColumn = 1;
    for (i = 0; i < offset + length; i++) {
      currentColumn++;
      if (this.initialSourceCode.charAt(i) === '\n') {
        line++;
        currentColumn = 1;
      }
    }
    return {
      line,
      currentColumn,
    };
  }

  /**
   * Given a lineNumber and a column this function computes the offset in the original source code.
   * @param lineNumberStart from the truncated cobol code.
   * @param column from the truncated cobol code.
   */
  calculateRealOffset(lineNumberStart: number, column: number): number {
    const splitContent = this.initialSourceCode.split('\n');
    let offset = 0;
    for (let i = 0; i < lineNumberStart - 1 && i < splitContent.length; i++) {
      // +1 is for the line break.
      offset += splitContent[i].length + 1;
    }
    offset += column - 1;
    return offset;
  }

  /**
   * Get the class name of the glyph ( right bar highlighting for annotation-types)
   * @param type The type of the annotation.
   */
  getGlyphClassName(type: AnnotationPojo.TypeEnum): string {
    let loadedGlyph: string;
    const classPrefix = 'annotation-editor-container__glyph--';
    switch (type) {
      case 'DATABASE':
        loadedGlyph = classPrefix + 'database';
        break;
      case 'DEAD_CODE':
        loadedGlyph = classPrefix + 'dead-code';
        break;
      case 'EXCLUDE':
        loadedGlyph = classPrefix + 'exclude';
        break;
      case 'RULE':
        loadedGlyph = classPrefix + 'rule';
        break;
    }
    return loadedGlyph;
  }

  /**
   * Display the data dictionary dialog.
   * We do this in the ngZone-context since this action can be triggered by the
   * monaco editor(right-click, add-to-data-dictionary/edit) and angular often fails
   * to notice the need to draw the dialog immediately.
   * @param isNewRecord Whether the data dictionary record is newly created.
   * @param data populates the value in the editor.
   * @param resultDecorators decorators for the selected text.
   * @param lineNumber line number to add the highlighted line.
   */
  openDataDictionaryDialog(isNewRecord: boolean, data?: DataFieldFormat, resultDecorators?: string[], lineNumber?: number): void {
    this.isNewDDEntry = isNewRecord;
    this.currentEntryId = isNewRecord ? -1 : this.currentEntryId;
    this.showCrossBtn = true;
    this.headerTitle = this.translateService.instant('sharedDataDictionaryEditor.headerTitle', { moduleName: this.moduleName });
    const id = isNewRecord ? data['fieldName'] : this.dataDictionaryEntryMap.get(this.currentEntryId).dataElementName;
    const lineId = 'line' + id;
    const stateColor = this.stateColor['DDENTRY'];
    if (this.childComponentRef) {
      this.childComponentRef.destroy();
      if (this.currentEntryId !== -1 && this.dictionaryDecorations.get(-1)) {
        this.monacoEditor.deltaDecorations(this.dictionaryDecorations.get(-1) as string[], []);
      }
      if (this.childComponentRef.instance?.isNewEntry && isNewRecord && this.currentEntryId !== -1) {
        this.monacoEditor.deltaDecorations(this.dictionaryDecorations.get(this.currentEntryId) as string[], []);
      }
      this.removeZone(this.viewZoneId);
    }
    if (isNewRecord) {
      if (this.dictionaryDecorations.get(this.currentEntryId)) {
        this.monacoEditor.deltaDecorations(this.dictionaryDecorations.get(this.currentEntryId) as string[], []);
      }
      this.dictionaryDecorations.set(this.currentEntryId, resultDecorators);
    } else {
      const lineAndColumn = this.calculateLineAndColumn(this.dataDictionaryEntryMap.get(this.currentEntryId).location.offset,
        this.dataDictionaryEntryMap.get(this.currentEntryId).location.length);
      lineNumber = lineAndColumn.line;
    }
    /**
     * Creates the horizontal line in the editor to the corresponding DD Entry.
     */
    this.viewZoneId = this.changeViewZone(lineId, stateColor as string, lineNumber, monacoEditorStyle.editorSelectedLineHeight);
    const dataDictionaryEntry: DataDictionaryPojo = isNewRecord ? {
      dataElementName: data['fieldName'],
      length: data.byteLength,
      format: data.languageType,
      location: data.location,
      createdByUserId: this.authService.getUsername(),
      isBusiness: false,
      fieldTransformation: '',
      sourceInput: '',
      targetOutput: '',
      scopes: {},
      usage: data.usage,
      definedLocation: data.definedLocation as DataDictionaryPojo.DefinedLocationEnum
    } : this.dataDictionaryEntryMap.get(this.currentEntryId);
    this.sharedAnnotationEditorStatusClass = sharedAnnotationEditorClass + 'data-dictionary';
    this.editorVisibility.visibility = 'visible';
    const componentFactory = this.CFR.resolveComponentFactory(SharedDataDictionaryEditorComponent);
    this.childComponentRef = this.VCR.createComponent(componentFactory);
    this.childComponentRef.instance.moduleId = this.moduleId;
    this.childComponentRef.instance.dataDictionaryItem = dataDictionaryEntry;
    this.childComponentRef.instance.isNewEntry = isNewRecord;
    this.childComponentRef.instance.isCodeViewerScreen = true;
    this.childComponentRef.instance.showButtons = false;
    this.childComponentRef.instance.formResult.subscribe((event: FormResponse<DataDictionaryPojo>) => {
      this.handleSharedDDFormResult(event, resultDecorators);
    });
  }

  /**
   * Syntax highlight all data dictionary entries.
   */
  drawLoadedDataDictionaryEntries(): void {
    this.dataDictionaryEntryMap.forEach((entry) => {
      /* Add delta decorations in correct places */
      const offset = entry.location.offset;
      const length = entry.location.length;
      const res = this.calculateLineAndColumn(offset, 0);
      const lineNumberStart: number = res.line;

      /* Since we're displaying truncated COBOL this has to be taken into account. */
      const columnStart: number = res.currentColumn;

      /* Select correct glyph classes. */
      const deltaDeco = this.createDictionaryDecoration(
        entry.id,
        lineNumberStart,
        columnStart,
        lineNumberStart,
        columnStart + length
      );
      const decoRef = this.monacoEditor.deltaDecorations([], [deltaDeco]);
      this.dictionaryDecorations.set(entry.id, decoRef);
    });
  }

  /**
   * Handles the output of the shared annotation editor
   * @param result output of the shared annotation editor
   */
  handleSharedFormResult(formResponse: FormResponse<AnnotationPojo>, annotationDetails: AnnotationDetails): void {
    switch (formResponse.result) {
      case FormResult.Canceled:
        this.onCancel();
        break;
      case FormResult.Error:
        this.onError();
        break;
      case FormResult.Saved:
        if (formResponse.returnedObject) {
          this.postUpdate(formResponse.returnedObject);
        } else {
          this.updateAnnotation(annotationDetails.initialDecorators,
            annotationDetails.lineNumberStart, annotationDetails.lineNumberEnd, annotationDetails.codeAnnotationComponent);
        }
        break;
      case FormResult.Deleted:
        this.deleteAnnotation(annotationDetails.initialDecorators, annotationDetails.codeAnnotationComponent.data);
        break;
      case FormResult.Disabled:
        this.isClose = true;
    }
  }

  /**
   * Handles the output of the shared data dictionary editor
   * @param formResponse Form action emitted from the DD component.
   * @param resultDecorators decorators to be added.
   */
  handleSharedDDFormResult(formResponse: FormResponse<DataDictionaryPojo>, resultDecorators: string[]): void {
    switch (formResponse.result) {
      case FormResult.Canceled:
        this.onCancel();
        break;
      case FormResult.Saved:
        this.updateDD(formResponse.returnedObject, resultDecorators);
        break;
      case FormResult.Deleted:
        this.onDelete(this.currentEntryId);
        break;
      case FormResult.Disabled:
        this.isClose = true;
    }
  }

  /**
   * Closes the currently open annotation editor.
   * @param annotationDetails annotation details to update the line.
   */
  onCancel(): void {
    this.isClose = true;
    if (this.isClose) {
      this.closeEditor();
    }
    this.removeZone(this.viewZoneId);
    if (this.isNewDDEntry) {
      this.dictionaryDecorations.get(this.currentEntryId) ? this.monacoEditor.deltaDecorations(
        this.dictionaryDecorations.get(this.currentEntryId) as string[], []) :
        this.dictionaryDecorations.set(this.currentEntryId, []);
    }
    this.openedAnnotationDetails = null;
  }

  /**
   * Validates Data Dictionary record, highlighting the code and glyph margin class on the left.
   * calls {createDataDictionaryEntry} function to add the record into DB.
   */
  addDictionaryRecord(): void {
    if (this.hasAstNodes) {
      let lineNumber: number = this.monacoEditor.getSelection().getStartPosition().lineNumber;
      const startColumn: number = this.monacoEditor.getSelection().getStartPosition().column;
      const offset = this.calculateRealOffset(lineNumber, startColumn);
      this.dataDictionaryController.getFormatIfSelectionIsValid(this.projectId, this.moduleId, offset).subscribe(
        (response: DataFieldFormat) => {
          let resultDecorators: string[] = [];
          if (response.dataDictionaryEntryId === null) {
            const endColumn = this.calculateLineAndColumn(response.location.offset, response.location.length).currentColumn as number;
            lineNumber = this.calculateLineAndColumn(response.location.offset, response.location.length).line as number;
            resultDecorators = this.monacoEditor.deltaDecorations([], [this.createDecoration(lineNumber, endColumn - response.location.length, endColumn)]);
            this.createDataDictionaryEntry(resultDecorators, lineNumber, response);
          } else {
            this.currentEntryId = response.dataDictionaryEntryId;
            this.openDataDictionaryDialog(false, response, resultDecorators, lineNumber);
          }
        },
        (err: HttpErrorResponse) => {
          this.errorHandlerForTextSelection(err);
        }
      );
    } else {
      this.moduleControllerService.storeAstNodes(this.projectId, this.moduleId).subscribe(
        (resp) => {
          if (resp) {
            this.hasAstNodes = resp;
            this.addDictionaryRecord();
            this.drawLoadedDataDictionaryEntries();
          } else {
            this.showError('codeViewer.errorInitDataDictionary');
          }
        },
        (err) => {
          this.showError('codeViewer.errorInitDataDictionary', `${err.message}`);
        }
      );
    }
  }

  /**
   * Removes Data Dictionary record from DB and UI.
   */
  removeDataDictionaryEntry(): void {
    const entryId = this.currentEntryId;
    this.dataDictionaryController.deleteDataDictionaryEntry(this.projectId, this.moduleId, entryId).subscribe(
      () => {
        this.onDelete(entryId);
        this.messageService.success(this.translateService.instant('dataDictionary.deleteEntrySuccess') as string);
      },
      (err) => {
        const msg = 'Error occurred while deleting dictionary record: ' + err.message;
        this.showError('codeViewer.dictionaryDeleteErr', msg);
      }
    );
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
    this.scrollEventSubscription?.unsubscribe();
    this.moduleControllerServiceSub?.unsubscribe();
    if (this.routingSubscription) {
      this.routingSubscription.unsubscribe();
    }
  }

  /**
   * Check if the annotation form is dirty or not.
   */
  isFormDirty(): boolean {
    return ! this.childComponentRef?.instance?.annotationForm?.dirty;
  }

  private showDataLineage() {
    const lineNumberStart = this.monacoEditor.getSelection().getStartPosition();
    const offset: number = this.monacoEditor.getModel().getOffsetAt(lineNumberStart);
    let path = 'data-lineage?offset=' + offset;
    if (this.assembled) {
      path += '&assembled=true';
    }
    this.dataDictionaryController.getFormatIfSelectionIsValid(this.projectId, this.moduleId, offset, this.assembled).subscribe(
      (response: DataFieldFormat) => {
        if (response) {
          path += '&field=' + response.fieldName;
          openInNewTab(this.projectId, this.moduleId, path, this.$window);
        }
      },
      (err: HttpErrorResponse) => {
        this.errorHandlerForTextSelection(err);
      });
  }

  private scrollToOffset(offset: number): void {
    const linenumber: number = this.calculateLineAndColumn(offset, 0).line;
    this.monacoEditor.revealLineInCenter(linenumber);
  }

  /**
   * Updates the DD Entry in the code viewer.
   * @param data saved DD entry data
   * @param resultDecorators decorators for the selected text.
   */
  private updateDD(data: DataDictionaryPojo, resultDecorators: string[]) {
    this.currentEntryId = data.id;
    this.dataDictionaryEntryMap.set(data.id, data);
    this.dictionaryDecorations.set(this.currentEntryId, resultDecorators);
    this.dictionaryDecorations.set(-1, []);
    this.closeEditor();
    this.removeZone(this.viewZoneId);
  }

  /**
   * Closes the editor and removes zone in case of error.
   */
  private onError(): void {
    this.removeZone(this.viewZoneId);
  }

  /**
   * Gets name and Source code for the loaded module.
   * @param moduleId Id of the currently loaded Module.
   * @param projectId Id of the currently loaded Project.
   */
  private fetchModule(module: ModulePojo) {
    this.moduleName = module.name;
    if (module.content) {
      /* Sets the theme and the language for Monaco Editor */
      const moduleTechnology: ModulePojo.TechnologyEnum = module.technology;
      const tech = moduleTechnology.toLowerCase();
      if (
        module.technology === ModulePojo.TechnologyEnum.CICS &&
        (module.type === ModulePojo.TypeEnum.BMS_MAP || module.type === ModulePojo.TypeEnum.BMS_MAPSET
      )) {
        this.EDITOR_OPTIONS.language = 'bms';
      } else {
        this.EDITOR_OPTIONS.language = tech;
      }
      this.EDITOR_OPTIONS.theme = 'vs';

      /* Sets the code */
      if (this.assembled) {
        this.moduleControllerService.getAssembledContent(this.projectId, this.moduleId).subscribe(assembledContent => {
          this.assembledContent = assembledContent;
          if (assembledContent.available) {
            this.initialSourceCode = assembledContent.content;
            this.code = assembledContent.content;
          } else {
            /* no assembled content available for module - use regular module.content */
            this.initialSourceCode = module.content;
            this.code = module.content;
          }
          this.loadState = LoaderState.success;
        });
      } else {
        this.initialSourceCode = module.content;
        this.code = module.content;
        this.loadState = LoaderState.success;
      }
      this.checkAstNodesForModule(module.id, module.projectId);
      this.editorDataLoaded = false;
    } else {
      this.loadState = LoaderState.error;
      this.loadStateError = this.translateService.instant('codeViewer.annotationModuleContentEmpty');
    }
  }

  /**
   * Determines if the module has AST node or not.
   * @param moduleId Id of the currently loaded Module.
   * @param projectId Id of the currently loaded Project.
   */
  private checkAstNodesForModule(moduleId: EntityId, projectId: number) {
    this.moduleControllerService.hasAstNodes(projectId, moduleId).subscribe((resp) => {
      this.hasAstNodes = resp;
      if ( ! this.hasAstNodes) {
        this.moduleControllerService
          .storeAstNodes(projectId, moduleId)
          .subscribe((astEntryNodeId) => (this.hasAstNodes = astEntryNodeId));
      }
    });
  }

  /**
   * Adds annotations fetched from DB to Editor.
   */
  private drawLoadedAnnotations() {
    this.codeAnnotationArray.forEach((element) => {
      if ( ! this.drawnAnnotationIds.has(element.id)) {
        this.drawnAnnotationIds.add(element.id);
        const offset = element.location.offset;
        const length = element.location.length;
        const startLine: number = this.calculateLineAndColumn(offset, 0).line;
        const endLine: number = this.calculateLineAndColumn(offset, length).line;
        this.addAnnotationToEditor(startLine, endLine, element);
      }
    });
  }

  /**
   * This is the main method for annotating the code, this includes creation of the annotation component, highlighting the source code,
   * adding glyphs(left bar highlighting).
   * @param lineNumberStart The starting line number of the annotation.
   * @param lineNumberEnd The ending line number of the annotation.
   * @param annotation The actual annotation.
   */
  private addAnnotationToEditor(
    lineNumberStart: number,
    lineNumberEnd: number,
    annotation: AnnotationPojo
  ) {
    const annotationId = annotation.id;
    const stateColor = this.stateColor[annotation.state];
    const stateClass = 'annotation-editor-container__annotation--' + annotation.state.toLowerCase().replace('_', '-');
    const newDeltaDecorations: editor.IModelDecoration = this.createAnnotationDecoration(
      annotationId,
      lineNumberStart,
      lineNumberEnd,
      stateClass
    );
    const annotationData = this.getDataForAnnotationComponent(annotation);
    const initialDecorators =  this.monacoEditor.deltaDecorations([], [newDeltaDecorations]);

    const domNode = this.renderer.createElement('code-annotation-editor-element');
    const lineId = 'line' + annotationId;

    /**
     * Creates the horizontal line in the editor to the corresponding Annotation.
     * Sets the top for the annotation position in the editor.
     */

    /* line top must be calculated as not initially rendered if below initial view */
    const annotationComponentId = annotationElementIdPrefix + annotationId;
    domNode.id = annotationComponentId;
    domNode.className = 'code-annotation-editor-component-container';

    /* Populate the component with the values from the annotation. */
    const codeAnnotationComponent: NgElement & WithProperties<CodeAnnotationEditorComponent> = domNode;
    codeAnnotationComponent.data = annotationData;
    codeAnnotationComponent.projectLevelFgs = this.projectLevelFgs;
    codeAnnotationComponent.annotationsOfModule = this.codeAnnotationArray;
    codeAnnotationComponent.projectId =  this.projectId;
    codeAnnotationComponent.data.viewZoneId  = this.changeViewZone(
      lineId, stateColor as string, lineNumberStart, monacoEditorStyle.annotationMinHeight, codeAnnotationComponent );
    /* Define all callback functions. */
    codeAnnotationComponent.deletedCallback = this.deleteAnnotation.bind(
      this,
      initialDecorators,
      codeAnnotationComponent.data
    );
    codeAnnotationComponent.showError = (message: string) => this.showError(message);
    const annotationDetails = {
      initialDecorators,
      lineNumberStart,
      lineNumberEnd,
      codeAnnotationComponent
    };
    codeAnnotationComponent.openEditor = this.openSharedAnnotationEditor.bind(this, annotation, false, annotationDetails);
  }

  /**
   * returns the data for the code Annotation custom element we have created.
   * @param annotation annotation data saved in the system.
   * @returns object of type {AnnotationElementData}
   */
  private getDataForAnnotationComponent(annotation: AnnotationPojo, viewZoneId?: string): AnnotationElementData {
    const data: AnnotationElementData = {
      viewZoneId,
      annotation,
      moduleId: this.moduleId,
      moduleName: this.moduleName,
      projectId: this.projectId,
      borderColor: this.stateColor[annotation.state],
      typeLabel: this.annotationType.find((item: any) => item.value === annotation.type).label,
      stateLabel: this.annotationState.find((item: any) => item.value === annotation.state).label,
      modulePath: this.modulePath,
      isEdit: this.isEdit,
      canCopyAnnotation: this.canAddAnnotation
    };
    return data;
  }

  /**
   * Creates the Data dictionary in the system and shows the Data Dictionary Model.
   * @param resultDecorators decoration for Monaco editor.
   * @param lineNumber line no in the code for DD record.
   * @param response Response received after validation.
   * @param startColumn Staring column no in the code for DD record.
   * @param text selected Data dictionary text.
   */
  private createDataDictionaryEntry(
    resultDecorators: string[],
    lineNumber: number,
    response: DataFieldFormat ) {
    this.isNewDDEntry = true;
    this.openDataDictionaryDialog(true, response, resultDecorators, lineNumber);
  }

  /**
   * Shows the toast notification for errors.
   * Logs error to console, if errMsg passed.
   * @param toastMsg the message needs to be displayed to user.
   * @param errMsg the message needs to shown in console for developer.
   */
  private showError(toastMsg: string, errMsg: string = '') {
    this.messageService.error(`${this.translateService.instant(toastMsg)}`);

    if (errMsg) {
      log.error('ProjectID( ' + this.projectId + ') | ModuleID(' + this.moduleId + ') | ' + errMsg);
    }
  }

  /**
   * Shows the toast notification for errors.
   * Logs error to console, if errMsg passed.
   * @param toastMsg the message needs to be displayed to user.
   * @param errMsg the message needs to shown in console for developer.
   */
  private showWarning(toastMsg: string, errMsg: string = ''): void {
    this.messageService.warning(`${this.translateService.instant(toastMsg)}`);

    if (errMsg) {
      log.error('ProjectID( ' + this.projectId + ') | ModuleID(' + this.moduleId + ') | ' + errMsg);
    }
  }


  /**
   * Returns the range for the selection.
   * @param lineNumberStart starting line no for selection
   * @param columnStart starting column no in selection
   * @param lineNumberEnd ending line no for selection
   * @param columnEnd ending column no in selection
   */
  private getRange(lineNumberStart: number, columnStart: number, lineNumberEnd: number, columnEnd: number) {
    return new Range(lineNumberStart, columnStart, lineNumberEnd, columnEnd);
  }

  private postUpdate(annotation: AnnotationPojo): void {
    const offset = annotation.location.offset;
    const length = annotation.location.length;
    const lineNumberStart: number = this.calculateLineAndColumn(offset, 0).line;
    const lineNumberEnd: number = this.calculateLineAndColumn(offset, length).line;
    this.codeAnnotationArray.push(annotation);
    this.removeZone(this.viewZoneId);
    this.addAnnotationToEditor(lineNumberStart, lineNumberEnd, annotation);
    this.closeEditor();
  }

  private removeZone(zoneId: string) {
    this.monacoEditor.changeViewZones((changeAccessor: any) => {
      changeAccessor.removeZone(zoneId);
    });
  }

  private changeViewZone(lineId: string, stateColor: string, lineNumber: number,
    lineHeight: number, codeAnnotationComponent?: any): string {
    let viewZoneId: string;
    const domNode = codeAnnotationComponent || document.createElement('div');
    if (! codeAnnotationComponent) {
      domNode.setAttribute('id', lineId);
      domNode.setAttribute('style', `background-color: ${stateColor};`);
    }
    this.monacoEditor.changeViewZones((changeAccessor: any) => {
      viewZoneId = changeAccessor.addZone({
        afterLineNumber: lineNumber - 1,
        heightInPx: codeAnnotationComponent ? lineHeight - 10 : lineHeight,
        domNode
      });
    });
    return viewZoneId;
  }

  private closeEditor() {
    if (this.openedAnnotationDetails) {
      this.openedAnnotationDetails.codeAnnotationComponent.data.isEdit = false;
    }
    this.isSave = false;
    this.isDelete = false;
    this.isClose = false;
    this.showCrossBtn = false;
    this.editorVisibility.visibility = 'hidden';
    this.childComponentRef?.destroy();
    this.sharedAnnotationEditorService.setEditorState(false);
  }

  private createDecoration(lineNumber: number, startColumn: number, endColumn: number): editor.IModelDeltaDecoration {
    return {
      range: this.getRange(lineNumber, startColumn, lineNumber, endColumn),
      options: {
        className: 'annotation-editor-container__data-dictionary',
        glyphMarginClassName: 'annotation-editor-container__glyph--data-dictionary'
      },
    };
  }

  private onDelete(entryId: number) {
    this.dataDictionaryEntryMap.delete(entryId);
    this.closeEditor();
    if (this.dictionaryDecorations.get(entryId)) {
      this.monacoEditor.deltaDecorations(this.dictionaryDecorations.get(entryId) as string[], []);
    }
    this.removeZone(this.viewZoneId);
  }

  /**
   * Shows specific error message based on error or failure.
   * @param err response that represents an error or failure.
   */
  private errorHandlerForTextSelection(err: HttpErrorResponse) {
    const msg = 'Error occurred while validating selected text for dictonary: ' + err.message;
    if (err.status === 400) {
      this.showWarning('codeViewer.dictionaryCursorNotOnDataField', msg);
    } else {
      this.showError('codeViewer.dictionaryValidatingErr', msg);
    }
  }

  private fetchAnnotationByLines(startLine: number, endLine: number): void {
    const startOffset = this.calculateRealOffset(startLine, this.monacoEditor.getVisibleRanges()[0].startColumn);
    const endOffset = this.calculateRealOffset(endLine, this.monacoEditor.getVisibleRanges()[0].endColumn);
    this.annotationControllerService.findAnnotationBasedOnOffset(this.projectId, this.moduleId, startOffset, endOffset).subscribe((resp) => {
      if (resp.length) {
        this.codeAnnotationArray = resp;
        this.drawLoadedAnnotations();
      }
    });
  }

  private fetchAnnotationOnScroll(): void {
    const startLine = this.monacoEditor.getVisibleRanges()[0].startLineNumber;
    const endLine = this.monacoEditor.getVisibleRanges()[0].endLineNumber + virtualScrollThreshold;
    const linesToFetch = [];
    for (let line = startLine; line <= endLine; line++) {
      if ( ! this.loadedAnnotations.includes(line)) {
        linesToFetch.push(line); // Lines to fetch based on current scroll.
        this.loadedAnnotations.push(line); // Mark as fetched for the overall module.
      }
    }
    if (linesToFetch.length) {
      this.loadedAnnotations.push(...Array.from({ length: virtualScrollBuffer }, (_, index) => endLine + index));
      this.fetchAnnotationByLines(linesToFetch[0], linesToFetch[linesToFetch.length - 1] + virtualScrollBuffer);
    }
  }

  private fetchDDEntries(ddRecords: DataDictionaryPojo[]): void {
    if (ddRecords.length) {
      for (const entry of ddRecords) {
        this.dataDictionaryEntryMap.set(entry.id, entry);
      }
      this.drawLoadedDataDictionaryEntries();
    }
  }

  private scrollToEntryBasedOnOffset(): void {
    /**
     * if there is no annotation or DD at that time as we are using forkJoin(emits no value)
     * change detection is not happening for editorDataLoaded if we refresh
     * the page, so calling ngZone to force the change detection.
     */
      this.editorDataLoaded = true;
      if (Number(this.route.queryParams['_value'].offset)) {
        const offsetNumber = Number(this.route.queryParams['_value'].offset as string);
        this.scrollToOffset(offsetNumber);
      }
  }

  private getAnnotationCountRequestQuery(): { [key: string]: any } {
    const additionalProperties: string[] = ['totalElements'];
    const filter = {
      content_module_id: {
        eq: this.moduleId
      }
    };
    const getGraphQlParam = this.parametersService.getGraphQlParam(this.projectId, []);
    return {
      'query': graphQlQuery(
        'annotations',
        getGraphQlParam,
        [],
        additionalProperties,
        true),
      'variables': {
        filter
      }
    };
  }

  /**
   * Adds new annotation for module and renders new widget in the UI for the same.
   */
  private addNewAnnotation() {
    this.showCrossBtn = true;
    const lineNumberStart: number = this.monacoEditor.getSelection().getStartPosition().lineNumber;
    const lineNumberEnd: number = this.monacoEditor.getSelection().getEndPosition().lineNumber;
    const startOffset = this.calculateRealOffset(lineNumberStart, 1);
    const endOffset = this.calculateRealOffset(lineNumberEnd + 1, 1) - 1;
    const moduleLocation: ModuleLocation = {
      length: endOffset - startOffset,
      offset: startOffset,
    };
    const newDescription = '';
    this.removeZone(this.viewZoneId);
    this.sharedAnnotationEditor?.resetForm();
    this.newAnnotation = {
      projectEntity: this.projectId,
      state: 'IN_ANALYSIS',
      type: 'RULE',
      name: newDescription,
      categoryId: null,
      categoryName: '',
      createdByUserId: this.authService.getUsername(),
      moduleName: this.moduleName,
      location: moduleLocation,
      sourceAttachment: this.monacoEditor.getModel().getValueInRange(this.monacoEditor.getSelection()),
    };
    this.viewZoneId = this.changeViewZone( '', this.stateColor[this.newAnnotation.state] as string,
    lineNumberStart, monacoEditorStyle.editorSelectedLineHeight);
    this.openSharedAnnotationEditor(this.newAnnotation, true);
    this.isSave = false;
    this.isDelete = false;
    this.isClose = false;
  }

}
