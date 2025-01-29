/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, Renderer2, ViewChild, ComponentRef, ViewContainerRef,
 ComponentFactoryResolver } from '@angular/core';
import { CfgMetaDataService } from '@app/core/services/cfg-meta-data.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { CfgMetaData } from '@app/modules/graph/models/cfg-meta-data.model';
import { CfgMetaDataPanelState } from './cfg-meta-data.interface';
import { Observable, Subscription } from 'rxjs';
import { ActivatedRoute } from '@angular/router';
import { AnnotationControllerService, AnnotationPojo, AnnotationReport, ControlFlowNode, EntityId, ModulePojo} from '@innowake/mining-api-angular-client';
import styles from '../../../../theme/style.json';
import { monacoEditorStyle } from '../../../../theme/monaco-editor-style';
import {
  CodeAnnotationEditorComponent,
  annotationElementIdPrefix,
} from '../../../modules/mining-code-viewer/code-annotation/code-annotation-editor.component';
import { NgElement, WithProperties } from '@angular/elements';
import { AnnotationDetails, AnnotationElementData } from '@app/modules/mining-code-viewer/mining-code-viewer';
import { AnnotationEditor, FormResponse, FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { SharedAnnotationEditorComponent } from '../shared-annotation-editor/shared-annotation-editor.component';
import { Logger } from '@app/core';
import { editor, Range } from 'monaco-editor/esm/vs/editor/editor.api';
import { DataLineageNode } from '@app/modules/module-data-lineage/models/data-lineage.node';
import { ControlFlowNodeDetails } from '@app/modules/graph/module-control-flow/models/control-flow-node-details';

const log = new Logger('ControlFlowMetaData');
const sharedAnnotationEditorClass = 'mining__card-drawer shared-side-viewer__drawer-editor shared-side-viewer__drawer-editor--open-';

@Component({
  selector: 'control-flow-meta-data',
  templateUrl: './control-flow-meta-data.component.html'
})
export class ControlFlowMetaDataComponent implements OnInit, OnDestroy {
  @Input() sidePanelTitle: string;
  @Input() fromDataLineage: boolean;
  @Input() projectId: number;
  @Input() cfgMetaData: CfgMetaData[];
  @Input() nodeSelection: Observable<{controlFlowNode: ControlFlowNodeDetails, graphNodes: ControlFlowNodeDetails[]}>;
  @Input() dlNodeSelection: Observable<{dataLineageNode: DataLineageNode, module: ModulePojo}>;
  @Input() errorMarkerSubscription: Observable<any>;
  @Input() isErrorMarker = false;
  @Output() hideShowDrawer: EventEmitter<boolean> = new EventEmitter();
  @Output() annotationChange: EventEmitter<any> = new EventEmitter();
  @ViewChild('viewContainerRef', { read: ViewContainerRef })
  VCR: ViewContainerRef;
  childComponentRef: ComponentRef<any>;
  editorVisibility = { visibility: 'hidden' };

  public EDITOR_OPTIONS: editor.IStandaloneEditorConstructionOptions = {
    scrollbar: {
      horizontal: 'hidden',
      alwaysConsumeMouseWheel: false
    },
    minimap: { enabled: false },
    theme: 'vs',
    language: 'cobol' || 'natural',
    readOnly: true,
    glyphMargin: false,
    automaticLayout: true,
    renderLineHighlight: 'none',
    contextmenu: true,
    wordWrap: 'on'
  };
  monacoEditor: editor.ICodeEditor;
  deltaDeco: any;
  maintainState: CfgMetaDataPanelState = {
    inputFiles: false,
    outputFiles: false
  };
  eventsSubscription: Subscription;
  code: string;
  widePanel = true;
  parentComponent = AnnotationEditor.CODEVIEWER_ANNOTATION_EDITOR;
  annotationType: any[] = [];
  annotationState: any[] = [];
  openedAnnotationDetails: AnnotationDetails;
  sharedAnnotationEditorStatusClass: string = sharedAnnotationEditorClass;
  moduleId: EntityId;
  modulePath: string;
  moduleName: string;
  headerTitle: string;
  viewZoneId: string;
  showCrossBtn = true;
  isSave = false;
  isDelete = false;
  isClose = false;
  controlFlowNode: ControlFlowNodeDetails;
  graphNodes: ControlFlowNodeDetails[];
  private MAX_LINE_CHARS = 160; // 160 is the max length we are expecting in the code, it could be more as well.
  private codeAnnotationArray: AnnotationPojo[] = [];
  private stateColor = {
    APPROVED: styles.$approvedAnnotation,
    CANDIDATE: styles.$candidateAnnotation,
    FOR_REVIEW: styles.$forReviewAnnotation,
    IN_ANALYSIS: styles.$inAnalysisAnnotation,
    REJECTED: styles.$rejectedAnnotation,
    INVALID: styles.$invalidAnnotation,
    DDENTRY: styles.$dictionaryBackground
  };

  constructor(
    private cfgMetaDataService: CfgMetaDataService,
    private annotationControllerService: AnnotationControllerService,
    private translateService: TranslateService,
    private messageService: NzMessageService,
    private CFR: ComponentFactoryResolver,
    private renderer: Renderer2,
    private route: ActivatedRoute
  ) {
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
    this.maintainState = this.cfgMetaDataService.getState();
    if (this.isErrorMarker) {
      this.widePanel = false;
      this.fetchModuleDetailsFromRoute();
      this.eventsSubscription = this.errorMarkerSubscription.subscribe((res) => {
       this.code = res.code;
       this.setOffset(res.offset as number, res.length as number);
      });
    } else if (this.fromDataLineage) {
      this.widePanel = false;
      this.eventsSubscription = this.dlNodeSelection.subscribe((dataLineageData: {dataLineageNode: DataLineageNode, module: ModulePojo}) => {
        if (dataLineageData.module && dataLineageData.dataLineageNode) {
          this.code = dataLineageData.module.content;
          this.moduleId = dataLineageData.module.id;
          this.modulePath = dataLineageData.module.path;
          this.moduleName = dataLineageData.module.name;

          /* Remove the existing View Zones */
          this.removeZone(this.viewZoneId);
          if (this.monacoEditor) {
            this.monacoEditor.setValue(this.code);
          }
          this.onSelectionCodeViewerFromDl(dataLineageData.dataLineageNode);
        }
      });
    } else {
      /*
        Note: Fetching the module code from the route, this will only work when we have the component being
        used inside any route which is using the Module Resolver to resolve data.
      */
      this.fetchModuleDetailsFromRoute();

      this.eventsSubscription = this.nodeSelection.subscribe((controlFlowData: {
        controlFlowNode: ControlFlowNodeDetails, graphNodes: ControlFlowNodeDetails[]}) => {
        this.graphNodes = controlFlowData.graphNodes;
        this.controlFlowNode = controlFlowData.controlFlowNode;

        /* Remove the existing View Zones */
        this.removeZone(this.viewZoneId);

        /* Fetching the Annotation record for the Selected Annotation node */
        if (this.controlFlowNode.entity === ControlFlowNode.EntityEnum.ANNOTATION) {
          const selectedAnnotation = this.annotationControllerService.findAnnotationById(this.projectId, this.controlFlowNode.recordId);
          selectedAnnotation.subscribe(annotation => {
            if (annotation != null) {
              this.codeAnnotationArray = [];
              this.codeAnnotationArray.push(annotation);
              this.drawLoadedAnnotations(this.monacoEditor);
            }
          });
        }
        this.onSelectionCodeViewerNode(controlFlowData.graphNodes, controlFlowData.controlFlowNode);
      });
    }
  }

  /**
   * method to open file(input/ output)
   * @param  moduleId module id
   * @returns path created with module and project id
   */
  buildRouteForModule(moduleId: number): string {
    return RouteBuilder.buildModuleRoute(this.projectId, moduleId, '/details/overview');
  }

  /**
   * method to close the CFG meta data drawer
   */
  closeCfgEditor(): void {
    this.hideShowDrawer.emit(false);
    this.widePanel = true;
    this.removeZone(this.viewZoneId);
  }

  /**
   * method to change the width of the CFG side panel on click of toggle button
   */
  onToggleClick(): void {
    this.widePanel = ! this.widePanel;
  }

  /**
   * Method to maintain the collapsed panel state(open/close)
   * @param selectedTitle user clicked panel title.
   */
  maintainSectionState(selectedTitle: string): void {
    if ( ! this.maintainState[selectedTitle]) {
      this.maintainState[selectedTitle] = true;
    } else {
      this.maintainState[selectedTitle] = false;
    }
    this.cfgMetaDataService.setState(this.maintainState);
  }

  onMonacoInit(editor?: editor.ICodeEditor): void {
    this.monacoEditor = editor;
  }

  ngOnDestroy(): void {
    this.cfgMetaDataService.setState({
      inputFiles: false,
      outputFiles: false
    });
    this.eventsSubscription?.unsubscribe();
  }

  /**
   * Updates the annotation and change the style fo the annotation accordingly.
   * removes the viewZone for editor and adds new one for state Change and adjust
   * the position of annotation again.
   * @param editor instance of the Monaco Editor.
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
    const additionalClass =
      'annotation-editor-container__annotation--' + annotation.state.toLowerCase().replace('_', '-');
    const deltaDeco: editor.IModelDeltaDecoration = this.createAnnotationDecoration(
      annotation.id,
      lineNumberStart,
      lineNumberEnd,
      additionalClass
    );
    initialDecorators = this.monacoEditor.deltaDecorations([initialDecorators] as string[], [deltaDeco]);

    this.onAnnotationUpdateOrDelete();
    this.closeEditor();
  }

  onAnnotationUpdateOrDelete(): void {
    this.annotationChange.emit();
  }

  /**
   * Deletes the Annotation and updates the UI accordingly.
   * @param editor instance of the Monaco Editor.
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
    this.onAnnotationUpdateOrDelete();
  }

  /**
   * Method is to open the shared-annotation-editor dynamically.
   * @param entity instance of the Editor.
   * @param isCreateMode to check if the Annotation is in create or edit mode..
   * @param annotationDetails data contained by the Annotation element
   */
  openSharedAnnotationEditor(entity: any, isCreateMode: boolean, annotationDetails?: any) {
    this.headerTitle = this.translateService.instant('annotationReporting.sharedAnnotationEditorTitle', { moduleName: this.moduleName });
    if (this.childComponentRef) {
      this.childComponentRef.destroy();
    }
    this.showCrossBtn = true;
    this.sharedAnnotationEditorStatusClass = sharedAnnotationEditorClass + entity.state.toLowerCase().replace('_', '-');
    this.editorVisibility.visibility = 'visible';
    const componentFactory = this.CFR.resolveComponentFactory(SharedAnnotationEditorComponent);
    this.openedAnnotationDetails = annotationDetails;
    this.childComponentRef = this.VCR.createComponent(componentFactory);
    this.childComponentRef.instance.annotation = entity;
    this.childComponentRef.instance.moduleId = this.moduleId;
    this.childComponentRef.instance.isCreateMode = isCreateMode;
    this.childComponentRef.instance.parentComponent = this.parentComponent;
    this.childComponentRef.instance.formResult.subscribe((event: FormResponse<AnnotationPojo>) => {
      this.handleSharedFormResult(event, annotationDetails as AnnotationDetails);
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
        this.updateAnnotation(annotationDetails.initialDecorators,
          annotationDetails.lineNumberStart, annotationDetails.lineNumberEnd, annotationDetails.codeAnnotationComponent);
        break;
      case FormResult.Deleted:
        this.deleteAnnotation(annotationDetails.initialDecorators, annotationDetails.codeAnnotationComponent.data);
        break;
      case FormResult.Disabled:
        this.isClose = true;
    }
  }

  /**
   * Closes the currently open annotation editor.
   */
  onCancel(): void {
    this.isClose = true;
    if (this.isClose) {
      this.closeEditor();
    }
    this.openedAnnotationDetails = null;
  }

  private removeZone(zoneId: string) {
    this.monacoEditor.changeViewZones((changeAccessor: any) => {
      changeAccessor.removeZone(zoneId);
    });
  }

  private closeEditor() {
    this.isSave = false;
    this.isDelete = false;
    this.isClose = false;
    this.showCrossBtn = false;
    this.editorVisibility.visibility = 'hidden';
    this.childComponentRef?.destroy();
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
   * Closes the editor and removes zone in case of error.
   */
  private onError(): void {
    this.closeEditor();
    this.removeZone(this.viewZoneId);
  }

  /**
   * returns the data for the code Annotation custom element we have created.
   * @param annotation annotation data saved in the system.
   * @returns object of type {AnnotationElementData}
   */
  private getDataForAnnotationComponent(annotation: AnnotationPojo): AnnotationElementData {
    const data: AnnotationElementData = {
      annotation,
      moduleName: this.moduleName,
      moduleId: this.moduleId,
      projectId: this.projectId,
      borderColor: this.stateColor[annotation.state],
      typeLabel: this.annotationType.find((item: any) => item.value === annotation.type).label,
      stateLabel: this.annotationState.find((item: any) => item.value === annotation.state).label,
      modulePath: this.modulePath
    };
    return data;
  }

  private createDecoration(
    lineNumberStart: number,
    columnStart: number,
    lineNumberEnd: number,
    columnEnd: number
  ): any {
    const additionalClass = 'control-flow-meta-data__background';
    return {
      range: this.getRange(lineNumberStart, columnStart, lineNumberEnd, columnEnd),
      options: {
        className: additionalClass,
      },
    };
  }

  /**
   * Delta decoration for annotations.
   * @param id id of the data dictionary record.
   * @param lineNumberStart starting line no in the code for Annotation.
   * @param lineNumberEnd ending line no in the code for Annotation
   * @param additionalClass class for the Annotation code highlight
   */
  private createAnnotationDecoration(id: number, lineNumberStart: number, lineNumberEnd: number, additionalClass: string): any {
    return {
      range: this.getRange(lineNumberStart, 1, lineNumberEnd, this.MAX_LINE_CHARS),
      options: {
        id: 'annotation-code-lines-' + id,
        className: additionalClass,
      },
    };
  }

  private getRange(lineNumberStart: number, columnStart: number, lineNumberEnd: number, columnEnd: number) {
    return new Range(lineNumberStart, columnStart, lineNumberEnd, columnEnd);
  }

  /**
   * Adds annotations fetched from DB to Editor.
   * @param editor The instance of the monaco editor.
   * editor The instance of the monaco editor.
   */
  private drawLoadedAnnotations(editor: editor.ICodeEditor) {
    this.codeAnnotationArray.forEach((element) => {
      const offset = element.location.offset;
      const length = element.location.length;
      const startLine: number = this.calculateLineAndColumn(offset, 0).line;
      const endLine: number = this.calculateLineAndColumn(offset, length).line;
      this.addAnnotationToEditor(editor, startLine, endLine, element);
    });
  }

  /**
   * This is the main method for annotating the code, this includes creation of the annotation component, highlighting the source code,
   * adding glyphs(left bar highlighting).
   * @param editor The instance of the monaco editor
   * @param lineNumberStart The starting line number of the annotation.
   * @param lineNumberEnd The ending line number of the annotation.
   * @param annotation The actual annotation.
   */
  private addAnnotationToEditor(
    editor: editor.ICodeEditor,
    lineNumberStart: number,
    lineNumberEnd: number,
    annotation: AnnotationPojo
  ) {
    const annotationId = annotation.id;
    const stateClass = 'annotation-editor-container__annotation--' + annotation.state.toLowerCase().replace('_', '-');
    const newDeltaDecorations: editor.IModelDecoration = this.createAnnotationDecoration(
      annotationId,
      lineNumberStart,
      lineNumberEnd,
      stateClass
    );
    const initialDecorators = editor.deltaDecorations([], [newDeltaDecorations]);
    const domNode = this.renderer.createElement('code-annotation-editor-element');
    const lineId = 'line' + annotationId;

    domNode.style.zIndex = '1';
    domNode.setAttribute('id', lineId);
    const annotationComponentId = annotationElementIdPrefix + annotationId;
    domNode.id = annotationComponentId;
    domNode.className = 'code-annotation-editor-component-container';
    let viewZoneId: string = null;

    /* Populate the component with the values from the annotation. */
    const codeAnnotationComponent: NgElement & WithProperties<CodeAnnotationEditorComponent> = domNode;
    codeAnnotationComponent.annotationsOfModule = this.codeAnnotationArray;
    codeAnnotationComponent.projectId =  this.projectId;

    const annotationDetails = {
      editor,
      initialDecorators,
      lineNumberStart,
      lineNumberEnd,
      codeAnnotationComponent
    };
    codeAnnotationComponent.isCfg = true;
    codeAnnotationComponent.data = this.getDataForAnnotationComponent(annotation);
    editor.changeViewZones((changeAccessor: any) => {
      viewZoneId = changeAccessor.addZone({
        afterLineNumber: lineNumberStart - 1,
        heightInPx: monacoEditorStyle.annotationMinHeight - 15,
        domNode,
      });
    });
    codeAnnotationComponent.data.viewZoneId = viewZoneId;
    this.viewZoneId = viewZoneId;
    /* Define all callback functions. */
    codeAnnotationComponent.deletedCallback = this.deleteAnnotation.bind(
      this,
      editor,
      initialDecorators,
      codeAnnotationComponent.data
    );
    codeAnnotationComponent.showError = (message: string) => this.showError(message);
    codeAnnotationComponent.openEditor = this.openSharedAnnotationEditor.bind(this, annotation, false, annotationDetails);
  }

  private identifyMaxOffsetNode(selectedNode: ControlFlowNodeDetails, nodes: ControlFlowNodeDetails[]): ControlFlowNodeDetails {
    const children: string[] = this.identifyNodeChildren(selectedNode, nodes);
    if (selectedNode.superTypes !== null && Array.from(selectedNode.superTypes).includes('CfgCollapsibleNode')) {
      let maxOffset: ControlFlowNodeDetails = selectedNode;
      nodes.forEach(node => {
        if (children.includes(node.recordId) && node.offset > maxOffset.offset) {
          maxOffset = node;
        }
      });
      return maxOffset;
    }
    return selectedNode;
  }

  private onSelectionCodeViewerFromDl(selectedNode: DataLineageNode) {
    const selectedCode = selectedNode;
    const offset = selectedCode.offset;
    const length = selectedNode.length;
    const res = this.calculateLineAndColumn(offset, length);
    /* Select correct glyph classes. */
    const deltaDeco1: editor.IModelDeltaDecoration = this.createDecoration(
      res.line as number,
      res.currentColumn as number,
      res.lineNumberEnd as number,
      0
    );
    this.deltaDeco = this.monacoEditor.deltaDecorations([this.deltaDeco] as string[], [deltaDeco1]);
    this.monacoEditor.revealLineInCenter(res.line as number);
  }

  private onSelectionCodeViewerNode(graphNodes: ControlFlowNodeDetails[], selectedNode?: ControlFlowNodeDetails): void {
    const selectedCode = selectedNode;
    const offset = selectedCode.offset;
    let length = selectedNode.length;
    const childrenIds: string[] = this.identifyNodeChildren(selectedNode, graphNodes);
    if (childrenIds.length !== 0) {
      const maxOffset = this.identifyMaxOffsetNode(selectedNode, graphNodes);
      length = maxOffset.offset + maxOffset.length - selectedNode.offset;
      if (selectedNode.superTypes !== null && Array.from(selectedNode.superTypes).includes('BranchStatement')) {
        let children = graphNodes.filter(node => childrenIds.includes(node.recordId));
        if (children.length !== 0) {
          children = children.sort((a, b) => a.offset - b.offset);
          length = children[0].offset - selectedNode.offset - 1;
        }
      }
    }
    const res = this.calculateLineAndColumn(offset, length);
    const lineNumberStart: number = res.line;

    let lineNumberEnd = lineNumberStart;
    if (selectedNode.superTypes) {
      lineNumberEnd = Array.from(selectedNode.superTypes).includes('BranchStatement')
        && ! Array.from(selectedNode.superTypes).includes('JumpStatement') ? res.lineNumberEnd - 1 : res.lineNumberEnd;
    }

    /* Since we're displaying truncated COBOL this has to be taken into account. */
    const columnStart: number = res.currentColumn;

    /* Select correct glyph classes. */
    const deltaDeco1: editor.IModelDeltaDecoration = this.createDecoration(
      lineNumberStart,
      columnStart,
      lineNumberEnd,
      0
    );
    this.deltaDeco = this.monacoEditor.deltaDecorations([this.deltaDeco] as string[], [deltaDeco1]);
    this.monacoEditor.revealLineInCenter(lineNumberStart);
  }

  private calculateLineAndColumn(offset: number, length: number): any {
    let i: number;
    /* Initial line is 1! */
    let line = 1;
    let currentColumn = 1;
    let totalLines = 1;
    for (i = 0; i < offset + length; i++) {
      if (i < offset) {
        currentColumn++;
      }
      if (this.code.charAt(i) === '\n') {
        if (i > offset) {
          totalLines++;
        } else {
          line++;
          currentColumn = 1;
        }
      }
    }
    return {
      line,
      currentColumn,
      lineNumberEnd: line + totalLines
    };
  }

  private identifyNodeChildren(parent: ControlFlowNodeDetails, graphNodes: ControlFlowNodeDetails[]): string[] {
    const childrenIds: string[] = [];
    graphNodes.forEach(node => {
      if (node.parent === parent.recordId) {
        childrenIds.push(node.recordId);
      }
    });
    return childrenIds;
  }

  private fetchModuleDetailsFromRoute(): void {
    this.route.data.subscribe((data: { module: ModulePojo }) => {
      this.moduleId = data.module.id;
      this.code = data.module.content;
      this.modulePath = data.module.path;
      this.moduleName = data.module.name;
    });
  }

  private setOffset(offset: number, length: number): void {
    const res = this.calculateLineAndColumn(offset, length);
    /* Select correct glyph classes. */
    const deltaDeco1: editor.IModelDeltaDecoration = this.createDecoration(
      res.line as number,
      res.currentColumn as number,
      res.line as number,
      res.currentColumn as number + length
    );
    this.deltaDeco = this.monacoEditor.deltaDecorations([this.deltaDeco] as string[], [deltaDeco1]);
    this.monacoEditor.revealLineInCenter(res.line as number);
  }
}
