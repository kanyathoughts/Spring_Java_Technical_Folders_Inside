/* eslint-disable sonarjs/no-duplicate-string */
import {
  ChangeDetectorRef,
  Component,
  EventEmitter,
  Inject,
  Input,
  NgZone,
  OnChanges,
  OnInit,
  Output,
  Renderer2,
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import {
  AnnotationReport, FunctionalBlockControllerService, FunctionalBlockPojo, FunctionalBlockPojoPrototype,
  AnnotationToFunctionalBlockControllerService, AnnotationPojo
} from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import {
  CreateEditFunctionalGroupComponent,
  RESULT_DELETED,
  RESULT_EDITED,
} from '@app/shared/components/create-edit-functional-group/create-edit-functional-group.component';
import { NzFormatBeforeDropEvent, NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { NzMessageService } from 'ng-zorro-antd/message';
import { ModuleDetails } from '../functional-analysis.interface';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { editor } from 'monaco-editor/esm/vs/editor/editor.api';
import { NgElement, WithProperties } from '@angular/elements';
import {
  CodeAnnotationEditorComponent,
  annotationElementIdPrefix,
} from '@app/modules/mining-code-viewer/code-annotation/code-annotation-editor.component';
import { monacoEditorStyle } from '../../../../theme/monaco-editor-style';
import { AnnotationElementData } from '@app/modules/mining-code-viewer/mining-code-viewer';
import { WindowToken } from '@app/core/utils/window';
import styles from '../../../../theme/style.json';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { ControlFlowUtility } from '../utils/functional-analysis-utility';
import { FunctionalAnalysisGraphInfo } from '../models/functional-analysis-graph-info';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { NzFormatEmitEvent } from 'ng-zorro-antd/tree';
import { AnnotationItem } from '@app/core/services/functional-group.service';
import { catchError, mergeMap } from 'rxjs/operators';
import { Observable, of } from 'rxjs';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { Router } from '@angular/router';
import {
  FaChildrenDeepGQL,
  FaTreeGQL,
  FilterObject_FunctionalBlocks,
  FunctionalBlockType,
  SortDirection,
  SortObject_FunctionalBlocks
} from '@app/graphql/generated/generated';
import { JobInformation } from '@innowake/mining-api-angular-client';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
const wrapClassName = 'vertical-center-modal';
const modulesPerTaxonomy = 'Taxonomies & Modules';
const annotationsPerPage = 10;

@Component({
  selector: 'app-functional-analysis-tree',
  templateUrl: './functional-analysis-tree.component.html'
})

export class FunctionalAnalysisTreeComponent implements OnInit, OnChanges {
  @Input() projectId: number;
  @Input() selectedViewMode: string;
  @Input() updateAnnotations: boolean;
  @Output() emitGroupedFBDetails: EventEmitter<{
    keys: Array<{ uid: string; type: string; parent: NzTreeNodeOptions; }>,
    btnState: boolean
  }> = new EventEmitter();
  @Output() emitFilterDetails: EventEmitter<any> = new EventEmitter();
  @Output() treeViewData: EventEmitter<string[]> = new EventEmitter();
  @Output() selectedView: EventEmitter<boolean> = new EventEmitter();
  @Output() showFunctionalDetails: EventEmitter<{buttonStatus: boolean, msg: string}> = new EventEmitter();
  @Output() emitAnnotationsToBeRemoved: EventEmitter<Map<string, NzTreeNodeOptions>> = new EventEmitter();
  @Input() canEditFB: boolean;

  panelState = {
    'Description': true,
    'Taxonomies & Modules': true,
    'Code': true,
    'Description & Additional Details': true
  };
  deletedUID = '';
  startLine = 1;
  EDITOR_OPTIONS = {
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
    renderLineHighlight: false,
    contextmenu: true,
    wordWrap: 'on'
  };
  monacoEditor: editor.ICodeEditor;
  filterDetails = {};
  sortDetails = {};
  showSortOptions = false;
  functionalAnalysisTree: NzTreeNodeOptions[] = [];
  functionalPanelData = {};
  isAnnotationSelected = false;
  functionalKeys: string[] = [];
  selectedBlock = {
    title: '',
    uid: '',
    isLeaf: false,
    selectedType: '',
    annotationState: '',
    updatedBy: '',
    currentNode: {},
    immediateParent: ''
  };
  sortState = {
    name: 'ASC',
    lastModified: 'ASC',
    currentSort: 'name'
  };
  clickedTitle: NzFormatEmitEvent[] = [];
  perPage = 30;
  moduleId = 0;
  moduleName = '';
  unGroupedAnnotations = 0;
  completeCode = '';
  groupedFunctionalBlockDetails: Array<{ uid: string, type: string, parent: NzTreeNodeOptions }> = [];
  checkBoxState = false;
  selectedPageIndex = 0;
  confirmModal: NzModalRef;
  breadCrumb: string[] = [];
  selectedBlockAnnotations: Array<{ [key: string]: any }> = [];
  defaultExpandedKeys: string[] = [];
  defaultSelectedKeys: string[] = [];
  isDescriptionAvailable = false;
  annotationDetailsRes: { [key: string]: any };
  annotationType: any[] = [];
  annotationState: any[] = [];
  totalTreeItemsCount: number;
  noTaxModules = true;
  graphInfo: FunctionalAnalysisGraphInfo;
  controlFlowUtility: ControlFlowUtility;
  currentEditorInstances: editor.ICodeEditor[] = [];
  selectedAnnotations: Map<string, NzTreeNodeOptions> = new Map();
  enableRestoreBranch = true;
  loader: { [key: string]: LoaderState } = {
    details: LoaderState.loading,
    page: LoaderState.loading,
    code: LoaderState.success
  };
  excludedBranches: any[] = [];
  lastEnteredNode: NzTreeNodeOptions;
  timerId: NodeJS.Timeout;
  public stateColor = {
    APPROVED: styles.$approvedAnnotation,
    CANDIDATE: styles.$candidateAnnotation,
    FOR_REVIEW: styles.$forReviewAnnotation,
    IN_ANALYSIS: styles.$inAnalysisAnnotation,
    REJECTED: styles.$rejectedAnnotation,
    INVALID: styles.$invalidAnnotation,
    DDENTRY: styles.$dictionaryBackground,
  };

  SortDetails = {
    ASC: 'ASC',
    DESC: 'DESC',
    NAME: 'name',
    LAST_MODIFIED: 'lastModified',
    CONTENT_NAME: 'content_name',
    CONTENT_UPDATED: 'content_updated'
  };
  loadAnnotations = true;
  currentAnnotationPage = 0;
  loadingAnnotations = false;

  private codeAnnotationArray: Array<{ [key: string]: any }> = [];
  private draggedNodeParents: { [key: string]: any } = {};
  private disableTooltip = false;
  private childrenFilterObject: FilterObject_FunctionalBlocks = {
    content_type: { in: [FunctionalBlockType.FunctionalGroup, FunctionalBlockType.FunctionalUnit]}
  };
  private reorderAnnotation = false;

  constructor(
    private route: ActivatedRoute,
    private graphQlControllerService: GraphQlControllerService,
    private translateService: TranslateService,
    private modalService: NzModalService,
    private functionalBlockController: FunctionalBlockControllerService,
    private messageService: NzMessageService,
    private labelMappingService: LabelMappingService,
    private functionalBlockControllerService: FunctionalBlockControllerService,
    private annotationFunctionalBlockController: AnnotationToFunctionalBlockControllerService,
    private renderer: Renderer2,
    private ngZone: NgZone,
    private cdr: ChangeDetectorRef,
    private languageProviderService: LanguageProviderService,
    private router: Router,
    private faTreeGQL: FaTreeGQL,
    private faChildrenDeepGQL: FaChildrenDeepGQL,
    private jobManager: JobManagerService,
    @Inject(WindowToken) private $window: Window
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
    this.route.queryParamMap.subscribe((params) => {
      this.selectedBlock = {
        title: '', uid: '', isLeaf: false, selectedType: '', annotationState: '', updatedBy: '', currentNode: null, immediateParent: ''
      };
      this.filterDetails = JSON.parse(params.get('filterApplied')) || {};
      this.sortDetails = params.get('sortApplied') ? JSON.parse(params.get('sortApplied')) : {content_name: 'ASC'};
      this.functionalKeys.length = 0;
      this.getTreeDetailsBasedOnFilter(this.filterDetails, 0, undefined, this.sortDetails);
    });
    this.getUnGroupedAnnotation();
    void this.languageProviderService.loadMonacoWASM();
    this.controlFlowUtility = new ControlFlowUtility();
  }

  ngOnChanges(): void {
    this.ngZone.run(() => {
      if (this.updateAnnotations) {
        let clearSelectedBlock = false;
        // Removes the selected annotations from the tree first and then makes the server call to update DB.
        const parents: Map<string, NzTreeNodeOptions> = new Map();
        for (const node of this.selectedAnnotations.values()) {
          if (this.selectedBlock.uid === node.origin.uid) {
            clearSelectedBlock = true;
          }
          this.updateGroupedFunctionalBlockDetails(node.origin, node.parentNode.origin as NzTreeNodeOptions);
          parents.set(node.parentNode.origin.uid as string, node.parentNode as NzTreeNodeOptions);
          node.remove();
        }

        for (const parent of parents.values()) {
          const reqBody = this.createReqBodyFromNode(parent.origin as NzTreeNodeOptions);
          this.updateNode(reqBody, parent).subscribe(() => {
            // To remove the expand icon if the node has no children.
            if (parent.origin.children.length < 1) {
              parent.origin.children = null;
            }
          });
        }

        // Clears the removed annotations from details section.
        if (clearSelectedBlock) {
          this.functionalKeys.length = 0;
        } else {
          this.selectedBlockAnnotations = this.selectedBlockAnnotations.filter((annotation) => ! this.selectedAnnotations.has(annotation.uid as string));
        }

        // Clears the selection so it does not impact the next selection.
        this.selectedAnnotations.clear();
        this.emitAnnotationsToBeRemoved.emit(this.selectedAnnotations);
        this.annotationFunctionalBlockController.deleteEmptyAutoGeneratedFunctionalUnits(this.projectId, AnnotationPojo.TypeEnum.FUNCTIONAL).subscribe();
      }
      this.cdr.detectChanges();
    });
  }

  /**
   * method to capture the event when tree item is selected of node is expanded
   * @param event: type of the event
   */
  nzEvent(event: NzFormatEmitEvent, operation: string): void {
    if (event && event.node && event.node.origin) {
      const node = event.node;
      const { uid, type, title } = node.origin;
      if (operation === 'click') {
        if (this.selectedBlock && this.selectedBlock.uid === uid) {
          return;
        }
        this.noTaxModules = true;
        this.selectedView.emit(true);
        this.showFunctionalDetails.emit({buttonStatus: true, msg: ''});
        this.prepareBreadCrumb(node.level, node);

        this.loader.details = LoaderState.loading;
        this.selectedBlockAnnotations = [];

        // Dispose the current Monaco Editor instance
        if (this.currentEditorInstances.length) {
          this.currentEditorInstances.forEach((editorInstance) => {
            editorInstance.dispose();
          });
        }

        if (type !== 'FUNCTIONAL_UNIT') {
          this.selectedBlock.title = title;
        }
        this.selectedBlock = {
          ...this.selectedBlock, currentNode: event?.node, isLeaf: type === 'FUNCTIONAL_UNIT' ? true : false, uid,
          immediateParent: node?.parentNode?.origin?.uid
        };
        this.highlightNode(event);

        if (type === 'FUNCTIONAL_GROUP') {
          this.generateFunctionalKeys();
          this.getGroupLevelDetails(uid as string, this.projectId);
          this.prepareBreadCrumb(node.level, node);
          this.selectedBlock.selectedType = 'FUNCTIONAL_GROUP';

        } else if (type === 'FUNCTIONAL_BLOCK') {
          this.generateFunctionalKeys();
          this.getFunctionalUnitsDetails(uid as string);
          this.selectedBlock.selectedType = type;
        } else if (type === 'FUNCTIONAL_UNIT') {
          this.getAnnotationDetails(uid as string, node.level, node);
          this.selectedBlock.selectedType = 'FUNCTIONAL_UNIT';
          this.selectedBlock = { ...this.selectedBlock, isLeaf: true };
        }
      }

      if (operation === 'expand' && ! node.origin.isLoaded) {
        this.loadTreeNodeChildren(node);
      }
      this.deletedUID = uid;
    }
    this.cdr.detectChanges();
  }

  /**
   * Get the Functional blocks as CFG
   * @param uid of the selected block.
   * @param moduleName
   */
  getFunctionalBlockAsCFG(uid: string, moduleName: string): void {
    this.graphInfo = null;
    this.functionalBlockController.getFunctionalBlockAsControlFlowGraph(this.projectId, uid).subscribe((result) => {
      if (result && result.nodes.length > 0) {
        this.graphInfo = this.controlFlowUtility.getGraphInfo(result, moduleName);
        this.loader.details = LoaderState.success;
      }

      this.showFunctionalDetails.emit({
        buttonStatus: ! (result && result.edges.length > 0),
        msg: (result && result.edges.length > 0) ? '': this.translateService.instant('functionalAnalysis.cfgIsNotGeneratedSuccessfully')
      });
    }, () => {
      this.loader.details = LoaderState.success;
      this.showFunctionalDetails.emit({ buttonStatus: true, msg: this.translateService.instant('functionalAnalysis.cffFetchError') });
    }
  );
  }

  /**
   * method to call on the pagination change
   * @param current indicates which page is selected
   */
  onPaginationChange(current: number): void {
    const pageIndex = current - 1;
    this.selectedPageIndex = pageIndex;
    this.getTreeDetailsBasedOnFilter(this.filterDetails, pageIndex, undefined, this.sortDetails);
  }

  /**
   * method to build route for individual module
   * @param moduleId: particular module id
   * @returns route build using project and module id
   */
  buildRouteForModule(moduleId: number): string {
    return RouteBuilder.buildModuleRoute(this.projectId, moduleId, '/details/overview');
  }

  /**
   * This method is called when the monaco editor gets ready. When the onInit-method is called the
   * code still might not be displayed completely.
   * @param editor An instance of the monaco editor.
   * @param uid of functional block or group to open an instance of code editor
   */
  onMonacoInit(editor?: editor.ICodeEditor, codeAnnotation?: { [key: string]: any }): void {
    this.currentEditorInstances.push(editor);
    this.monacoEditor = editor;
    if (this.selectedBlock.selectedType !== 'FUNCTIONAL_UNIT') {
      this.addAnnotationToEditor(this.monacoEditor, codeAnnotation);
    }
  }

  /**
   * method to maintain the state of panel.
   * @param event: click event
   * @param key: which panel is clicked
   */
  toggleDropDown(event: Event, key: string): void {
    event.stopPropagation();
    this.panelState[key] = !this.panelState[key];
  }

  /**
   * method to decide the state of the button and get the selected node details
   * @param node: particular node which is clicked
   */
  onGrouping(node: { [key: string]: any }): void {
    node.isChecked = ! node.isChecked;
    this.updateGroupedFunctionalBlockDetails(node.origin, node?.parentNode?.origin as NzTreeNodeOptions);
    this.emitGroupedFBDetails.emit({
      keys: this.groupedFunctionalBlockDetails,
      btnState: this.checkCreateGroupStatus(true)
    });
    this.emitAnnotationsToBeRemoved.emit(this.selectedAnnotations);
  }

  /**
   * Handles the check event for an annotation node.
   * @param node - The annotation node.
   * @param isChecked - Indicates whether the node is checked or unchecked.
   */
   onAnnotationCheck(node: NzTreeNodeOptions, isChecked: boolean): void {
    const nodeId: string = node.origin.uid;
    this.updateGroupedFunctionalBlockDetails(node.origin, node.parentNode.origin as NzTreeNodeOptions);
    if (isChecked) {
      this.selectedAnnotations.set(nodeId, node);
    } else {
      if (this.selectedAnnotations.has(nodeId)) {
        this.selectedAnnotations.delete(nodeId);
      }
    }
    this.emitGroupedFBDetails.emit({
      keys: this.groupedFunctionalBlockDetails,
      btnState: this.checkCreateGroupStatus(false),
    });
    this.emitAnnotationsToBeRemoved.emit(this.selectedAnnotations);
  }

  checkCreateGroupStatus(multipleParentSupport: boolean): boolean {
    if (this.groupedFunctionalBlockDetails.length <= 1) {
      return true;
    }
    const parents: string[] = [...new Set(this.groupedFunctionalBlockDetails.filter(item => item.parent != null && item.parent.key != null)
      .map(item => item.parent.key))];
    const types: string[] = [...new Set(this.groupedFunctionalBlockDetails.map(item => item.type))];
    if (types.includes('FUNCTIONAL_BLOCK') && types.includes('FUNCTIONAL_GROUP')) {
      types.splice(types.indexOf('FUNCTIONAL_BLOCK'), 1);
    }
    return types.length > 1 || (!multipleParentSupport && parents.length > 1);
  }

  /**
   * method to open modal for un grouping FGs
   */
  onDeleteFunctionGroup(): void {
    this.confirmModal = this.modalService.confirm({
      nzTitle: this.translateService.instant('functionalAnalysis.unGroupedModalTitle'),
      nzContent: this.translateService.instant('functionalAnalysis.unGroupedModalBody'),
      nzOkText: this.translateService.instant('functionalAnalysis.unGroupedModalOkBtnTxt'),
      nzOkType: 'primary',
      nzOkDanger: true,
      nzOnOk: () => this.unGroupedFG(),
      nzCancelText: this.translateService.instant('btnLabel.cancel'),
      nzOnCancel: () => null
    });
  }

  /**
   * method to un group FBs
   */
  unGroupedFG(): void {
    if (this.selectedBlock?.immediateParent && this.selectedBlock.uid) {
      this.functionalBlockController.deleteFbOnUnGroup(this.projectId, this.selectedBlock.immediateParent, this.selectedBlock.uid).subscribe(() => {
        this.postDeleteActions();
      });
    } else {
      this.deleteFunctionalBlock();
    }
  }

  deleteFunctionalBlock(): void {
    this.functionalBlockController.deleteFunctionalBlock(this.projectId, this.deletedUID).subscribe(() => {
      this.postDeleteActions();
    });
  }

  postDeleteActions(): void {
    this.confirmModal.close();
    this.clearSelectedBlocks();
    this.messageService.success(this.translateService.instant('functionalAnalysis.functionalBlockDeleted') as string);
    this.getTreeDetailsBasedOnFilter(this.filterDetails, this.selectedPageIndex || 0, undefined, this.sortDetails);
    this.deletedUID = '';
    this.resetKeys();
    this.selectedBlock.title = '';
    this.getUnGroupedAnnotation();
  }

  /**
   * update ( update the name or description) of FB
   */
  updateFunctionalBlock(): void {
    const modal = this.modalService.create<CreateEditFunctionalGroupComponent>({
      nzTitle: this.translateService.instant('functionalBlock.editFunctionalBlockModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzWrapClassName: wrapClassName,
      nzClassName: 'fa-content__edit-window',
      nzKeyboard: true,
      nzContent: CreateEditFunctionalGroupComponent
    });
    const instance = modal.getContentComponent();
    instance.annotationGroupToEdit = this.selectedBlock.uid;
    instance.projectId = this.projectId;
    modal.afterClose.subscribe((result) => {
      if (result.state === RESULT_EDITED) {
        this.updateSelectedBlock(this.selectedBlock.uid, result.updatedFunctionalGroup);
      } else if (result.state === RESULT_DELETED) {
        this.functionalKeys.length = 0;
        this.selectedBlock = { title: '', uid: '', isLeaf: false, selectedType: '', annotationState: '', updatedBy: '', currentNode: null, immediateParent:'' };
        this.showFunctionalDetails.emit({buttonStatus: true, msg: ''});
        this.getTreeDetailsBasedOnFilter(this.filterDetails, this.selectedPageIndex || 0, undefined, this.sortDetails);
      }
      this.getUnGroupedAnnotation();
    });
  }

  /**
   * method to get the get the details( code , modules ) to show on the left side
   * @param uid: uid of particular FB
   * @param projectId: contains projectId
   */
  getGroupLevelDetails(uid: string, projectId: number): void {
    const requestQuery = {
      'query': `{
        functionalBlock(uid: "${uid}", projectId: ${projectId}) {
          name
          uid
          type
          description
          flags
          parents {
            content {
              uid
              name
            }
          }
          resolvedModuleParts {
            module {
              id
              name
              source
              taxonomies {
                  name
                  type {
                    name
                  }
              }
            }
          }
          childrenDeep(filterObject:  {content_type: {in:[FUNCTIONAL_UNIT]}}) {
            totalElements
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response) {
        this.selectedBlock.immediateParent = response?.data?.functionalBlock?.parents?.content[0]?.uid;
        if (response?.data?.functionalBlock?.childrenDeep === null || response.data.functionalBlock.childrenDeep.totalElements === 0) {
          this.noTaxModules = false;
        } else if (
          response?.data?.functionalBlock?.resolvedModuleParts === null &&
          response?.data?.functionalBlock?.childrenDeep !== null
        ) {
          this.noTaxModules = true;
        }
        this.setGroupDetails(response);
        this.annotationDetailsRes = response;
        this.loader.details = LoaderState.success;
      }
    });
  }

  /**
   * method to get initial data functional block/group
   * @param filterDetails: initial filters
   * @param pageNo: current page no
   */
  getTreeDetailsBasedOnFilter(filterDetails: { [key: string]: any }, pageNo: number, uid?: string, sortDetails?: { [key: string]: any }): void {
    this.loader.page = LoaderState.loading;
    this.groupedFunctionalBlockDetails.length = 0;
    this.emitFilterDetails.emit({ filterDetails, pageNo });

    const finalFilter: FilterObject_FunctionalBlocks = {
      content_type: { eq: FunctionalBlockType.FunctionalGroup},
      content_parents: { notEq: { content_type: { eq: FunctionalBlockType.FunctionalGroup}}}
    };
    let sorting: SortObject_FunctionalBlocks;

    if (filterDetails?.taxonomyIds?.length) {
      finalFilter.content_resolvedModuleParts_referencedTaxonomies_id = { in: filterDetails.taxonomyIds};
    }

    if (filterDetails?.moduleIds?.length) {
      finalFilter.content_resolvedModuleParts_moduleId = { in: filterDetails.moduleIds};
    }

    if (filterDetails?.blockNameSearch?.length) {
      const searchNames: string[] = filterDetails?.blockNameSearch?.map((blockName: string) => blockName.trim());
      finalFilter.content_deepName =  { in: searchNames };
    }

    if (filterDetails?.reachabilityIds?.length) {
      finalFilter.content_peers= { eq: { content_uid: { in: filterDetails?.reachabilityIds}}};
    }

    const ddeNames: string[] = filterDetails?.ddIds?.map((ddIdItem: string) => ddIdItem.trim()) || [];
    if (ddeNames.length) {
      finalFilter.content_referencedDataDictionaryNames = { in: ddeNames};
    }
    if (sortDetails) {
      sorting = sortDetails;
    } else {
      sorting = { content_name: SortDirection.Asc };
    }

    this.faTreeGQL.fetch({
      projectId: this.projectId,
      page: pageNo,
      size: this.perPage,
      sortObject: sorting,
      filterObject: finalFilter,
      childrenFilterObject: this.childrenFilterObject,
      useChildrenDeep: false
    }, {fetchPolicy: 'network-only'}).subscribe((response: { [key: string]: any }) => {
      if (response.data.functionalBlocks.totalElements) {
        const functionalBlocks = response.data.functionalBlocks;
        this.totalTreeItemsCount = functionalBlocks.totalElements;
        this.functionalAnalysisTree.length = 0;

        const parentContent = functionalBlocks.content as Array<{[key: string]: any;}>;
        if (filterDetails?.blockNameSearch?.length) {
          const rootIds = functionalBlocks.content.map((item: { [key: string]: any }) => item.uid);
          const contentUidFilter: FilterObject_FunctionalBlocks = {content_uid: {in: rootIds}};

          this.faTreeGQL.fetch({
            projectId: this.projectId,
            sortObject: sorting,
            filterObject: contentUidFilter,
            childrenFilterObject: this.childrenFilterObject,
            useChildrenDeep: true
          }, {fetchPolicy: 'network-only'}).subscribe((childrenResponse: { [key: string]: any }) => {
            this.loader.page = LoaderState.success;
            if (childrenResponse) {
              const childrenContent = childrenResponse.data.functionalBlocks.content as Array<{[key: string]: any;}>;
              this.createInitialSearchedFATree(parentContent, childrenContent, uid);
            }
          });
        } else {
          this.loader.page = LoaderState.success;
          this.createInitialFATree(parentContent, uid);
        }
        if (uid) {
          this.defaultSelectedKeys.push(uid);
        }
      } else {
        this.totalTreeItemsCount = 0;
        this.functionalAnalysisTree.length = 0;
        this.loader.page = LoaderState.success;
      }
    });
  }

  /**
   * method to stop closing the panel area in collapse panel
   * @param event: Event emitter
   */
  preventPanelClosing(event: Event): void {
    event.stopPropagation();
  }

  /**
   * method tp generate keys ( headers for the panel)
   */
  generateFunctionalKeys(): void {
    this.functionalKeys = [this.translateService.instant('functionalTree.description'),
    this.translateService.instant('functionalTree.modules'),
    this.translateService.instant('functionalTree.code')];
  }

  /**
   * @method navigateToUnGroupedAnnotationsSavedSearch Navigates to the ungrouped annotations saved search page.
   * It opens the page in a new tab.
   */
  navigateToUnGroupedAnnotationsSavedSearch(): void {
    const path = 'annotations?savedSearch=Ungrouped Annotations';
    openInNewTab(this.projectId, null, path, this.$window);
  }

  /**
   * Handles the drag start event.
   * @param event - The drag event emitted by the NzFormatEmitEvent.
   */
  onDragStart(event: NzFormatEmitEvent): void {
    this.disableTooltip = true;
    this.draggedNodeParents = {};
    this.ngZone.run(() => {
      const nodeData = event?.node?.origin;
      if (nodeData?.type !== 'FUNCTIONAL_UNIT' && nodeData?.type !== 'FUNCTIONAL_CONDITION') {
        event.event.preventDefault();
      }
      this.draggedNodeParents = event?.dragNode?.parentNode?.origin;
    });
  }

  /**
   * Handles the dragging event.
   * @param event - The drag event emitted by the NzFormatEmitEvent.
   */
  onDragging(event: NzFormatEmitEvent): void {
    const node = event.node;
    if (! node.origin.isLeaf && (! this.lastEnteredNode || this.lastEnteredNode?.origin.key !== node?.origin?.key)) {
      // This is to make sure we close the node if it is not loaded But was expanded because of dragging.
      if (this.lastEnteredNode && ! this.lastEnteredNode?.origin.isLoaded) {
        this.lastEnteredNode.setExpanded(false);
      }
      this.lastEnteredNode = node;
      clearTimeout(this.timerId);

      // We are waiting for a sec before making the request to load the children, to make sure we load data only when intended.
      this.timerId = setTimeout(() => {
        if (! node?.origin.isLoaded && node?.getChildren().length === 0 && node?.isExpanded) {
          this.loadTreeNodeChildren(node);
        }
      }, 1000);
    }
  }

  /**
   * Handles the event before the drop. event.pos can -1, 0, 1.
   * 0 Adds as child of Group.
   * -1 & 1 Adds as sibling of Group/leaf node.
   * @param event - The event object containing information about the drop event.
   */
  beforeDrop(event: NzFormatBeforeDropEvent): Observable<boolean> {
    let node = event.node.origin;
    const position = event.pos;
    this.disableTooltip = false;
    this.reorderAnnotation = false;

    // Checks if the user is trying to drop the node on the first root node and shows an error message.
    if (position === -1 && event.node.title === this.functionalAnalysisTree[0].title) {
      this.messageService.error(this.translateService.instant('functionalAnalysis.dropNotAllowed') as string);
      return of(false);

    // Shows error message if the user tries to drop a node on a group node which is not yet loaded
    } else if(! node.isLoaded && node.type !== 'FUNCTIONAL_UNIT') {
      this.loadTreeNodeChildren(event.node);
      this.messageService.error(this.translateService.instant('functionalAnalysis.dropFailed') as string);
      return of(false);

    } else {
      if (node.isLeaf) {
        node = event.node.parentNode.origin;
      }
      const draggedNodeId: string = event.dragNode.origin.uid;
      const sourceReqBody = this.createSourceReqBody(event.dragNode.parentNode.origin, draggedNodeId);
      const targetReqBody = this.createReqBodyFromNode(node);

      let index = 0;
      if (targetReqBody.children && ! targetReqBody.children.includes(draggedNodeId)) {
        const dragTargetedNode = event.node;
        if (! dragTargetedNode.isLeaf) {
          targetReqBody.children.unshift(draggedNodeId);
        } else {
          index = targetReqBody.children.indexOf(dragTargetedNode.origin.uid as string);
          if (position === -1) {
            targetReqBody.children.splice(index, 0, draggedNodeId);
          } else {
            ++index;
            targetReqBody.children.splice(index, 0, draggedNodeId);
          }
        }
      } else {
        targetReqBody.children = [draggedNodeId];
      }

      if (sourceReqBody.uid === targetReqBody.uid) {
        this.reorderAnnotation = true;
        return of (true);
      } else {
        this.updateNode(sourceReqBody).pipe(
          mergeMap(() => this.updateNode(targetReqBody, node)),
          catchError(() => of(null))
        ).subscribe(() => {
          const flattenFunctionalTree = this.flattenTree(this.functionalAnalysisTree);
          for (const fg of flattenFunctionalTree) {
            if (fg.key === targetReqBody.uid) {
              fg.children = fg.children?.filter((child: NzTreeNodeOptions) =>
                targetReqBody.children.includes(child.uid as string)
              );
            } else if (fg.uid === sourceReqBody.uid) {
              fg.children = fg.children?.filter((child: NzTreeNodeOptions) =>
                sourceReqBody.children.includes(child.uid as string)
              );
            }
          }

          // Adds the newly added node to the target nod, if its still loading calls the load function.
          const newNode = event.dragNode.origin;
          const parentNode = event.node.origin.isLeaf ? event.node.parentNode : event.node;
          parentNode.addChildren([newNode], index);

          /*
            Removes the dragged node from the source node and checks for children otherwise sets it to null
            so we don't see expand icon.
          */
          event.dragNode.remove();
          if (event.dragNode.parentNode.origin.children.length < 1) {
            event.dragNode.parentNode.origin.children = null;
          }
          this.cdr.detectChanges();
        });
        return of(false);
      }
    }
  }

  /**
   * Handles the drop event when a node is dropped onto the tree component.
   * We are using it only for the reordering of the annotations, as we can't
   * get the updated order in the beforeDrop event.
   * @param event - The event object containing information about the drop event.
   */
  onDrop(event: NzFormatEmitEvent): void {
    if (this.reorderAnnotation) {
      this.ngZone.run(() => {
        const node = event.node?.parentNode?.origin;
        const targetReqBody = this.createReqBodyFromNode(node);
        this.updateNode(targetReqBody).subscribe(() => {
          const flattenFunctionalTree = this.flattenTree(this.functionalAnalysisTree);
          for (const fg of flattenFunctionalTree) {
            if (fg.key === targetReqBody.uid) {
              fg.children = fg.children.filter((child: NzTreeNodeOptions) =>
                targetReqBody.children.includes(child.uid as string)
              );
            }
          }
          this.cdr.detectChanges();
        });
      });
    }
  }

  /**
   * Gets the tooltip title.
   * @param title - The title to be displayed in the tooltip.
   * @returns The tooltip title if tooltips are not disabled, otherwise an empty string.
   */
  getTooltipTitle(title: string): string {
    if (!this.disableTooltip) {
      return title;
    }
    return '';
  }

  /**
   * Sorts the data by the 'name' property.
   * It sets the current sort state to 'name' and navigates to the updated URL.
   */
  sortByName(): void {
    this.sort('name');
  }

  /**
   * Sorts the data by the 'lastModified' property.
   * It sets the current sort state to 'lastModified' and navigates to the updated URL.
   */
  sortByLastModified(): void {
    this.sort('lastModified');
  }

  /**
   * Handles the re-creation of the graph.
   */
  handleReCreateGraph(): void {
    this.controlFlowUtility = new ControlFlowUtility();
    this.getFunctionalBlockAsCFG(this.selectedBlock.uid, this.moduleName);
    this.loader.details = LoaderState.loading;
    this.enableRestoreBranch = false;
  }

  /**
   * disables the restore branch functionality.
   */
  handleRestore(): void {
    this.excludedBranches = [];
    this.controlFlowUtility = new ControlFlowUtility();
    this.getFunctionalBlockAsCFG(this.selectedBlock.uid, this.moduleName);
    this.enableRestoreBranch = true;
  }

  /**
   * Checks if the title of a given node is included in the 'blockNameSearch' filter details.
   *
   * @param node - The node to be searched.
   * @returns Returns true if the node's title is included in the 'blockNameSearch' filter details, otherwise returns false.
   */
  treeSearch(node: NzTreeNodeOptions): boolean {
    if (node && this.filterDetails['blockNameSearch']) {
      const data = this.filterDetails['blockNameSearch'].map((item: string) => item.toLowerCase());
      if (data.includes(node.title.toLowerCase())) {
        return true;
      }
    }
    return false;
  }

  /**
   * its used for handling the rendering of the code editors in detail section.
   */
  trackByKey(index: number, code: { [key: string]: any }): string {
    return code.uid;
  }

  /**
   * Checks for the scroll even and make request to fetch the next page of annotations.
   */
  onScroll(): void {
    if (this.loadAnnotations) {
      this.loadingAnnotations = true;
      this.currentAnnotationPage++;
      this.setAnnotationsForSelectedNode(this.currentAnnotationPage);
    }
  }

  /**
   * Handles the clearance of selected blocks for grouping and removal of annotations
   */
  clearSelectedBlocks(): void {
    this.selectedAnnotations.clear();
    this.groupedFunctionalBlockDetails.length = 0;
    this.emitGroupedFBDetails.emit({
      keys: this.groupedFunctionalBlockDetails,
      btnState: this.checkCreateGroupStatus(true)
    });
    this.emitAnnotationsToBeRemoved.emit(this.selectedAnnotations);
  }

  private createReqBodyFromNode(node: NzTreeNodeOptions): FunctionalBlockPojoPrototype {
    const annotationUUIDs = node?.children?.map((annotation: AnnotationItem) => annotation.uid);
    return {
      uid: node.uid,
      description: node.description,
      name: node.name,
      children: annotationUUIDs,
    };
  }

  private createSourceReqBody(node: NzTreeNodeOptions, dragId: string): FunctionalBlockPojoPrototype {
    return {
      uid: node.key,
      description: node.description,
      name: node.name,
      children: node?.children
        .filter((annotation: AnnotationItem) => annotation.uid !== dragId)
        .map((annotation: AnnotationItem) => annotation.uid),
    };
  }

  private updateNode(
    reqBody: FunctionalBlockPojoPrototype,
    node?: NzTreeNodeOptions
  ): Observable<FunctionalBlockPojo | null | string[]> {
    const nodeId = node ? node.key : (this.draggedNodeParents.uid as string);
    return this.functionalBlockControllerService.updateFunctionalBlock(this.projectId, nodeId, reqBody).pipe(
      mergeMap(() => this.functionalBlockControllerService.computeFunctionalBlock(this.projectId, new Set([nodeId]))),
      catchError(() => of(null))
    );
  }

  private prepareTreeData(data: Array<{ [key: string]: any }>, uid?: string, childrenMap?: Map<string, any[]>): void {
    const uidArray: string[] = data.map((item: { [key: string]: any }) => item.uid);
    this.treeViewData.emit(uidArray);
    data.forEach((dataItem: { [key: string]: any }) => {
      const children = childrenMap.get(dataItem.uid as string) || dataItem?.children?.content;
      if (children) {
        const sortedChildren = dataItem?.children?.content
          .map((child: any) => children.find((c: { [key: string]: any }) => c.uid === child.uid))
          .filter((child: { [key: string]: any }) => child !== undefined);
        const extraChildren = children.filter((c: { [key: string]: any }) => !sortedChildren.some((child: { [key: string]: any }) => child.uid === c.uid));
        children.splice(0, children.length, ...sortedChildren, ...extraChildren);
      }
      const typeDecides = children.every(
        (childItem: { [key: string]: any }) =>
          childItem.type === 'FUNCTIONAL_UNIT' ||
          childItem.type === 'FUNCTIONAL_CONDITION' ||
          childItem.type === 'FUNCTIONAL_STATEMENT'
      );
      const functionalAnalysisItem = {
        'title': dataItem.name ? dataItem.name : dataItem.title,
        'key': dataItem.uid,
        'uid': dataItem.uid,
        'description': dataItem.description,
        isCheckBox: false,
        type: typeDecides ? 'FUNCTIONAL_BLOCK' : 'FUNCTIONAL_GROUP',
        isDescriptionProvided: true,
        children:  children && children?.length ? children : null,
        generatedBy: dataItem.flags?.GENERATED_BY,
        isLoaded: true,
      };
      this.functionalAnalysisTree = [...this.functionalAnalysisTree, { ...functionalAnalysisItem }];
      if (uid) {
        this.functionalAnalysisTree.forEach((item: { [key: string]: any }) => {
          if (item.uid === uid) {
            item.isHighlighted = true;
          } else {
            item.isHighlighted = false;
          }
        });
      }
    });
  }

  private getFunctionalUnitsDetails(functionBlockId: string): void {
    const requestQuery = {
      'query': `{
        functionalBlock(uid: "${functionBlockId}", projectId: ${this.projectId}) {
          resolvedModuleParts {
            module {
              name
              id
              source
              taxonomies {
                name
                type {
                  name
                }
              }
            }
          }
          description
          flags
          childrenDeep(filterObject:  {content_type: {in:[FUNCTIONAL_UNIT]}}) {
            totalElements
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response['data']?.['functionalBlock']?.['flags']?.['FB_EXCLUDED_BRANCHES']?.length > 0) {
        this.enableRestoreBranch = false;
        this.excludedBranches = response['data']?.['functionalBlock']?.['flags']?.['FB_EXCLUDED_BRANCHES'];
      } else {
        this.enableRestoreBranch = true;
      }
      this.setFunctionalUnitsDetails(response, functionBlockId);
      this.loader.details = LoaderState.success;
      this.annotationDetailsRes = response;
    });
  }

  private setFunctionalUnitsDetails(response: { [key: string]: any }, functionBlockId: string) {
    if (response) {
      const childrenDeepDetails = response['data']['functionalBlock']['childrenDeep'];
      this.functionalPanelData['Description'] = response['data']['functionalBlock'].description;
      this.functionalPanelData[modulesPerTaxonomy] = this.groupModulesForTaxonomies(response);
      if (response['data']['functionalBlock'].resolvedModuleParts) {
        this.moduleId = response['data']['functionalBlock']?.resolvedModuleParts[0].module?.id;
      }

      if (childrenDeepDetails && childrenDeepDetails.totalElements) {
        this.loader.code = LoaderState.loading;
        this.setAnnotationsForSelectedNode(0);
      } else {
        this.noTaxModules = false;
      }

      if (response?.data?.functionalBlock?.flags?.HAS_CFG === undefined) {
        this.computeAndGenerateGraph(functionBlockId);
      } else if (response?.data?.functionalBlock?.flags?.HAS_CFG === true && childrenDeepDetails) {
        this.showFunctionalDetails.emit({buttonStatus: false, msg: ''});
        this.controlFlowUtility = new ControlFlowUtility();
        this.moduleName = response?.data?.functionalBlock?.resolvedModuleParts[0]?.module?.name as string;
        this.getFunctionalBlockAsCFG(functionBlockId, this.moduleName);
      } else {
        this.selectedView.emit(true);
        this.showFunctionalDetails.emit({buttonStatus: true, msg: this.translateService.instant('functionalAnalysis.cfgComputeIsNotAvailable')});
        this.graphInfo = null;
      }
    }
  }

  private getAnnotationDetails(uid: string, level: number, node: { [key: string]: any }): void {
    const requestQuery = {
      'query': `{
        functionalBlock(uid: "${uid}", projectId: ${this.projectId}) {
          resolvedModuleParts {
            module {
              name
              id
              content
              taxonomies {
                name
                type {
                  name
                }
              }
            }
          }
          generatedFrom {
            annotation {
              location {
                offset
                length
              }
              module {
                id
                name
                taxonomies {
                  name
                  type {
                    name
                  }
                }
              }
              sourceAttachment
              updatedByUserName
              name
              state
              type
              categoryName
              dataDictionaryEntries
              id
            }
          }
        }
      }`
    };

    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response) {
        this.setAnnotationDetails(response, uid, level, node);
        this.loader.details = LoaderState.success;
        this.loader.code = LoaderState.success;
      }
    });
  }

  private setAnnotationDetails(response: { [key: string]: any }, uid: string, level: number, node: { [key: string]: any }): void {
    const annotation = response.data.functionalBlock.generatedFrom.annotation;
    this.selectedBlock.title = `${this.translateService.instant('codeViewer.annotationId')} ${annotation.id}`;
    this.prepareBreadCrumb(level, this.selectedBlock.currentNode, node);
    let moduleDetails: ModuleDetails[] = [];
    this.selectedBlock['isLeaf'] = true;
    this.selectedBlock['updatedBy'] = annotation.updatedByUserName;
    const startLine = this.calculateLine(this.completeCode, annotation.location.offset as number) || 1;
    const EditorOption = { ...this.EDITOR_OPTIONS, lineNumbers: this.calculateLinesForCode.bind(this, startLine) };
    moduleDetails = response.data.functionalBlock.generatedFrom.annotation.module;
    this.functionalKeys = [
      this.translateService.instant('functionalTree.additionalDetails'),
      this.translateService.instant('functionalTree.modules'),
      this.translateService.instant('functionalTree.code')
    ];
    this.selectedBlockAnnotations = [{
      content: annotation.sourceAttachment,
      EditorOption,
      moduleDetails,
      codeAnnotation: annotation,
      uid
    }];
    const categoryName = annotation?.categoryName ? annotation?.categoryName : this.translateService.instant('none');
    const annotationDataArray = [
      { key: this.translateService.instant('type'), value: annotation?.type },
      { key: this.translateService.instant('category'), value: categoryName },
      { key: this.translateService.instant('state'), value: annotation?.state }
    ];
    this.isDescriptionAvailable = annotation.name ? true : false;
    const annotationDescription = {
      description: annotation.name ? annotation.name : this.translateService.instant('reachability.noDescription'),
      annotationData: annotationDataArray
    };
    const annotationStateLabel = this.labelMappingService.mapLabel(LabelType.ANNOTATION_STATES, annotation?.state as string);
    this.selectedBlock.annotationState = annotation?.stateLink ? annotationStateLabel : '';
    if (Object.keys(annotationDescription).length) {
      this.functionalPanelData['Description & Additional Details'] = annotationDescription;
    }
    this.functionalPanelData[modulesPerTaxonomy] = this.getTaxonomiesForAnnotation(response);
  }

  private getUnGroupedAnnotation(): void {
    const requestQuery = {
      'query': `{
        annotations(projectId: ${this.projectId},
          filterObject: {content_functionalGroups_name: {is: null}}) {
            totalElements
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response) {
        this.unGroupedAnnotations = response?.data?.annotations?.totalElements;
      }
    });
  }

  private calculateLinesForCode(originalLineNumber: number, startLine: number): number {
    return originalLineNumber + startLine;
  }

  private calculateLine(initialSourceCode: string, offset: number): number {
    let line = 0;
    if (initialSourceCode?.length) {
      for (let i = 0; i < offset; i++) {
        if (initialSourceCode?.charAt(i) === '\n') {
          line++;
        }
      }
    }
    return line;
  }

  private resetKeys(): void {
    this.functionalKeys.length = 0;
  }

  private updateSelectedBlock(uid: string, updatedFunctionalGroup: any): void {
    this.ngZone.run(() => {
      this.functionalPanelData['Description'] = updatedFunctionalGroup?.description;
      const flattenFunctionalTree = this.flattenTree(this.functionalAnalysisTree);
      for (const fg of flattenFunctionalTree) {
        if (fg.uid === uid) {
          fg.title = updatedFunctionalGroup?.name as string;
          break;
        }
      }
      setTimeout(() => {
        this.selectedBlock = { ...this.selectedBlock, title: updatedFunctionalGroup?.name };
        this.prepareBreadCrumb(this.selectedBlock?.currentNode['level'] as number, this.selectedBlock?.currentNode as { [key: string]: any; });
      }, 0);
    });
  }

  private setGroupDetails(response: { [key: string]: any }) {
    if (response) {
      this.functionalPanelData = {};
      this.functionalPanelData['Description'] = response.data.functionalBlock.description;
      this.selectedBlockAnnotations = [];
      if (response.data.functionalBlock.childrenDeep) {
        this.loader.code = LoaderState.loading;
        this.setAnnotationsForSelectedNode(0);
      }
      if (response?.data?.functionalBlock?.flags?.HAS_CFG === undefined) {
        this.computeAndGenerateGraph(response.data.functionalBlock.uid as string);
      } else if (response?.data?.functionalBlock?.flags?.HAS_CFG === true) {
        this.controlFlowUtility = new ControlFlowUtility();
        this.moduleName = response?.data?.functionalBlock?.resolvedModuleParts[0]?.module?.name as string;
        this.getFunctionalBlockAsCFG(response?.data?.functionalBlock?.uid as string, this.moduleName);
      } else {
        this.selectedView.emit(true);
        this.showFunctionalDetails.emit({buttonStatus: true, msg: this.translateService.instant('functionalAnalysis.cfgComputeIsNotAvailable')});
        this.graphInfo = null;
      }
    }
    this.functionalPanelData[modulesPerTaxonomy] = this.groupModulesForTaxonomies(response);
  }

  private highlightNode(event: NzFormatEmitEvent): void {
    this.functionalAnalysisTree.forEach((item: { [key: string]: any }) => item.isHighlighted = false);
    this.clickedTitle.forEach((item) => item.node.origin.isHighlighted = false);
    this.clickedTitle.push(event);
    event.node.origin.isHighlighted = !event.node.origin.isHighlighted;
  }

  private groupModulesForTaxonomies(response: { [key: string]: any }): { [key: string]: any } {
    const groupedData = {};
    const uniqueModuleIds = new Set();
    const moduleDetails: Array<{ moduleId: string, moduleName: string }> = [];
    response.data.functionalBlock?.resolvedModuleParts?.forEach((resolvedModule: { [key: string]: any }) => {
      const moduleId = resolvedModule.module.id;
      const moduleName = resolvedModule.module.name;
      this.moduleName = moduleName;
      const taxonomies = resolvedModule?.module?.taxonomies;
      if (!uniqueModuleIds.has(moduleId)) {
        uniqueModuleIds.add(moduleId);
        if (taxonomies && taxonomies.length) {
          taxonomies?.forEach((taxonomy: any) => {
            const taxonomyName = taxonomy.name;
            const taxonomyType = taxonomy.type.name;
            const taxonomyProperty = `${taxonomyType}: ${taxonomyName}`;
            if (!groupedData[taxonomyProperty]) {
              groupedData[taxonomyProperty] = [];
            }
            groupedData[taxonomyProperty].push({
              moduleId,
              moduleName,
            });
          });
        } else {
          moduleDetails.push({ moduleId, moduleName });
        }
      }
    });
    return { taxonomy: groupedData, moduleDetails };
  }

  private getTaxonomiesForAnnotation(response: any): { [key: string]: any } {
    const groupedData = {};
    const moduleDetails: Array<{ moduleId: string, moduleName: string }> = [];
    const moduleId = response.data.functionalBlock.generatedFrom.annotation.module.id;
    const moduleName = response.data.functionalBlock.generatedFrom.annotation.module.name;
    if (response.data.functionalBlock.generatedFrom.annotation.module?.taxonomies.length > 0) {
      response.data.functionalBlock.generatedFrom.annotation.module.taxonomies.forEach((resolvedModule: { [key: string]: any }) => {
        const taxonomyName = resolvedModule.name;
        const taxonomyType = resolvedModule.type.name;
        const taxonomyProperty = `${taxonomyType}: ${taxonomyName}`;
        if (!groupedData[taxonomyProperty]) {
          groupedData[taxonomyProperty] = [];
        }
        groupedData[taxonomyProperty].push({
          moduleId,
          moduleName
        });
      });
    } else {
      moduleDetails.push({ moduleId, moduleName });
    }
    return { taxonomy: groupedData, moduleDetails };
  }

  private prepareBreadCrumb(level: number, node: { [key: string]: any }, annotationNode?: { [key: string]: any }): void {
    this.breadCrumb.length = 0;
    if (level !== 0) {
      if (level > 1) {
        this.breadCrumb.push('..');
      }
      if (annotationNode) {
        this.breadCrumb.push(annotationNode?.parentNode?.origin?.title as string);
        this.breadCrumb.push(this.selectedBlock?.title);
      } else {
        this.breadCrumb.push(node?.parentNode?.origin?.title as string);
        this.breadCrumb.push(node?.origin?.title as string);
      }
    }
  }

  /**
   * This is the main method for annotating the code, this includes creation of the annotation component, highlighting the source code,
   * adding glyphs(left bar highlighting).
   * @param editor The instance of the monaco editor
   * @param annotation The actual annotation.
   */
  private addAnnotationToEditor(
    editor: editor.ICodeEditor,
    annotation: { [key: string]: any }
  ) {
    const annotationId: number = annotation?.generatedFrom?.annotation?.id;
    const domNode = this.renderer.createElement('code-annotation-editor-element');
    const lineId = 'line' + annotationId;
    domNode.style.zIndex = '1';
    domNode.setAttribute('id', lineId);
    const annotationComponentId = annotationElementIdPrefix + annotationId;
    domNode.id = annotationComponentId;
    domNode.className = 'code-annotation-editor-component-container';
    let viewZoneId: string;
    /* Populate the component with the values from the annotation. */
    const codeAnnotationComponent: NgElement & WithProperties<CodeAnnotationEditorComponent> = domNode;
    codeAnnotationComponent.annotationsOfModule = this.codeAnnotationArray;
    codeAnnotationComponent.projectId = this.projectId;
    codeAnnotationComponent.isCfg = true;
    codeAnnotationComponent.data = this.getDataForAnnotationComponent(annotation);
    codeAnnotationComponent.disableActionButtons = true;
    editor.changeViewZones((changeAccessor: any) => {
      viewZoneId = changeAccessor.addZone({
        afterLineNumber: 0,
        heightInPx: monacoEditorStyle.annotationMinHeight - 15,
        domNode,
      });
    });
    codeAnnotationComponent.data.viewZoneId = viewZoneId;
  }

  /**
   * returns the data for the code Annotation custom element we have created.
   * @param annotation annotation data saved in the system.
   * @returns object of type {AnnotationElementData}
   */
  private getDataForAnnotationComponent(annotation: any): AnnotationElementData {
    const data: AnnotationElementData = {
      annotation,
      moduleName: annotation?.module?.name,
      moduleId: annotation?.module?.id,
      projectId: this.projectId,
      borderColor: this.stateColor[annotation?.state],
      typeLabel: this.annotationType.find((item: any) => item.value === annotation?.type)?.label,
      stateLabel: this.annotationState.find(
        (item: any) => item.value === annotation?.state

      )?.label,
      modulePath: annotation?.moduleName
    };
    return data;
  }

  private createChildHierarchy(data: [{ [key: string]: any }], parentId: string) {
    const nodeMap = new Map();
    const childOfCurrentParent: string[] = [];
    data.forEach(node => {
      const data = this.createTreeNode(node, true);
      nodeMap.set(node.uid, data);
    });

    data.forEach(node => {
      if (node.parents && node.parents.content) {
        node.parents.content.forEach((parent: { [key: string]: any }) => {
          const parentNode = nodeMap.get(parent.uid);
          if (parentNode) {
            const childNode = nodeMap.get(node.uid);
            if (childNode && ! parentNode.children?.find((child: { [key: string]: any }) => child.uid === childNode.uid)) {
              if (parentNode.children) {
                parentNode.children.push(childNode);
              } else {
                parentNode.children = [childNode];
                parentNode.isLoaded = true;
              }
              if (node.type[0] === 'FUNCTIONAL_UNIT') {
                parentNode.type = 'FUNCTIONAL_BLOCK';
              }
            }
          }

          if (parent.uid === parentId) {
            childOfCurrentParent.push(node.uid as string);
          }
        });
      }
    });
    return Array.from(nodeMap.values()).filter(node => childOfCurrentParent.includes(node.uid as string));
  }

  private flattenTree(tree: NzTreeNodeOptions[]): NzTreeNodeOptions[] {
    const flattened: NzTreeNodeOptions[] = [];
    tree.forEach((node) => {
      flattened.push(node);
      if (node.children && node.children.length > 0) {
        flattened.push(...this.flattenTree(node.children));
      }
    });
    return flattened;
  }

  private sort(sortStateKey: string): void {
    this.sortState.currentSort = sortStateKey;
    const currentParams = this.route.snapshot.paramMap;
    const sortValue: string = this.sortState[sortStateKey] === this.SortDetails.ASC ? this.SortDetails.DESC : this.SortDetails.ASC;
    this.sortState[sortStateKey] = sortValue;
    const sortKey: string = sortStateKey === this.SortDetails.NAME ? this.SortDetails.CONTENT_NAME : this.SortDetails.CONTENT_UPDATED;
    currentParams['sortApplied'] = JSON.stringify({[sortKey]: sortValue.toUpperCase()});
    this.sortDetails = {[sortKey]: sortValue.toUpperCase()};
    if (this.filterDetails) {
      currentParams['filterApplied'] = JSON.stringify(this.filterDetails);
    }
    this.getTreeDetailsBasedOnFilter(this.filterDetails, this.selectedPageIndex, undefined, this.sortDetails);
    const url = `/project-${this.projectId}/functional-analysis`;
    const route = this.router.createUrlTree([url], { queryParams: currentParams, relativeTo: null }).toString();
    window.history.replaceState({}, '', '/#' + route);
  }

  private setAnnotationsForSelectedNode(page: number): void {
    this.currentAnnotationPage = page;
    this.faChildrenDeepGQL.fetch({
      projectId: this.projectId,
      uid: this.selectedBlock.uid,
      page,
      size: annotationsPerPage,
      filterObject: {content_type: {eq: FunctionalBlockType.FunctionalUnit}},
    }, {fetchPolicy: 'network-only'}).subscribe((response: { [key: string]: any }) => {
      const children = response.data.functionalBlock.childrenDeep;
      if (children?.totalElements) {
        for (const child of children.content  ) {
          const codeAnnotation = child.generatedFrom.annotation;
          const annotationId = codeAnnotation.id;
          const startLine = this.calculateLine(codeAnnotation.sourceAttachment as string, codeAnnotation.location.offset as number) || 1;
          const EditorOption = { ...this.EDITOR_OPTIONS, lineNumbers: this.calculateLinesForCode.bind(this, startLine) };
          const previousBlock = this.selectedBlockAnnotations[this.selectedBlockAnnotations.length - 1];
          const currentBlockName = child.parents.content[0].name;
          const fgName = previousBlock?.parents[0].name !== currentBlockName ? currentBlockName : '';
          this.selectedBlockAnnotations.push({
            content: codeAnnotation.sourceAttachment,
            EditorOption,
            fgName,
            moduleDetails: codeAnnotation.module,
            annotationId,
            parents: child.parents.content,
            codeAnnotation,
            uid: child.uid
          });
        }
        const loadedAnnotations = this.selectedBlockAnnotations.length;
        if (children.totalElements > loadedAnnotations) {
          this.loadAnnotations = true;
        }
      } else {
        this.loadAnnotations = false;
      }
      this.loadingAnnotations = false;
      this.loader.code = LoaderState.success;
    });
  }

  private updateGroupedFunctionalBlockDetails(origin: any, parent: NzTreeNodeOptions): void {
    const existingIndex = this.groupedFunctionalBlockDetails.findIndex(item => item.uid === origin.uid);
    if (existingIndex === -1) {
      this.groupedFunctionalBlockDetails.push({ uid: origin.uid, type: origin.type, parent });
    } else {
      this.groupedFunctionalBlockDetails = this.groupedFunctionalBlockDetails.filter(
        (nodeItem: any) => nodeItem.uid !== origin.uid);
    }
  }

  private computeAndGenerateGraph(functionBlockId: string): void {
    this.messageService.info(this.translateService.instant('functionalAnalysis.cfgComputeAsCfgNotDone') as string);
    this.functionalBlockControllerService.computeFunctionalBlock(this.projectId, new Set([functionBlockId])).subscribe({
      next: (jobProgressId) => {
        if (jobProgressId) {
          this.jobManager.register({ jobId: jobProgressId as unknown as string })?.status$.subscribe((status: JobInformation.StatusEnum) => {
            if (status === JobInformation.StatusEnum.SUCCESS) {
              const requestQuery = {
                'query': `{
                  functionalBlock(uid: "${functionBlockId}", projectId: ${this.projectId}) {
                    uid
                    name
                    flags
                    resolvedModuleParts {
                      module {
                        name
                        id
                      }
                    }
                  }
                }`
              };
              this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
                if (response?.data?.functionalBlock?.flags?.HAS_CFG === true) {
                  this.messageService.success(this.translateService.instant('functionalAnalysis.cfgComputeDone') as string);
                  this.controlFlowUtility = new ControlFlowUtility();
                  const moduleName = response?.data?.functionalBlock?.resolvedModuleParts[0]?.module?.name as string;
                  this.getFunctionalBlockAsCFG(functionBlockId, moduleName);
                  this.moduleName = moduleName;
                } else {
                  this.messageService.error(this.translateService.instant('functionalAnalysis.cfgComputeIsNotAvailable') as string);
                  this.showFunctionalDetails.emit({buttonStatus: true, msg: this.translateService.instant('functionalAnalysis.cfgComputeIsNotAvailable')});
                }
              });
            }
          });
        }
      },
      error: () => {}
    });
  }

  private createInitialSearchedFATree(parentContent: Array<{ [key: string]: any }>, childrenContent: Array<{ [key: string]: any }>, uid: string): void {
    const childrenMap: Map<string, any[]> = new Map();
    for (const item of childrenContent) {
      const children = item.childrenDeep;
      childrenMap.set(item.uid as string,
        children?.content
          ? this.createChildHierarchy(children.content as [{ [key: string]: any; }], item.uid as string)
          : []
      );
    }
    this.functionalAnalysisTree = [];
    this.prepareTreeData(parentContent, uid, childrenMap);
  }

  private createInitialFATree(content: Array<{ [key: string]: any }>, uid: string): void {
    const uidArray: string[] = content.map((item: { [key: string]: any }) => item.uid);
    this.treeViewData.emit(uidArray);

    this.functionalAnalysisTree = [];
    content.forEach((node: { [key: string]: any }) => {
      const children = node.children?.content.map((child: { [key: string]: any }) => this.createTreeNode(child, false));
      const typeDecides = children.every(
        (childItem: { [key: string]: any }) =>
          childItem.type === 'FUNCTIONAL_UNIT' ||
          childItem.type === 'FUNCTIONAL_CONDITION' ||
          childItem.type === 'FUNCTIONAL_STATEMENT'
      );
      const functionalAnalysisItem = this.createTreeNode(node, true);
      functionalAnalysisItem.children = children && children?.length ? children : null;
      functionalAnalysisItem.isLoaded = true;
      functionalAnalysisItem.type = typeDecides ? 'FUNCTIONAL_BLOCK' : 'FUNCTIONAL_GROUP';
      functionalAnalysisItem.isHighlighted = node.uid === uid;
      this.functionalAnalysisTree = [...this.functionalAnalysisTree, { ...functionalAnalysisItem }];
    });
  }

  private createTreeNode(node: { [key: string]: any }, isLoaded: boolean): NzTreeNodeOptions {
    const type = node.type[0];
    return {
      title: node.name,
      key: node.uid,
      uid: node.uid,
      isCheckBox: false,
      isDescriptionProvided: true,
      type,
      isLeaf: type === 'FUNCTIONAL_UNIT' ? true : false,
      isLoaded,
      children: node.children?.totalElements > 0 ? [] : null,
      generatedBy: node.flags?.GENERATED_BY
    };
  }

  private loadTreeNodeChildren(node: NzTreeNodeOptions): void {
    const uidFilter: FilterObject_FunctionalBlocks = {content_uid: {eq: node.key}};
    this.faTreeGQL.fetch({
      projectId: this.projectId,
      sortObject: {},
      filterObject: uidFilter,
      childrenFilterObject: this.childrenFilterObject,
      useChildrenDeep: false
    }, {fetchPolicy: 'network-only'}).subscribe((response: { [key: string]: any }) => {
      if (response && response.data.functionalBlocks.totalElements) {
        const parent = response.data.functionalBlocks.content[0];
        if (parent.children.content) {
          const childrenContent = parent.children.content;
          const childrenData: Array<{ [key: string]: any }> = [];
          childrenContent.forEach((content: { [key: string]: any }) => {
            childrenData.push(this.createTreeNode(content, false));
          });
          node.addChildren(childrenData);
          node.origin.isLoaded = true;
          this.cdr.detectChanges();
        }
      }
    });
  }
}


