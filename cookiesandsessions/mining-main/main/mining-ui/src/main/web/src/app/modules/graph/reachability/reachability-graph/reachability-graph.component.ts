import {
  AfterViewInit,
  Component,
  ComponentFactoryResolver,
  ElementRef,
  ViewChild,
  Output,
  EventEmitter,
  Input,
  OnInit,
  ChangeDetectorRef,
  Inject,
  OnChanges,
  ComponentRef,
  ViewContainerRef,
  Type,
  OnDestroy
} from '@angular/core';
import { GraphUtility } from '../../dependency/utils/dependency-graph-utility';
import { Logger } from '@app/core';
import {
  GraphComponent,
  IGraph,
  GraphBuilder,
  GraphViewerInputMode,
  ItemClickedEventArgs,
  IModelItem,
  GraphMLSupport,
  StorageLocation,
  GraphBuilderItemEventArgs,
  IEdge,
  INode,
  GraphItemTypes,
  QueryItemToolTipEventArgs,
  TimeSpan,
  FilteredGraphWrapper,
  DefaultGraph,
  CompositeLayoutData,
  GraphOverviewComponent,
  ILayoutAlgorithm,
  MultiStageLayout,
  HighlightIndicatorManager,
  ILabel,
  EdgesSource,
  NodesSource,
  GraphEditorInputMode,
  ShapeNodeStyle,
  IconLabelStyle,
  InteriorLabelModel,
  Insets,
  DefaultLabelStyle,
  LabelShape,
  InteriorLabelModelPosition,
  HorizontalTextAlignment,
  TextWrapping,
  VerticalTextAlignment,
  ExteriorLabelModel,
  DashStyle
} from 'yfiles';
import {
  UntypedFormGroup,
  UntypedFormBuilder,
  UntypedFormControl
} from '@angular/forms';
import {
  LAYOUT_MAP,
  LayoutType,
  LAYOUT_LIMIT_MAP,
  HIERARCHIC_LAYOUT_DATA,
  PARTIAL_LAYOUT_DATA,
  PARTIAL_LAYOUT,
  LARGE_GRAPH_CUTOFF
} from '../../utils/graph-layouts';
import {
  NODE_CONFIG,
  NodeWidthSizes,
  NodeHeightSizes,
  NodeType
} from '../../utils/node-configurations';
import { FastGraphModelManager } from '@app/modules/graph/utils/yfiles-util/fast-graph-model-manager';
import CollapseAndExpandNodes from '@app/modules/graph/utils/yfiles-util/collapse-expand';
import { TranslateService } from '@ngx-translate/core';
import { ActivatedRoute, Router } from '@angular/router';
import { YFileGraphInfo, FilterParameters, YNode } from '@app/modules/graph/models/yfile-graph-info.model';
import {
  GraphGlobalStyles, DEFAULT_NODE_SHAPE, SELECTED_NODE_STROKE_THICKNESS,
  DEFAULT_STROKE_THICKNESS,
  FUNCTIONAL_COUNT_FILL,
  FUNCTIONAL_COUNT_COLOR,
  FUNCTIONAL_COUNT_SIZE,
  BODY_BACKGROUND_COLOR,
  DEFAULT_EDGE_COLOR,
  DEFAULT_EDGE_LABEL_INSET,
  ARTIFICIAL_EDGE_COLOR,
  DELETED_EDGE_COLOR,
} from '../../utils/graph-global-styles';
import { WindowToken } from '@app/core/utils/window';
import { NODE_COLORS, NodeColor } from '../../utils/node-colors';
import { forkJoin, Observable, of, Subject } from 'rxjs';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { MiningSelectOption } from '@app/shared/interfaces/mining-select-option.interface';
import { ContextToolbarType, NodeVisibility } from '../../dependency/dependency-graph/context-menu-toolbar/context-menu-options';
import { ScrollDirection, ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { CodeViewerButtonComponent } from '@app/shared/components/code-viewer-button/code-viewer-button.component';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { catchError } from 'rxjs/operators';
import { addHighlights, initializeHighlightStyles, setGraphSelectionIndicators } from '../../utils/graph-highlight.util';
import { DepedendencyGraphEdgeMetaData, InputForArtificialEdge } from '../../dependency/dependency-graph/graph-edge-meta-data/graph-edge-meta-data.interface';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { NzModalService } from 'ng-zorro-antd/modal';
import { SharedModuleDetailsComponent } from '@app/shared/components/shared-module-details/shared-module-details.component';
import { GraphEdgeMetaDataComponent } from '../../dependency/dependency-graph/graph-edge-meta-data/graph-edge-meta-data.component';
import { DependencyGraph, ModuleControllerService, ModulePojo, ModuleRelationshipPojo, ReferenceControllerService, TechnologyType }
  from '@innowake/mining-api-angular-client';
import { ReachabilityService } from '@app/modules/reachability-ui-product-vision/utils/reachability.service';
import {
  addNodeAndEdge, getGraphLayoutExecutor, getNodeLevels, getRelationshipLabel,
  getTechnologyTypeForModule, getValuesFromUncheckedOptions, installGraphModelManager,
  reEvaluateLazyLoadingNodes, setGraphOverviewPanel, setNodeVisibility, zoomToRootNodes
} from '../../utils/dependency-reachability-graph.util';
import { TaxonomyFilterComponent } from '@app/shared/components/taxonomy-filter/taxonomy-filter.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { ListOfFunctionalBlocks, ListOfTechnologyType, ReachabilityGraphFilter
} from '@app/modules/reachability-ui-product-vision/utils/reachability-interface';
import { sortNaturally } from '@app/core/utils/sort.util';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { LabelType } from '@app/core/utils/mapping.utils';
import { ReachabilityBlockGraphFilterRequest } from '@innowake/mining-api-angular-client';
import { Innowake_Mining_Shared_Model_RelationshipDirection } from '@app/graphql/generated/generated';
import { ReachabilityEdgeMetaDataComponent } from './reachability-edge-meta-data/reachability-edge-meta-data.component';
import { LABEL_TEXT_SIZE, DEFAULT_LABEL_FONT } from '../../utils/node-label-configuration';

const logger = new Logger('DependencyGraphComponent');
const MAX_DEPTH = 10;
const COLLAPSIBLE_LEVEL = 1;
const REFERENCE_TYPES = ['Calls', 'Includes', 'References', 'Accesses'];
declare const java__callback__openMiningUiPage: (path: string) => Promise<string>;
const REFERENCE_TYPE =
[ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum.ACCESSES, ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum.ARTIFICIAL,
ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum.CALLS, ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum.CONTAINS,
ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum.INCLUDES, ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum.NONE,
ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum.PRECEDES, ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum.REFERENCES];

@Component({
  selector: 'reachability-graph',
  templateUrl: './reachability-graph.component.html'
})
export class ReachabilityGraphComponent implements AfterViewInit, OnInit, OnChanges, OnDestroy {
  @ViewChild('taxonomyFilter') taxonomyFilterComponent: TaxonomyFilterComponent;
  @Input() public eclipseView = false;
  @Input() public graphInfo: YFileGraphInfo;
  @Input() reachabilityBreadcrumbItems: string[] = [];
  @Input() selectedDepth: number;
  @Input() layout: string;
  @Input() filterSelectedOptions: FilterParameters;
  @Input() showExports: boolean;
  @Input() projectId: number;
  @Input() reachabilityGraph: DependencyGraph;
  @Input() networkUid = '';
  @Output() filterChange: EventEmitter<any> = new EventEmitter();
  @Output() filterRBGraph: EventEmitter<ReachabilityGraphFilter> = new EventEmitter();
  @ViewChild(CodeViewerButtonComponent) codeViewerButton: CodeViewerButtonComponent;
  @ViewChild('graphComponentRef') graphComponentRef!: ElementRef;
  @ViewChild('sidePanel', { read: ViewContainerRef })
  sidePanel: ViewContainerRef;
  nodeTypes: NodeType[] = [];
  depthOptions: MiningSelectOption[] = [];
  filterOptions: MiningSelectOption[] = [];
  graphFilter: UntypedFormGroup;
  graphBuilder: GraphBuilder;
  filterOptionsBeforeChange: string[];
  collapseAndExpandNodes: CollapseAndExpandNodes;
  nodeIdToNodeMap: Map<number, INode> = new Map();
  displayModal = false;
  showOverview = true;
  isGraphEmpty = true;
  depthItems: MiningSelectOption[] = [];
  layoutItems: MiningSelectOption[] = [];
  rootModules: ModulePojo[];
  rootModuleIds: Set<string>;
  graphRootNodes: INode[] = [];
  selectedNode: any;
  rootNodeEvent: Subject<INode> = new Subject<INode>();
  resetFilterOptions: Subject<boolean> = new Subject();
  resetGraph: Subject<boolean> = new Subject();
  isEclipseFeatureActive = false;
  moduleDetailRoute: string;
  moduleName: string;
  moduleId: number;
  parentModule: ModulePojo;
  currentModule: ModulePojo;
  graphComponent!: GraphComponent;
  overviewComponent!: GraphOverviewComponent;
  moduleDetails: ModulePojo;
  scrollPosition: ScrollDirection;
  selectedItem: any;
  showHide: string;
  disableCodeViewer = false;
  codeViewerTooltip: string;
  nodesSource: NodesSource<YNode>;
  edgesSource: EdgesSource<ModuleRelationshipPojo>;
  defaultFiltered = false;
  explorable = false;
  inputForEdgeMetadata: DepedendencyGraphEdgeMetaData[] = [];
  inputForArtificialEdgeMetadata: InputForArtificialEdge[] = [];
  referenceId: number;
  isArtificialEdge: boolean;
  sidePanelIconState = false;
  relationShipType: string;
  fromId: number;
  toId: number;
  nodeEdgeClicked: string;
  relationshipLabel: string;
  componentInstances: Array<ComponentRef<any>> = [];
  isFiltersActive = false;
  toggleFilter: boolean;
  selectedTaxonomyIds: number[] = [];
  selectedFunctionalBlock: string[] = [];
  selectedTechnologyAndType: TechnologyType[] = [];
  selectedReferenceType: ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum[] = [];
  listOfFunctionalBlocks: ListOfFunctionalBlocks[] = [];
  listOfTechnologyType: ListOfTechnologyType[] = [];
  listOfReferenceType: ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum[];
  selectedNodes: IModelItem[] = [];
  contextMenuOptionType: string[] = [ContextToolbarType.REACHABILITY_BLOCK];
  sharedModuleDetails: any[];
  isSharedResource: boolean;
  showMergeInfoBanner = true;
  networkGraphExportName: string;
  modulesWithErrorsAndWarnings: { [key: string]: any };

  constructor(
    public reachabilityService: ReachabilityService,
    private modal: NzModalService,
    private formBuilder: UntypedFormBuilder,
    private translateService: TranslateService,
    private cdRef: ChangeDetectorRef,
    @Inject(WindowToken) private $window: Window,
    private deepLinkService: DeepLinkService,
    private moduleControllerService: ModuleControllerService,
    private referenceControllerService: ReferenceControllerService,
    private route: ActivatedRoute,
    private scrollEventService: ScrollEventService,
    private featureToggleService: FeatureToggleService,
    private CFR: ComponentFactoryResolver,
    private router: Router,
    private graphQlControllerService: GraphQlControllerService,
    private labelMappingService: LabelMappingService
  ) {
    for (let index = 1; index <= MAX_DEPTH; index++) {
      this.depthItems.push({ label: 'Depth: ' + index, value: index });
    }
  }

  public async runLayout(graphComponent: GraphComponent, toggledNode?: INode, expand?: boolean, isOnLoad?: boolean,
    animateViewport: boolean = true): Promise<void> {
    const graph = graphComponent.graph as FilteredGraphWrapper;
    if (this.explorable) {
      (graphComponent.inputMode as GraphEditorInputMode).selectablePredicate = item => ! item.tag.info?.isClue;
    }
    if (GraphUtility.enableInfoAlert(graph)) {
      this.modal.info({
        nzContent: this.translateService.instant('dependencyGraph.infoAlertMessageForMaxGraphNodes') as string,
        nzOnOk: () => true
      });
    }
    if (GraphUtility.isLarge(graph)) {
      graphComponent.graph.applyLayout(LAYOUT_MAP[this.graphFilter.value.selectedLayout] as ILayoutAlgorithm);
    } else {
      let currentLayout = LAYOUT_MAP[this.graphFilter.value.selectedLayout];
      if (graph.edges.size > LARGE_GRAPH_CUTOFF) {
        currentLayout = LAYOUT_MAP['CIRCULAR_LAYOUT'];
      }
      let currentLayoutData;
      if (this.graphFilter.value.selectedLayout === LayoutType.HIERARCHIC_LAYOUT
       || this.graphFilter.value.selectedLayout === LayoutType.HIERARCHIC_LAYOUT_LEFT_TO_RIGHT) {
        currentLayoutData = new CompositeLayoutData(HIERARCHIC_LAYOUT_DATA);
      } else {
        currentLayoutData = new CompositeLayoutData();
      }
      this.collapseAndExpandNodes.configureLayout(toggledNode, expand, currentLayoutData, currentLayout as MultiStageLayout);
      const layoutExecutor = getGraphLayoutExecutor(graphComponent,
        currentLayout as ILayoutAlgorithm, currentLayoutData, toggledNode, isOnLoad, animateViewport);
      const partialLayoutExecutor= getGraphLayoutExecutor(graphComponent,
        PARTIAL_LAYOUT as ILayoutAlgorithm, PARTIAL_LAYOUT_DATA, toggledNode, isOnLoad, animateViewport);
      try {
        await layoutExecutor.start();
        if (this.explorable) {
          await partialLayoutExecutor.start();
        }
      } catch (error) {
        logger.error(error);
      }
    }
  }

  ngOnInit(): void {
    this.featureToggleService.isActive('dependencyGraphExplore').subscribe((isActive: boolean) => {
      this.explorable = isActive;
      this.updateVisualClues();
    });
    this.modulesWithErrorsAndWarnings = JSON.parse(localStorage.getItem('ModulesWithErrorsAndWarnings'));
    this.projectId = this.route.snapshot.params['projectId']?.split('-')[1];
    this.onTechnologyTypeSearch();
    this.moduleName = this.reachabilityBreadcrumbItems[1];
    if (this.graphInfo) {
      this.setRootModules();
      this.createForm();
    }
    Object.keys(LayoutType).forEach(key => {
      this.layoutItems.push({ label: LayoutType[key], value: key });
    });
    this.deepLinkService.featureIsActive().subscribe(resp => {
      this.isEclipseFeatureActive = resp;
    });
    this.scrollEventService.getScrollObservable().subscribe((scrollevent) => {
      this.scrollPosition = scrollevent;
      this.showHide = 'mining-fixed-page-header-' + scrollevent;
    });
  }

  ngAfterViewInit(): void {
    if (this.graphComponent) {
      this.graphComponent.cleanUp();
    }
    if(this.networkUid) {
      this.contextMenuOptionType = [ContextToolbarType.REACHABILITY_NETWORK];
      this.networkGraphExportName = `${this.translateService.instant('reachability.networkGraph')}`;
    }
    this.graphComponent = new GraphComponent(this.graphComponentRef?.nativeElement as string);
    setGraphOverviewPanel(this.graphComponent, this.overviewComponent);
    this.collapseAndExpandNodes = new CollapseAndExpandNodes(this.graphComponent);
    this.addIndicatorNodes(this.graphInfo);
    void this.createGraph();
    this.updateVisualClues();
    this.cdRef.detectChanges();
  }

  ngOnChanges(): void {
    if (this.graphComponent) {
      this.graphRootNodes = [];
      this.setRootModules();
      void this.createGraph().then(() => zoomToRootNodes(this.graphComponent, this.graphRootNodes));
      this.resetGraph.next(true);
      this.graphComponent.highlightIndicatorManager.clearHighlights();
    }
  }

  /**
   * Method to update the Outdated RB.
   */
  updateOutdatedRb(): void {
    this.reachabilityService.updateOutdatedBlock = true;
    void this.router.navigate(['/project-' + this.projectId + '/reachability/overview']);
  }

  updateVisualClues(): void {
    if ( ! this.explorable) {
      return;
    }
    const filteredGraph = (this.graphComponent.graph) as FilteredGraphWrapper;
    const unfilteredGraph = filteredGraph.wrappedGraph;
    const clueNodes = unfilteredGraph.nodes.filter(node => node.tag.info?.isClue);

    const observables: {[id: string]: Observable<ModuleRelationshipPojo[]>} = {};
    clueNodes.forEach(node => {
      const outObservable = this.buildFindReferenceObservable(node, unfilteredGraph, filteredGraph, GraphUtility.OUT);
      if (outObservable) {
        observables[node.tag.id + '_' + GraphUtility.OUT] = outObservable;
      }
      const inObservable = this.buildFindReferenceObservable(node, unfilteredGraph, filteredGraph, GraphUtility.IN);
      if (inObservable) {
        observables[node.tag.id + '_' + GraphUtility.IN] = inObservable;
      }
    });
    forkJoin(observables).subscribe((data: {[id: string]: ModuleRelationshipPojo[]}) => {
      clueNodes.forEach(node => {
        if (data[node.tag.id + '_' + GraphUtility.IN]) {
          this.updateClue(node, unfilteredGraph, filteredGraph, GraphUtility.IN, data[node.tag.id + '_' + GraphUtility.IN].length);
        }
        if (data[node.tag.id + '_' + GraphUtility.OUT]) {
          this.updateClue(node, unfilteredGraph, filteredGraph, GraphUtility.OUT, data[node.tag.id + '_' + GraphUtility.OUT].length);
        }
      });
      (this.graphComponent.graph as FilteredGraphWrapper).nodePredicateChanged();
      void this.runLayout(this.graphComponent, null, null, null, false);
    });
  }

  /**
   * Captures the event emitted by the contextual Toolbar component
   * and sets the node visibility.
   */
  contextMenuHideNode(data: NodeVisibility): void {
    setNodeVisibility(data.node, data.visibility, this.collapseAndExpandNodes);
  }

  /**
   * Method to trigger highlighting edge/node on opening detail panel.
   */
  highlightItem(): void {
    this.onHoveredItemChanged(this.selectedItem as IModelItem, 'hover');
  }

  /**
   * Captures the event emitted by the contextual Toolbar component
   * and redirects to the code viewer or module details in new tab.
   * @param path route to be navigated to.
   */
  openInNewBrowserTab(path: string): void {
    const moduleId: number = this.selectedNode.tag.id;
    if (this.eclipseView) {
      void java__callback__openMiningUiPage('/#' + RouteBuilder.buildModuleRoute(this.projectId, moduleId, path));
    } else {
      if (path === 'code-viewer') {
        this.codeViewerButton.openInNewBrowserTab(path);
      } else {
        openInNewTab(this.projectId, moduleId, path, this.$window);
      }
    }
  }

  /**
   * Captures the event emitted by the contextual Toolbar component &
   * Calls the deeplink service to open the selected module in Eclipse.
   */
  openInEclipse(): void {
    const node = this.selectedNode;
    this.moduleControllerService.findModuleById(node.tag.projectId as number, node.tag.id as number).subscribe((response: ModulePojo) => {
      this.deepLinkService.showModuleInEclipse(response);
    });
  }

  /**
   * method to show filter again
   */
  showFilter(): void {
    this.toggleFilter = ! this.toggleFilter;
  }

  /**
   * Gets the user selected taxonomy details.
   * @param event taxonomy filter selected.
   */
  getTaxonomySelection(event: TaxonomyFilterSelected[]): void {
    this.selectedTaxonomyIds = [];
    event?.forEach((taxonomy: TaxonomyFilterSelected) => {
      const taxonomyIds = `${taxonomy.taxonomyId}`.split(',').map(Number);
      this.selectedTaxonomyIds.push(...taxonomyIds);
    });
  }

  /**
   * Opens the modules table for a given tag.
   * @param tag - The tag to open the modules table for error/warnings.
   */
  openModulesTable(tag: string): void {
    this.reachabilityService.openModulesTable(tag, this.projectId);
  }

  /**
   * Method to search functional blocks on type
   * @param search: typed character
   */
  // @yjs:keep
  onFunctionalBlockSearch(search: string): void {
    this.listOfFunctionalBlocks = [];
    let contentTypeFilter = '{content_type: {eq: FUNCTIONAL_GROUP}';
    contentTypeFilter += 'content_parents:{notEq: {content_type: {eq: FUNCTIONAL_GROUP}}}}';
    const requestQuery = {
      'query': `{
        functionalBlocks(projectId: ${this.projectId},
          filterObject: ${contentTypeFilter}) {
          content {
            uid
            name
            type
            children {
              content {
                name
                uid
                type
              }
            }
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response && response?.data.functionalBlocks.content?.length) {
        const functionalBlocks = sortNaturally(response.data.functionalBlocks.content as Array<{[key: string]: any}>, 'name').map((block: any) => ({
          name: block.name, uid: block.uid }));
        this.listOfFunctionalBlocks =  functionalBlocks.filter((block: any) => block.name.toLowerCase().includes(search.toLocaleLowerCase()));
      }
    });
  }

  /**
   * Method to search Reference Type for RB Graph Filter
   */
  onReferenceTypeSearch(): void {
    this.listOfReferenceType = [];
    REFERENCE_TYPE.forEach((type: ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum) => {
      this.listOfReferenceType.push(type);
    });
  }

  /**
   * To reset the filter values
   */
  onResetFilter(): void {
    this.selectedTaxonomyIds = [];
    this.selectedFunctionalBlock = [];
    this.isFiltersActive = false;
    this.selectedTechnologyAndType = [];
    this.selectedReferenceType = [];
    this.taxonomyFilterComponent.selectedValue = [];
    this.onApplyFilter();
  }

  /**
   * Method to filter reachability graph based on applied filters.
   */
  onApplyFilter(): void {
    const filterDetails: ReachabilityGraphFilter = this.constructFilterDetails();
    this.toggleFilter = ! this.toggleFilter;
    this.isFiltersActive = this.checkFilterApplied(filterDetails);
    this.filterRBGraph.emit(filterDetails);
  }

  /**
   * method to check if buttons should be disabled or not
   * @returns state of the buttons
   */
  isButtonDisabled(): boolean {
    return !(!!this.selectedTechnologyAndType.length || !!this.selectedTaxonomyIds.length ||
      !!this.selectedFunctionalBlock.length || !!this.selectedReferenceType.length);
  }

  /**
   * Method to get the graph component.
   * @returns graphComponent.
   */
  getGraphComponent(): GraphComponent {
    return this.graphComponent;
  }

  /**
   * Method to get the nodes selected in the graph.
   * @returns the selected node
   */
  getSelectedNode(): INode {
    if (this.enableSelection(this.selectedNode)) {
      return this.selectedNode;
    }
  }

  /**
   * Method to set the filter options before changing them.
   * This helps in identifying if the filter options were made dirty or not.
   * Consider the following scenario:
   * We have three options in the filter
   * COBOL PROGRAM
   * JCL EXEC PGM
   * CALLS
   * If we unselect COBOL PROGRAM and then select it again, the dropdown is not dirty.
   * However, the FormControl#isDirty() will still return true.
   * This will trigger a call to the back-end which can be very expensive for large graphs.
   */
  setOptionsBeforeChange(): void {
    this.filterOptionsBeforeChange = this.graphFilter.value.typeControl;
  }

  /**
   * Method to check if the Filter dropdown is dirty or not.
   */
  isFilterDirty(): boolean {
    const filterValues: string[] = this.graphFilter.value.typeControl;
    if (this.isFilterPristine(this.filterOptionsBeforeChange, filterValues)) {
      this.graphFilter.controls['typeControl'].markAsPristine();
    } else {
      this.graphFilter.controls['typeControl'].markAsDirty();
    }
    // @yjs:keep
    return this.graphFilter.controls['typeControl'].dirty;
  }

  /**
   * Method to fetch the path of the icon related to the corresponding node type.
   * The icon will be displayed in the Filter dropdown.
   *
   * @param value - node type value
   * @returns image url.
   */
  getImagePath(value: string): string {
    return NODE_CONFIG[value] ? NODE_CONFIG[value].imageUrl : '';
  }

  /**
   * Method to fetch the style for the icon to display in Filter dropdown.
   *
   * @param value - node type value
   * @returns style Object
   */
  getStyleForImage(value: string): { border: string; background: string; } | Record<string, unknown> {
    const nodeConfig = NODE_CONFIG[value];
    if (nodeConfig) {
      const color: NodeColor = nodeConfig.color;
      return {
        'border': '1px solid ' + color.strokeColor,
        'background': color.fillColor,
      };
    }
    return {};
  }

  /**
   * Method to filter the graph based on the parameters selected in the Filter dropdown.
   */
  public filterGraph(): void {
    const selectedFilterOptions: string[] = this.graphFilter.value.typeControl;
    const filterNodes = this.graphInfo.nodeTypes.filter(nodeType => ! selectedFilterOptions.includes(nodeType)).map(type => type.replace(/ /g, '_'));
    const filterRelations: string[] = Array.from(this.graphInfo.relationships).filter(nodeType => ! selectedFilterOptions.includes(nodeType));
    const filterParams: FilterParameters = {
      moduleFilter: filterNodes,
      relationshipFilter: filterRelations
    };
    this.filterChange.emit(filterParams);
  }

  /**
   * Method to toggle the side panel
   */
  toggleSidePanel(): void {
    this.sidePanelIconState = ! this.sidePanelIconState;
    this.componentInstances.forEach((componentInstance) => {
      componentInstance?.destroy();
    });
    if (this.sidePanelIconState) {
      switch (this.nodeEdgeClicked) {
        case 'node':
          this.createSidePanel(SharedModuleDetailsComponent as Component,this.nodeEdgeClicked);
          break;
        case 'edge':
          this.createSidePanel(GraphEdgeMetaDataComponent as Component,this.nodeEdgeClicked);
          break;
      }
    }
  }

  /**
   * Checks if the type filter options closed or not.
   * @param state state of the filter type dropdown panel.
   */
  public filterTypeOpen(state: boolean): void {
    if (state) {
      this.setOptionsBeforeChange();
    } else {
      if (this.isFilterDirty()) {
        this.filterGraph();
        this.graphFilter.controls['typeControl'].markAsPristine();
      }
    }
  }

  /**
   * Highlights the hovered/selected item.
   * @param item item that is hovered/selected.
   * @param manager to hightlight the item.
   */
  addHighlights(item: IModelItem, manager: HighlightIndicatorManager<IModelItem>): void {
    // then see where we are hovering over, now
    const newItem = item;
    if (newItem !== null) {
      if (INode.isInstance(newItem)) {
        manager.addHighlight(newItem);
        // and if it's a node, we highlight all adjacent edges, too
        this.graphComponent.graph.edgesAt(newItem).forEach(edge => {
          this.highlightEdge(edge, manager);
        });
        // we are checking the for edge
      } else if (IEdge.isInstance(newItem)) {
        manager.addHighlight(newItem);
        // if it's an edge - we highlight the adjacent nodes
        this.highlightEdge(newItem, manager);
      } else if (ILabel.isInstance(newItem) && newItem.owner instanceof IEdge) {
        this.highlightEdge(newItem.owner, manager);
      }
    }
  }

  /**
   * Method to highlight the corresponding nodes/edges when hovered/clicked.
   * @param item node/edge item which hovered/clicked.
   * @param type type of user action - hover/click.
   */
  public onHoveredItemChanged(item: IModelItem, type: string): void {
    // we use the highlight manager of the GraphComponent to highlight related items
    const manager = this.graphComponent.highlightIndicatorManager;
    manager.clearHighlights();
    if (this.selectedNodes.length) {
      this.selectedNodes.forEach(node => {
        if (this.enableSelection(node)) {
          if (node.tag) {
            node.tag.IsSelected = true;
          }
          this.applyInteractiveStyles(node);
        }
      });
      this.selectedNodes.forEach(node => {
        if (node.tag) {
          node.tag.IsSelected = false;
        }
        addHighlights(node, manager, this.graphComponent);
      });
    } else if (this.selectedItem && this.enableSelection(this.selectedItem)) {
      if (this.selectedItem.tag) {
        this.selectedItem.tag.IsSelected = true;
      }
      this.applyInteractiveStyles(this.selectedItem as IModelItem);
      addHighlights(this.selectedItem as IModelItem, manager, this.graphComponent);
      if (this.selectedItem.tag) {
        this.selectedItem.tag.IsSelected = false;
      }
    }
    if (type !== 'empty' && this.enableSelection(item)) {
      this.applyInteractiveStyles(item);
      addHighlights(item, manager, this.graphComponent);
    }
  }

  applyInteractiveStyles(node: IModelItem): void {
    // label shape of pill will be 2, hence we need to check if the selected item is shared resource or not
    this.isSharedResource = ILabel.isInstance(node) && node.style['shape'] > 1;
    initializeHighlightStyles('click', this.graphComponent, node as INode, 'sameSizeNodes', this.isSharedResource);
  }

  public updateGraph(graphInfo: YFileGraphInfo): void {
    graphInfo.nodes = graphInfo.nodes.filter(node => ! this.graphInfo.nodes.find(n => n.uid === node.uid));
    this.addIndicatorNodes(graphInfo);
    const unfilteredGraph = (this.graphComponent.graph as FilteredGraphWrapper).wrappedGraph;
    this.graphInfo.nodes = this.graphInfo.nodes.concat(graphInfo.nodes);
    graphInfo.links.forEach(link => {
      const sourceNode = unfilteredGraph.nodes.find(node => node.tag.uid === link.srcModule);
      const sourceNodeNew = graphInfo.nodes.find(node => node.uid === link.srcModule);
      const targetNode = unfilteredGraph.nodes.find(node => node.tag.uid === link.dstModule);
      const targetNodeNew = graphInfo.nodes.find(node => node.uid === link.dstModule);
      if ((sourceNode && targetNodeNew && ! sourceNodeNew && ! targetNode) || (sourceNodeNew && targetNode && ! sourceNode && ! targetNodeNew)) {
        link.properties['filtered'] = false as any;
      }
    });

    graphInfo.links = graphInfo.links.filter(link => !!! this.graphInfo.links.find(l => l.srcModule === link.srcModule && l.dstModule === link.dstModule));
    this.graphInfo.links = this.graphInfo.links.concat(graphInfo.links);
    this.graphBuilder.setData(this.nodesSource, this.graphInfo.nodes);
    this.graphBuilder.setData(this.edgesSource, this.graphInfo.links);
    this.defaultFiltered = true;
    this.graphBuilder.updateGraph();
  }

  /**
   * Method to store module ids in local storage and redirect to module Reporting page with the same module ids
   */
  redirectToModuleReporting(): void {
    const filteredGraph = (this.graphComponent.graph) as FilteredGraphWrapper;
    const idsFromDPGraph: string[] = [];
    filteredGraph.nodes.forEach((item) => {
      idsFromDPGraph.push(item.tag.id as string);
    });
    const randomValue = new Uint8Array(5);
    crypto.getRandomValues(randomValue);
    const uniqueIdsKeys = `call-chain-${Array.from(randomValue).map(byte => byte.toString(36)).join('')}`;
    const uniqueIdsValue = { createdTime: dateFormatter(new Date()), moduleIds: idsFromDPGraph };
    localStorage.setItem(uniqueIdsKeys, JSON.stringify(uniqueIdsValue));
    this.$window.open(`/#${RouteBuilder.buildProjectRoute(this.projectId, 'modules')}?preFilter=${uniqueIdsKeys}`);
  }

  /**
   * Method to close info banner when dismiss is clicked
   */
  closeMergeInfoBanner(): void {
    this.showMergeInfoBanner = false;
  }

  ngOnDestroy(): void {
    if (this.graphComponent) {
      this.graphComponent.cleanUp();
    }
    if (this.overviewComponent) {
      this.overviewComponent.cleanUp();
    }
    if(this.route.snapshot.url[0].path !== 'overview') {
      // @yjs:keep
      localStorage.removeItem('ModulesWithErrorsAndWarnings');
    }
  }

  private enableSelection(item: any): boolean {
    return ! item?.tag?.info?.DELETED && ! item?.tag?.info?.OUTDATED && ! item?.tag?.info?.STATUS;
  }

  private setRootModules(): void {
    this.rootModules = this.graphInfo.nodes.filter(n => n.isRoot);
    this.rootModuleIds = new Set<string>(this.rootModules.map(n => n.uid));
    this.graphInfo.nodes.forEach(n => {
      if (n.isRoot) {
        this.rootModules.push(n);
        this.rootModuleIds.add(n.uid);
      }
    });
  }

  // @yjs:keep
  private onTechnologyTypeSearch(): void {
    const requestQuery = {
      'query': `{
      functionalBlocks(projectId: ${this.projectId}) {
        aggregations {
          groupBy {
            REFERENCED_MODULE_TYPE
            REFERENCED_MODULE_TECHNOLOGY
          }
        }
      }
    }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response && response.data && response.data.functionalBlocks) {
        response.data.functionalBlocks.aggregations.forEach((aggregationItem: any) => {
          if (aggregationItem && aggregationItem.groupBy) {
            const mappedTechnology: string = this.labelMappingService.mapLabel(LabelType.TECHNOLOGY,
              aggregationItem.groupBy.REFERENCED_MODULE_TECHNOLOGY as string);
            const mappedType: string = this.labelMappingService.mapLabel(LabelType.TYPE,
              aggregationItem.groupBy.REFERENCED_MODULE_TYPE as string);
            const combinedLabel = `${mappedTechnology} ${mappedType}`;
            this.listOfTechnologyType.push({ label: combinedLabel,
              value: {
                technology: aggregationItem.groupBy.REFERENCED_MODULE_TECHNOLOGY,
                type: aggregationItem.groupBy.REFERENCED_MODULE_TYPE
              } });
          }
        });
      }
    });
  }

  private constructFilterDetails(): ReachabilityGraphFilter {
    return {
      taxonomyIds: this.selectedTaxonomyIds,
      functionalBlockIds: this.selectedFunctionalBlock,
      technologyTypes: this.selectedTechnologyAndType,
      relationshipTypes: this.selectedReferenceType
    };
  }

  private checkFilterApplied(filterApplied: ReachabilityGraphFilter): boolean {
    for (const key in filterApplied) {
      if (Array.isArray(filterApplied[key]) && filterApplied[key].length > 0) {
        return true;
      }
    }
    return false;
  }

  private buildFindReferenceObservable(node: INode, unfilteredGraph: IGraph, filteredGraph: FilteredGraphWrapper,
    direction: string): Observable<ModuleRelationshipPojo[]> {
    const edges = direction === GraphUtility.IN ? unfilteredGraph.outEdgesAt(node) : unfilteredGraph.inEdgesAt(node);
    if (edges.size === 0) {
      return;
    }
    const edge = edges.get(0);  /* There is only one in- or outgoing edge for each clue node */
    const sourceNode = direction === GraphUtility.IN ? edge.targetNode : edge.sourceNode;
    if ( ! filteredGraph.nodes.find(node => node.tag.id === sourceNode.tag.id)) {
      return;
    }
    return this.referenceControllerService.findAllforModuleMultipleTypes(this.projectId,
      sourceNode.tag.id as number, direction, REFERENCE_TYPES, true).pipe(catchError(error => of(error)));
  }

  private updateClue(clueNode: INode, unfilteredGraph: IGraph, filteredGraph: FilteredGraphWrapper, direction: string, referenceNum: number) {
    const edges = direction === GraphUtility.IN ? unfilteredGraph.outEdgesAt(clueNode) : unfilteredGraph.inEdgesAt(clueNode);
    if (edges.size === 0) {
      return;
    }
    const edge = edges.get(0);
    const node = direction === GraphUtility.IN ? edge.targetNode : edge.sourceNode;
    if ( ! filteredGraph.nodes.find(potentialNode => potentialNode.tag.uid === node.tag.uid)) {
      return;
    }
    const edgesInGraph = direction === GraphUtility.IN ? this.graphComponent.graph.inEdgesAt(node) : this.graphComponent.graph.outEdgesAt(node);
    const edgesWithoutCluesNum = edgesInGraph.filter(edge => ! edge.tag.properties?.isClue).size;
    const newClueNum = referenceNum - edgesWithoutCluesNum;
    clueNode.tag.name = newClueNum;

    this.graphComponent.graph.setLabelText(clueNode.labels.get(0), newClueNum.toString());
    this.graphComponent.graph
      .setNodeLayout(clueNode, GraphGlobalStyles.getResizedNodeLayout(clueNode.layout.topLeft, clueNode.labels.first().preferredSize, true, true));
  }

  private highlightEdge(edge: IEdge, manager: HighlightIndicatorManager<IModelItem>): void {
    manager.addHighlight(edge);
    manager.addHighlight(edge.sourceNode);
    manager.addHighlight(edge.targetNode);
    edge.labels.forEach(label => {
      manager.addHighlight(label);
    });
  }

  private async createGraph() {
    this.setLevelToNodes();
    const inputMode = this.getGraphInputMode();
    inputMode.addItemLeftClickedListener(void this.getSingleClickExpandListener());
    this.graphComponent.inputMode = inputMode;
    setGraphSelectionIndicators(this.graphComponent);
    this.enableGraphML(this.graphComponent);
    this.graphComponent.graph = this.initializeGraph();
    reEvaluateLazyLoadingNodes(this.graphComponent.graph, this.collapseAndExpandNodes);
    this.setModelManagerIfLargeGraph(this.graphComponent.graph);
    await this.runLayout(this.graphComponent, null, false, true);
    zoomToRootNodes(this.graphComponent, this.graphRootNodes);
    this.isGraphEmpty = false;
  }

  private getSingleClickExpandListener() {
    return (sender: any, args: ItemClickedEventArgs<INode>) => {
      this.displayModal = true;
      const node = args.item;
      if ( ! node || ! node.tag || ! node.tag.connectingNodeId) {
        this.displayModal = false;
        this.openContextMenu(node);
      }
    };
  }

  private setModelManagerIfLargeGraph(graph: IGraph) {
    if (GraphUtility.isLarge(graph) && ! (this.graphComponent.graphModelManager instanceof FastGraphModelManager)) {
     installGraphModelManager(this.graphComponent);
      this.disableLayoutOptions(graph);
    }
  }

  private disableLayoutOptions(graph: IGraph) {
    const nodesLength = graph.nodes.size;
    const linksLength = graph.edges.size;
    this.layoutItems = [];
    let hasLayoutChanged = false;
    Object.keys(LayoutType).forEach(key => {
      const layoutLimit = LAYOUT_LIMIT_MAP[key];
      const isLayoutDisabled = (layoutLimit !== 0) ? (layoutLimit <= nodesLength && layoutLimit <= linksLength) : false;
      let labelText = LayoutType[key];
      if (isLayoutDisabled) {
        labelText = labelText + ' ' + this.translateService.instant('dependencyGraph.disabledLargeGraph');
      }
      this.layoutItems.push({ label: labelText, value: key, disabled: isLayoutDisabled });
      if (key === this.layout && isLayoutDisabled) {
        hasLayoutChanged = true;
        this.graphFilter.patchValue({ selectedLayout: 'CIRCULAR_LAYOUT' });
      }
    });
    if (hasLayoutChanged) {
      this.cdRef.detectChanges();
    }
  }

  private initializeGraph() {
    const completeGraph = new DefaultGraph();
    (completeGraph.nodeDefaults.style as any).styleTag = { collapsed: true };
    completeGraph.nodeDefaults.shareStyleInstance = false;

    this.initializeGraphBuilder(completeGraph);
    GraphGlobalStyles.setDefaultStyles(completeGraph, NodeWidthSizes.L, NodeHeightSizes.S);
    this.graphBuilder.addEdgeCreatedListener(this.getAddEdgeCreatedListener(completeGraph));
    this.graphBuilder.addNodeCreatedListener(this.getAddNodeCreatedListener(completeGraph));
    this.graphBuilder.buildGraph();

    completeGraph.nodes.forEach(node => {
      // Initially, Level-1 is expanded and thus, 2 levels are visible.
      setNodeVisibility(node, node.tag.level < COLLAPSIBLE_LEVEL + 1, this.collapseAndExpandNodes);
    });

    /* Sets filter criteria for edge's visibility. */
    const edgePredicate = (edge: IEdge): boolean => ! edge.tag || ! edge.tag.filtered;
    return new FilteredGraphWrapper(
      completeGraph,
      node => this.collapseAndExpandNodes.getNodeVisibility(node),
      edgePredicate
    );
  }

  private initializeGraphBuilder(graph: IGraph) {
    this.graphBuilder = new GraphBuilder(graph);
    this.nodesSource = this.graphBuilder.createNodesSource(
      {
        data: this.graphInfo.nodes,
        id: 'uid',
        labels: ['name']
      });
      const labelCreator = this.nodesSource.nodeCreator.createLabelBinding((item) => item.peerCount);
      labelCreator.styleProvider = () => (new DefaultLabelStyle({
        insets: [4,4],
        backgroundFill: FUNCTIONAL_COUNT_FILL,
        backgroundStroke: FUNCTIONAL_COUNT_FILL,
        shape: LabelShape.PILL,
        textFill: FUNCTIONAL_COUNT_COLOR,
        textSize: FUNCTIONAL_COUNT_SIZE
      }));

      labelCreator.defaults.layoutParameter = new InteriorLabelModel({ insets: new Insets(10, 10, 10, 10) }).createParameter(InteriorLabelModelPosition.EAST);
      this.graphInfo.links.forEach((link) => {
        this.edgesSource = this.graphBuilder.createEdgesSource({
          data: [link],
          sourceId: 'srcModule',
          targetId: 'dstModule'
        });
        const sharedResourceId = link.properties?.RA_SHARED_RESOURCE_ID ?
        Object.values(link.properties.RA_SHARED_RESOURCE_ID).length : 0;
        const isSharedLowerBound: boolean = sharedResourceId > 0 && link.relationship !== GraphUtility.ARTIFICIAL.toUpperCase();
        const edgeLabel = this.edgesSource.edgeCreator.createLabelBinding(() => (isSharedLowerBound)
        ? sharedResourceId : getRelationshipLabel(link));
        if (isSharedLowerBound) {
          edgeLabel.styleProvider = () => (new DefaultLabelStyle({
            textFill: DEFAULT_EDGE_COLOR,
            textSize: LABEL_TEXT_SIZE,
            font: DEFAULT_LABEL_FONT,
            backgroundFill: BODY_BACKGROUND_COLOR,
            backgroundStroke: DEFAULT_EDGE_COLOR,
            insets: DEFAULT_EDGE_LABEL_INSET,
            horizontalTextAlignment: HorizontalTextAlignment.CENTER,
            verticalTextAlignment: VerticalTextAlignment.CENTER,
            wrapping: TextWrapping.WORD_ELLIPSIS,
            shape: 'rectangle'
          }));
        }
      });
  }

  private setLevelToNodes() {
    const connectedEdgeMap: Map<string, string[]> = this.getBidirectionalMap(this.graphInfo.links);
    const nodeIdToLevelMap = getNodeLevels(this.rootModuleIds, connectedEdgeMap);
    this.graphInfo.nodes.forEach(node => {
      node.level = nodeIdToLevelMap.get(node.uid);
    });
  }

  private getBidirectionalMap(links: ModuleRelationshipPojo[]): Map<string, string[]> {
    const connectedEdgeMap: Map<string, string[]> = new Map();
    links.forEach(ref => {
      const fromId = ref.srcModule;
      const toId = ref.dstModule;
      if ( ! connectedEdgeMap.has(fromId)) {
        connectedEdgeMap.set(fromId, []);
      }
      if ( ! connectedEdgeMap.has(toId)) {
        connectedEdgeMap.set(toId, []);
      }
      connectedEdgeMap.get(fromId).push(toId);
      connectedEdgeMap.get(toId).push(fromId);
    });
    return connectedEdgeMap;
  }

  private isFilterPristine(beforeChange: string[], afterChange: string[]): boolean {
    if (beforeChange === afterChange) {
      return true;
    }
    if ( ! beforeChange || ! afterChange) {
      return false;
    }
    if (beforeChange.length !== afterChange.length) {
      return false;
    }
    const sortedBeforeChange = beforeChange.slice(0).sort();
    const sortedAfterChange = afterChange.slice(0).sort();

    for (let index = 0; index < sortedBeforeChange.length; index++) {
      if (sortedBeforeChange[index] !== sortedAfterChange[index]) {
        return false;
      }
    }
    return true;
  }

  private getGraphInputMode(): GraphViewerInputMode {
    const inputMode = new GraphViewerInputMode();
    /* event listener for tool-tip */
    inputMode.toolTipItems = GraphItemTypes.NODE + GraphItemTypes.EDGE_LABEL;
    inputMode.clickableItems = GraphItemTypes.NODE_LABEL + GraphItemTypes.NODE + GraphItemTypes.EDGE + GraphItemTypes.EDGE_LABEL;
    inputMode.itemHoverInputMode.enabled = true;
    inputMode.itemHoverInputMode.hoverItems = GraphItemTypes.EDGE + GraphItemTypes.NODE + GraphItemTypes.EDGE_LABEL;
    inputMode.itemHoverInputMode.addHoveredItemChangedListener((sender, evt) => {
      this.onHoveredItemChanged(evt.item, 'hover');
    });
    inputMode.addItemClickedListener((sender, evt) => {
      this.relationshipLabel = '';
      // The below condition is to check for edge and edge label meta data
      if ((ILabel.isInstance(evt.item) || IEdge.isInstance(evt.item))) {
        this.inputForEdgeMetadata.length = 0;
        if (ILabel.isInstance(evt.item)) {
          this.inputForEdgeMetadata.push(...getTechnologyTypeForModule(evt.item.owner.tag.srcModule as string, this.graphInfo),
            ...getTechnologyTypeForModule(evt.item.owner.tag.dstModule as string, this.graphInfo));
          this.relationShipType = evt.item.owner.tag.relationship;
          this.fromId = evt.item.owner.tag.srcModule;
          this.toId = evt.item.owner.tag.dstModule;
          this.isArtificialEdge = evt.item.owner.tag?.relationship === GraphUtility.ARTIFICIAL.toUpperCase();
          this.relationshipLabel = evt.item.text;
          if (evt.item.owner.tag && evt.item.owner.tag.properties?.RA_SHARED_RESOURCE_ID && evt.item.owner.tag.properties.RA_SHARED_RESOURCE_ID.length > 0) {
            this.getSharedLowerBoundDetails(evt.item.owner.tag as ModuleRelationshipPojo);
          }
        } else {
          this.inputForEdgeMetadata.push(...getTechnologyTypeForModule(evt.item.tag.srcModule as string, this.graphInfo),
            ...getTechnologyTypeForModule(evt.item.tag.dstModule as string, this.graphInfo));
          this.relationShipType = evt.item.tag.relationship;
          this.fromId = evt.item.tag.srcModule;
          this.toId = evt.item.tag.dstModule;
          this.isArtificialEdge = evt.item.tag?.relationship === GraphUtility.ARTIFICIAL.toUpperCase();
          this.relationshipLabel = getRelationshipLabel(evt.item.tag);
        }
        if (this.isArtificialEdge) {
          if (ILabel.isInstance(evt.item)) {
            this.inputForArtificialEdgeMetadata = [{
              depth: [this.graphFilter.value.depthControl],
              startModule: [evt.item.owner.tag.srcModule],
              endModule: [evt.item.owner.tag.dstModule]
            }];
          } else {
            this.inputForArtificialEdgeMetadata = [{
              depth: [this.graphFilter.value.depthControl],
              startModule: [evt.item.tag.srcModule],
              endModule: [evt.item.tag.dstModule]
            }];
          }
        }
        this.inputForEdgeMetadata = this.inputForEdgeMetadata.filter(
          (module, index, self) => self.findIndex((m) => m.moduleId === module.moduleId) === index
        );
        this.moduleDetails = {};
        this.nodeEdgeClicked = 'edge';
        if (this.sidePanelIconState) {
          this.createSidePanel(GraphEdgeMetaDataComponent as Component, this.nodeEdgeClicked);
        }
      }
      if ( ! INode.isInstance(evt.item)) {
        this.selectedItem = evt.item;
        this.onHoveredItemChanged(evt.item, 'click');
      }
    });
    inputMode.addCanvasClickedListener(() => {
      this.selectedItem = null;
      this.selectedNodes = [];
      this.onHoveredItemChanged(null, 'empty');
    });
    inputMode.itemHoverInputMode.discardInvalidItems = false;
    inputMode.mouseHoverInputMode.duration = new TimeSpan(1, 0, 0, 0, 0);
    inputMode.addQueryItemToolTipListener(this.getNodeToolTipListener());

    inputMode.addMultiSelectionFinishedListener((src, args) => {
      // this implementation of the contextual toolbar only supports nodes, edges and labels
      this.selectedNodes = args.selection.filter(item => INode.isInstance(item) &&
        (!item.tag?.info?.DELETED && !item.tag?.info?.OUTDATED && !item.tag?.info?.STATUS)).toArray();
      if (this.selectedNodes && this.selectedNodes.length && !this.selectedNodes[0].tag.connectingNodeId) {
        this.selectedItem = this.selectedNodes[0];
        this.selectedNode = this.selectedNodes[0];
        const node: INode = this.getSelectedNode();
        this.moduleControllerService.findModuleById(this.projectId, node.tag.id as number).subscribe((data: ModulePojo) => {
          this.moduleDetails = data;
          this.isArtificialEdge = false;
          this.inputForEdgeMetadata.length = 0;
          this.nodeEdgeClicked = 'node';
          if (this.sidePanelIconState) {
            this.createSidePanel(SharedModuleDetailsComponent as Component, this.nodeEdgeClicked);
          }
        });
        this.onHoveredItemChanged(node, 'click');
      } else {
        this.selectedNode = null;
        this.selectedNodes = [];
      }
    });

    // if an item is deselected or deleted, we remove that element from the selectedItems
    this.graphComponent.selection.addItemSelectionChangedListener((src, args) => {
      if ( ! args.itemSelected) {
        // remove the context menu if seletion has changed
        this.selectedNode = null;
        this.selectedItem = null;
        this.selectedNodes = [];
      }
    });

    return inputMode;
  }

  // @yjs:keep
  private getSharedLowerBoundDetails(edge: ModuleRelationshipPojo): void {
    let sharedResourceId = edge.properties['RA_SHARED_RESOURCE_ID'] as string[];
    sharedResourceId = sharedResourceId.map((id: string) => JSON.stringify(id));
    const requestQuery = {
      'query': `{
        sharedModuleDetails: functionalBlocks(
          projectId: ${this.projectId}
          filterObject: {content_uid: {in: [${sharedResourceId}]}}
        ) {
          content {
            generatedFrom {
              module {
                id
                name
                technology
                type
                taxonomies {
                  type {
                    category {
                      name
                    }
                    name
                  }
                  name
                }
              }
            }
          }
        }
        writeReadBlockDetails: functionalBlocks(
          projectId: ${this.projectId}
          filterObject: {content_uid: {in: ["${edge.srcModule}", "${edge.dstModule}"]}}
        ) {
          content {
            children(
              filterObject: {content_type: {eq: RA_LOWER_BOUND}, content_children: {eq: {content_uid: {in:
                [${sharedResourceId}]}}}}
            ) {
              content {
                resolvedModuleParts {
                  module {
                    id
                    name
                  }
                }
                flags
              }
            }
            name
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response && response.data) {
       this.sharedModuleDetails = [];
        let modulePartDetails = {} as any;
        if (response.data.sharedModuleDetails && response.data.sharedModuleDetails?.content.length) {
          response.data.sharedModuleDetails.content.forEach((sharedModule: any) => {
            const taxonomies = sharedModule.generatedFrom.module.taxonomies.map((taxonomy: any) => [taxonomy.type.name + ':' + taxonomy.name]);
            modulePartDetails = {
              id: sharedModule.generatedFrom.module.id,
              name: sharedModule.generatedFrom.module.name,
              technology: sharedModule.generatedFrom.module.technology,
              type: sharedModule.generatedFrom.module.type,
              taxonomies
            };
            if (response.data.writeReadBlockDetails && response.data.writeReadBlockDetails?.content.length) {
              const accesTypes: any[] = [];
              response.data.writeReadBlockDetails.content.forEach((writeReadBlock: any) => {
                writeReadBlock.children.content.forEach((modulePart: any) => {
                  if(modulePartDetails.id === modulePart.resolvedModuleParts[0].module.id) {
                  accesTypes.push({
                    name: writeReadBlock.name,
                    type: modulePart.flags.RA_ACCESS_TYPE.join(', ')
                  });
                }
                });
                modulePartDetails['accessTypes'] = accesTypes.filter((accessType, index, self) => self.findIndex((a) => a.name === accessType.name) === index);
              });
            }
            this.sharedModuleDetails.push(modulePartDetails);
          });
        }
        this.createSidePanel(ReachabilityEdgeMetaDataComponent as Component, 'reachabilityEdge');
      }
    });
  }

  private openContextMenu(node: any) {
    if ( ! INode.isInstance(node)) {
      return;
    }
    // this implementation of the contextual toolbar only supports nodes, edges and labels
    this.graphComponent.selection.clear();
    this.graphComponent.selection.setSelected(node, true);
    this.selectedNode = node;
  }

  private getNodeToolTipListener() {
    return (sender: any, args: QueryItemToolTipEventArgs<IModelItem>) => {
      if ( ! ILabel.isInstance(args.item)) {
        const node: YNode = args.item.tag;
        this.updateTooltip(args, node);
      } else {
        const edge: ModuleRelationshipPojo = args.item.owner.tag;
        if (edge.properties?.RA_SHARED_RESOURCE_ID && Object.values(edge.properties.RA_SHARED_RESOURCE_ID).length > 0) {
          args.toolTip = this.translateService.instant('reachability.edgeTooltip', {count: Object.values(edge.properties.RA_SHARED_RESOURCE_ID).length});
        }
      }
      args.handled = true;
    };
  }

  private updateTooltip(args: QueryItemToolTipEventArgs<IModelItem>, node: YNode) {
    if (this.explorable && node.info && node.info?.isClue) {
      args.toolTip = 'There ' + (+node.name === 1 ? 'is ' : 'are ') + node.name + '  additional depenenc' + (+node.name === 1 ? 'y.' : 'ies.');
    } else if (this.moduleDetails && node?.uid === this.moduleDetails.uid) {
      args.toolTip = this.moduleDetails.description !== '' ? this.moduleDetails.description :
        this.translateService.instant('dependencyGraph.nodeTooltipNotProvided');
    } else if (node?.info && typeof node.info.STATUS === 'string' && node.info?.STATUS === 'INACTIVE') {
      args.toolTip = this.translateService.instant('reachability.inactiveTooltip');
    } else if (node?.info && node.info?.DELETED) {
      args.toolTip = this.translateService.instant('reachability.upperBoundRemovedTooltip');
    } else if (node?.info && node.info?.OUTDATED) {
      args.toolTip = this.translateService.instant('reachability.outdatedTooltip');
    } else if (node?.info && node.info?.deletedChildren) {
      args.toolTip = this.translateService.instant('reachability.mergedUpperBoundRemoved');
    } else if (node?.uid) {
      args.toolTip = node.description ? node.description : this.translateService.instant('dependencyGraph.nodeTooltipNotProvided');
    }
  }


  private getAddNodeCreatedListener(graph: IGraph): (sender: any, evt: GraphBuilderItemEventArgs<INode, any>) => void {
    return (sender, event) => {
      const node = event.item;
      const label = event.item.labels.first();
      const tag = event.item.tag;
      if (this.rootModuleIds.has(node.tag.uid as string)) {
        this.graphRootNodes.push(node);
        this.rootNodeEvent.next(node);
      }
      const strokeThickness = tag.isRoot ? SELECTED_NODE_STROKE_THICKNESS : DEFAULT_STROKE_THICKNESS;
      let nodeStyle = GraphGlobalStyles.getShapeNodeStyle(tag.style.color as NodeColor,
        DEFAULT_NODE_SHAPE, strokeThickness, false, !!node.tag.info?.isClue);
      let labelStyle = GraphGlobalStyles.getNodeIconLabelStyle(tag.style.imageUrl as string, graph.nodeDefaults.labels.style);
      if (tag && tag?.info) {
        const iconPath = this.getIconPath(tag)?.path;
        if (iconPath) {
          nodeStyle = this.addNodeStyle(node, graph, strokeThickness, iconPath).nodeStyle;
          labelStyle = this.addNodeStyle(node, graph, strokeThickness, iconPath).labelStyle;
          if (this.getIconPath(tag).twoIcons) {
            labelStyle = this.addNodeStyle(node, graph, strokeThickness, iconPath, true).labelStyle;
          }
          if (this.getIconPath(tag).dashedBorder) {
            nodeStyle = this.addNodeStyle(node, graph, strokeThickness, iconPath, false, true).nodeStyle;
          }
          if (this.getIconPath(tag).placement) {
            labelStyle = this.addNodeStyle(node, graph, strokeThickness, iconPath, false, false, true).labelStyle;
          }
        }
      }
      /* set node fill and stroke color */
      graph.setStyle(node, nodeStyle);
      /* set icon */
      graph.setStyle(label, labelStyle);
      /* resize node to fit label */
      graph.setNodeLayout(node, GraphGlobalStyles.getResizedNodeLayout(node.layout.topLeft, label.preferredSize, true, !!node.tag.info?.isClue));
    };
  }

  private getIconPath(tag: any): {path: string, twoIcons?: boolean, dashedBorder?: boolean, placement?: boolean} {
    switch (true) {
      case tag.info?.STATUS === 'INACTIVE':
        return {path: '/assets/reachability-img/EyeInvisible.png'};
      case tag.info?.DELETED:
        return {path: '/assets/reachability-img/delete.png', dashedBorder: true};
      case tag.info?.OUTDATED && tag.info?.deletedChildren:
        return {path: '/assets/icons/outdated_graph_icon.png', twoIcons: true};
      case tag.info?.OUTDATED:
        return {path: '/assets/icons/outdated_graph_icon.png'};
      case tag.info?.deletedChildren:
        return {path: '/assets/reachability-img/exclamation-circle.png', placement: true};
    }
  }

  private addNodeStyle(node: INode, graph: IGraph, strokeThickness: number, imageUrl: string,
    twoIcons: boolean = false, dashedBorder: boolean = false, placement: boolean = false): { nodeStyle: ShapeNodeStyle, labelStyle: IconLabelStyle } {
    let nodeStyle = GraphGlobalStyles.getShapeNodeStyle(NODE_COLORS.OUTDATED, DEFAULT_NODE_SHAPE,
      strokeThickness, true, !!node.tag.info?.isClue, dashedBorder);
    let labelStyle = GraphGlobalStyles.getNodeIconLabelStyle(imageUrl, graph.nodeDefaults.labels.style, new Insets(10, 0, 0, 0));
    if (node.tag.info?.DELETED && !this.networkUid) {
      const color: NodeColor = {...node.tag.style.color, strokeColor: NODE_COLORS.DELETED.strokeColor};
      nodeStyle = GraphGlobalStyles.getShapeNodeStyle(color, DEFAULT_NODE_SHAPE, strokeThickness, true, !!node.tag.info?.isClue, false);
    }
    if (placement) {
      labelStyle = GraphGlobalStyles.getNodeIconLabelStyle(imageUrl, graph.nodeDefaults.labels.style, new Insets(0, 0, 10, 0), ExteriorLabelModel.EAST);
    }
    if (twoIcons) {
      const innerLabelStyle = GraphGlobalStyles.getNodeIconLabelStyle('/assets/reachability-img/exclamation-circle.png',
        graph.nodeDefaults.labels.style, new Insets(0, 0, 10, 0), ExteriorLabelModel.EAST);
      labelStyle = GraphGlobalStyles.getNodeIconLabelStyle(imageUrl, innerLabelStyle, new Insets(10, 0, 0, 0));
    }
    return { nodeStyle, labelStyle };
  }


  private getAddEdgeCreatedListener(graph: IGraph): (sender: any, evt: GraphBuilderItemEventArgs<IEdge, any>) => void {
    return (sender, event) => {
      const edge = event.item;
      const tag = event.item.tag;
      if (this.explorable && tag.attributes && tag.attributes['filtered'] !== null) {
        tag.filtered = tag.attributes['filtered'];
      }
      if (this.explorable && tag.filtered === null) {
        tag.filtered = this.defaultFiltered;
      }

      if(!this.networkUid && (edge.sourceNode.tag?.info?.DELETED || edge.targetNode.tag?.info?.DELETED)){
        const deletedEdgeStyle = GraphGlobalStyles.getEdgeStyle(DashStyle.DASH, DELETED_EDGE_COLOR);
        graph.setStyle(edge, deletedEdgeStyle);
        const eventLabel = event.item.labels.first();
        const artificialEdgeLabel = GraphGlobalStyles.getEdgeLabelStyle(DEFAULT_EDGE_COLOR, DELETED_EDGE_COLOR);
        graph.setStyle(eventLabel, artificialEdgeLabel);
      }
      /* Change style for artificial edges */
      if (tag.relationship && tag.relationship.toUpperCase() === GraphUtility.ARTIFICIAL.toUpperCase()) {
        const artificialEdgeStyle = GraphGlobalStyles.getEdgeStyle(DashStyle.SOLID, ARTIFICIAL_EDGE_COLOR);
        graph.setStyle(edge, artificialEdgeStyle);
        const eventLabel = event.item.labels.first();
        const artificialEdgeLabel = GraphGlobalStyles.getEdgeLabelStyle(ARTIFICIAL_EDGE_COLOR, ARTIFICIAL_EDGE_COLOR);
        graph.setStyle(eventLabel, artificialEdgeLabel);
      } else if (tag?.properties && tag.properties?.TYPE && tag.properties.TYPE?.indexOf('RA_FROM_SCHEDULER_INFO') > -1) {
        const schedulerEdge = GraphGlobalStyles.getEdgeStyle(DashStyle.DASH, DEFAULT_EDGE_COLOR);
        graph.setStyle(edge, schedulerEdge);
        const eventLabel = event.item.labels.first();
        const labelStyle = GraphGlobalStyles.getNodeIconLabelStyle('/assets/mining-icons/scheduler.svg' as string, graph.nodeDefaults.labels.style);
        graph.setStyle(eventLabel, labelStyle);
      }
      if(tag && tag?.direction && tag.direction?.toUpperCase() === Innowake_Mining_Shared_Model_RelationshipDirection.Both.toUpperCase()) {
        const bidirectionalEdgeStyle = GraphGlobalStyles.getEdgeStyle(DashStyle.SOLID, DEFAULT_EDGE_COLOR, true);
        graph.setStyle(edge, bidirectionalEdgeStyle);
      }
    };
  }

  private createForm() {
    const selectedValues = getValuesFromUncheckedOptions(this.filterSelectedOptions);
    Array.from(Array(MAX_DEPTH).keys()).forEach(count => this.depthOptions.push({ label: (count + 1).toString(), value: count + 1 }));
    this.graphInfo.nodeTypes
      .forEach(nodeOption => this.filterOptions.push({ label: NodeType[nodeOption] || NodeType.GENERIC, value: nodeOption }));
    this.graphInfo.relationships
      .forEach(relationship => this.filterOptions.push({ label: relationship, value: relationship }));
    this.graphFilter = this.formBuilder.group({
      typeControl: new UntypedFormControl(this.filterOptions
        .filter(nodeType => !selectedValues.length || selectedValues.indexOf(`${nodeType.value}`) === -1)
        .map(nodeType => nodeType.value)),
      depthControl: new UntypedFormControl(this.selectedDepth),
      selectedLayout: new UntypedFormControl(this.layout)
    });
  }

  private enableGraphML(graphComponent: GraphComponent): GraphMLSupport {
    return new GraphMLSupport({
      graphComponent,
      storageLocation: StorageLocation.FILE_SYSTEM
    });
  }

  /**
   * Adds two visual clue nodes and two edges for every node in the specified graphInfo. The nodes are hidden by default due to their label being 0.
   * @param graphInfo the nodes and edges will be added to this
   */
  private addIndicatorNodes(graphInfo: YFileGraphInfo) {
    const newNodes: YNode[] = [];
    const newEdges: ModuleRelationshipPojo[] = [];
    graphInfo.nodes.forEach(node => {
      addNodeAndEdge(newNodes, newEdges, node, GraphUtility.OUT);
      addNodeAndEdge(newNodes, newEdges, node, GraphUtility.IN);
    });
    graphInfo.graphNodes.push(...newNodes);
    graphInfo.links.push(...newEdges);
  }

  /**
   * Method for creating Side Panel Component
   */
  private createSidePanel(ComponentRef: Component, type: string): void {
    const componentFactory = this.CFR.resolveComponentFactory(ComponentRef as Type<unknown>);
    this.componentInstances.push(this.sidePanel.createComponent(componentFactory));
    const componentInstanceLength = this.componentInstances.length -1;
    this.componentInstances[componentInstanceLength].instance.projectId = this.projectId;
    switch (type) {
      case 'node':
        this.createSharedSidePanel(componentInstanceLength);
        this.closeSidePanel(componentInstanceLength, 'hideShowTab');
        break;
      case 'edge':
        this.createEdgeSidePanel(componentInstanceLength);
        this.closeSidePanel(componentInstanceLength, 'closeEdgeSidePanel');
        break;
      case 'reachabilityEdge':
        this.createReachabilitySidePanel(componentInstanceLength);
        this.closeSidePanel(componentInstanceLength, 'closeReachabilitySidePanel');
        break;
    }
  }

  private createSharedSidePanel(componentInstanceLength: number): void {
    this.componentInstances[componentInstanceLength].instance.moduleDetails = this.moduleDetails;
    this.componentInstances[componentInstanceLength].instance.scrollPosition = this.scrollPosition;
    this.componentInstances[componentInstanceLength].instance.selectedItem = this.selectedItem;
    this.componentInstances[componentInstanceLength].instance.getDataForSharedModule();
  }

  private createEdgeSidePanel(componentInstanceLength: number): void {
    this.componentInstances[componentInstanceLength].instance.inputForArtificialEdgeMetadata = this.inputForArtificialEdgeMetadata;
    this.componentInstances[componentInstanceLength].instance.projectId = this.projectId;
    this.componentInstances[componentInstanceLength].instance.isArtificialEdge = this.isArtificialEdge;
    this.componentInstances[componentInstanceLength].instance.relationShipType = this.relationShipType;
    this.componentInstances[componentInstanceLength].instance.fromId = this.fromId;
    this.componentInstances[componentInstanceLength].instance.toId = this.toId;
    this.componentInstances[componentInstanceLength].instance.edgeFileAccessType = this.relationshipLabel;
    this.componentInstances[componentInstanceLength].instance.getDataForEdgeModule();
  }

  private createReachabilitySidePanel(componentInstanceLength: number): void {
    this.componentInstances[componentInstanceLength].instance.sharedModuleDetails = this.sharedModuleDetails;
  };


  private closeSidePanel(noOfInstances: number, type: string): void {
    this.componentInstances[noOfInstances].instance[type]?.subscribe(($event: boolean) => {
      this.componentInstances.forEach((componentInstance) => {
        componentInstance?.destroy();
      });
      this.sidePanelIconState = $event;
    });
  }
}
