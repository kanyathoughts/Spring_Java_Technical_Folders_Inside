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
import { GraphUtility } from '../utils/dependency-graph-utility';
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
  DashStyle,
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
  PARTIAL_LAYOUT
} from '../../utils/graph-layouts';
import {
  NodeType,
  NODE_CONFIG,
  NodeWidthSizes,
  NodeHeightSizes
} from '../../utils/node-configurations';
import { FastGraphModelManager } from '@app/modules/graph/utils/yfiles-util/fast-graph-model-manager';
import CollapseAndExpandNodes from '@app/modules/graph/utils/yfiles-util/collapse-expand';
import { TranslateService } from '@ngx-translate/core';
import { ActivatedRoute } from '@angular/router';
import { YFileGraphInfo, FilterParameters, YNode } from '@app/modules/graph/models/yfile-graph-info.model';
import {
  GraphGlobalStyles, DEFAULT_NODE_SHAPE, SELECTED_NODE_STROKE_THICKNESS,
  DEFAULT_STROKE_THICKNESS,
  ARTIFICIAL_EDGE_COLOR,
} from '../../utils/graph-global-styles';
import { WindowToken } from '@app/core/utils/window';
import { NodeColor } from '../../utils/node-colors';
import { forkJoin, Observable, of, Subject } from 'rxjs';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { MiningSelectOption } from '@app/shared/interfaces/mining-select-option.interface';
import { NodeVisibility } from './context-menu-toolbar/context-menu-options';
import { ScrollDirection, ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { CodeViewerButtonComponent } from '@app/shared/components/code-viewer-button/code-viewer-button.component';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { catchError } from 'rxjs/operators';
import { addHighlights, initializeHighlightStyles, setGraphSelectionIndicators } from '../../utils/graph-highlight.util';
import { DepedendencyGraphEdgeMetaData, InputForArtificialEdge } from './graph-edge-meta-data/graph-edge-meta-data.interface';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { NzModalService } from 'ng-zorro-antd/modal';
import { SharedModuleDetailsComponent } from '@app/shared/components/shared-module-details/shared-module-details.component';
import { GraphEdgeMetaDataComponent } from './graph-edge-meta-data/graph-edge-meta-data.component';
import { DependencyGraph, ModuleControllerService, ModulePojo, ModuleRelationshipPojo, ReferenceControllerService }
  from '@innowake/mining-api-angular-client';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import {
  addNodeAndEdge, getGraphLayoutExecutor, getNodeLevels, getRelationshipLabel,
  getTechnologyTypeForModule, getValuesFromUncheckedOptions, installGraphModelManager,
  reEvaluateLazyLoadingNodes, setGraphOverviewPanel, setNodeVisibility, zoomToRootNodes
} from '../../utils/dependency-reachability-graph.util';
import { LabelType } from '@app/core/utils/mapping.utils';

const logger = new Logger('DependencyGraphComponent');
const MAX_DEPTH = 10;
const COLLAPSIBLE_LEVEL = 1;
const REFERENCE_TYPES = ['Calls', 'Includes', 'References', 'Accesses'];
declare const java__callback__openMiningUiPage: (path: string) => Promise<string>;

@Component({
  selector: 'dependency-graph',
  templateUrl: './dependency-graph.component.html'
})
export class DependencyGraphComponent implements AfterViewInit, OnInit, OnChanges, OnDestroy {
  @Input() public eclipseView = false;
  @Input() public graphInfo: YFileGraphInfo;
  @Input() public isDPGraphFromCallChain = false;
  @Input() selectedDepth: number;
  @Input() layout: string;
  @Input() filterSelectedOptions: FilterParameters;
  @Input() showDepthControl: boolean;
  @Input() showGraphFilters: boolean;
  @Input() showBreadCrumb: boolean;
  @Input() showExports: boolean;
  @Input() projectId: number;
  @Input() dependencyGraph: DependencyGraph;
  @Input() networkUid = '';
  @Output() depthChange: EventEmitter<any> = new EventEmitter();
  @Output() layoutChange: EventEmitter<any> = new EventEmitter();
  @Output() filterChange: EventEmitter<any> = new EventEmitter();
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

  constructor(
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
      const currentLayout = LAYOUT_MAP[this.graphFilter.value.selectedLayout];
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

    if ( ! this.isDPGraphFromCallChain) {
      this.route.data.subscribe((data: { module: ModulePojo }) => {
        this.currentModule = {
          name: data.module.name, id: data.module.id, projectId: data.module.projectId,
          parent: data.module.parent
        };
        this.projectId = data.module.projectId;
        this.moduleName = data.module.name;
        this.moduleId = data.module.id;
        this.moduleDetailRoute = RouteBuilder.buildModuleRoute(data.module.projectId, data.module.linkHash, 'details/overview');
      });
    }
    this.setRootModules();
    this.createForm();
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
    return this.selectedNode;
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
   * Method to change the layout of the graph based on the value selected in the dropdown.
   */
  async changeLayout(): Promise<void> {
    this.displayModal = true;
    this.layoutChange.emit(this.graphFilter.value.selectedLayout);
    await this.runLayout(this.graphComponent, null, false, false);
    this.graphComponent.fitGraphBounds();
    this.displayModal = false;
  }

  /**
   * Method to change the depth of the graph based on the value selected in the dropdown.
   */
  changeDepth(): void {
    this.depthChange.emit(this.graphFilter.value.depthControl);
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
    if (this.selectedItem) {
      initializeHighlightStyles('click', this.graphComponent, this.selectedItem as INode, 'sameSizeNodes');
      addHighlights(this.selectedItem as IModelItem, manager, this.graphComponent,);
    }
    if (type !== 'empty') {
      initializeHighlightStyles('hover', this.graphComponent, item as INode, 'sameSizeNodes');
      addHighlights(item, manager, this.graphComponent);

    }
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

  ngOnDestroy(): void {
    if (this.graphComponent) {
      this.graphComponent.cleanUp();
    }
    if (this.overviewComponent) {
      this.overviewComponent.cleanUp();
    }
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
    this.graphInfo.links.forEach((link) => {
      this.edgesSource = this.graphBuilder.createEdgesSource({
        data: [link],
        sourceId: 'srcModule',
        targetId: 'dstModule',
        labels: [() =>  getRelationshipLabel(link)]
      });
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
    inputMode.toolTipItems = GraphItemTypes.NODE;
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
      this.onHoveredItemChanged(null, 'empty');
    });
    inputMode.itemHoverInputMode.discardInvalidItems = false;
    inputMode.mouseHoverInputMode.duration = new TimeSpan(1, 0, 0, 0, 0);
    inputMode.addQueryItemToolTipListener(this.getNodeToolTipListener());

    inputMode.addMultiSelectionFinishedListener((src, args) => {
      // this implementation of the contextual toolbar only supports nodes, edges and labels
      const selection = args.selection.filter(item => INode.isInstance(item)).toArray();
      if (selection && selection.length && !selection[0].tag.connectingNodeId) {
        this.selectedItem = selection[0];
        this.selectedNode = selection[0];
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
      }
    });

    // if an item is deselected or deleted, we remove that element from the selectedItems
    this.graphComponent.selection.addItemSelectionChangedListener((src, args) => {
      if ( ! args.itemSelected) {
        // remove the context menu if seletion has changed
        this.selectedNode = null;
        this.selectedItem = null;
      }
    });

    return inputMode;
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
      const node = args.item.tag;
      if (this.explorable && node.info && node.info?.isClue) {
        args.toolTip = 'There ' + (+node.name === 1 ? 'is ' : 'are ') + node.name + '  additional depenenc' + (+node.name === 1 ? 'y.' : 'ies.');
      } else if (this.moduleDetails && node?.uid === this.moduleDetails.uid) {
        args.toolTip = this.moduleDetails.description !== '' ? this.moduleDetails.description :
          this.translateService.instant('dependencyGraph.nodeTooltipNotProvided');
      } else if (node && node.uid) {
        args.toolTip = node.description ? node.description : this.translateService.instant('dependencyGraph.nodeTooltipNotProvided');
      }
      args.handled = true;
    };
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
      const nodeStyle = GraphGlobalStyles.getShapeNodeStyle(tag.style.color as NodeColor, DEFAULT_NODE_SHAPE, strokeThickness, false, !!node.tag.info?.isClue);
      const labelStyle = GraphGlobalStyles.getNodeIconLabelStyle(tag.style.imageUrl as string, graph.nodeDefaults.labels.style);

      /* set node fill and stroke color */
      graph.setStyle(node, nodeStyle);
      /* set icon */
      graph.setStyle(label, labelStyle);
      /* resize node to fit label */
      graph.setNodeLayout(node, GraphGlobalStyles.getResizedNodeLayout(node.layout.topLeft, label.preferredSize, true, !!node.tag.info?.isClue));
    };
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
      /* Change style for artificial edges */
      if (tag.relationship && tag.relationship.toUpperCase() === GraphUtility.ARTIFICIAL.toUpperCase()) {
        const artificialEdgeStyle = GraphGlobalStyles.getEdgeStyle(DashStyle.SOLID, ARTIFICIAL_EDGE_COLOR);
        graph.setStyle(edge, artificialEdgeStyle);
        const eventLabel = event.item.labels.first();
        const artificialEdgeLabel = GraphGlobalStyles.getEdgeLabelStyle(ARTIFICIAL_EDGE_COLOR, ARTIFICIAL_EDGE_COLOR);
        graph.setStyle(eventLabel, artificialEdgeLabel);
      }
    };
  }

  private createForm() {
    const selectedValues = getValuesFromUncheckedOptions(this.filterSelectedOptions);
    Array.from(Array(MAX_DEPTH).keys()).forEach(count => this.depthOptions.push({ label: (count + 1).toString(), value: count + 1 }));

    this.graphInfo.nodeTypes
      .forEach(nodeOption => {
        const typeSplit = nodeOption.split(' ');
        const label = this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, typeSplit[0]) + ' ' + this.labelMappingService.mapLabel(
          LabelType.TYPE, typeSplit.slice(1).join('_'));
        this.filterOptions.push({ label: label.toUpperCase() , value: nodeOption });
      });
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
    }
  }

  private createSharedSidePanel(componentInstanceLength: number): void {
    const instance = this.componentInstances[componentInstanceLength].instance;
    instance.moduleDetails = this.moduleDetails;
    instance.isDPGraphFromCallChain = this.isDPGraphFromCallChain;
    instance.scrollPosition = this.scrollPosition;
    instance.selectedItem = this.selectedItem;
    instance.getDataForSharedModule();
  }

  private createEdgeSidePanel(componentInstanceLength: number): void {
    const instance = this.componentInstances[componentInstanceLength].instance;
    instance.inputForEdgeMetadata = this.inputForEdgeMetadata;
    instance.inputForArtificialEdgeMetadata = this.inputForArtificialEdgeMetadata;
    instance.isDPGraphFromCallChain = this.isDPGraphFromCallChain;
    instance.projectId = this.projectId;
    instance.isArtificialEdge = this.isArtificialEdge;
    instance.relationShipType = this.relationShipType;
    instance.fromId = this.fromId;
    instance.toId = this.toId;
    instance.edgeFileAccessType = this.relationshipLabel;
    instance.getDataForEdgeModule();
  }


  private closeSidePanel(noOfInstances: number, type: string): void {
    this.componentInstances[noOfInstances].instance[type]?.subscribe(($event: boolean) => {
      this.componentInstances.forEach((componentInstance) => {
        componentInstance?.destroy();
      });
      this.sidePanelIconState = $event;
    });
  }
}
