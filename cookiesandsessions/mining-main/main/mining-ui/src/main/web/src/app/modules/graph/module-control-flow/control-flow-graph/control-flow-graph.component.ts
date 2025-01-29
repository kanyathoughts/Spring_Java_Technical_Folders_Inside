import {
  Component,
  AfterViewInit,
  ViewChild,
  ElementRef,
  Input,
  Output,
  EventEmitter,
  OnDestroy
} from '@angular/core';
import {
  GraphComponent,
  GraphViewerInputMode,
  ItemClickedEventArgs,
  IModelItem,
  IGraph,
  GraphMLSupport,
  StorageLocation,
  GraphBuilder,
  GraphBuilderItemEventArgs,
  INode,
  HierarchicLayout,
  GraphOverviewComponent,
  RenderModes,
  LayoutExecutor,
  PortAdjustmentPolicy,
  Point,
  FoldingManager,
  CollapsibleNodeStyleDecorator,
  IFoldingView,
  Insets,
  GraphItemTypes,
  TimeSpan,
  MergingFoldingEdgeConverter,
  WebGLTaperedEdgeStyle,
  Color,
  SimplexNodePlacer,
  ShapeNodeStyle,
  Stroke,
  DashStyle
} from 'yfiles';
import { ControlFlowGraphInfo } from '../models/control-flow-graph-info';
import { ControlFlowNodeDetails } from '../models/control-flow-node-details';
import {
  CONTROL_FLOW_NODE_SHAPE_MAPPING, BRANCH_NODE_STROKE_THICKENESS
} from '../utils/control-flow-utility';
import { ControlFlowPortOptimizer } from '@app/modules/graph/utils/yfiles-util/port-optimizer';
import { OverviewPanelVisualCreator } from '@app/modules/graph/utils/yfiles-util/overview-panel-visual-creator';
import { NODE_COLORS } from '../../utils/node-colors';
import { NodeWidthSizes, NodeHeightSizes } from '../../utils/node-configurations';
import { LAYOUT_MAP } from '../../utils/graph-layouts';
import { GraphGlobalStyles, DEFAULT_STROKE_THICKNESS, GROUP_INSET, GROUP_LABEL_INSET, GROUP_LABEL_OFFSET } from '../../utils/graph-global-styles';
import { NODE_LABEL_CONFIG } from '../../utils/node-label-configuration';
import HierarchicGrouping from '../../utils/yfiles-util/hierarchic-grouping';
import { graphSelectionIndicators } from '../../utils/graph.util';
import { CfgMetaData } from '../../models/cfg-meta-data.model';
import { addHighlights, initializeHighlightStyles } from '../../utils/graph-highlight.util';
import { Subject } from 'rxjs';
import { FastGraphModelManager, OptimizationMode } from '../../utils/yfiles-util/fast-graph-model-manager';
import { GraphUtility } from '../../dependency/utils/dependency-graph-utility';
import { ControlFlowNode } from '@innowake/mining-api-angular-client';

const NODE_ZOOM_VALUE = 1;
const ZOOM_CENTER_Y_COORDINATE = 280;

@Component({
  selector: 'control-flow-graph',
  templateUrl: './control-flow-graph.component.html'
})
export class ControlFlowGraphComponent implements AfterViewInit, OnDestroy {
  @Input() public eclipseView = false;
  @Input() public graphInfo: ControlFlowGraphInfo;
  @Input() projectId: number;
  @Input() cfgMetaData: any;
  @Input() annotationId: string;
  @Output() nodeClick: EventEmitter<any> = new EventEmitter();
  @Output() recreateGraph: EventEmitter<any> = new EventEmitter();
  @ViewChild('graphComponentRef') graphComponentRef!: ElementRef;
  graphComponent!: GraphComponent;
  overviewComponent!: GraphOverviewComponent;
  rootNode: INode;
  selectedItem: any;
  selectedNode: any;
  hierarchicGrouping: HierarchicGrouping;
  isCFGMetaDataVisible = false;
  panelElements: any = [];
  sidePanelTitle: string;
  foldingView: IFoldingView;
  nodeSelectionEvent: Subject<{controlFlowNode: ControlFlowNodeDetails, graphNodes: ControlFlowNodeDetails[]}> = new Subject<{
    controlFlowNode: ControlFlowNodeDetails, graphNodes: ControlFlowNodeDetails[]}>();
  desiredNode: INode;
  graphBuilder: GraphBuilder;

  ngAfterViewInit(): void {
    this.graphComponent = new GraphComponent(this.graphComponentRef.nativeElement as string);
    this.setInputMode();
    const foldingManager = new FoldingManager();
    const foldingView = foldingManager.createFoldingView();
    foldingManager.folderNodeConverter = GraphGlobalStyles.getFolderNodeConverter(false);
    foldingManager.foldingEdgeConverter = new MergingFoldingEdgeConverter();
    this.graphComponent.graph = foldingView.graph;
    this.hierarchicGrouping = new HierarchicGrouping(this.graphComponent);
    const graph = this.graphComponent.graph;
    this.setGraphOverviewPanel(this.graphComponent);
    GraphGlobalStyles.setDefaultStyles(graph, NodeWidthSizes.XL, NodeHeightSizes.L);
    this.enableGraphML(this.graphComponent);
    this.createGraph(this.graphComponent, this.graphInfo);
  }

  ngOnDestroy(): void {
    if (this.graphComponent) {
      this.graphComponent.cleanUp();
    }
    if (this.overviewComponent) {
      this.overviewComponent.cleanUp();
    }
  }

  getNodeSingleClickListener() {
    return (sender: string, evt: ItemClickedEventArgs<IModelItem>): void => {
      if (this.eclipseView && evt.item.tag && (evt.item.tag instanceof ControlFlowNodeDetails)) {
        this.nodeClick.emit(evt.item.tag.offset);
      }
      if (evt.item.tag instanceof ControlFlowNodeDetails) {
        this.sidePanelTitle = evt.item.tag.label;
        this.panelElements.length = 0;
        if (this.cfgMetaData && this.cfgMetaData[evt.item.tag.recordId]) {
          this.panelElements.push(this.buildMetaData(this.cfgMetaData, evt.item.tag.recordId));
        }
      }
    };
  }

  /**
   * Method to get the nodes selected in the graph.
   * @returns the selected node
   */
   getSelectedNode(): INode {
    return this.selectedNode;
  }

  /**
   * Collapses all group nodes and relayouts the graph
   */
  collapseAllGroups(): void {
    const graph = this.graphComponent.graph;
    this.collapseAllGroupsInArray(graph.nodes.toArray(), graph, graph.foldingView);
    this.runLayout(true);
  }

  /**
   * Expands all group nodes and relayouts the graph
   */
  expandAllGroups(): void {
    const graph = this.graphComponent.graph;
    this.expandAllGroupsInArray(graph.nodes.toArray(), graph, graph.foldingView);
    this.runLayout(true);
  }

  showCfgMetaDataDrawer(event: boolean): void {
    this.isCFGMetaDataVisible = event;
  }

  onAnnotationChange(): void {
    this.recreateGraph.emit();
  }

  private buildMetaData(cfgMetaData: any, recordId: string): CfgMetaData {
    const characteristics: CfgMetaData = {
      inputFiles: {
        title: 'Input Files',
        modules: []
      },
      outputFiles: {
        title: 'Output Files',
        modules: []
      }
    };
    cfgMetaData[recordId]?.forEach((io: any) => {
      io['Input Files']?.forEach((module: any) => {
        characteristics.inputFiles['modules'].push({
          name: module.name,
          id: module.id
        });
      });
      io['Output Files']?.forEach((module: any) => {
        characteristics.outputFiles['modules'].push({
          name: module.name,
          id: module.id
        });
      });
    });
    return characteristics;
  }

  /**
   * Method to highlight the corresponding nodes/edges when hovered/clicked.
   * @param item node/edge item which hovered/clicked.
   * @param type type of user action - hover/click.
   */
  private onHoveredItemChanged(item: IModelItem, type: string): void {
    // we use the highlight manager of the GraphComponent to highlight related items
    const manager = this.graphComponent.highlightIndicatorManager;
    manager.clearHighlights();
    if (this.selectedItem && this.selectedItem.tag?.nodeShapeType === 'group' && this.foldingView.isExpanded(this.selectedItem as INode)) {
      this.selectedItem = null;
    }
    initializeHighlightStyles('click', this.graphComponent, this.selectedItem as INode, 'differentSizeNodes');
    addHighlights(this.selectedItem as IModelItem, manager, this.graphComponent);
    if (type !== 'empty') {
      let hoverNode = item;
      if (hoverNode && hoverNode.tag?.nodeShapeType === 'group' && this.foldingView.isExpanded(hoverNode as INode)) {
        hoverNode = null;
      }
      initializeHighlightStyles('hover', this.graphComponent, hoverNode as INode, 'differentSizeNodes');
      addHighlights(hoverNode, manager, this.graphComponent);
    }
  }

  private setInputMode() {
    const inputMode = new GraphViewerInputMode();
    inputMode.toolTipItems = GraphItemTypes.NODE;
    inputMode.itemHoverInputMode.enabled = true;
    inputMode.itemHoverInputMode.hoverItems = GraphItemTypes.EDGE + GraphItemTypes.NODE + GraphItemTypes.EDGE_LABEL;
    inputMode.itemHoverInputMode.addHoveredItemChangedListener((sender, evt) => {
      this.onHoveredItemChanged(evt.item, 'hover');
    });
    inputMode.addItemClickedListener((sender, evt) => {
      if (evt.item) {
        if (evt.item.tag?.nodeShapeType === 'group' && this.foldingView.isExpanded(evt.item as INode)) {
          this.isCFGMetaDataVisible = false;
        } else if (evt.item.tag instanceof ControlFlowNodeDetails) {
          this.isCFGMetaDataVisible = true;
          const metadata: ControlFlowNodeDetails[] = this.graphInfo.graphNodes.concat(this.graphInfo.graphGroups);
          this.nodeSelectionEvent.next({ controlFlowNode: evt.item.tag, graphNodes: metadata});
        } else {
          this.isCFGMetaDataVisible = false;
        }
      }
    });
    inputMode.addCanvasClickedListener(() => {
      this.selectedItem = null;
      this.onHoveredItemChanged(null, 'empty');
    });
    inputMode.itemHoverInputMode.discardInvalidItems = false;
    inputMode.mouseHoverInputMode.duration = new TimeSpan(1, 0, 0, 0, 0);

    inputMode.addMultiSelectionFinishedListener((src, args) => {
      // this implementation of the contextual toolbar only supports nodes, edges and labels
      const selection = args.selection.filter(item => INode.isInstance(item)).toArray();
      if (selection && selection.length && !selection[0].tag.connectingNodeId) {
        // in case of group node selection do not overide the previous selection
        if( ! this.graphComponent.graph.isGroupNode(selection[0] as INode)){
          this.selectedItem = selection[0];
          this.selectedNode = selection[0];
        }
        const node: INode = this.getSelectedNode();
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
      }
    });
    /* event listener for single click */
    // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
    inputMode.addItemClickedListener(this.getNodeSingleClickListener() as any);
    this.graphComponent.inputMode = inputMode;
  }

  private setGraphOverviewPanel(graphComponent: GraphComponent) {
    this.overviewComponent = new GraphOverviewComponent('overviewComponentRef', graphComponent);
    this.overviewComponent.renderMode = RenderModes.CANVAS;
    this.overviewComponent.graphVisualCreator = new OverviewPanelVisualCreator(graphComponent.graph);
  }

  private enableGraphML(graphComponent: GraphComponent): GraphMLSupport {
    return new GraphMLSupport({
      graphComponent,
      storageLocation: StorageLocation.FILE_SYSTEM
    });
  }

  private createGraph(graphComponent: GraphComponent, graphInfo: ControlFlowGraphInfo) {
    if (this.graphInfo === null) {
      return;
    }
    this.foldingView = graphComponent.graph.foldingView;

    this.foldingView.addGroupExpandedListener(() => {
      this.onHoveredItemChanged(null, 'empty');
    });

    this.foldingView.addGroupCollapsedListener(() => {
      this.selectedItem = null;
      this.onHoveredItemChanged(null, 'empty');
    });

    const builder = new GraphBuilder({
      graph: this.foldingView.manager.masterGraph,
      nodes: [
        {
          data: graphInfo.graphNodes,
          id: 'recordId',
          labels: ['description'],
          parentId: 'group'
        }
      ],
      edges: [
        {
          data: graphInfo.graphEdges,
          sourceId: 'fromId',
          targetId: 'toId',
          labels: ['label'],
        }
      ]
    });
    builder.createGroupNodesSource({
      data: graphInfo.graphGroups,
      id: 'recordId',
      labels: ['description'],
      parentId: 'group'
    });
    builder.addNodeCreatedListener(this.getNodeCreatedListener(this.foldingView.manager.masterGraph));
    this.setModelManagerIfLargeGraph(this.graphComponent.graph);
    builder.buildGraph();
    this.graphBuilder = builder;

    const nodesArray: INode[] = graphComponent.graph.nodes.toArray();
    this.runLayout(); /* We need to run layout before collapsing so nodes in groups are layouted correctly */
    this.collapseAllGroupsInArray(nodesArray, graphComponent.graph, this.foldingView);
    this.runLayout();

    const graph = graphComponent.graph;
    graph.edges.forEach(edge => {
      if (graph.getChildren(edge.targetNode).toArray().length && ! graph.getChildren(edge.sourceNode).toArray().length && edge.tag.label) {
        graph.addLabel(edge, edge.tag.label as string);
      }
    });
    this.zoomToRootNode(graphComponent);
    this.setGraphSelectionIndicators(graphComponent);
  }

  private setModelManagerIfLargeGraph(graph: IGraph) {
    if (GraphUtility.isLarge(graph) && ! (this.graphComponent.graphModelManager instanceof FastGraphModelManager)) {
      this.installGraphModelManager(this.graphComponent);
    }
  }

  private installGraphModelManager(graphComponent: GraphComponent) {
    const graphModelManager = new FastGraphModelManager(graphComponent, graphComponent.contentGroup);
    graphModelManager.overviewEdgeStyle = new WebGLTaperedEdgeStyle({
      thickness: 10,
      color: new Color(100, 100, 100)
    });
    graphModelManager.graphOptimizationMode = OptimizationMode.LEVEL_OF_DETAIL;
    graphComponent.graphModelManager = graphModelManager;
  }

  /**
   * Recursively collapses all group nodes.
   * @param nodes - nodes that will be collapsed
   * @param graph - graph containing the nodes
   * @param foldingView - used to collapse the groups
   */
  private collapseAllGroupsInArray(nodes: INode[], graph: IGraph, foldingView: IFoldingView) {
    nodes.forEach(node => {
      if (graph.contains(node) && graph.isGroupNode(node)) {
        if (this.annotationId !== null) {
          if ( ! this.isNodeHasAnnotationId(node,graph, this.annotationId, foldingView)) {
            const child = graph.getChildren(node).toArray();
            this.collapseAllGroupsInArray(child, graph, foldingView);
            if (child.length < 2 && node.tag?.entity !== ControlFlowNode.EntityEnum.ANNOTATION) {
              graph.remove(node);
            } else {
              foldingView.collapse(node);
            }
          }
        } else {
          this.collapseAllGroupsInArray(graph.getChildren(node).toArray(), graph, foldingView);
          foldingView.collapse(node);
        }
      }
    });
  }

  // Helper function to check if a node or its descendants has the specified recordId
  private isNodeHasAnnotationId(node: INode, graph: IGraph, recordId: string, foldingView: IFoldingView): boolean {
    if (node.tag && node.tag.recordId === recordId) {
      this.desiredNode = node;
      foldingView.collapse(node);
      return true;
    }

    const children = graph.getChildren(node).toArray();
    for (const child of children) {
      if (this.isNodeHasAnnotationId(child, graph, recordId, foldingView)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Recursively expands all group nodes.
   * @param nodes - nodes that will be expanded
   * @param graph - graph containing the nodes
   * @param foldingView - used to expand the groups
   */
  private expandAllGroupsInArray(nodes: INode[], graph: IGraph, foldingView: IFoldingView) {
    nodes.forEach(node => {
      if (graph.contains(node)) {
        foldingView.expand(node);
        this.expandAllGroupsInArray(graph.getChildren(node).toArray(), graph, foldingView);
      }
    });
  }

  private getNodeCreatedListener(graph: IGraph): (sender: any, evt: GraphBuilderItemEventArgs<INode, any>) => void {
    return (sender: any, evt: GraphBuilderItemEventArgs<INode, any>) => {
      const node: INode = evt.item;
      const tag: ControlFlowNodeDetails = node.tag;
      const label = node.labels.first();
      const ANNOTATION_NODE_STROKE = new Stroke({
        r: 128,
        g: 177,
        b: 192,
        dashStyle: DashStyle.DASH_DOT
      });
      if (graph.isGroupNode(node)) {
        const collapsibleStyleDec: CollapsibleNodeStyleDecorator = (node.style as CollapsibleNodeStyleDecorator).clone();
        if (tag.entity === ControlFlowNode.EntityEnum.ANNOTATION) {
          GraphGlobalStyles.getFolderNodeConverter(true, node, graph);
          collapsibleStyleDec.wrapped = new ShapeNodeStyle({ fill: NODE_COLORS['GROUP'].fillColor, stroke: ANNOTATION_NODE_STROKE });
        }
        collapsibleStyleDec.insets = new Insets(GROUP_LABEL_INSET + GROUP_LABEL_OFFSET + label.preferredSize.width,
          GROUP_INSET, GROUP_LABEL_INSET + GROUP_LABEL_OFFSET + label.preferredSize.width, GROUP_INSET);
        graph.setStyle(node, collapsibleStyleDec);
      } else {
        const strokeThickness = tag.isNodeBranch ? BRANCH_NODE_STROKE_THICKENESS : DEFAULT_STROKE_THICKNESS;
        const nodeColor = tag.isNodeBranchStatement ? NODE_COLORS.decision : NODE_COLORS[tag.shapeType];
        const nodeStyle = GraphGlobalStyles.getShapeNodeStyle(
          nodeColor,
          CONTROL_FLOW_NODE_SHAPE_MAPPING[tag.shapeType],
          strokeThickness,
          tag.isNodeEntryExit
        );
        graph.setStyle(node, nodeStyle);
        let nodeLabelStyle = GraphGlobalStyles.getNodeLabelStyle(NodeWidthSizes.XL, NodeHeightSizes.L, NODE_LABEL_CONFIG.default);
        /* change Label for terminal */
        if (tag.isNodeEntryExit) {
          nodeLabelStyle = GraphGlobalStyles.getNodeLabelStyle(NodeWidthSizes.XL, NodeHeightSizes.L, NODE_LABEL_CONFIG.heading_invers);
        }
        /* change Label for branch */
        if (tag.isNodeBranch) {
          nodeLabelStyle = GraphGlobalStyles.getNodeLabelStyle(NodeWidthSizes.S, NodeHeightSizes.L, NODE_LABEL_CONFIG.default);
        }
        graph.setStyle(label, nodeLabelStyle);
        const newNodeRect = GraphGlobalStyles.getResizedNodeLayout(node.layout.topLeft, label.preferredSize, tag.isNodeBranch);
        graph.setNodeLayout(node, newNodeRect);
        if (tag.entity === ControlFlowNode.EntityEnum.TERMINAL && tag.type === 'ENTRY') {
          this.rootNode = node;
        }
      }
    };
  }

  private runLayout(animate: boolean = false) {
    const nodePlacer = new SimplexNodePlacer({
      barycenterMode: false,
      straightenEdges: true
    });

    const hierarchicLayout = LAYOUT_MAP.HIERARCHIC_LAYOUT as HierarchicLayout;
    hierarchicLayout.hierarchicLayoutCore.portConstraintOptimizer = new ControlFlowPortOptimizer();
    hierarchicLayout.nodePlacer = nodePlacer;

    const layoutExcecutor = new LayoutExecutor(this.graphComponent, hierarchicLayout);
    layoutExcecutor.portAdjustmentPolicy = PortAdjustmentPolicy.ALWAYS;
    if (animate) {
      layoutExcecutor.duration = new TimeSpan(500);
      layoutExcecutor.animateViewport = true;
    }
    void layoutExcecutor.start();
  }

  private zoomToRootNode(graphComponent: GraphComponent) {
    if (this.annotationId && this.desiredNode) {
      graphComponent.fitGraphBounds();
      graphComponent.zoomTo(new Point(this.desiredNode.layout.toPoint().x, this.desiredNode.layout.toPoint().y), 1);
      this.highlightFocusedAnnotationNode();
    } else {
      graphComponent.fitGraphBounds();
      graphComponent.zoomTo(new Point(this.rootNode.layout.toPoint().x, ZOOM_CENTER_Y_COORDINATE), NODE_ZOOM_VALUE);
    }
  }

  private highlightFocusedAnnotationNode() {
    const manager = this.graphComponent.highlightIndicatorManager;
    this.selectedItem = this.desiredNode;
    initializeHighlightStyles('click', this.graphComponent, this.desiredNode, 'differentSizeNodes');
    addHighlights(this.desiredNode as IModelItem, manager, this.graphComponent);
  }

  private setGraphSelectionIndicators(graphComponent: GraphComponent) {
    graphSelectionIndicators(graphComponent);
  }
}
