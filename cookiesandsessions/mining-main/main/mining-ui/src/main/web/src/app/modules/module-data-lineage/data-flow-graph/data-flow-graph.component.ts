import { AfterViewInit, Component, ComponentFactoryResolver, ComponentRef, ElementRef, Input, OnDestroy, Type, ViewChild,
  ViewContainerRef } from '@angular/core';
import { DEFAULT_STROKE_THICKNESS, GROUP_INSET, GROUP_LABEL_INSET, GROUP_LABEL_OFFSET, GraphGlobalStyles,
  SELECTED_NODE_STROKE_THICKNESS } from '@app/modules/graph/utils/graph-global-styles';
import {
  CollapsibleNodeStyleDecorator, Color, FoldingManager, GraphBuilder,
  GraphBuilderItemEventArgs, GraphComponent, GraphItemTypes, GraphViewerInputMode, HierarchicLayout,
  IFoldingView, IGraph, IModelItem, INode, Insets, InteriorLabelModel, LayoutExecutor, MergingFoldingEdgeConverter,
  PortAdjustmentPolicy, ShapeNodeShape, ShapeNodeStyle, TimeSpan, WebGLTaperedEdgeStyle
} from 'yfiles';
import { DataLineageGraphInfo } from '../models/data-lineage-graph.info';
import { LAYOUT_MAP } from '@app/modules/graph/utils/graph-layouts';
import { NODE_COLORS } from '@app/modules/graph/utils/node-colors';
import { NodeHeightSizes, NodeWidthSizes } from '@app/modules/graph/utils/node-configurations';
import { NODE_LABEL_CONFIG } from '@app/modules/graph/utils/node-label-configuration';
import { graphSelectionIndicators } from '@app/modules/graph/utils/graph.util';
import HierarchicGrouping from '@app/modules/graph/utils/yfiles-util/hierarchic-grouping';
import { ControlFlowPortOptimizer } from '@app/modules/graph/utils/yfiles-util/port-optimizer';
import { GraphUtility } from '@app/modules/graph/dependency/utils/dependency-graph-utility';
import { FastGraphModelManager, OptimizationMode } from '@app/modules/graph/utils/yfiles-util/fast-graph-model-manager';
import { addHighlights, initializeHighlightStyles } from '@app/modules/graph/utils/graph-highlight.util';
import { SharedModuleDetailsComponent } from '@app/shared/components/shared-module-details/shared-module-details.component';
import { ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';
import { DataLineageNode } from '../models/data-lineage.node';
import { Subject } from 'rxjs';

const mdouleIdRegex = /module-(\d+)/;
@Component({
  selector: 'data-flow-graph',
  templateUrl: './data-flow-graph.component.html'
})

export class DataFlowGraphComponent implements AfterViewInit, OnDestroy {
  @ViewChild('graphComponentRef') graphComponentRef!: ElementRef;
  @ViewChild('sidePanel', { read: ViewContainerRef }) sidePanel: ViewContainerRef;
  @Input() graphInfo: DataLineageGraphInfo;
  @Input() moduleId: number;
  @Input() fieldName: string;
  @Input() projectId: number;
  graphComponent!: GraphComponent;
  foldingView: IFoldingView;
  hierarchicGrouping: HierarchicGrouping;
  selectedItem: any;
  selectedNode: any;
  moduleDetails: ModulePojo;
  nodeEdgeClicked: string;
  showModuleDetails = false;
  componentInstances: Array<ComponentRef<any>> = [];
  nodeSelectionEvent: Subject<{dataLineageNode: DataLineageNode, module: ModulePojo}> = new Subject<{
    dataLineageNode: DataLineageNode, module: ModulePojo}>();
  sidePanelTitle: string;
  isSourcePanelVisible = false;
  previousModuleSidePanelState = false;
  previousSourceSidePanelState = false;

  constructor(private moduleControllerService: ModuleControllerService,
    private CFR: ComponentFactoryResolver,
  ) { }

  ngAfterViewInit(): void {
    this.graphComponent = new GraphComponent(this.graphComponentRef.nativeElement as string);
    this.initializeInputMode();
    const foldingManager = new FoldingManager();
    const foldingView = foldingManager.createFoldingView();
    foldingManager.folderNodeConverter = GraphGlobalStyles.getFolderNodeConverter();
    foldingManager.foldingEdgeConverter = new MergingFoldingEdgeConverter();
    this.graphComponent.graph = foldingView.graph;
    this.hierarchicGrouping = new HierarchicGrouping(this.graphComponent);
    const graph = this.graphComponent.graph;
    GraphGlobalStyles.setDefaultStyles(graph, NodeWidthSizes.L, NodeHeightSizes.M);
    this.loadDataLineageGraph();
  }

  ngOnDestroy(): void {
    if (this.graphComponent) {
      this.graphComponent.cleanUp();
    }
  }

  /**
   * Method to change the visibility of Source code panel.
   * @param event boolean event to change the visibility of the panel.
   */
  showSourcePanelDrawer(event: boolean): void {
    this.isSourcePanelVisible = event;
  }

  /**
   * Method to get the nodes selected in the graph.
   * @returns the selected node
   */
  getSelectedNode(): INode {
    return this.selectedNode;
  }

  /**
   * Expands all group nodes and relayouts the graph
   */
  expandAllGroups(): void {
    const graph = this.graphComponent.graph;
    this.expandAllGroupsInArray(graph.nodes.toArray(), graph, graph.foldingView);
    this.runLayout(true);
  }

  /**
   * Collapses all group nodes and relayouts the graph
   */
  collapseAllGroups(): void {
    const graph = this.graphComponent.graph;
    this.collapseAllGroupsInArray(graph.nodes.toArray(), graph, graph.foldingView);
    this.runLayout(true);
  }

  private initializeInputMode(): void {
    const inputMode = new GraphViewerInputMode();
    inputMode.toolTipItems = GraphItemTypes.NODE;
    inputMode.itemHoverInputMode.enabled = true;
    inputMode.itemHoverInputMode.hoverItems = GraphItemTypes.EDGE + GraphItemTypes.NODE + GraphItemTypes.EDGE_LABEL;
    inputMode.selectableItems = GraphItemTypes.EDGE + GraphItemTypes.NODE + GraphItemTypes.EDGE_LABEL;
    // create the interaction mode
    inputMode.itemHoverInputMode.addHoveredItemChangedListener((sender, evt) => {
      this.onHoveredItemChanged(evt.item, 'hover');
    });
    inputMode.addCanvasClickedListener(() => {
      this.selectedItem = null;
      this.onHoveredItemChanged(null, 'empty');
    });
    inputMode.itemHoverInputMode.discardInvalidItems = false;
    inputMode.mouseHoverInputMode.duration = new TimeSpan(1, 0, 0, 0, 0);

    inputMode.navigationInputMode.addGroupCollapsedListener((src, evt) => {
      const selection = evt.item;
      this.showModuleDetails = this.previousModuleSidePanelState;
      if (selection && selection.tag.type === 'MODULE') {
        this.isSourcePanelVisible = ! this.showModuleDetails ? false : this.previousSourceSidePanelState;
      } else {
        this.isSourcePanelVisible = this.previousSourceSidePanelState;
      }
    });

    inputMode.navigationInputMode.addGroupExpandedListener((src, evt) => {
      const selection = evt.item;
      this.showModuleDetails = this.previousModuleSidePanelState;
      if (selection && selection.tag.type === 'MODULE') {
        this.isSourcePanelVisible = ! this.showModuleDetails ? false : this.previousSourceSidePanelState;
      } else {
        this.isSourcePanelVisible = this.previousSourceSidePanelState;
      }
    });

    inputMode.addMultiSelectionFinishedListener((src, args) => {
      this.previousModuleSidePanelState = this.showModuleDetails;
      this.previousSourceSidePanelState = this.isSourcePanelVisible;
      // this implementation of the contextual toolbar only supports nodes, edges and labels
      const selection = args.selection.filter(item => INode.isInstance(item)).toArray();
      if (selection && selection.length && !selection[0].tag.connectingNodeId) {
        // in case of group node selection do not overide the previous selection
        if (!this.graphComponent.graph.isGroupNode(selection[0] as INode)) {
          this.selectedItem = selection[0];
          this.selectedNode = selection[0];
        }
        if (selection[0].tag.type === 'MODULE') {
          const highlightedFields = this.graphInfo.graphNodes.filter(node => (node.parentModule === selection[0].tag.id) && node.type === 'DATA_INTERFACE');
          this.showModuleDetails = true;
          this.isSourcePanelVisible = false;
          const id = parseInt(selection[0].tag.id.match(mdouleIdRegex)[1] as string, 10);
          if (id !== this.moduleDetails?.id) {
            this.getModuleDetails(id, highlightedFields);
          } else {
              this.nodeEdgeClicked = 'node';
              this.createSidePanel(SharedModuleDetailsComponent as Component, highlightedFields);
          }
        } else if (selection[0].tag.type !== 'MODULE' && selection[0].tag.type !== 'DATA_INTERFACE') {
          this.showModuleDetails = false;
          this.isSourcePanelVisible = true;
          const selectedNode = selection[0].tag;
          const id = parseInt(selectedNode.id.split('-')[1] as string, 10);
          this.sidePanelTitle = selectedNode.label != null ? selectedNode.label : selectedNode.name;
          if ( ! this.moduleDetails || id !== this.moduleDetails?.id) {
            this.getModuleDetails(id);
          } else {
            this.nodeSelectionEvent.next({ dataLineageNode: selectedNode, module: this.moduleDetails});
          }
        }
        const node: INode = this.getSelectedNode();
        this.onHoveredItemChanged(node, 'click');
      } else {
        this.selectedNode = null;
      }
    });
    // if an item is deselected or deleted, we remove that element from the selectedItems
    this.graphComponent.selection.addItemSelectionChangedListener((src, args) => {
      if (!args.itemSelected) {
        // remove the context menu if seletion has changed
        this.selectedNode = null;
      }
    });
    /* event listener for single click */
    this.graphComponent.inputMode = inputMode;
  }

  private getModuleDetails(id: number, highlightedFields?: DataLineageNode[]): void {
    this.moduleControllerService.findModuleById(this.projectId, id, true).subscribe((data: ModulePojo) => {
      this.moduleDetails = data;
      if (this.showModuleDetails) {
        this.nodeEdgeClicked = 'node';
        this.createSidePanel(SharedModuleDetailsComponent as Component, highlightedFields);
      } else if (this.isSourcePanelVisible) {
        this.nodeSelectionEvent.next({ dataLineageNode: this.selectedItem.tag, module: this.moduleDetails});
      }
    });
  }

  private createSidePanel(ComponentRef: Component, highlightedNodes: DataLineageNode[]): void {
    const componentFactory = this.CFR.resolveComponentFactory(ComponentRef as Type<unknown>);
    this.componentInstances.forEach((componentInstance) => {
      componentInstance?.destroy();
    });
    this.componentInstances?.push(this.sidePanel?.createComponent(componentFactory));
    const componentInstanceLength = this.componentInstances.length - 1;
    if (this.componentInstances[componentInstanceLength]) {
      this.componentInstances[componentInstanceLength].instance.projectId = this.projectId;
      this.componentInstances[componentInstanceLength].instance.fromDL = true;
      this.componentInstances[componentInstanceLength].instance.highlightedNodes = highlightedNodes;
      this.componentInstances[componentInstanceLength].instance.hideShowTab.subscribe(() => {
        this.showModuleDetails = false;
      });
      this.createSharedSidePanel(componentInstanceLength);
      this.closeSidePanel(componentInstanceLength, 'hideShowTab');
    }
  }

  private createSharedSidePanel(componentInstanceLength: number): void {
    this.componentInstances[componentInstanceLength].instance.moduleDetails = this.moduleDetails;
    this.componentInstances[componentInstanceLength].instance.selectedItem = this.selectedItem;
    this.componentInstances[componentInstanceLength].instance.getDataForSharedModule();
  }

  private closeSidePanel(noOfInstances: number, type: string): void {
    this.componentInstances[noOfInstances].instance[type].subscribe(() => {
      this.componentInstances.forEach((componentInstance) => {
        componentInstance?.destroy();
      });
    });
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
    initializeHighlightStyles('click', this.graphComponent, this.selectedItem as INode, 'sameSizeNodes');
    addHighlights(this.selectedItem as IModelItem, manager, this.graphComponent);
    if (type !== 'empty') {
      let hoverNode = item;
      let nodeHighlightStyles = 'sameSizeNodes';
      if (hoverNode && hoverNode.tag?.nodeShapeType === 'group' && this.foldingView.isExpanded(hoverNode as INode)) {
        hoverNode = null;
        nodeHighlightStyles = 'differentSizeNodes';
      }
      if (hoverNode && ! (hoverNode.tag?.type === 'FIELD' || hoverNode.tag?.type === 'DATA_INTERFACE')) {
        nodeHighlightStyles = 'differentSizeNodes';
      }
      initializeHighlightStyles('hover', this.graphComponent, hoverNode as INode, nodeHighlightStyles);
      addHighlights(hoverNode, manager, this.graphComponent);
    }
  }

  private loadDataLineageGraph(): void {
    this.buildGraph(this.graphComponent.graph);
    // center the arranged graph in the visible area
    this.graphComponent.fitGraphBounds();
  }

  private buildGraph(graph: IGraph): void {
    // use the main graph to build the unfolded graph from the data
    this.foldingView = graph.foldingView;
    const mainGraph = this.foldingView.manager.masterGraph;

    const builder = new GraphBuilder(mainGraph);
    builder.createNodesSource({
      data: this.graphInfo.graphNodes,
      id: 'id',
      labels: ['name'],
      parentId: 'group'
    });
    builder.createGroupNodesSource({
      data: this.graphInfo.graphGroups,
      id: 'id',
      labels: ['label'],
      parentId: 'parentGroup'
    });
    builder.createEdgesSource({
      data: this.graphInfo.graphEdges,
      sourceId: 'fromId',
      targetId: 'toId'
    });
    builder.addNodeCreatedListener(this.getNodeCreatedListener(this.foldingView.manager.masterGraph));
    this.setModelManagerIfLargeGraph(this.graphComponent.graph);
    builder.buildGraph();
    const nodesArray: INode[] = graph.nodes.toArray();
    this.createStatementNodes(nodesArray, graph, true);
    this.runLayout();
    this.collapseAllGroupsInArray(nodesArray, graph, this.foldingView);
    this.runLayout();
    this.setGraphSelectionIndicators(this.graphComponent);
  }

  private runLayout(animate: boolean = false) {
    const hierarchicLayout = LAYOUT_MAP.HIERARCHIC_LAYOUT as HierarchicLayout;
    hierarchicLayout.hierarchicLayoutCore.portConstraintOptimizer = new ControlFlowPortOptimizer();
    const layoutExecutor = new LayoutExecutor(this.graphComponent, hierarchicLayout);
    layoutExecutor.portAdjustmentPolicy = PortAdjustmentPolicy.ALWAYS;
    if (animate) {
      layoutExecutor.duration = new TimeSpan(500);
      layoutExecutor.animateViewport = true;
    }
    void layoutExecutor.start();
  }

  private setModelManagerIfLargeGraph(graph: IGraph) {
    if (GraphUtility.isLarge(graph) && !(this.graphComponent.graphModelManager instanceof FastGraphModelManager)) {
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


  private setGraphSelectionIndicators(graphComponent: GraphComponent) {
    graphSelectionIndicators(graphComponent);
  }

  /**
   * Collapses all group nodes.
   * @param nodes - nodes that will be collapsed
   * @param graph - graph containing the nodes
   * @param foldingView - used to collapse the groups
   * @param setStatementNodes - flag to set statement nodes on graph load.
   */
  private collapseAllGroupsInArray(nodes: INode[], graph: IGraph, foldingView: IFoldingView) {
    nodes.forEach(node => {
      if (graph.contains(node) && graph.isGroupNode(node)) {
        const child = graph.getChildren(node).toArray();
        if (child.length > 0) {
          this.collapseAllGroupsInArray(child, graph, foldingView);
        }
        const id = parseInt(node.tag.id.match(mdouleIdRegex)[1] as string, 10);
        if (node.tag?.type !== 'MODULE' || id !== this.moduleId) {
          foldingView.collapse(node);
        }
      }
    });
  }

  private createStatementNodes(nodes: INode[], graph: IGraph, setStatementNodes = false) {
    nodes.forEach(node => {
      if (graph.contains(node) && graph.isGroupNode(node) && node.tag?.type === 'STATEMENT' && node.tag.statementLabel && setStatementNodes) {
        this.setStatementNodes(node, graph);
      }
    });
  }

  private setStatementNodes(node: INode, graph: IGraph): void {
    const collapsibleStyleDec: CollapsibleNodeStyleDecorator = (node.style as CollapsibleNodeStyleDecorator).clone();
    collapsibleStyleDec.wrapped = new ShapeNodeStyle({ fill: '#F5F7FA', stroke: '#13C2C2' });
    graph.setStyle(node, collapsibleStyleDec);
    const expandedNode = graph.createNode();
    graph.addLabel(expandedNode, node.tag.statementLabel as string, InteriorLabelModel.NORTH_WEST);
    graph.setParent(expandedNode, node);
    const expandedNodeStyle = new ShapeNodeStyle({
      shape: 'rectangle',
      stroke: null,
      fill: '#F5F7FA'
    });
    graph.setStyle(expandedNode, expandedNodeStyle);
    const targetId = node.tag?.direction === 'OUTGOING' ? `${node.tag.parentGroup}-Outgoing` : `${node.tag.parentGroup}-Incoming`;
    const targetGroup = graph.nodes.find(n => n.tag?.id === targetId);
    if (targetGroup) {
      graph.setParent(node, targetGroup);
    }
  }

  private getNodeCreatedListener(graph: IGraph): (sender: any, evt: GraphBuilderItemEventArgs<INode, any>) => void {
    return (sender: any, evt: GraphBuilderItemEventArgs<INode, any>) => {
      const node: INode = evt.item;
      const label = node.labels.first();
      if (graph.isGroupNode(node)) {
        const collapsibleStyleDec: CollapsibleNodeStyleDecorator = (node.style as CollapsibleNodeStyleDecorator).clone();
        if (node.tag.label === 'Data-Interfaces') {
          collapsibleStyleDec.wrapped = new ShapeNodeStyle({ fill: '#fff0f6'});
        }
        collapsibleStyleDec.insets = new Insets(GROUP_LABEL_INSET + GROUP_LABEL_OFFSET + label.preferredSize.width,
          GROUP_INSET, GROUP_LABEL_INSET + GROUP_LABEL_OFFSET + label.preferredSize.width, GROUP_INSET);
        graph.setStyle(node, collapsibleStyleDec);
      } else {
        let strokeThickness = DEFAULT_STROKE_THICKNESS;
        let nodeColor;
        switch (node.tag.type) {
          case 'DATA_INTERFACE':
            nodeColor = NODE_COLORS.DATA_INTERFACE;
            break;
          case 'FIELD':
            nodeColor = NODE_COLORS.FIELDS;
            if (node.tag.name === this.fieldName) {
              nodeColor = {...nodeColor, strokeColor: '#3b6e8f'};
              strokeThickness = SELECTED_NODE_STROKE_THICKNESS;
            }
            break;
          default:
            nodeColor = NODE_COLORS.default;
            break;
        }
        const nodeStyle = GraphGlobalStyles.getShapeNodeStyle(
          nodeColor,
          ShapeNodeShape.ROUND_RECTANGLE,
          strokeThickness
        );
        graph.setStyle(node, nodeStyle);
        const nodeLabelStyle = GraphGlobalStyles.getNodeLabelStyle(NodeWidthSizes.XL, NodeHeightSizes.L, NODE_LABEL_CONFIG.default);
        graph.setStyle(label, nodeLabelStyle);
        const newNodeRect = GraphGlobalStyles.getResizedNodeLayout(node.layout.topLeft, label.preferredSize, false);
        graph.setNodeLayout(node, newNodeRect);
      }
    };
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
}
