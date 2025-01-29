import {
  CompositeLayoutData,
  FixNodeLayoutData,
  FixNodeLayoutStage,
  FixPointPolicy,
  GivenCoordinatesStage,
  GivenCoordinatesStageData,
  GraphComponent,
  GraphInputMode,
  GroupingSupport,
  HierarchicLayout,
  HierarchicLayoutData,
  IEdge,
  IEnumerable,
  IGraph,
  IMapper,
  INode,
  IPoint,
  IRectangle,
  ISize,
  LayoutExecutor,
  LayoutMode,
  List,
  Mapper,
  NodeAlignmentPolicy,
  Point,
  Size
} from 'yfiles';
import { LAYOUT_MAP } from '../graph-layouts';
import { ControlFlowPortOptimizer } from './port-optimizer';

/**
 * This class is based on the HierarchicGrouping class in the yFiles demo HierarchicGrouping (https://live.yworks.com/demos/complete/hierarchicgrouping/).
 * Its used to handle collapsing/expanding group nodes and ensure the layout stays intact.
 */
export default class HierarchicGrouping {
  /* The last group node that was collapsed/expanded. */
  private changedGroupNode: INode = null;
  /* A mapper containing alternative bounds for the collapsed/expanded group node. */
  private readonly alternativeGroupBounds: Mapper<INode, IRectangle>;
  /* A mapper containing alternative path for the edges connecting to groups, group content or folder nodes. */
  private readonly alternativeEdgePaths: Mapper<IEdge, List<IPoint>>;

  constructor(private readonly graphComponent: GraphComponent) {
    this.alternativeGroupBounds = new Mapper();
    this.alternativeEdgePaths = new Mapper();
    this.configureInputMode(graphComponent.inputMode as GraphInputMode);
  }

/**
 * Applies the incremental layout after each expanding and collapsing.
 * @param nodesCoordinates the coordinates of the nodes before the layout
 * @param nodeSizes the sizes of the nodes before the layout
 * @param edgesCoordinates the coordinates of the edges before the layout
 */
  async applyIncrementalLayout(
    nodesCoordinates: IMapper<INode, IPoint>,
    nodeSizes: IMapper<INode, ISize>,
    edgesCoordinates: IMapper<IEdge, IEnumerable<IPoint>>
  ): Promise<void> {
    /* Configure hierarchic layout for a most stable outcome */
    const layout = LAYOUT_MAP.HIERARCHIC_LAYOUT.memberwiseClone() as HierarchicLayout;
    layout.layoutMode = LayoutMode.INCREMENTAL;
    layout.hierarchicLayoutCore.portConstraintOptimizer = new ControlFlowPortOptimizer();
    /* The FixNodeLayoutStage is used to make sure that the expanded/collapsed group stays at their location.
     * Note that an input mode with the corresponding 'group node alignment policy' is used, too. */
    const fixNodeLayout = new FixNodeLayoutStage(layout);
    fixNodeLayout.fixPointPolicy = FixPointPolicy.UPPER_RIGHT;

    /* Prepare graph so the layout will consider which node is fixed and what bounds to use for from-sketch placement */
    const graph = this.graphComponent.graph;
    const foldingView = graph.foldingView;
    const layoutData = new CompositeLayoutData(
      new HierarchicLayoutData({
        alternativeGroupBounds: node => {
          const masterNode = foldingView.getMasterItem(node);
          return this.alternativeGroupBounds.get(masterNode);
        },
        alternativeEdgePaths: edge => {
          const masterEdge = foldingView.getMasterItem(edge);
          return this.alternativeEdgePaths.get(masterEdge);
        },
        /* Mark folder nodes to treat them differently than normal nodes during layout */
        folderNodes: node => !foldingView.isExpanded(node)
      }),
      new FixNodeLayoutData({
        fixedNodes: this.changedGroupNode
      }),
      new GivenCoordinatesStageData({
        nodeLocations: nodesCoordinates,
        nodeSizes,
        edgePaths: edgesCoordinates
      })
    );

    /* The GivenCoordinatesStage will move the nodes to their previous locations to be able to run an incremental
     * layout although all nodes inside a group node were placed at the same location. */
    const layoutExecutor = new LayoutExecutor({
      graphComponent: this.graphComponent,
      layout: new GivenCoordinatesStage(fixNodeLayout),
      layoutData,
      easedAnimation: true,
      duration: '0.5s'
    });
    try {
      await layoutExecutor.start();
    } catch (error) {
      const reportError = (window as any).reportError;
      if (typeof reportError === 'function') {
        reportError(error);
      } else {
        throw error;
      }
    }
    this.graphComponent.updateContentRect();
  }

  /**
   * Enables the folding commands on the <code>navigationInputMode</code> of the provided <code>inputMode</code> and
   * registers event listeners for the expand and collapse commands that trigger the automatic layout.
   *
   * @param inputMode The input mode to be configured.
   */
  private configureInputMode(inputMode: GraphInputMode): void {
    /* Create an input mode and set a group node alignment policy
     that makes sure that the expand/collapse button of the current group node keeps its location. */
    const navigationInputMode = inputMode.navigationInputMode;
    navigationInputMode.autoGroupNodeAlignmentPolicy = NodeAlignmentPolicy.TOP_RIGHT;

    navigationInputMode.allowCollapseGroup = true;
    navigationInputMode.allowExpandGroup = true;

    /* FitContent interferes with our view port animation setup */
    navigationInputMode.fitContentAfterGroupActions = false;

    navigationInputMode.addGroupExpandingListener((sender, evt) =>
      this.beforeExpandingGroup(evt.item)
    );
    navigationInputMode.addGroupCollapsingListener((sender, evt) =>
      this.beforeCollapsingGroup(evt.item)
    );

    navigationInputMode.addGroupExpandedListener((sender, evt) =>
      this.afterGroupStateChanged(evt.item)
    );
    navigationInputMode.addGroupCollapsedListener((sender, evt) =>
      this.afterGroupStateChanged(evt.item)
    );
  }

  /**
   * Stores information about the layout of a group before expanding the group.
   *
   * @param group The group that will be expanded.
   */
  private beforeExpandingGroup(group: INode): void {
    const edgesToBackup = (graph: IGraph, group: INode) => graph.edgesAt(group).toArray();
    this.beforeGroupStateChanged(group, edgesToBackup);
  }

  /**
   * Stores information about the layout of a group before collapsing the group.
   *
   * @param group The group that will be collapsed.
   */
  private beforeCollapsingGroup(group: INode): void {
    const edgesToBackup = (graph: IGraph, group: INode) => this.getAffectedEdges(graph, group);
    this.beforeGroupStateChanged(group, edgesToBackup);
  }

  /**
   * Stores information about the layout of a group before collapsing or expanding the group.
   *
   * @param group The group that will be collapsed or expanded.
   * @param edgesToBackup The edges whose paths should be stored as well.
   */
  private beforeGroupStateChanged(
    group: INode,
    edgesToBackup: (graph: IGraph, group: INode) => Iterable<IEdge>
  ): void {
    const graph = this.graphComponent.graph;
    const foldingView = graph.foldingView;
    const layout = group.layout;

    /* store the collapsed group node */
    this.changedGroupNode = group;

    /* store the group bounds of the collapsed group node before layout */
    this.alternativeGroupBounds.clear();
    this.alternativeGroupBounds.set(foldingView.getMasterItem(group), layout.toRect());

    /* store all edge paths that connect to/into the collapsed group before layout */
    this.alternativeEdgePaths.clear();
    for (const edge of edgesToBackup(graph, group)) {
      this.alternativeEdgePaths.set(foldingView.getMasterItem(edge), this.getPointList(edge));
    }
  }

  /**
   * Performs an incremental layout on the graph after a group was closed/expanded interactively.
   *
   * @param group The group that was expanded or collapsed.
   */
  private afterGroupStateChanged(group: INode): void {
    /* store the current locations of nodes and edges to keep them for incremental layout */
    const graph = this.graphComponent.graph;
    const nodesCoordinates = new Mapper<INode, IPoint>();
    const nodeSizes = new Mapper<INode, ISize>();
    const edgesCoordinates = new Mapper<IEdge, List<IPoint>>();

    const groupingSupport = graph.groupingSupport;
    if (graph.isGroupNode(group)) {
      /* reset the paths and the centers of the child nodes so that morphing looks smoother */
      const descendants = groupingSupport.getDescendants(group);
      const visitedEdges = new Set();
      descendants.forEach(childNode => {
        graph.edgesAt(childNode).forEach(edge => {
          /* store path and clear bends afterwards */
          if (!visitedEdges.has(edge)) {
            edgesCoordinates.set(edge, this.getPointList(edge));
            graph.clearBends(edge);
            visitedEdges.add(edge);
          }
        });
        /* store coordinates and center node afterwards */
        const layout = childNode.layout;
        nodesCoordinates.set(childNode, Point.from(layout));
        nodeSizes.set(childNode, Size.from(layout));
      });
    }

    /* reset adjacent edge paths to get smoother layout transitions */
    graph.edgesAt(group).forEach(edge => {
      /* store path and clear bends afterwards */
      edgesCoordinates.set(edge, this.getPointList(edge));
      graph.clearBends(edge);
    });

    this.applyIncrementalLayout(nodesCoordinates, nodeSizes, edgesCoordinates).catch(() => {});
  }

  /**
   * Retrieves the affected edges when a group node is collapsed.
   * Edges are affected when they connect to the group node directly or to a descendant of the group node.
   *
   * @param graph The graph to which the group node belongs.
   * @param group The group node which is collapsed.
   *
   * @return An array of all affected edges.
   */
  private getAffectedEdges(graph: IGraph, group: INode): IEdge[] {
    /* Collect all edges that connect to the group node. */
    const crossingEdges = graph.edgesAt(group).toArray();

    /* Collect all edges that cross the group node's border. */
    let groupingSupport: GroupingSupport = graph.groupingSupport;
    if (groupingSupport === null) {
      groupingSupport = new GroupingSupport(graph);
    }
    const descendants = groupingSupport.getDescendants(group);
    const visitedEdges = new Set();
    descendants.forEach(descendant => {
      graph.edgesAt(descendant).forEach(edge => {
        if (!visitedEdges.has(edge)) {
          if (!groupingSupport.isDescendant(edge.opposite(descendant) as INode, group)) {
            crossingEdges.push(edge);
          }
          visitedEdges.add(edge);
        }
      });
    });

    return crossingEdges;
  }

  /**
   * Returns the control points of the given edge.
   * The control points of an edge are its source port location, its bend locations, and its target
   * port location.
   * @param edge the edge whose control points are collected.
   */
  private getPointList(edge: IEdge): List<IPoint> {
    const points = new List<IPoint>();
    if (edge.sourcePort) {
      points.add(edge.sourcePort.location);
    }
    for (const bend of edge.bends.toArray()) {
      points.add(bend.location);
    }
    if (edge.targetPort) {
      points.add(edge.targetPort.location);
    }
    return points;
  }
}
