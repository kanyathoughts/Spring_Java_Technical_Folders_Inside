import {
  PortConstraintOptimizerBase,
  PortCandidateOptimizer,
  LayoutOrientation,
  LayoutGraph,
  ILayers,
  ILayoutDataProvider,
  IItemFactory,
  Maps,
  EdgeList,
  NodeDataType,
  PortConstraint,
  PortSide,
  YNode,
  IComparer,
  Edge,
  BaseClass,
  IDataProvider,
  ICollection,
  PortDirections
} from 'yfiles';

class SameLayerNodePositionComparer extends BaseClass(IComparer) {
  ldp: ILayoutDataProvider;
  constructor(ldp: ILayoutDataProvider) {
    super();
    this.ldp = ldp;
  }

  compare(o1: YNode, o2: YNode) {
    const position1 = this.ldp.getNodeData(o1).position;
    const position2 = this.ldp.getNodeData(o2).position;
    if (position1 === position2) {
      return 0;
    }
    return position1 < position2 ? -1 : 1;
  }
}

class SingleSidePortConstraintComparer extends BaseClass(IComparer) {
  compare(o1: PortConstraint, o2: PortConstraint) {
    const pc1 = o1;
    const pc2 = o2;
    // we use NORTH as neutral element since we care only about EST and WEST
    const b1 = pc1 ? pc1.side : PortSide.NORTH;
    const b2 = pc2 ? pc2.side : PortSide.NORTH;
    if (b1 === b2) {
      return 0;
    }
    return b1 === PortSide.WEST || b2 === PortSide.EAST ? -1 : 1;
  }
}

class PositionEdgeComparer extends BaseClass(IComparer) {
  sameLayerNodePositionComparer: SameLayerNodePositionComparer;
  portConstraintComparer: SingleSidePortConstraintComparer;
  source: boolean;
  ldp: ILayoutDataProvider;

  constructor(source: boolean, ldp: ILayoutDataProvider) {
    super();
    this.sameLayerNodePositionComparer = new SameLayerNodePositionComparer(ldp);
    this.portConstraintComparer = new SingleSidePortConstraintComparer();
    this.source = source;
    this.ldp = ldp;
  }

  compare(o1: Edge, o2: Edge) {
    const e1 = o1;
    const e2 = o2;
    // compare positions at specified end
    const comparePos = this.sameLayerNodePositionComparer.compare(
      this.source ? e1.source : e1.target,
      this.source ? e2.source : e2.target
    );
    if (comparePos !== 0) {
      return comparePos;
    }
    // compare constraints at specified end
    const compareConstraints = this.portConstraintComparer.compare(
      this.source
        ? this.ldp.getEdgeData(e1).sourcePortConstraint
        : this.ldp.getEdgeData(e1).targetPortConstraint,
      this.source
        ? this.ldp.getEdgeData(e2).sourcePortConstraint
        : this.ldp.getEdgeData(e2).targetPortConstraint
    );
    if (compareConstraints !== 0) {
      return compareConstraints;
    }
    // compare constraints at opposite end
    return this.portConstraintComparer.compare(
      this.source
        ? this.ldp.getEdgeData(e1).targetPortConstraint
        : this.ldp.getEdgeData(e1).sourcePortConstraint,
      this.source
        ? this.ldp.getEdgeData(e2).targetPortConstraint
        : this.ldp.getEdgeData(e2).sourcePortConstraint
    );
  }
}

export class ControlFlowPortOptimizer extends PortConstraintOptimizerBase {

  $layoutOrientation: LayoutOrientation;
  pcListOptimizer: PortCandidateOptimizer;
  constructor() {
    super();
    this.pcListOptimizer = new PortCandidateOptimizer();
    this.$layoutOrientation = LayoutOrientation.TOP_TO_BOTTOM;
  }

  static get LANE_ALIGNMENT_LEFT(): number {
    return 0;
  }

  static get LANE_ALIGNMENT_RIGHT(): number {
    return 1;
  }

  static get PRIORITY_LOW(): number {
    return 1;
  }

  static get PRIORITY_BASIC(): number {
    return 3;
  }

  static get PRIORITY_HIGH(): number {
    return 5000;
  }

  static get NODE_TO_ALIGN_DP_KEY(): string {
    return 'y.layout.hierarchic.incremental.SimlexNodePlacer.NODE_TO_ALIGN_WITH';
  }

  get layoutOrientations(): LayoutOrientation {
    return this.$layoutOrientation;
  }

  set layoutOrientations(value: LayoutOrientation) {
    this.$layoutOrientation = value;
  }

  static isTemporarySameLayerEdge(edge: Edge, ldp: ILayoutDataProvider): boolean | null {
    return ControlFlowPortOptimizer.isTemporarySameLayerNode(edge.target, ldp);
  }

  static isTemporarySameLayerNode(node: YNode, ldp: ILayoutDataProvider): boolean | null {
    return node.inDegree === 2 && node.outDegree === 0 && ldp.getNodeData(node) === null;
  }

  static getPreferredSideForTemporarySameLayerEdge(edge: Edge, ldp: ILayoutDataProvider): PortSide {
    const originalEdge = ldp.getEdgeData(edge).associatedEdge;
    const source = originalEdge.source.equals(edge.source);
    const sData = ldp.getNodeData(originalEdge.source);
    const tData = ldp.getNodeData(originalEdge.target);
    if (sData.position < tData.position) {
      return source ? PortSide.EAST : PortSide.WEST;
    }
    return !source ? PortSide.EAST : PortSide.WEST;
  }

  static getAllSameLayerEdges(graph: LayoutGraph, ldp: ILayoutDataProvider): EdgeList {
    const sameLayerEdges = new EdgeList();
    const edge2Seen = Maps.createHashedEdgeMap();
    graph.nodes.forEach(node => {
      const nData = ldp.getNodeData(node);
      for (let cell = nData.firstSameLayerEdgeCell; cell !== null; cell = cell.succ()) {
        const sameLayerEdge: Edge = cell.info as Edge;
        const opposite = sameLayerEdge.opposite(node);
        if (!edge2Seen.getBoolean(sameLayerEdge) && graph.contains(opposite)) {
          sameLayerEdges.addLast(sameLayerEdge);
          edge2Seen.setBoolean(sameLayerEdge, true);
        }
      }
    });
    return sameLayerEdges;
  }

  static getSameLayerEdges(node: YNode, left: boolean, ldp: ILayoutDataProvider): EdgeList {
    const nData = ldp.getNodeData(node);
    const nPos = nData.position;
    const result = new EdgeList();
    for (let cell = nData.firstSameLayerEdgeCell; cell !== null; cell = cell.succ()) {
      const sameLayerEdge = cell.info as Edge;
      const other = sameLayerEdge.opposite(node);
      const otherPos = ldp.getNodeData(other).position;
      if ((left && otherPos < nPos) || (!left && otherPos > nPos)) {
        result.addLast(sameLayerEdge);
      }
    }
    return result;
  }

  static hasSameLayerEdge(n: YNode, left: boolean, ldp: ILayoutDataProvider): boolean {
    return !ControlFlowPortOptimizer.getSameLayerEdges(n, left, ldp).isEmpty();
  }

  static isFlatwisePortConstraint(portConstraint: PortConstraint): boolean {
    return portConstraint && (portConstraint.atEast || portConstraint.atWest);
  }

  static isFlatwiseCandidateCollection(portCandidates: ICollection<any>, layoutOrientation: number): boolean {
    if (!portCandidates) {
      return false;
    }
    let containsEast = false;
    let containsWest = false;
    portCandidates.forEach(pc => {
      const direction = pc.getDirectionForLayoutOrientation(layoutOrientation);
      if (!containsEast && (PortDirections.EAST && direction) !== 0) {
        containsEast = true;
      }
      if (!containsWest && (PortDirections.WEST && direction) !== 0) {
        containsWest = true;
      }
    });
    return containsEast && containsWest;
  }

  static isBackEdge(edge: Edge, ldp: ILayoutDataProvider): boolean {
    return ldp.getEdgeData(edge).reversed;
  }

  static getOriginalEdge(edge: Edge, ldp: ILayoutDataProvider): Edge {
    const sData = ldp.getNodeData(edge.source);
    if (sData.type === NodeDataType.BEND && sData.associatedEdge !== null) {
      return sData.associatedEdge;
    }
    const tData = ldp.getNodeData(edge.target);
    if (tData.type === NodeDataType.BEND && tData.associatedEdge !== null) {
      return tData.associatedEdge;
    }
    return edge;
  }

  static getSwimlaneId(node: YNode, ldp: ILayoutDataProvider): number {
    const laneDesc = ldp.getNodeData(node).swimLaneDescriptor;
    return laneDesc === null ? -1 : laneDesc.computedLaneIndex;
  }

  static isToLeftPartition(source: YNode, target: YNode, layoutData: ILayoutDataProvider): boolean {
    const sourceDesc = layoutData.getNodeData(source).swimLaneDescriptor;
    const targetDesc = layoutData.getNodeData(target).swimLaneDescriptor;
    return (
      sourceDesc !== targetDesc &&
      sourceDesc !== null &&
      targetDesc !== null &&
      sourceDesc.computedLaneIndex > targetDesc.computedLaneIndex
    );
  }

  static isToRightPartition(source: YNode, target: YNode, layoutData: ILayoutDataProvider): boolean {
    const sourceDesc = layoutData.getNodeData(source).swimLaneDescriptor;
    const targetDesc = layoutData.getNodeData(target).swimLaneDescriptor;
    return (
      sourceDesc !== targetDesc &&
      sourceDesc !== null &&
      targetDesc !== null &&
      sourceDesc.computedLaneIndex < targetDesc.computedLaneIndex
    );
  }

  optimizeAfterLayering(graph: LayoutGraph, layers: ILayers, ldp: ILayoutDataProvider, itemFactory: IItemFactory): void {
    this.pcListOptimizer.optimizeAfterLayering(graph, layers, ldp, itemFactory);
  }

  optimizeAfterSequencing(graph: LayoutGraph, layers: ILayers, ldp: ILayoutDataProvider, itemFactory: IItemFactory): void {
    super.optimizeAfterSequencing(graph, layers, ldp, itemFactory);
    const edgePriority = Maps.createHashedEdgeMap();
    const nodeAlignment = Maps.createHashedNodeMap();
    this.optimizeForAlignment(graph, ldp, itemFactory, nodeAlignment, edgePriority);
    this.optimizeMessageNodes(graph, ldp, itemFactory);
  }

  optimizeMessageNodes(graph: LayoutGraph, ldp: ILayoutDataProvider, factory: IItemFactory): void {
    const edges = new EdgeList(graph.getEdgeCursor());
    edges.splice(ControlFlowPortOptimizer.getAllSameLayerEdges(graph, ldp));
    edges.forEach((e: Edge) => {
      const original = ControlFlowPortOptimizer.getOriginalEdge(e, ldp);
      const sourceLaneId = ControlFlowPortOptimizer.getSwimlaneId(original.source, ldp);
      const targetLaneId = ControlFlowPortOptimizer.getSwimlaneId(original.target, ldp);
      if (sourceLaneId !== targetLaneId) {
        factory.setTemporaryPortConstraint(e, true,
          PortConstraint.create(sourceLaneId < targetLaneId ? PortSide.EAST : PortSide.WEST)
        );
      }
    });
  }

  optimizeAfterSequencingForSingleNode(node: YNode, inEdgeOrder: IComparer<any>, outEdgeOrder: IComparer<any>, graph: LayoutGraph,
    ldp: ILayoutDataProvider, itemFactory: IItemFactory): void {
    // set EAST or WEST temporary constraints for the same layer edges
    node.edges.forEach(edge => {
      if (ControlFlowPortOptimizer.isTemporarySameLayerEdge(edge, ldp)) {
        const preferredSide = ControlFlowPortOptimizer.getPreferredSideForTemporarySameLayerEdge(
          edge,
          ldp
        );
        itemFactory.setTemporaryPortConstraint(
          edge,
          node.equals(edge.source),
          PortConstraint.create(preferredSide)
        );
      }
    });
    // choose final temporary constraint for all non-assigned flatwise edges
    this.optimizeFlatwiseEdges(node, true, outEdgeOrder, ldp, itemFactory);
    this.optimizeFlatwiseEdges(node, false, inEdgeOrder, ldp, itemFactory);
  }

  optimizeFlatwiseEdges(node: YNode, source: boolean, edgeOrder: IComparer<any>, ldp: ILayoutDataProvider, itemFactory: IItemFactory): void {
    const flatwiseEdges = Maps.createHashSet();
    const centralEdges = new EdgeList();
    const edges = source ? node.outEdges : node.inEdges;
    edges.forEach(edge => {
      const edgeData = ldp.getEdgeData(edge);
      const constraint = source ? edgeData.sourcePortConstraint : edgeData.targetPortConstraint;
      const candidates = source ? edgeData.sourcePortCandidates : edgeData.targetPortCandidates;
      if (constraint && (constraint.atEast || constraint.atWest)) {
        return;
      }
      if (ControlFlowPortOptimizer.isFlatwiseCandidateCollection(candidates, this.layoutOrientation)) {
        flatwiseEdges.add(edge);
      } else {
        centralEdges.add(edge);
      }
    });
    if (flatwiseEdges.size === 0) {
      return;
    }
    centralEdges.addAll(flatwiseEdges);
    centralEdges.sort(edgeOrder);
    centralEdges.forEach((edge, i) => {
      if (flatwiseEdges.some(flatwiseEdge => flatwiseEdge === edge)) {
        const side = i < ((centralEdges.size / 2) || 0) ? PortSide.WEST : PortSide.EAST;
        itemFactory.setTemporaryPortConstraint(edge as Edge, source, PortConstraint.create(side));
      }
    });
  }

  getCriticalInEdge(node: YNode, node2AlignWith: IDataProvider, edge2Length: IDataProvider): Edge {
    let bestEdge: Edge = null;
    node.inEdges.forEach(edge => {
      if (node2AlignWith.get(node) === edge.source &&
        (bestEdge === null || edge2Length.getNumber(bestEdge) < edge2Length.getInt(edge))) {
        bestEdge = edge;
      }
    });
    return bestEdge;
  }

  getCriticalOutEdge(node: YNode, node2AlignWith: IDataProvider, edge2Length: IDataProvider): Edge {
    let bestEdge: Edge = null;
    node.outEdges.forEach(edge => {
      if (node2AlignWith.get(edge.target) === node &&
        (bestEdge === null || edge2Length.getNumber(bestEdge) < edge2Length.getInt(edge))) {
        bestEdge = edge;
      }
    });
    return bestEdge;
  }

  optimizeWithCriticalEdges(node: YNode, ldp: ILayoutDataProvider, factory: IItemFactory, criticalInEdge: Edge, criticalOutEdge: Edge): void {
    const firstIn = node.firstInEdge;
    const firstOut = node.firstOutEdge;
    const lastIn = node.lastInEdge;
    const lastOut = node.lastOutEdge;
    if (node.degree === 3 && node.outDegree === 2 && criticalOutEdge === null) {
      // Special case: the only in-edge is critical and there are two free out-edges
      if ((!ControlFlowPortOptimizer.isToRightPartition(firstOut.source, firstOut.target, ldp) &&
        ControlFlowPortOptimizer.isBackEdge(firstOut, ldp)) ||
        ControlFlowPortOptimizer.isToLeftPartition(firstOut.source, firstOut.target, ldp)) {
        this.setOptimizedPortConstraint(firstOut, true, PortSide.WEST, ldp, factory);
        if ((!ControlFlowPortOptimizer.isToLeftPartition(lastOut.source, lastOut.target, ldp) &&
          ControlFlowPortOptimizer.isBackEdge(lastOut, ldp)) ||
          ControlFlowPortOptimizer.isToRightPartition(lastOut.source, lastOut.target, ldp)) {
          this.setOptimizedPortConstraint(lastOut, true, PortSide.EAST, ldp, factory);
        }
      } else {
        this.setOptimizedPortConstraint(lastOut, true, PortSide.EAST, ldp, factory);
      }
    } else if (node.degree === 3 && node.inDegree === 2 && criticalInEdge === null) {
      // Special case: the only out-edge is critical and there are two free in-edges
      if ((!ControlFlowPortOptimizer.isToRightPartition(firstIn.target, firstIn.source, ldp) &&
        ControlFlowPortOptimizer.isBackEdge(firstIn, ldp)) ||
        ControlFlowPortOptimizer.isToLeftPartition(firstIn.target, firstIn.source, ldp)) {
        this.setOptimizedPortConstraint(firstIn, false, PortSide.WEST, ldp, factory);
        if ((!ControlFlowPortOptimizer.isToRightPartition(lastIn.target, lastIn.source, ldp) &&
          ControlFlowPortOptimizer.isBackEdge(lastIn, ldp)) ||
          ControlFlowPortOptimizer.isToLeftPartition(lastIn.target, lastIn.source, ldp)) {
          this.setOptimizedPortConstraint(lastIn, true, PortSide.EAST, ldp, factory);
        }
      } else {
        this.setOptimizedPortConstraint(lastIn, true, PortSide.EAST, ldp, factory);
      }
    } else if (criticalInEdge === null ||
      (node.outDegree > node.inDegree && criticalOutEdge !== null)) {
      if (!ControlFlowPortOptimizer.hasSameLayerEdge(node, true, ldp)) {
        if (firstOut !== criticalOutEdge) {
          this.setOptimizedPortConstraint(firstOut, true, PortSide.WEST, ldp, factory);
        } else if (firstIn !== null && firstIn !== criticalInEdge && node.inDegree > 1) {
          this.setOptimizedPortConstraint(firstIn, false, PortSide.WEST, ldp, factory);
        }
      }
      if (!ControlFlowPortOptimizer.hasSameLayerEdge(node, false, ldp)) {
        if (lastOut !== criticalOutEdge) {
          this.setOptimizedPortConstraint(lastOut, true, PortSide.EAST, ldp, factory);
        } else if (lastIn !== null && lastIn !== criticalInEdge && node.inDegree > 1) {
          this.setOptimizedPortConstraint(lastIn, false, PortSide.EAST, ldp, factory);
        }
      }
    } else {
      if (!ControlFlowPortOptimizer.hasSameLayerEdge(node, true, ldp)) {
        if (firstIn !== criticalInEdge) {
          this.setOptimizedPortConstraint(firstIn, false, PortSide.WEST, ldp, factory);
        } else if (firstOut !== null && firstOut !== criticalOutEdge && node.outDegree > 1) {
          this.setOptimizedPortConstraint(firstOut, true, PortSide.WEST, ldp, factory);
        }
      }
      if (!ControlFlowPortOptimizer.hasSameLayerEdge(node, false, ldp)) {
        if (lastIn !== criticalInEdge) {
          this.setOptimizedPortConstraint(lastIn, false, PortSide.EAST, ldp, factory);
        } else if (lastOut !== null && lastOut !== criticalOutEdge && node.outDegree > 1) {
          this.setOptimizedPortConstraint(lastOut, true, PortSide.EAST, ldp, factory);
        }
      }
    }
  }

  setOptimizedPortConstraint(edge: Edge, source: boolean, direction: number, ldp: ILayoutDataProvider, factory: IItemFactory): void {
    if (!this.isAtPreferredPort(edge, source, ldp)) {
      factory.setTemporaryPortConstraint(edge, source, PortConstraint.create(direction));
    }
  }

  isAtPreferredPort(edge: Edge, source: boolean, ldp: ILayoutDataProvider): boolean {
    const e = ControlFlowPortOptimizer.getOriginalEdge(edge, ldp);
    const edgeData = ldp.getEdgeData(e);
    const pc = source ? edgeData.sourcePortConstraint : edgeData.targetPortConstraint;
    return pc && (pc.strong || pc.atEast || pc.atWest);
  }

  optimizeWithoutCriticalEdges(node: YNode, ldp: ILayoutDataProvider, factory: IItemFactory): void {
    if (node.outDegree > node.inDegree) {
      const firstOut = node.firstOutEdge;
      const lastOut = node.lastOutEdge;
      if (!ControlFlowPortOptimizer.hasSameLayerEdge(node, true, ldp) &&
        !this.isAtPreferredPort(firstOut, true, ldp) &&
        (node.outDegree !== 2 ||
          !ControlFlowPortOptimizer.isToRightPartition(firstOut.source, firstOut.target, ldp) ||
          ControlFlowPortOptimizer.hasSameLayerEdge(node, false, ldp))) {
        factory.setTemporaryPortConstraint(firstOut, true, PortConstraint.create(PortSide.WEST));
      } else if (!ControlFlowPortOptimizer.hasSameLayerEdge(node, false, ldp) &&
        !this.isAtPreferredPort(lastOut, true, ldp) &&
        (node.outDegree !== 2 ||
          !ControlFlowPortOptimizer.isToLeftPartition(lastOut.source, lastOut.target, ldp))) {
        factory.setTemporaryPortConstraint(lastOut, true, PortConstraint.create(PortSide.EAST));
      }
    } else {
      const firstIn = node.firstInEdge;
      const lastIn = node.lastInEdge;
      if (!ControlFlowPortOptimizer.hasSameLayerEdge(node, true, ldp) &&
        !this.isAtPreferredPort(firstIn, false, ldp) &&
        (node.degree !== 3 ||
          !ControlFlowPortOptimizer.isToRightPartition(firstIn.target, firstIn.source, ldp) ||
          ControlFlowPortOptimizer.hasSameLayerEdge(node, false, ldp))) {
        factory.setTemporaryPortConstraint(firstIn, false, PortConstraint.create(PortSide.WEST));
      } else if (!ControlFlowPortOptimizer.hasSameLayerEdge(node, false, ldp) &&
        !this.isAtPreferredPort(lastIn, false, ldp) &&
        (node.degree !== 3 ||
          !ControlFlowPortOptimizer.isToLeftPartition(lastIn.target, lastIn.source, ldp))) {
        factory.setTemporaryPortConstraint(lastIn, false, PortConstraint.create(PortSide.EAST));
      }
    }
  }

  optimizeForAlignment(graph: LayoutGraph, ldp: ILayoutDataProvider, itemFactory: IItemFactory, node2AlignWith: IDataProvider, edge2Length: IDataProvider):
  void {
    graph.nodes.forEach(node => {
      node.sortOutEdges(new PositionEdgeComparer(false, ldp));
      node.sortInEdges(new PositionEdgeComparer(true, ldp));
      const criticalInEdge = this.getCriticalInEdge(node, node2AlignWith, edge2Length);
      const criticalOutEdge = this.getCriticalOutEdge(node, node2AlignWith, edge2Length);
      if (criticalInEdge !== null || criticalOutEdge !== null) {
        this.optimizeWithCriticalEdges(node, ldp, itemFactory, criticalInEdge, criticalOutEdge);
      } else if (node.degree > 2) {
        this.optimizeWithoutCriticalEdges(node, ldp, itemFactory);
      }
      // Parallel edges of the critical edges which have a port constraints at the left or right side must have a
      // port constraint for the same side at the opposite end, too. Otherwise, such an edge gets many bends and
      // may even destroy the alignment.
      if (criticalInEdge !== null) {
        node.inEdges.forEach(edge => {
          if (criticalInEdge !== edge && criticalInEdge.source === edge.source) {
            const pc = ldp.getEdgeData(edge).targetPortConstraint;
            if (ControlFlowPortOptimizer.isFlatwisePortConstraint(pc)) {
              itemFactory.setTemporaryPortConstraint(edge, true, PortConstraint.create(pc.side));
            }
          }
        });
      }
      if (criticalOutEdge !== null) {
        node.outEdges.forEach(edge => {
          if (criticalOutEdge !== edge && criticalOutEdge.target === edge.target) {
            const pc = ldp.getEdgeData(edge).sourcePortConstraint;
            if (ControlFlowPortOptimizer.isFlatwisePortConstraint(pc)) {
              itemFactory.setTemporaryPortConstraint(edge, true, PortConstraint.create(pc.side));
            }
          }
        });
      }
    });
  }
}
