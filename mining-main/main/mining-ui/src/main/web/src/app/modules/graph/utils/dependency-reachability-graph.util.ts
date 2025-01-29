import {
  Color, FilteredGraphWrapper, GraphComponent, GraphOverviewComponent, IGraph,
  ILayoutAlgorithm, INode, LayoutData, LayoutExecutor, RenderModes,
  TimeSpan, WebGLTaperedEdgeStyle
} from 'yfiles';
import { FastGraphModelManager, OptimizationMode } from './yfiles-util/fast-graph-model-manager';
import { FilterParameters, YFileGraphInfo, YNode } from '../models/yfile-graph-info.model';
import { ModuleRelationshipPojo } from '@innowake/mining-api-angular-client';
import { OverviewPanelVisualCreator } from './yfiles-util/overview-panel-visual-creator';
import { GeometryUtility } from './yfiles-util/geometry-utility';
import { NODE_CONFIG, NodeHeightSizes, NodeWidthSizes } from './node-configurations';
import CollapseAndExpandNodes from './yfiles-util/collapse-expand';
import { EllipsisPipe } from '@app/shared/pipes/ellipsis-pipe';
import { GraphUtility } from '../dependency/utils/dependency-graph-utility';
import { DepedendencyGraphEdgeMetaData } from '../dependency/dependency-graph/graph-edge-meta-data/graph-edge-meta-data.interface';

const NODE_ZOOM_VALUE = 1;

export const getNodeLevels = (rootIds: Set<string>, biDirectionalMap: Map<string, string[]>): Map<string, number> => {
  const nodeIdToLevelMap: Map<string, number> = new Map();
  const marked: Set<string> = new Set();
  rootIds.forEach(n => marked.add(n));
  const queue: string[] = [];
  rootIds.forEach(n => queue.push(n));
  rootIds.forEach(n => nodeIdToLevelMap.set(n, 0));
  while (queue.length > 0) {
    const root = queue.shift();
    if (biDirectionalMap.has(root)) {
      for (const child of biDirectionalMap.get(root)) {
        if (!marked.has(child)) {
          queue.push(child);
          nodeIdToLevelMap.set(child, nodeIdToLevelMap.get(root) + 1);
          marked.add(child);
        }
      }
    }
  }
  return nodeIdToLevelMap;
};

export const installGraphModelManager = (graphComponent: GraphComponent): void => {
  const graphModelManager = new FastGraphModelManager(graphComponent, graphComponent.contentGroup);
  graphModelManager.overviewEdgeStyle = new WebGLTaperedEdgeStyle({
    thickness: 10,
    color: new Color(100, 100, 100)
  });
  graphModelManager.graphOptimizationMode = OptimizationMode.LEVEL_OF_DETAIL;
  graphComponent.graphModelManager = graphModelManager;
};

export const getGraphLayoutExecutor = (
  graphComponent: GraphComponent,
  layout: ILayoutAlgorithm,
  layoutData: LayoutData,
  toggledNode: INode,
  isOnLoad: boolean,
  animateViewport: boolean = true
): LayoutExecutor => {
  const layoutExecutor = new LayoutExecutor({
    graphComponent,
    layout,
    layoutData
  });
  if (!isOnLoad) {
    layoutExecutor.duration = TimeSpan.fromMilliseconds(300);
    layoutExecutor.animateViewport = animateViewport && !toggledNode;
  }
  return layoutExecutor;
};

export const setGraphOverviewPanel = (graphComponent: GraphComponent, overviewComponent: GraphOverviewComponent): void => {
  if (overviewComponent) {
    overviewComponent.cleanUp();
  }
  overviewComponent = new GraphOverviewComponent('overviewComponentRef', graphComponent);
  overviewComponent.renderMode = RenderModes.CANVAS;
  overviewComponent.graphVisualCreator = new OverviewPanelVisualCreator(graphComponent.graph);
};

export const zoomToRootNodes = (graphComponent: GraphComponent, graphRootNodes: INode[]): void => {
  graphComponent.fitGraphBounds();
  if (graphRootNodes.length === 0) {
    return;
  } else if (graphRootNodes.length === 1) {
    graphComponent.zoomTo(graphRootNodes[0].layout.toPoint(), NODE_ZOOM_VALUE);
  } else {
    const closestNode = GeometryUtility.findClosestNode(graphRootNodes[0], graphRootNodes.slice(1));
    const sourroundingRect = GeometryUtility.findSurroundingRect([graphRootNodes[0], closestNode], NodeWidthSizes.L, NodeHeightSizes.S);
    const prevMaxZoom = graphComponent.maximumZoom;
    graphComponent.maximumZoom = NODE_ZOOM_VALUE; /* Setting maximum zoom effectively gives the rectangle a minimum size */
    graphComponent.zoomTo(sourroundingRect);
    graphComponent.maximumZoom = prevMaxZoom;
  }
};

export const reEvaluateLazyLoadingNodes = (graph: IGraph, collapseAndExpandNodes: CollapseAndExpandNodes): void => {
  const initialNodeCount = graph.nodes.size;
  const filteredGraph = graph as FilteredGraphWrapper;
  for (let i = 0; i < initialNodeCount; i++) {
    const node = graph.nodes.get(i);
    setParentAndChildrenVisible(node, filteredGraph, collapseAndExpandNodes);
  }
  filteredGraph.nodePredicateChanged();
};

export const setParentAndChildrenVisible = (node: INode, filteredGraph: FilteredGraphWrapper, collapseAndExpandNodes: CollapseAndExpandNodes): void => {
  setNodeVisibility(node, true, collapseAndExpandNodes);
  collapseAndExpandNodes.getConnectedNodes(filteredGraph.wrappedGraph, node, (filteredNode: INode) => {
    collapseAndExpandNodes.getCollapsed(filteredNode);
  }).forEach((connectedNode: INode) => {
    setNodeVisibility(connectedNode, true, collapseAndExpandNodes);
  });
};

export const setNodeVisibility = (node: INode, isVisible: boolean, collapseAndExpandNodes: CollapseAndExpandNodes): void => {
  (node.style as any).styleTag = { collapsed: !isVisible };
  collapseAndExpandNodes.setCollapsed(node, !isVisible);
  collapseAndExpandNodes.setNodeVisibility(node, isVisible);
};

export const getValuesFromUncheckedOptions = (filterUncheckedOptions: FilterParameters): string[] => {
  const values: string[] = [];
  if (filterUncheckedOptions) {
    if (filterUncheckedOptions.moduleFilter && filterUncheckedOptions.moduleFilter.length) {
      filterUncheckedOptions.moduleFilter.forEach(option => values.push(option.replace(/_/g, ' ')));
    }
    if (filterUncheckedOptions.relationshipFilter && filterUncheckedOptions.relationshipFilter.length) {
      filterUncheckedOptions.relationshipFilter.forEach(option => values.push(option));
    }
  }
  return values;
};

export const getTechnologyTypeForModule = (id: string, graphInfo: YFileGraphInfo): DepedendencyGraphEdgeMetaData[] => (
  graphInfo.nodes.filter((nodeItem) => nodeItem.uid === id)
    .map((filterItem) => ({
      type: filterItem.type, technology: filterItem.technology, module: filterItem.name,
      moduleId: filterItem.id, shortName: new EllipsisPipe().transform(filterItem.name, 8)
    }))
);

export const addNodeAndEdge = (newNodes: YNode[], newEdges: ModuleRelationshipPojo[], node: YNode, direction: string): void => {
  const info: { [key: string]: any; } = { isClue: 'true', belongsToId: node.uid };
  const newNode = { info, id: GraphUtility.getNewClueNodeId(), name: '0', style: NODE_CONFIG['VISUAL_CLUE'] };
  newNodes.push(newNode);
  const properties: { [key: string]: object; } = { isClue: 'true' as any };

  const newRelationship: ModuleRelationshipPojo = direction === GraphUtility.IN ?
    { srcModule: newNode.id.toString(), dstModule: node.uid, id: GraphUtility.getNewClueEdgeId().toString(), properties }
    : { srcModule: node.uid, dstModule: newNode.id.toString(), id: GraphUtility.getNewClueEdgeId().toString(), properties };
  newEdges.push(newRelationship);
};
// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export const getRelationshipLabel = (link: any): any => {
  const fileAccessType = link['properties']?.['FILE_ACCESS_TYPE'];
  if (link?.properties && link.properties?.TYPE && link.properties.TYPE?.indexOf('RA_FROM_SCHEDULER_INFO') > -1) {
    return '';
  }

  const dbAccessType = link['properties']?.['DB_ACCESS_TYPE'];
  if (link.relationship === 'ACCESSES') {
    if (fileAccessType) {
      return fileAccessType.toString().charAt(0) !== '[' ?
        fileAccessType.toString().replace(',', ', ')
        : JSON.parse(fileAccessType.toString() as string)?.join(', ');
    } else if (dbAccessType) {
      return dbAccessType.toString().charAt(0) !== '[' ?
        dbAccessType.toString().replace(',', ', ')
        : JSON.parse(dbAccessType.toString() as string)?.join(', ');
    }
  }
  return link.relationship;
};
