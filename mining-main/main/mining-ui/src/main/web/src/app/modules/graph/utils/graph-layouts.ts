import {
    HierarchicLayout,
    LayoutOrientation,
    HierarchicLayoutEdgeLayoutDescriptor,
    HierarchicLayoutRoutingStyle,
    HierarchicLayoutEdgeRoutingStyle,
    OrganicLayout,
    RadialLayout,
    CenterNodesPolicy,
    RadialLayoutLayeringStrategy,
    RadialLayoutEdgeRoutingStrategy,
    CircularLayout,
    CircularLayoutStyle,
    PartitionStyle,
    MultiStageLayout,
    HierarchicLayoutNodeLayoutDescriptor,
    ComponentArrangementPolicy,
    OrganicLayoutStarSubstructureStyle,
    CycleSubstructureStyle,
    OrganicLayoutScope,
    ChainSubstructureStyle,
    ParallelSubstructureStyle,
    BalloonLayout,
    TreeReductionStage,
    HierarchicLayoutData,
    PolylineEdgeStyle,
    IArrow,
    PartialLayout,
    PartialLayoutEdgeRoutingStrategy,
    PartialLayoutData,
    INode
} from 'yfiles';

import { cloneDeep } from 'lodash';

const MAXIMUM_DURATION = 1000;
export const LARGE_GRAPH_CUTOFF = 801;

const HIERARCHIC_LAYOUT = new HierarchicLayout({
    maximumDuration: MAXIMUM_DURATION,
    nodeToNodeDistance: 50,
    nodeToEdgeDistance: 25,
    edgeToEdgeDistance: 25,
    minimumLayerDistance: 50,
    layoutOrientation: LayoutOrientation.TOP_TO_BOTTOM,
    nodeLayoutDescriptor: new HierarchicLayoutNodeLayoutDescriptor({
        minimumDistance: 25,
        minimumLayerHeight: 0
    }),
    edgeLayoutDescriptor: new HierarchicLayoutEdgeLayoutDescriptor({
        minimumFirstSegmentLength: 15,
        minimumLastSegmentLength: 15,
        minimumLength: 20,
        minimumDistance: 25,
        minimumSlope: 0.25,
        routingStyle: new HierarchicLayoutRoutingStyle(HierarchicLayoutEdgeRoutingStyle.ORTHOGONAL),
        targetPortOptimization: true,
        sourcePortOptimization: true
    }),
    componentArrangementPolicy: ComponentArrangementPolicy.TOPMOST,
    considerNodeLabels: true,
    integratedEdgeLabeling: true,
    gridSpacing: 10
});

const HIERARCHIC_LAYOUT_LEFT_TO_RIGHT = cloneDeep(HIERARCHIC_LAYOUT);
HIERARCHIC_LAYOUT_LEFT_TO_RIGHT.layoutOrientation = LayoutOrientation.LEFT_TO_RIGHT;

export const HIERARCHIC_LAYOUT_DATA = new HierarchicLayoutData({
    edgeThickness: (edge) => {
        const style = edge.style;
        if (style instanceof PolylineEdgeStyle) {
          return style.stroke.thickness;
        }
        return 0;
    },
    edgeDirectedness: (edge) => {
        const edgeStyle = edge.style as PolylineEdgeStyle;
        if ((edgeStyle.targetArrow && edgeStyle.targetArrow !== IArrow.NONE)) {
          return 1;
        }
        return 0;
    }
});

const ORGANIC_LAYOUT = new OrganicLayout({
    scope: OrganicLayoutScope.ALL,
    preferredEdgeLength: 500,
    considerNodeLabels: true,
    minimumNodeDistance: 100,
    nodeEdgeOverlapAvoided: true,
    compactnessFactor: 0.2,
    maximumDuration: MAXIMUM_DURATION,
    considerNodeSizes: true,
    cycleSubstructureStyle: CycleSubstructureStyle.CIRCULAR,
    chainSubstructureStyle: ChainSubstructureStyle.NONE,
    starSubstructureStyle: OrganicLayoutStarSubstructureStyle.CIRCULAR,
    parallelSubstructureStyle: ParallelSubstructureStyle.NONE
});

const BALLOON_LAYOUT = new BalloonLayout();
BALLOON_LAYOUT.appendStage(new TreeReductionStage());

const RADIAL_LAYOUT = new RadialLayout({
    centerNodesPolicy: CenterNodesPolicy.WEIGHTED_CENTRALITY,
    layeringStrategy: RadialLayoutLayeringStrategy.BFS,
    minimumLayerDistance: 100,
    minimumNodeToNodeDistance: 50,
    edgeRoutingStrategy: RadialLayoutEdgeRoutingStrategy.POLYLINE,
    minimumBendAngle: 10
});

const CIRCULAR_LAYOUT = new CircularLayout({
    layoutStyle: CircularLayoutStyle.BCC_COMPACT,
    subgraphLayoutEnabled: false,
    partitionStyle: PartitionStyle.CYCLE,
    placeChildrenOnCommonRadius: true
});

export const PARTIAL_LAYOUT = new PartialLayout({
    considerNodeAlignment: true,
    edgeRoutingStrategy: PartialLayoutEdgeRoutingStrategy.STRAIGHTLINE
});

export const PARTIAL_LAYOUT_DATA = new PartialLayoutData({
    affectedNodes: (node: INode): boolean => node.tag.info.isClue,
});

export enum LayoutType {
    'HIERARCHIC_LAYOUT' = 'Hierarchical',
    'HIERARCHIC_LAYOUT_LEFT_TO_RIGHT' = 'Left-to-Right',
    'ORGANIC_LAYOUT' = 'Organic',
    'BALLOON_LAYOUT' = 'Balloon',
    'RADIAL_LAYOUT' = 'Radial',
    'CIRCULAR_LAYOUT' = 'Cyclic'
}

export const LAYOUT_LIMIT_MAP: { [key in keyof typeof LayoutType]: number} = {
    'HIERARCHIC_LAYOUT': LARGE_GRAPH_CUTOFF,
    'HIERARCHIC_LAYOUT_LEFT_TO_RIGHT': LARGE_GRAPH_CUTOFF,
    'ORGANIC_LAYOUT': 0,
    'BALLOON_LAYOUT': 0,
    'RADIAL_LAYOUT': LARGE_GRAPH_CUTOFF,
    'CIRCULAR_LAYOUT': 0
};

export const LAYOUT_MAP: { [key in keyof typeof LayoutType]: MultiStageLayout} = {
    HIERARCHIC_LAYOUT,
    HIERARCHIC_LAYOUT_LEFT_TO_RIGHT,
    ORGANIC_LAYOUT,
    BALLOON_LAYOUT,
    RADIAL_LAYOUT,
    CIRCULAR_LAYOUT
};
