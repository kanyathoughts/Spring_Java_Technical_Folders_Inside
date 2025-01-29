import {
    GraphComponent,
    HashMap,
    List,
    PlaceNodesAtBarycenterStageData,
    PlaceNodesAtBarycenterStage,
    FixNodeLayoutData,
    HierarchicLayout,
    LayoutMode,
    HierarchicLayoutData,
    INode,
    TemplateNodeStyleBase,
    IGraph,
    CompositeLayoutData,
    MultiStageLayout,
    OrganicLayout,
    OrganicLayoutScope,
    Bfs,
    OrganicLayoutData,
    FilteredGraphWrapper
} from 'yfiles';

/**
 * Class to enable collapsing and expanding the graph based on the level of the node.
 */
export default class CollapseAndExpandNodes {
    graphComponent: GraphComponent;
    nodeCollapsedMap: HashMap<INode, boolean>;
    nodeVisibility: HashMap<number, boolean>;

    constructor(graphComponent: GraphComponent) {
        this.nodeCollapsedMap = new HashMap();
        this.nodeVisibility = new HashMap();
        this.graphComponent = graphComponent;
    }

    setCollapsed(node: INode, collapsed: boolean): void {
        this.nodeCollapsedMap.set(node, collapsed);
    }

    getCollapsed(node: INode): boolean {
        return !! this.nodeCollapsedMap.get(node);
    }

    setNodeVisibility(node: INode, visible: boolean): void {
        this.nodeVisibility.set(node.tag.id as number, visible);
    }

    getNodeVisibility(node: INode): boolean {
        if (node.tag?.info?.isClue) {
            return +node.tag.name > 0 && this.nodeVisibility.get(+node.tag.info['belongsToId']);
        }

        const vis = this.nodeVisibility.get(node.tag.id as number);
        if (vis === null) {
            this.nodeVisibility.set(node.tag.id as number, false);
        }
        return !! this.nodeVisibility.get(node.tag.id as number);
    }

    /**
     * Method to expand the specified node and show it's children.
     *
     * @param node - the node to expand
     */
    expand(node: INode): void {
        // Stores the collapsed state of the node in the style tag in order
        // to be able to bind to it using a template binding.
        (node.style as TemplateNodeStyleBase).styleTag = { collapsed: false };
        this.nodeCollapsedMap.set(node, false);
        const filteredGraph = this.graphComponent.graph as FilteredGraphWrapper;
        this.getConnectedNodes(filteredGraph.wrappedGraph, node, (successor: INode) =>
            this.nodeCollapsedMap.get(successor)
        ).forEach((successor: INode) => {
            this.nodeVisibility.set(successor.tag.id as number, true);
        });
    }

    /**
     * Method to collapse the specified node and hide it's children.
     *
     * @param node - the node to collapse
     */
    collapse(node: INode): void {
        (node.style as TemplateNodeStyleBase).styleTag = { collapsed: true };
        this.nodeCollapsedMap.set(node, true);
        const filteredGraph = this.graphComponent.graph as FilteredGraphWrapper;
        this.getConnectedNodes(filteredGraph.wrappedGraph, node, (successor: INode) =>
            this.nodeCollapsedMap.get(successor)
        ).forEach((successor: INode) => {
            this.nodeVisibility.set(successor.tag.id as number, false);
        });
    }

    /**
     * Method to get all nodes connected to the given node in the given graph.
     *
     * @param graph - Graph to search for the predecessors and successors
     * @param node - Node to search the connected nodes for
     * @param recursionFilter - A filter, if needed, to filter out the connected nodes
     */
    getConnectedNodes(graph: IGraph, node: INode, recursionFilter?: (node: INode) => any): List<INode> {
        const visited = new HashMap();
        const descendants = new List<INode>();
        const nodes = [node];
        const nodesPresent: number[] = [];
        let nodeId: number;
        this.graphComponent.graph.nodes.forEach(visibleNode => {
            nodeId = visibleNode.tag.id;
            nodesPresent.push(nodeId);
        });
        while (nodes.length > 0) {
            const n = nodes.pop();
            graph.successors(n).forEach(s => {
                nodeId = s.tag.id;
                if ( ! visited.get(s) && ! nodesPresent.includes(nodeId)) {
                    visited.set(s, true);
                    descendants.add(s);
                    if (recursionFilter == null || ! recursionFilter(s)) {
                        nodes.push(s);
                    }
                }
            });
            graph.predecessors(n).forEach(p => {
                nodeId = p.tag.id;
                if ( ! visited.get(p) && ! nodesPresent.includes(nodeId)) {
                    visited.set(p, true);
                    descendants.add(p);
                    if (recursionFilter == null || ! recursionFilter(p)) {
                        nodes.push(p);
                    }
                }
            });
        }
        return descendants;
    }

    /**
     * Method to configure the layout so that it applies it in an incremental fashion.
     *
     * @param toggledNode - node thats supposed to be kept at its location
     * @param expand - boolean flag if the graph was expanded
     * @param currentLayoutData - layout data to configure for incremental layout application
     * @param currentLayout - layout to apply
     */
    configureLayout(toggledNode: INode, expand: boolean, currentLayoutData: CompositeLayoutData, currentLayout: MultiStageLayout): void {
        const graph = this.graphComponent.graph;
        if (toggledNode) {
            // Keep the clicked node at its location
            currentLayoutData.items.add(
                new FixNodeLayoutData({
                    fixedNodes: toggledNode
                })
            );
            const incrementalNodes = this.getIncrementalNodes(graph, toggledNode);
            const incrementalMap: HashMap<INode, boolean> = new HashMap();
            incrementalNodes.forEach((node: INode) => {
                incrementalMap.set(node, true);
            });
            if (expand) {
                // move the incremental nodes between their neighbors before expanding for a smooth animation
                this.prepareSmoothExpandLayoutAnimation(incrementalMap);
            } else {
                // configure PlaceNodesAtBarycenterStage for a smooth animation
                currentLayoutData.items.add(
                    new PlaceNodesAtBarycenterStageData({
                        affectedNodes: node => incrementalMap.has(node)
                    })
                );
            }
            if (currentLayout instanceof OrganicLayout) {
                currentLayout.compactnessFactor = 0.7;
                currentLayout.preferredEdgeLength = 60;
                currentLayout.considerNodeSizes = false;
                currentLayout.nodeOverlapsAllowed = false;
                currentLayout.minimumNodeDistance = 10;
                currentLayout.qualityTimeRatio = 1;
                currentLayout.maximumDuration = 1000 + graph.nodes.size * 50;
                currentLayout.scope = OrganicLayoutScope.ALL;
                const layerIds = new Bfs({
                  coreNodes: incrementalNodes.concat(toggledNode) as List<INode>,
                  traversalDirection: 'both'
                }).run(graph).nodeLayerIds;
                currentLayoutData.items.add(
                  new OrganicLayoutData({
                    nodeInertia: obj => 1 - 1 / (layerIds.get(obj) + 1),
                    nodeStress: obj => 1 / (layerIds.get(obj) + 1)
                  })
                );
            } else if (currentLayout instanceof HierarchicLayout) {
                currentLayout.layoutMode = LayoutMode.INCREMENTAL;
                currentLayoutData.items.add(
                    new HierarchicLayoutData({
                        incrementalHints: this.getIncrementalHints(incrementalNodes) as any
                    })
                );
            }
        } else {
            if (currentLayout instanceof OrganicLayout) {
                currentLayout.scope = OrganicLayoutScope.ALL;
            } else if (currentLayout instanceof HierarchicLayout) {
                currentLayout.layoutMode = LayoutMode.FROM_SCRATCH;
            }
        }
    }

    private prepareSmoothExpandLayoutAnimation(incrementalMap: HashMap<INode, boolean>) {
        const graph = this.graphComponent.graph;
        // mark the new nodes and place them between their neighbors
        const layoutData = new PlaceNodesAtBarycenterStageData({
            affectedNodes: node => incrementalMap.has(node)
        });
        const layout = new PlaceNodesAtBarycenterStage();
        graph.applyLayout(layout, layoutData);
    }

    private getIncrementalHints(incrementalNodes: any) {
        return (item: any, hintsFactory: any) => {
            if (incrementalNodes.includes(item)) {
                return hintsFactory.createLayerIncrementallyHint(item);
            }
        };
    }

    private getIncrementalNodes(graph: IGraph, node: INode): List<INode> {
        /* Note: This method could not be combined with getConnectedNodes() as it was causing the broswer page to crash. */
        const visited = new HashMap();
        const descendants = new List<INode>();
        const nodes = [node];
        while (nodes.length > 0) {
            const n = nodes.pop();
            graph.successors(n).forEach(s => {
                if ( ! visited.get(s)) {
                    visited.set(s, true);
                    descendants.add(s);
                }
            });
            graph.predecessors(n).forEach(p => {
                if ( ! visited.get(p)) {
                    visited.set(p, true);
                    descendants.add(p);
                }
            });
        }
        return descendants;
    }
}
