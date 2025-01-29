import { DataLineageNode } from './data-lineage.node';
import { DataLineageEdge } from './data-lineage.edge';
import { DataLineageGroupNode } from './data-lineage.groupnode';
/**
 * Class that contains the graph information, all the nodes and edges being displayed.
 */
export class DataLineageGraphInfo {
    private nodes: DataLineageNode[];
    private edges: DataLineageEdge[];
    private groupNodes: DataLineageGroupNode[];

    constructor() {
        this.nodes = [];
        this.groupNodes = [];
        this.edges = [];
    }

    get graphEdges(): DataLineageEdge[] {
        return this.edges;
    }
    get graphNodes(): DataLineageNode[] {
        return this.nodes;
    }
    get graphGroups(): DataLineageGroupNode[] {
        return this.groupNodes;
    }
    set graphNodes(nodes: DataLineageNode[]) {
        this.nodes = nodes;
    }
    set graphEdges(edges: DataLineageEdge[]) {
        this.edges = edges;
    }
    set graphGroups(groups: DataLineageGroupNode[]) {
        this.groupNodes = groups;
    }
}
