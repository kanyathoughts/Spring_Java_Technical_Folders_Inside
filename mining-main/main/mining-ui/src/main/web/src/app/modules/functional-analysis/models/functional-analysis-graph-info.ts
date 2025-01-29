import { ControlFlowEdge } from '@innowake/mining-api-angular-client';
import { FunctionalAnalysisNode } from './functional-analysis-node';

/**
 * Class that contains the graph information, all the nodes and edges being displayed.
 */
export class FunctionalAnalysisGraphInfo {
    private nodes: FunctionalAnalysisNode[];
    private edges: ControlFlowEdge[];
    private groups: FunctionalAnalysisNode[];

    constructor() {
        this.nodes = [];
        this.edges = [];
        this.groups = [];
    }

    get graphEdges(): ControlFlowEdge[] {
        return this.edges;
    }
    get graphNodes(): FunctionalAnalysisNode[] {
        return this.nodes;
    }
    get graphGroups(): FunctionalAnalysisNode[] {
        return this.groups;
    }
    set graphNodes(nodes: FunctionalAnalysisNode[]) {
        this.nodes = nodes;
    }
    set graphEdges(edges: ControlFlowEdge[]) {
        this.edges = edges;
    }
    set graphGroups(groups: FunctionalAnalysisNode[]) {
        this.groups = groups;
    }
}
