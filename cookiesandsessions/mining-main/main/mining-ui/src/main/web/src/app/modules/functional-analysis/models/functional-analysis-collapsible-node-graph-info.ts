import { ControlFlowEdge } from '@innowake/mining-api-angular-client';
import { FunctionlAnalysisCollapsibleNode } from './functional-analysis-collapsible-node';

/**
 * Class that contains the graph information, all the nodes and edges being displayed.
 */
export class FunctionalAnalysisCollapsibleNodeGraphInfo {
    private nodes: FunctionlAnalysisCollapsibleNode[];
    private edges: ControlFlowEdge[];

    constructor() {
        this.nodes = [];
        this.edges = [];
    }

    get graphNodes(): FunctionlAnalysisCollapsibleNode[] {
        return this.nodes;
    }
    get graphEdges(): ControlFlowEdge[] {
        return this.edges;
    }
    set graphNodes(nodes: FunctionlAnalysisCollapsibleNode[]) {
        this.nodes = nodes;
    }
    set graphEdges(edges: ControlFlowEdge[]) {
        this.edges = edges;
    }
}
