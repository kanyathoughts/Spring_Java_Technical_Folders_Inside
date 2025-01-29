import { ControlFlowEdge } from '@innowake/mining-api-angular-client';
import { ControlFlowNodeDetails } from './control-flow-node-details';

/**
 * Class that contains the graph information, all the nodes and edges being displayed.
 */
export class ControlFlowGraphInfo {
    private nodes: ControlFlowNodeDetails[];
    private edges: ControlFlowEdge[];
    private groups: ControlFlowNodeDetails[];

    constructor() {
        this.nodes = [];
        this.edges = [];
        this.groups = [];
    }

    get graphEdges(): ControlFlowEdge[] {
        return this.edges;
    }
    get graphNodes(): ControlFlowNodeDetails[] {
        return this.nodes;
    }
    get graphGroups(): ControlFlowNodeDetails[] {
        return this.groups;
    }
    set graphNodes(nodes: ControlFlowNodeDetails[]) {
        this.nodes = nodes;
    }
    set graphEdges(edges: ControlFlowEdge[]) {
        this.edges = edges;
    }
    set graphGroups(groups: ControlFlowNodeDetails[]) {
        this.groups = groups;
    }
}
