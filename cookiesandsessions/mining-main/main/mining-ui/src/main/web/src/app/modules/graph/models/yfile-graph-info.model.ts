import { NodeType } from '../utils/node-configurations';
import { NodeColor } from '../utils/node-colors';
import { ModulePojo, ModuleRelationshipPojo } from '@innowake/mining-api-angular-client';

export interface YNode extends ModulePojo {
    style?: { color: NodeColor, imageUrl: string };
    isRoot?: boolean;
    level?: number;
    isLeaf?: boolean;
    peerCount?: string;
}

export interface FilterParameters {
    moduleFilter: string[];
    relationshipFilter: string[];
}

export class YFileGraphInfo {
    nodes: YNode[];
    links: ModuleRelationshipPojo[];
    nodeTypes?: NodeType[];
    relationships?: string[];
    constructor(nodes: YNode[], links: ModuleRelationshipPojo[], nodeTypes?: NodeType[], relationships?: string[]) {
        this.nodes = nodes;
        this.links = links;
        if (nodeTypes) {
        this.nodeTypes = nodeTypes;
        }
        if (relationships) {
        this.relationships = relationships;
        }
    }
    get graphNodes(): YNode[] {
        return this.nodes;
    }
    get graphlinks(): ModuleRelationshipPojo[] {
        return this.links;
    }
    get graphNodeTypes(): NodeType[] {
        return this.nodeTypes;
    }
    get graphRelationships(): string[] {
        return this.relationships;
    }
    set graphNodes(nodes: YNode[]) {
        this.nodes = nodes;
    }
    set graphlinks(links: ModuleRelationshipPojo[]) {
        this.links = links;
    }
    set graphNodeTypes(nodeTypes: NodeType[]) {
        this.nodeTypes = nodeTypes;
    }
    set graphRelationships(relationships: string[]) {
        this.relationships = relationships;
    }
}
