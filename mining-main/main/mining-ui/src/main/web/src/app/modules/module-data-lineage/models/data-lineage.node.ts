import { DataFlowGraphNodesInner, ModuleLocation } from '@innowake/mining-api-angular-client';

interface SourceLocation {
    moduleId: number;
    moduleName: string;
    moduleLocation: ModuleLocation;
}

/**
 * Basic structure of a node to be displayed in Data Lineage graph.
 */
export class DataLineageNode {
    type?: string;
    name?: string;
    id: string;
    moduleId?: number;
    location?: ModuleLocation;
    incomings?: string[];
    outgoings?: string[];
    children?: string[];
    sourceLocation?: SourceLocation;
    group?: string;
    parentModule?: string;
    offset?: number;
    length?: number;

    constructor(node: DataFlowGraphNodesInner, groupName: string) {
        this.type = node.type;
        this.name = node.name;
        this.id = node.id;
        this.moduleId = (node as any).moduleId;
        this.incomings = Array.from(node.incomings);
        this.outgoings = Array.from(node.outgoings);
        this.children = Array.from(node.children);
        this.group = groupName;
        this.parentModule = node.parentModule;
        if (node.sourceLocation !== null) {
            this.sourceLocation = {
                moduleId: node.sourceLocation.moduleId,
                moduleName: node.sourceLocation.moduleName,
                moduleLocation: node.sourceLocation.moduleLocation
            };
            this.offset = node.sourceLocation.moduleLocation.offset;
            this.length = node.sourceLocation.moduleLocation.length;
        } else if (node.location !== null) {
            this.offset = node.location.offset;
            this.length = node.location.length;
        }
    }

    get nodeType(): string {
        return this.type;
    }

    get nodeName(): string {
        return this.name;
    }

    get nodeId(): string {
        return this.id;
    }

    get nodeModuleId(): number {
        return this.moduleId;
    }

    set nodeType(type: string) {
        this.type = type;
    }

    set nodeName(name: string) {
        this.name = name;
    }

    set nodeId(id: string) {
        this.id = id;
    }

    set nodeModuleId(moduleId: number) {
        this.moduleId = moduleId;
    }
}
