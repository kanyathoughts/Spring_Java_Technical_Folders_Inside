import { EllipsisPipe } from '@app/shared/pipes/ellipsis-pipe';
import { CfgNodeSuperType, CfgNodeType } from './functional-analysis-node';
import { ControlFlowNode } from '@innowake/mining-api-angular-client';

export const CHARACTER_LIMIT = 72;
let flag = false;

/**
 * Basic structure of a node to be displayed in the Control Flow Graph.
 */
export class FunctionlAnalysisCollapsibleNode implements ControlFlowNode {
    fromLink: string[];
    toLink: string[];
    parent: string;
    children: string[];
    properties: { [key: string]: object; };
    offset: number;
    label: string;
    recordId: string;
    type: string;
    entity: ControlFlowNode.EntityEnum;
    superTypes: Set<string>;
    public description: string;
    private readonly isEntryExit: boolean;
    private readonly isBranch: boolean;
    private readonly nodeShapeType: string;
    private readonly descLength: number;

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    constructor(astNode: ControlFlowNode, moduleName: string) {
        this.recordId = astNode.id;
        this.type = astNode.type;
        this.superTypes = astNode.superTypes;
        this.entity = astNode.entity;
        this.label = astNode.label ? astNode.label.replace(/\s+/g, ' ').trim() : astNode.label;
        this.description = new EllipsisPipe().transform(this.label, 72);
        this.nodeShapeType = 'process';
        this.offset = astNode.offset;

        if (! this.description) {
            this.description = this.type;
        }

        if (this.superTypes !== null &&
            (this.superTypes.has(CfgNodeSuperType.BRANCH_STATEMENT) || this.superTypes.has(CfgNodeSuperType.LOOP_STATEMENT))) {
            this.isBranch = true;
            this.nodeShapeType = 'decision';
        }

        if (this.type !== null && ( this.type === CfgNodeType.THEN_BLK || this.type === CfgNodeType.ELSE_BLK)) {
            this.description = this.type;
        }

        if (this.entity !== null && this.entity === ControlFlowNode.EntityEnum.TERMINAL) {
            this.isEntryExit = true;
            this.nodeShapeType = 'terminal';
            this.description = new EllipsisPipe().transform(this.label ?? moduleName, 72);
            this.label = this.label ?? moduleName;
        }
        this.parent = astNode.parent;
        this.children = [];
        this.properties = astNode.properties;
        this.descLength = this.description.length;
        if (this.properties && this.properties['name']) {
            this.description = this.properties['name'].toString();
        }
    }

    get nodeRecordId(): string {
        return this.recordId;
    }

    get nodeDescLength(): number {
        return this.descLength;
    }

    get nodeType(): string {
        return this.type;
    }
    get nodeEntity(): string {
        return this.entity;
    }
    get isNodeEntryExit(): boolean {
        return this.isEntryExit;
    }
    get isNodeBranch(): boolean {
        return this.isBranch;
    }
    get nodeDescription(): string {
        return this.description;
    }
    get shapeType(): string {
        return this.nodeShapeType;
    }
    get fromLinks(): string[] {
        return this.fromLink;
    }
    get toLinks(): string[] {
        return this.toLink;
    }
    get parentId(): string {
        return this.parent;
    }
    get childrenIds(): string[] {
        return this.children;
    }
    get propertyMap(): { [key: string]: object; } {
        return this.properties;
    }
    get nodeOffset(): number {
        return this.offset;
    }
    set nodeRecordId(recordId: string) {
        this.recordId = recordId;
    }
    set nodeType(type: string) {
        this.type = type;
    }
    set nodeEntity(entity: ControlFlowNode.EntityEnum) {
        this.entity = entity;
    }
    set nodeDescription(nodeDescription: string) {
        this.nodeDescription = nodeDescription.replace(/\s+/g, ' ').trim();
    }
    set fromLinks(fromLink: string[]) {
        this.fromLink = fromLink;
    }
    set toLinks(toLink: string[]) {
        this.toLink = toLink;
    }
    set parentId(parent: string) {
        this.parent = parent;
    }
    set childrenIds(children: string[]) {
        this.children = children;
    }
    set propertyMap(properties: { [key: string]: object; }) {
        this.properties = properties;
    }
    set nodeOffset(offset: number) {
        this.offset = offset;
    }

    changeDescription(): void {
        if (!flag) {
            this.description = this.label.replace(/\s+/g, ' ').trim();
            flag = true;
        } else {
            this.description = new EllipsisPipe().transform(this.label.replace(/\s+/g, ' ').trim(), 72);
            flag = false;
        }
    }
}
