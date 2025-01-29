import { EllipsisPipe } from '@app/shared/pipes/ellipsis-pipe';
import { ControlFlowNode } from '@innowake/mining-api-angular-client';

export enum CfgNodeType {
    MOVE_STMT = 'CobolMoveStmt',
    IF_STMT = 'CobolIfStmt',
    STOP_STMT = 'CobolStopStmt',
    DISPLAY_STMT = 'CobolDisplayStmt',
    THEN_BLK = 'CobolThenBlock',
    ELSE_BLK = 'CobolElseBlock'
}

export enum CfgNodeSuperType {
    BRANCH_STATEMENT = 'BranchStatement',
    STATEMENT = 'statement',
    LOOP_STATEMENT = 'LoopStatement'
}

export const CHARACTER_LIMIT = 72;

/**
 * Basic structure of a node to be displayed in the Control Flow Graph.
 */
export class ControlFlowNodeDetails implements ControlFlowNode {
    parent: string;
    children: string[];
    properties: { [key: string]: object; };
    offset: number;
    label: string;
    recordId: string;
    type: string;
    entity: ControlFlowNode.EntityEnum;
    group: string;
    superTypes: Set<string>;
    skippable = false;
    length: number;
    public description: string;
    private readonly isEntryExit: boolean;
    private readonly isBranch: boolean;
    private readonly isBranchStatement: boolean;
    private readonly nodeShapeType: string;
    private readonly descLength: number;

    constructor(astNode: ControlFlowNode, moduleName: string, groupName: string, isGroup: boolean = false) {
        this.recordId = astNode.id;
        this.type = astNode.type;
        this.superTypes = astNode.superTypes;
        this.entity = astNode.entity;
        this.label = astNode.label;
        this.children = [];
        this.description = new EllipsisPipe().transform(this.removeSpaces(this.label), CHARACTER_LIMIT);
        if (astNode.type === 'CobolSizeErrorStatement') {
            if (/^ERROR.*NOT ON SIZE ERROR/.exec(this.description)) {
                this.description = 'OnSizeErrorBlock';
            } else if (this.description.includes('ERROR')) {
                this.description = 'NotOnSizeErrorBlock';
            }
        }
        this.nodeShapeType = 'process';
        this.offset = astNode.offset;
        this.length = astNode.length;
        this.group = groupName;

        if ( ! this.description) {
            this.description = this.type;
        }

        if (this.superTypes !== null &&
            ((Array.from(this.superTypes)).includes(CfgNodeSuperType.BRANCH_STATEMENT) ||
              (Array.from(this.superTypes)).includes(CfgNodeSuperType.LOOP_STATEMENT))) {
            this.isBranch = true;
            this.nodeShapeType = 'decision';
        }

        if (this.type !== null && (this.type === CfgNodeType.THEN_BLK || this.type === CfgNodeType.ELSE_BLK)) {
            this.isBranchStatement = true;
            this.description = this.type === CfgNodeType.THEN_BLK ? 'TRUE' : 'FALSE';
        }

        if (this.entity !== null && this.entity === ControlFlowNode.EntityEnum.TERMINAL) {
            this.isEntryExit = true;
            this.nodeShapeType = 'terminal';
            this.description = new EllipsisPipe().transform(this.removeSpaces(this.label ?? moduleName), CHARACTER_LIMIT);
            this.label = this.label ?? moduleName;
        }
        this.parent = astNode.parent;
        this.properties = astNode.properties;
        this.descLength = this.description.length;
        if (this.properties && this.properties['name']) {
            this.description = this.properties['name'].toString();
        }

        if (isGroup) {
            this.nodeShapeType = 'group';
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
    get nodeEntity(): ControlFlowNode.EntityEnum {
        return this.entity;
    }
    get isNodeEntryExit(): boolean {
        return this.isEntryExit;
    }
    get isNodeBranch(): boolean {
        return this.isBranch;
    }
    get isNodeBranchStatement(): boolean {
        return this.isBranchStatement;
    }
    get nodeDescription(): string {
        return this.description;
    }
    get shapeType(): string {
        return this.nodeShapeType;
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
        this.nodeDescription = nodeDescription;
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

    /**
     * Removes the extra spaces from the string other than the one enclosed with the single or double quotes.
     * @param text text from which spaces needed to be removed.
     */
    private removeSpaces(text: string) {
        if (text) {
            return text.replace(/("[^"]*")|('[^']*')|([ \t]+)|([\n\r]+)/g, (x) => (x.charCodeAt(0) === 34 || x.charCodeAt(0) === 39) ? x : ' ');
        }
        return text;
    }
}
