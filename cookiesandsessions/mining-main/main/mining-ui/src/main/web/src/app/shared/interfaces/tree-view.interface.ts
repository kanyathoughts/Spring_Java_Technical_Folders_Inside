/**
 * Interface for the TreeNode.
 */
export interface TreeNode {
    name: string;
    id?: number;
    children?: TreeNode[];
    checkedState?: boolean;
    key?: number;
    hyperlink?: string;
    state?: string;
}

/**
 * Interface for the FlatNode of tree view.
 */
export interface FlatNode {
    expandable: boolean;
    name: string;
    level: number;
    checkedState?: boolean;
    id?: number;
    state?: string;
}

export class FilteredTreeResult {
    constructor(public treeData: TreeNode[], public needsToExpanded: TreeNode[] = []) {}
}
