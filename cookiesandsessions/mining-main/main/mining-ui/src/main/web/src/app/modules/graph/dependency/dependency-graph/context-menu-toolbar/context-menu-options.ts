import { INode } from 'yfiles';

/**
 * Interfaces for the Edges options we are setting for incoming/outgoing options for filtration.
 * This is being used in the {ContextualToolbarComponent}.
 */
export interface EdgeOptions {
    edges: any[];
    filterOptions: Map<string, FilterOptions>;
}

export interface FilterOptions {
    count: number;
    checked: boolean;
    indeterminate: boolean;
}

export interface NodeVisibility {
    node: INode;
    visibility: boolean;
}

/**
 * Enum for changing the view of Context menu.
 */
export enum ContextToolbarType {
    DEFAULT = 'DEFAULT',
    REACHABILITY_NETWORK = 'REACHABILITY_NETWORK',
    REACHABILITY_BLOCK = 'REACHABILITY_BLOCK',
    FUNCTIONALANALYSIS = 'FUNCTIONALANALYSIS'
}

export enum RemoveBranchType {
    REMOVE_BRANCH = 'REMOVE_BRANCH',
    REMOVE_ALL_BRANCHES = 'REMOVE_ALL_BRANCHES',
}
