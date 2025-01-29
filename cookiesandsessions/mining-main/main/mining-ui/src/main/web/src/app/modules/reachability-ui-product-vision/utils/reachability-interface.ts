import { ReachabilityBlockGraphFilterRequest, TechnologyType } from '@innowake/mining-api-angular-client';

export interface ReachabilityBlocks {
    uid?: string;
    name?: string;
    description?: string;
    status?: string;
    type?: string[];
    contains?: LowerBoundReachability[];
    sharedModule?: LowerBoundReachability[];
    totalCount?: number;
    isSelected?: boolean;
    upperBound?: UpperBoundReachability[];
    lowerBound?: LowerBoundReachability[];
    linesOfCode?: number;
    complexity?: string;
    resolvedModuleParts?: Array<Record<string, string>>;
    outdatedModule?: boolean;
    deletedModule?: boolean;
    blockState?: ReachabilityBlockState;
    upperTaxonomies?: Array<{name: string, id: string}>;
};

export interface ReachabilityBlockState {
    errorCount: number;
    warningsCount: number;
}

export interface UpperBoundReachability {
    name?: string;
    id?: number;
    linkHash?: string;
    technology?: string;
    type?: string;
    taxonomy?: TaxonomyItem[];
    upperTaxonomies?: Array<{name: string, id: string}>;
};

/**
 * Interface for merge block data in merge modal
 */
export interface MergeBlockData {
    blockName: string,
    blockDescription: string
}

export interface LowerBoundReachability {
    aggregations?: Aggregation[];
}

export interface Aggregation {
    fields: {[key: string]: {COUNT: number;};};
    groupBy: {REFERENCED_MODULE_TYPE: string, REFERENCED_MODULE_TECHNOLOGY: string};
}

export interface ReferencedTaxonomies {
    id?: number;
    name?: string;
};

/**
 * Interface used for Taxonomy and Module mapping.
 */
export interface TaxonomyItem {
    moduleId?: number;
    referencedTaxonomies: Array<{name: string}>;
}

/**
 * Interface for all filters used in reachability for both block/network view.
 */
export interface ReachabilityFilter {
    taxonomies?: string[] | number[];
    functionalBlocks?: string[];
    modulesIds?: string[];
    type?: string[];
    technology?: string[];
    referenceType?: string[];
    excludeParentTypes?: string[];
};

/**
 * Interface for all filters used in reachability block view.
 */
export interface BlockViewFilter {
    reachabilityFilter?: ReachabilityFilter;
    inActiveSwitch?: boolean;
};

/**
 * Interface for all filters used in reachability graph.
 */
export interface ReachabilityGraphFilter {
    taxonomyIds?: string[] | number[];
    functionalBlockIds?: string[];
    technologyTypes?: TechnologyType[];
    relationshipTypes?: ReachabilityBlockGraphFilterRequest.RelationshipTypesEnum[];
};

/**
 * Interface for module search filter in reachability in & functional analysis.
 */
export interface ModuleSearchFilter {
    value: string | number | number[];
    label: string;
    path?: string;
    description?: string;
};

/**
 * Interface for list of technology types.
 */
export interface ListOfTechnologyType {
    label: string,
    value: TechnologyType
}

/**
 * Interface for list of functional blocks
 */
export interface ListOfFunctionalBlocks {
    name: string,
    uid: string
}

/**
 * Interface for call chain details of a reachability block.
 */
export interface ReachabilityCallchainData {
    name: string;
    callChain: { content: [{uid: string}]}
};
