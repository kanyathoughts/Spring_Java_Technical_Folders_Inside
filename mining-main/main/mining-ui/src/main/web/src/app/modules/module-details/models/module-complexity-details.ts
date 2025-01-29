/**
 * Interface to capture the complexity details for a Module
 * that can be displayed to the user.
 */
export interface ModuleComplexityDetails {
    color?: string;
    description: string;
}

/**
  Interface for object created in module details page for maintaining badges count
 */
export interface ModuleBadgesSum {
    annotationCount: number,
    dataDictionaryCount: number,
    dependencyCount: number,
    schemaFieldCount: number
}

/**
  Enum created for  updating the badge count on Page load
 */

export enum UpdateBadgeSum {
    'HAS_ANNOTATION' = 'HAS_ANNOTATION',
    'HAS_DATA_DICTIONARY_ENTRY' = 'HAS_DATA_DICTIONARY_ENTRY',
    'HAS_DEPENDENCY' = 'CALLS,INCLUDES,REFERENCES,READS_WRITES'
}

/**
  Enum created for operation of updating the badge count from other component
 */
export enum BadgeCountUpdateOperation {
    'ANNOTATION_DELETED' = 'annotationDeleted',
    'DATA_DICTIONARY_DELETED' = 'dataDictionaryDeleted',
    'DELETE_ALL_WARNINGS' = 'deleteAllWarnings',
}

