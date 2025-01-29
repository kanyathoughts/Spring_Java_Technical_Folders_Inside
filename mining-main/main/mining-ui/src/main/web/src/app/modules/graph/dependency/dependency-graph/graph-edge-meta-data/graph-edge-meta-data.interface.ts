import { EntityId } from '@innowake/mining-api-angular-client';

/**
 * Interface for technology and type data for module
 */
export interface DepedendencyGraphEdgeMetaData {
    type: string,
    technology: string,
    module: string,
    moduleId: number,
    shortName?: string
}
/**
 * Interface for artificial edge meta data
 */
export interface InputForArtificialEdge {
    depth: string[];
    startModule: string[];
    endModule: string[]
}

export interface RelationshipTableData {
    reference: string,
    fromNode: RelationshipModuleData,
    toNode: RelationshipModuleData,
    properties?: {
        [key: string]: object;
    };
}

interface RelationshipModuleData {
    name: string,
    id: EntityId,
    moduleShortName: string
}
