import { ClientPojo, ProjectPojo } from '@innowake/mining-api-angular-client';

/**
 * Interface for the Client Row in the mining table.
 */
export interface ClientRow extends ClientPojo {
    admin?: string;
    nature?: string;
    memberCount?: string;
    members?: string[];
    children?: any[];
    nonExpandableRowIndent?: number;
}

/**
 * Interface for the Project row in the mining table.
 */
export interface ProjectRow extends ProjectPojo {
    admin?: string;
    nature?: string;
    memberCount?: string;
    members?: string[];
    children?: any[];
    parent?: ClientRow;
}
