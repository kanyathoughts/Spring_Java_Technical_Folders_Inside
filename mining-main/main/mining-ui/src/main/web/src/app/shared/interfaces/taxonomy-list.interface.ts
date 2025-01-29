import { TaxonomyPojo } from '@innowake/mining-api-angular-client';

/**
 * Interface for List of Taxonomy Types for categories.
 */
export interface TaxonomyList {
    name: string;
    type?: TaxonomyType[];
}

/**
 * Interface for List of Taxonomies for Taxonomy Types.
 */
export interface TaxonomyType {
    name: string;
    taxonomies: TaxonomyPojo[];
}

/**
 * Interface for Taxonomy Details
 */
 export interface TaxonomyDetails {
    taxonomyIds: number[];
    taxonomyTitles: string[];
}
