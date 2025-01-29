/**
 * Interface for the selected taxonomy.
 */
export interface TaxonomyFilterSelected {
  taxonomyId: number,
  taxonomyTitle: string,
  selectedTaxonomy?: Record<any, any>
}
