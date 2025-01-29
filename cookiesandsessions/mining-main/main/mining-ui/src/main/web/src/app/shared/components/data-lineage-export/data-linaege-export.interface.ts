/**
 * Interface for input data for data lineage
 */
export interface DataLineageInputData {
    [key: string]: string[]
    detailLevel: string[],
    moduleId?: string[],
    fieldOffset?: string[]
 }
