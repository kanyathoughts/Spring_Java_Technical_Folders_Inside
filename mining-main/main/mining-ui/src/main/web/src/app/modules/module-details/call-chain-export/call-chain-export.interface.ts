/**
 * Interface for the list Details.
 */
 export interface ListDetail {
    value: string | number | number[];
    label: string;
    count?: number;
}

/**
 * Interfce for start/end module when values changed.
 */
export interface ModuleValueChange {
    startModule: string[];
    endModule: string[];
    moduleDetails: Array<{ label: string, value: number[] }>;
    selectedRadio: string;
}
