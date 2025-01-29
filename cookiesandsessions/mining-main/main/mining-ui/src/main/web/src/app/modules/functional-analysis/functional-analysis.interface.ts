export interface ModuleDetails {
    name: string;
    id: string;
}

/**
 * Interface for module search filter in reachability in & functional analysis.
 */
export interface DDSearchFilter {
    value: string | number | number[];
    label: string;
    path?: string;
    description?: string;
};
