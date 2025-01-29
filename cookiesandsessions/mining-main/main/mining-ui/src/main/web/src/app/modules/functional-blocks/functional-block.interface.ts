/** Interface for functional block */
export interface FunctionalBlock {
    name?: string;
    uid?: string;
    description?: string;
    generatedFrom?: {annotationId: string};
    children?: {content: FunctionalBlock[]};
    childBlock?: FunctionalBlock[];
    level: number;
}
