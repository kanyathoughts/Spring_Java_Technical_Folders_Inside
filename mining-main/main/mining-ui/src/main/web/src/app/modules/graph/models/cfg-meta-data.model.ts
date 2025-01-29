/**
 * Interface for cfg meta data having input & output files.
 */
export interface CfgMetaData {
    inputFiles: {
        title: string;
        modules: Array<{[key: string]: any}>
    };
    outputFiles: {
        title: string;
        modules: Array<{[key: string]: any}>
    }
}
