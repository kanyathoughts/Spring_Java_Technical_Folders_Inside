export const MINING_PROJECT_CONFIG = Symbol('mining-project-config');

export interface MiningProjectConfig {
    server: {
        url: string;
        accessToken: string;
    };
    rootPath: string;
    clientId: number;
    projectId: number;
}