import { AxiosInstance } from 'axios';
import { ConfigurationParameters } from 'mining-api-client';

export const MINING_API_CONFIG = Symbol('mining-api-config');

export interface MiningApiConfig {
    basePath: string;
    config?: ConfigurationParameters;
    axios: AxiosInstance;

    init(): Promise<void>;
}