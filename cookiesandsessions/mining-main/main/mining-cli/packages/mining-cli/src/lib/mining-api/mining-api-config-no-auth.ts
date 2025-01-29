import axios from 'axios';
import { Component, Inject } from 'iw-ioc';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../mining-project/mining-project-config';
import { MiningApiConfig, MINING_API_CONFIG } from './mining-api-config';

const DEFAULT_URL = 'http://localhost:8080';

export class MiningApiNoAuthConfig implements MiningApiConfig {

    readonly url: string;

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly projectConfig: MiningProjectConfig) {
        this.url = projectConfig.server?.url ?? DEFAULT_URL;
    }

    async init() {
        /* nothing to be done */
    }

    get axios() {
        return axios.create({
            baseURL: this.url,
            maxBodyLength: 2147483648, /* 2GB */
            headers: {
                /* setting this header is required so that the server responds with 401 Unauthorized instead of redirecting to the login page */
                'X-Requested-With': 'XMLHttpRequest'
            }
        });
    }

    get basePath() {
        return this.url;
    }
}
