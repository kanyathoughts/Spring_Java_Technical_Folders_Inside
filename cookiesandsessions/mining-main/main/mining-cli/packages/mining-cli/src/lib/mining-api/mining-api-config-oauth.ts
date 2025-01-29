import axios from 'axios';
import { Inject } from 'iw-ioc';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../mining-project/mining-project-config';
import { MiningApiConfig } from './mining-api-config';

const DEFAULT_URL = 'http://localhost:8080';
const DEFAULT_API_TOKEN = '6c2000c1-d9a8-4d87-92f0-0a11139a037e';
const OFFLINE_TOKEN_LENGTH = 44;

export class MiningApiOAuthConfig implements MiningApiConfig {

    readonly url: string;
    readonly accessToken: string;

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly projectConfig: MiningProjectConfig) {
        this.url = projectConfig.server?.url ?? DEFAULT_URL;
        let accessToken = projectConfig.server?.accessToken ?? DEFAULT_API_TOKEN;
        if (accessToken.length === OFFLINE_TOKEN_LENGTH) {
            accessToken = 'offline.' + accessToken;
        }
        this.accessToken = accessToken;
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
                'X-Requested-With': 'XMLHttpRequest',
                Authorization: `Bearer ${this.accessToken}`
            }
        });
    }

    get basePath() {
        return this.url;
    }
}
