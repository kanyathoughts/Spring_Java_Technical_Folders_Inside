import axios, { AxiosStatic } from 'axios';
import { Component } from 'iw-ioc';
import { MiningApiConfig, MINING_API_CONFIG } from './mining-api-config';

jest.mock('axios');


export class MockMiningApiConfig implements MiningApiConfig {
    axios: AxiosStatic & jest.Mocked<AxiosStatic> = axios as AxiosStatic & jest.Mocked<AxiosStatic>;

    basePath = 'http://mock-server:8080';

    async init() {
        /* nothing to be done */
    }
}
