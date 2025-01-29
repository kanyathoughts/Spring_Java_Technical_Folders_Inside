import { registerApiProviders } from '../../lib/mining-api/mining-api-provider';
import { MockMiningApiConfig } from '../../lib/mining-api/mining-api-config-test';
import { VersionCommand } from '.';
import { IOC } from 'iw-ioc';
import { MINING_API_CONFIG } from '../../lib/mining-api/mining-api-config';

describe('Version Command', () => {

    let mockApi: MockMiningApiConfig;
    let cmd: VersionCommand;

    beforeAll(() => {
        IOC.registerComponent(MockMiningApiConfig, MINING_API_CONFIG);
        registerApiProviders(MockMiningApiConfig);
    });

    beforeEach(() => {
        mockApi = IOC.get(MockMiningApiConfig)!;
        cmd = IOC.get(VersionCommand)!;
    });

    test('it should request the version from api', async () => {
        mockApi.axios.request.mockReturnValue(Promise.resolve({
            data: {
                version: '123.45-mocked'
            }
        }));
        await cmd.printVersion();
        expect(mockApi.axios.request).toHaveBeenCalledWith({
            headers: {},
            method: 'GET',
            url: 'http://mock-server:8080/api/v1/version'
        });
    });
});