import { registerApiProviders } from '../../lib/mining-api/mining-api-provider';
import { MockMiningApiConfig } from '../../lib/mining-api/mining-api-config-test';
import { ModulesCommand } from '.';
import { IOC } from 'iw-ioc';
import { MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { MINING_API_CONFIG } from '../../lib/mining-api/mining-api-config';
import { asValue } from 'awilix';
import { URLSearchParams } from 'url';

describe('Modules Command', () => {

    let mockApi: MockMiningApiConfig;
    let cmd: ModulesCommand;

    beforeAll(() => {
        IOC.registerComponent(MockMiningApiConfig, MINING_API_CONFIG);
        registerApiProviders(MockMiningApiConfig);
        IOC.getRootContainer().register(MINING_PROJECT_CONFIG, asValue({
            "clientId": 1,
            "projectId": 3
        }));
    });

    beforeEach(() => {
        mockApi = IOC.get(MockMiningApiConfig)!;
        cmd = IOC.get(ModulesCommand)!;
        jest.resetAllMocks();
    });

    test('it should delete all modules without source objects by default', async () => {
        mockApi.axios.request.mockReturnValue(Promise.resolve({ data: null }));
        await cmd.deleteAll({});
        expect(mockApi.axios.request).toHaveBeenCalledWith({
            headers: {},
            method: 'DELETE',
            url: 'http://mock-server:8080/api/v1/projects/3/modules?deleteSourceObjects=false'
        });
    })

    test('it should delete all modules without source objects', async () => {
        mockApi.axios.request.mockReturnValue(Promise.resolve({ data: null }));
        await cmd.deleteAll({ withSource: false });
        expect(mockApi.axios.request).toHaveBeenCalledWith({
            headers: {},
            method: 'DELETE',
            url: 'http://mock-server:8080/api/v1/projects/3/modules?deleteSourceObjects=false'
        });
    })

    test('it should delete all modules with source objects', async () => {
        mockApi.axios.request.mockReturnValue(Promise.resolve({ data: null }));
        await cmd.deleteAll({ withSource: true });
        expect(mockApi.axios.request).toHaveBeenCalledWith({
            headers: {},
            method: 'DELETE',
            url: 'http://mock-server:8080/api/v1/projects/3/modules?deleteSourceObjects=true'
        });
    })

})
