import { IOC } from 'iw-ioc';
import { MiningCliApplication } from './app';
import { registerApiProviders } from './lib/mining-api/mining-api-provider';
import { createSampleProjectFile, loadProjectFile, PROJECT_FILE_NAME } from './lib/mining-project/mining-project-config-provider-from-file';
import FormData from 'form-data';
import { MiningApiOAuthConfig } from './lib/mining-api/mining-api-config-oauth';
import { MiningApiConfig, MINING_API_CONFIG } from './lib/mining-api/mining-api-config';
import { Newable } from 'ts-essentials';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from './lib/mining-project/mining-project-config';
import { MiningApiNoAuthConfig } from './lib/mining-api/mining-api-config-no-auth';
import { terminal } from 'terminal-kit';

/* manual "polyfill" for FormData, which is required by the generated mining-api-client */
(global as any).FormData = FormData;

async function main() {
    if ( ! await loadProjectFile(process.cwd())) {
        /* check if terminal is interactive - if yes: prompt user to create empty project file; otherwise exit with error message immediately */
        if (process.stdout.isTTY) {
            console.warn('No .mining-cli-project file found. Do you want to create one in the current directory? [Y|n]');
            const createSampleFile = await terminal.yesOrNo({ yes: [ 'y' , 'ENTER' ] , no: [ 'n' ] }).promise;
            if (createSampleFile) {
                await createSampleProjectFile(process.cwd());
                console.log('Created ', PROJECT_FILE_NAME, 'file in current directory. Please edit as needed and then re-run mining-cli.');
                process.exit(0);
            } else {
                process.exit(1);
            }
        } else {
            console.error('No .mining-cli-project file found. Please create one in the root directory of your project before using mining-cli.');
            process.exit(1);
        }
    }

    const projectConfig: MiningProjectConfig | undefined = IOC.get(MINING_PROJECT_CONFIG);
    let apiConfigToken: Newable<MiningApiConfig>;
    if (projectConfig === undefined) {
        apiConfigToken = MiningApiNoAuthConfig;
    } else if (projectConfig.server?.accessToken !== undefined) {
        apiConfigToken = MiningApiOAuthConfig;
    } else {
        apiConfigToken = MiningApiNoAuthConfig;
    }

    IOC.registerComponent(apiConfigToken, MINING_API_CONFIG);
    await registerApiProviders(apiConfigToken);

    const app = IOC.get(MiningCliApplication);

    if ( ! app) {
        console.error('Application initialization failed');
        process.exit(1);
    }

    await app.run(process.argv);
}

main();
