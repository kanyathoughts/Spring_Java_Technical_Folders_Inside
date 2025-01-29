import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { CommandProvider } from '../command-provider';
import { jobAction } from '../../lib/action-stereotypes/job-action';
import { createReadStream, readFile } from 'fs-extra';
import { MiningApiConfig, MINING_API_CONFIG } from '../../lib/mining-api/mining-api-config';
import { MiningApiUtils } from '../../lib/mining-api/mining-api-utils';
import { Configuration, JobControllerApiAxiosParamCreator } from 'mining-api-client';
import { inspect } from 'util';

@Component()
export class JobExtensionsCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(MINING_API_CONFIG) private readonly apiConfig: MiningApiConfig,
                @Inject(MiningApiUtils) private readonly apiUtils: MiningApiUtils) {}

    getCommand() {
        const command = new Command('job-extensions')
            .description('Submit job extension commands');
        command.command('submit <extension-id>')
            .description('Submit job')
            .option('-p, --parameters <values...>', 'Set parameters for the job. Give parameters in the format `key=value`. You can set multiple parameters by separating the key=value pairs with spaces. Example: -p foo=bar someOption=someValue')
            .option('--parameters-from-file <fileNames...>', 'Like --parameters but the values are interpreted as file names from which the actual value is read. Use this for large input parameters.')
            .option('-d --data <file>', 'Read input data for the job from a file. The data is passed as-is in the request body. Use this to upload (large) binary data.')
            .action(this.submitJob);
        return command as Command;
    }

    readonly submitJob = jobAction(
        { title: '', downloadResult: true },
        async (extensionId: string, options: any) => {
            const parameters: { [key: string]: string[] } = {};

            for (const param of options.parameters ?? []) {
                const [key, value] = (param as string).split('=');
                parameters[key] = [... (parameters[key] ?? []), value];
            }

            for (const fileParam of options.parametersFromFile ?? []) {
                const [key, fileName] = (fileParam as string).split('=');
                const fileContents = await readFile(fileName, 'utf8');
                parameters[key] = [... (parameters[key] ?? []), fileContents];
            }

            /* need to cast inputData to any in order to pass a stream - the generated code only accepts "string" */
            const requestArgs = await JobControllerApiAxiosParamCreator(new Configuration(this.apiConfig.config))
                    .submitJobExtensionV2(this.project.projectId, extensionId, parameters);

            if (options.data) {
                return this.apiUtils.uploadFile(requestArgs, options.data);
            } else {
                /* remove the generated entity body */
                delete requestArgs.options.data;
                delete requestArgs.options.headers?.['Content-Type'];
                return this.apiConfig.axios.request({ url: requestArgs.url, ... requestArgs.options });
            }
        }
    );
}
