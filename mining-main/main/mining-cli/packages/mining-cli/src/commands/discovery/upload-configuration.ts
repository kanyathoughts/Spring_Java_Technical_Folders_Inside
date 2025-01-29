import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { Configuration, DiscoveryControllerApiAxiosParamCreator } from 'mining-api-client';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { CommandProvider } from '../command-provider';
import { withDefaultErrorHandling } from '../../lib/default-error-handling';
import { resolve } from 'path';
import { withFile } from 'tmp-promise';
import { createWriteStream, readFile, pathExists } from 'fs-extra';
import archiver from 'archiver';
import { MiningApiConfig, MINING_API_CONFIG } from '../../lib/mining-api/mining-api-config';
import { MiningApiUtils } from '../../lib/mining-api/mining-api-utils';

const CONFIG_FILES = [
    'Discovery_Config.xml',
    'DNA_Sequencer_Config.xml',
    'DNA_LouvainRunner_Config.xml',
    'DNA_SimilarityProcessor_Config.xml',
    'discovery-search-order.xml',
    'utilities.xml'
];

@Component()
export class UploadConfigurationCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(MINING_API_CONFIG) private readonly apiConfig: MiningApiConfig,
                @Inject(MiningApiUtils) private readonly apiUtils: MiningApiUtils) {}

    getCommand() {
        return new Command('upload-config')
            .description('Upload Discovery configuration files located in the root of the project')
            .action(withDefaultErrorHandling(() => this.uploadConfig())) as Command;
    }

    async uploadConfig() {
        await withFile(async (tempFile) => {
            const numberOfFiles = await this.compress(tempFile.path);
            console.log(`Compressed ${numberOfFiles} files.`);
            await this.upload(tempFile.path);
        });
    }

    compress(tempFilePath: string): Promise<number> {
        return new Promise<number>(async (res, rej) => {
            const writeStream = createWriteStream(tempFilePath);
            const archive = archiver('zip');
            let numberOfFiles = 0;

            archive.on('error', (err) => {
                rej(err);
            });

            writeStream.on('close', () => {
                res(numberOfFiles);
            });

            archive.pipe(writeStream);

            await Promise.all(CONFIG_FILES.map(async (file: string) => {
                const filePath = resolve(this.project.rootPath, file);
                if (await pathExists(filePath)) {
                    numberOfFiles++;
                    const buffer = await readFile(filePath);
                    archive.append(buffer, { name: file });
                }
            }));

            archive.finalize();
        });
    }

    async upload(tempFilePath: string) {
        /* unfortunately, we must use this ugly workaround unless the generated client code is fixed */
        const requestArgs = await DiscoveryControllerApiAxiosParamCreator(new Configuration(this.apiConfig.config)).uploadDiscoveryConfiguration(this.project.projectId, '' as any);
        await this.apiUtils.uploadFile(requestArgs, tempFilePath);
        console.log('Upload complete.');
    }
}
