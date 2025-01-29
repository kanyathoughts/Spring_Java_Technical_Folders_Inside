import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { DiscoveryControllerApi } from 'mining-api-client';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { CommandProvider } from '../command-provider';
import { withDefaultErrorHandling } from '../../lib/default-error-handling';
import { withFile } from 'tmp-promise';
import { writeFile } from 'fs-extra';
import Zip from 'node-stream-zip';

@Component()
export class DownloadConfigurationCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(DiscoveryControllerApi) private readonly discoveryController: DiscoveryControllerApi) {}

    getCommand() {
        return new Command('download-config')
            .description('Download Discovery configuration files from the server into the root of the project')
            .action(withDefaultErrorHandling(() => this.downloadConfig())) as Command;
    }

    async downloadConfig() {
        await withFile(async (tempFile) => {
            /* configurations are downloaded from the server as a Zip archive, the archive is then extracted into the project root as-is
             * we don't verify the contents of the archive */
            const response = await this.discoveryController.downloadDiscoveryConfiguration(this.project.projectId, { responseType: 'arraybuffer' });
            await writeFile(tempFile.path, response.data);
            const archive = new Zip.async({ file: tempFile.path });
            // tslint:disable-next-line: no-null-keyword -- the library demands that we use null (it means "extract all")
            const numberOfFiles = await archive.extract(null, this.project.rootPath);
            await archive.close();
            console.log(`Downloaded ${numberOfFiles} files.`);
        });
    }
}
