import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { Configuration, DiscoveryControllerApi, IoControllerApi, IoControllerApiAxiosParamCreator } from 'mining-api-client';
import { terminal } from 'terminal-kit';
import { exportAction } from '../../lib/action-stereotypes/export-action';
import { jobAction } from '../../lib/action-stereotypes/job-action';
import { withDefaultErrorHandling } from '../../lib/default-error-handling';
import { MiningApiConfig, MINING_API_CONFIG } from '../../lib/mining-api/mining-api-config';
import { MiningApiUtils } from '../../lib/mining-api/mining-api-utils';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { CommandProvider } from '../command-provider';
import { DownloadConfigurationCommand } from './download-configuration';
import { DownloadSourceCodeCommand } from './download-source-code';
import { UploadConfigurationCommand } from './upload-configuration';
import { UploadSourceCodeCommand } from './upload-source-code';

@Component()
export class DiscoveryCommand implements CommandProvider {

    constructor(@Inject(DiscoveryControllerApi) private readonly discoveryController: DiscoveryControllerApi,
                @Inject(IoControllerApi) private readonly ioController: IoControllerApi,
                @Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(MINING_API_CONFIG) private readonly apiConfig: MiningApiConfig,
                @Inject(MiningApiUtils) private readonly apiUtils: MiningApiUtils,
                @Inject(DownloadConfigurationCommand) private readonly downloadConfig: DownloadConfigurationCommand,
                @Inject(DownloadSourceCodeCommand) private readonly downloadSource: DownloadSourceCodeCommand,
                @Inject(UploadConfigurationCommand) private readonly uploadConfig: UploadConfigurationCommand,
                @Inject(UploadSourceCodeCommand) private readonly uploadSource: UploadSourceCodeCommand) {}

    getCommand() {
        const command = new Command('discovery')
            .description('Discovery related sub-commands');
        command.command('discover-code')
            .description('Submit a Discover Code job')
            .action(this.discoverCode);
        command.command('discover-metrics')
            .description('Submit a Discover Metrics job')
            .action(this.discoverMetrics);
        command.command('discover-dna')
            .description('Submit a Find Communities job (Discover DNA)')
            .action(this.discoverDna);
        command.command('export-csv')
            .description('Download discovery result in CSV format')
            .action(this.exportCsv);
        command.command('export-excel')
            .description('Download discovery result in Excel format')
            .action(this.exportExcel);
        command.command('import-csv <file>')
            .description('Import Discovery CSV file')
            .action(withDefaultErrorHandling(this.importCsv.bind(this)));
        command.command('import-excel <file>')
            .description('Import Discovery Excel file')
            .action(withDefaultErrorHandling(this.importExcel.bind(this)));
        command.addCommand(this.downloadConfig.getCommand());
        command.addCommand(this.downloadSource.getCommand());
        command.addCommand(this.uploadConfig.getCommand());
        command.addCommand(this.uploadSource.getCommand());
        return command as Command;
    }

    discoverCode = jobAction(
        { title: 'Discover Code Job' },
        () => this.discoveryController.discoverCode(this.project.projectId)
    );

    discoverMetrics = jobAction(
        { title: 'Discover Metrics Job' },
        () => this.discoveryController.discoverMetrics(this.project.projectId)
    );

    discoverDna = jobAction(
        { title: 'Find Communities Job' },
        () => this.discoveryController.discoverDNA(this.project.projectId)
    );

    exportCsv = exportAction(
        () => this.ioController.exportCsv(this.project.projectId, { responseType: 'arraybuffer' })
    );

    exportExcel = exportAction(
        () => this.ioController.exportExcel(this.project.projectId, { responseType: 'arraybuffer' })
    );

    async importCsv(tempFilePath: string) {
        const progressBar = terminal.progressBar({
            title: 'Uploading CSV ...'
        });
        progressBar.update({ progress: null });

        /* unfortunately, we can't do this, unless we fix the client */
        // await this.ioController.importSourceObjectsUsingPOST(stream, this.project.projectId);

        try {
            const requestArgs = await IoControllerApiAxiosParamCreator(new Configuration(this.apiConfig.config)).importCSV(this.project.projectId, '' as any);
            await this.apiUtils.uploadFile(requestArgs, tempFilePath);
        } finally {
            progressBar.stop();
            terminal.eraseLine();
        }
        console.log('CSV import complete');
    }

    async importExcel(tempFilePath: string) {
        const progressBar = terminal.progressBar({
            title: 'Uploading Excel ...'
        });
        progressBar.update({ progress: null });

        /* unfortunately, we can't do this, unless we fix the client */
        // await this.ioController.importSourceObjectsUsingPOST(stream, this.project.projectId);

        try {
            const requestArgs = await IoControllerApiAxiosParamCreator(new Configuration(this.apiConfig.config)).importExcel(this.project.projectId, '' as any);
            await this.apiUtils.uploadFile(requestArgs, tempFilePath);
        } finally {
            progressBar.stop();
            terminal.eraseLine();
        }
        console.log('Excel import complete');
    }
}
