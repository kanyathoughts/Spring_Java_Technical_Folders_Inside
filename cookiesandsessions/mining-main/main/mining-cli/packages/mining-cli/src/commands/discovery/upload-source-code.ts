import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { Configuration, IoControllerApiAxiosParamCreator } from 'mining-api-client';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { CommandProvider } from '../command-provider';
import { withDefaultErrorHandling } from '../../lib/default-error-handling';
import { JobInformationStatusEnum } from 'mining-api-client';
import { promisify } from 'util';
import { walk } from '@nodelib/fs.walk';
import { relative, resolve } from 'path';
import { withFile } from 'tmp-promise';
import { createWriteStream, readFile } from 'fs-extra';
import archiver from 'archiver';
import { MiningApiConfig, MINING_API_CONFIG } from '../../lib/mining-api/mining-api-config';
import byteSize from 'byte-size';
import { terminal } from 'terminal-kit';
import { MiningApiUtils } from '../../lib/mining-api/mining-api-utils';
import { jobAction } from '../../lib/action-stereotypes/job-action';
import { MiningFileIndex } from './mining-file-index';

const walkAsync = promisify(walk);

@Component()
export class UploadSourceCodeCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(MINING_API_CONFIG) private readonly apiConfig: MiningApiConfig,
                @Inject(MiningApiUtils) private readonly apiUtils: MiningApiUtils) {}

    getCommand() {
        return new Command('upload-source')
            .description('Upload source code from a folder relative to the project root')
            .arguments('<dirs...>')
            .action(withDefaultErrorHandling((dirs: string[]) => this.uploadSource(dirs))) as Command;
    }

    async uploadSource(dirs: string[]) {
        for (const dir of dirs) {
            console.log('Uploading', dir);

            const progressBar = terminal.progressBar({
                title: 'Building file index ...',
                percent: true,
                eta: true
            });
            progressBar.update({ progress: null });

            let fileIndex: MiningFileIndex;
            try {
                fileIndex = await this.buildFileIndex(dir);
            } finally {
                progressBar.stop();
                terminal.eraseLine();
            }
            console.log('Found', fileIndex.files.length, 'files');

            await this.compressAndUpload(fileIndex);
        }
    }

    async buildFileIndex(dir: string): Promise<MiningFileIndex> {
        const files = await walkAsync(resolve(this.project.rootPath, dir), {
            /* include non-empty files only */
            stats: true,
            entryFilter: (entry) => entry.dirent.isFile() && (entry.stats?.size ?? 0) > 0
        });
        const indexFiles = files.map(file => ({
            path: relative(this.project.rootPath, file.path).split('\\').join('/'),
            contentIncluded: true
        }));
        return {
            files: indexFiles,
            scope: dir,
            version: 1
        };
    }

    async compressAndUpload(fileIndex: MiningFileIndex) {
        await withFile(async (tempFile) => {
            const bytes = await this.compress(fileIndex, tempFile.path);
            console.log('Compressed', byteSize(bytes).toString());
            await this.upload(tempFile.path);
        });
    }

    compress(fileIndex: MiningFileIndex, tempFilePath: string) {
        const progressBar = terminal.progressBar({
            title: 'Compressing files ...',
            percent: true,
            eta: true
        });

        return new Promise<number>(async (res, rej) => {
            const writeStream = createWriteStream(tempFilePath);
            const archive = archiver('zip');

            progressBar.update({ progress: null });

            archive.on('progress', (progressEvent) => {
                progressBar.update(progressEvent.entries.processed / fileIndex.files.length);
            });

            archive.on('error', (err) => {
                rej(err);
            });

            writeStream.on('close', () => {
                res(archive.pointer());
            });

            archive.pipe(writeStream);

            archive.append(JSON.stringify(fileIndex), { name: '.mining-file-index' });

            await Promise.all(fileIndex.files.map(async (file: any) => {
                const buffer = await readFile(resolve(this.project.rootPath, file.path));
                archive.append(buffer, { name: file.path });
            }));

            archive.finalize();
        })
        .finally(() => {
            progressBar.stop();
            terminal.eraseLine();
        });
    }

    async upload(tempFilePath: string) {
        console.log('Uploading ...');
        const requestArgs = await IoControllerApiAxiosParamCreator(new Configuration(this.apiConfig.config)).importSourceObjects(this.project.projectId, '' as any);
        const jobExecution = jobAction(
            { title: 'Upload Source Code...', downloadResult: false},
            () => this.apiUtils.uploadFile(requestArgs, tempFilePath),
            (jobId, jobStatus) => {
                if (jobStatus === JobInformationStatusEnum.SUCCESS) {
                    console.log('Upload complete');
                } else {
                    console.error('Import of Source Code into database failed');
                    process.exit(1);
                }
            });
        await jobExecution();
    }
}
