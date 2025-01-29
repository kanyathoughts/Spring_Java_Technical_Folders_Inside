import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { IoControllerApi } from 'mining-api-client';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { CommandProvider } from '../command-provider';
import { withDefaultErrorHandling } from '../../lib/default-error-handling';
import { promisify } from 'util';
import { walk } from '@nodelib/fs.walk';
import { dirname, relative, resolve } from 'path';
import { withFile } from 'tmp-promise';
import byteSize from 'byte-size';
import { writeFile } from 'fs-extra';
import { readdirSync, rmdirSync, statSync, unlinkSync } from 'fs';
import Zip from 'node-stream-zip';
import { MiningFileIndex } from './mining-file-index';

const walkAsync = promisify(walk);

@Component()
export class DownloadSourceCodeCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(IoControllerApi) private readonly ioController: IoControllerApi) {}

    getCommand() {
        return new Command('download-source')
            .description('Download source code from a folder relative to the project root')
            .action(withDefaultErrorHandling(() => this.downloadSource())) as Command;
    }

    async downloadSource() {
        await withFile(async (tempFile) => {
            const response = await this.ioController.downloadSourceObject(this.project.projectId, undefined, {responseType: 'arraybuffer'});
            await writeFile(tempFile.path, response.data);
            const archive = new Zip.async({ file: tempFile.path });
            // tslint:disable-next-line: no-null-keyword -- the library demands that we use null (it means "extract all")
            const numberOfFiles = await archive.extract(null, this.project.rootPath);
            const miningFileIndex: MiningFileIndex = JSON.parse((await archive.entryData('.mining-file-index')).toString('utf8'));

            const paths = new Set();
            for (let file of miningFileIndex.files) {
                paths.add(file.path);
            }

            const files = await walkAsync(resolve(this.project.rootPath, 'src'), {
                /* include files only */
                entryFilter: (entry) => entry.dirent.isFile()
            });

            var deletedItems = 0;
            var totalSize = 0.0;
            files.forEach(file => {
                const filePath = relative(this.project.rootPath, file.path).split('\\').join('/');
                if (!paths.has(filePath)) {
                    unlinkSync(filePath);
                    deletedItems++;
                    var parentPath = dirname(filePath);
                    if (readdirSync(parentPath).length === 0) {
                        rmdirSync(parentPath);
                    }
                } else {
                    totalSize += statSync(filePath).size;
                }
            })

            await archive.close();
            console.log(`Downloaded ${numberOfFiles} file(s) (${byteSize(totalSize)}); ${deletedItems} file(s) deleted`);
        });
    }
}
