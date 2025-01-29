import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { CandidateIdentificationControllerApi, ModuleControllerApi, TaxonomyControllerApi } from 'mining-api-client';
import { CommandProvider } from '../command-provider';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { jobAction } from '../../lib/action-stereotypes/job-action';
import { walk } from '@nodelib/fs.walk';
import { promisify } from 'util';
import { relative, resolve } from 'path';
import { stat } from 'fs-extra';

const walkAsync = promisify(walk);

@Component()
export class AnalysisCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(CandidateIdentificationControllerApi) private readonly candidateIdentificationController: CandidateIdentificationControllerApi,
                @Inject(ModuleControllerApi) private readonly moduleController: ModuleControllerApi,
                @Inject(TaxonomyControllerApi) private readonly taxonomyController: TaxonomyControllerApi) {}

    getCommand() {
        const command = new Command('analysis')
            .description('Commands to invoke automated analyses');
        command.command('identify-candidates <paths...>')
            .description('Run business rule candidate identification on the modules in the given paths')
            .action(this.identifyCandidates);
        command.command('identify-module-descriptions <paths...>')
            .description('Run module description identification on the modules in the given paths')
            .action(this.identifyModuleDescriptions);
        command.command('identify-technical-taxonomies <paths...>')
            .description('Run technical taxonomy identification on the modules in the given paths')
            .action(this.identifyTaxonomies);
        return command as Command;
    }

    readonly identifyCandidates = jobAction(
        { title: 'Identify Candidates' },
        async (paths: string[]) => {
            const fileList = await this.buildFileList(paths);
            console.log(`Starting Identify Candidates job on ${fileList.length} files ...`);
            return this.candidateIdentificationController.identifyAllCandidates(this.project.projectId, {pathPatterns: fileList});
        }
    );

    readonly identifyModuleDescriptions = jobAction(
        { title: 'Identify Module Descriptions' },
        async (paths: string[]) => {
            const fileList = await this.buildFileList(paths);
            console.log(`Starting Identify Module Descriptions job on ${fileList.length} files ...`);
            return this.moduleController.identifyModuleDescriptions(this.project.projectId, {pathPatterns:  fileList});
        }
    );

    readonly identifyTaxonomies = jobAction(
        { title: 'Identify Technical Taxonomies' },
        async (paths: string[]) => {
            const fileList = await this.buildFileList(paths);
            console.log(`Starting Identify Technical Taxonomies job on ${fileList.length} files ...`);
        return this.taxonomyController.identifyTechnicalTaxonomies(this.project.projectId, {pathPatterns: fileList});
        }
    );

    private async buildFileList(paths : string[]): Promise<string[]> {
        const fileList = await Promise.all(paths.map(async (path) => {
            const absolutePath = resolve(this.project.rootPath, path);
            const entry = await stat(absolutePath);
            if (entry.isFile()) {
                /* if path is file, return path relative to project root directory, in order to match the path that is recorded in the server's database */
                return relative(this.project.rootPath, absolutePath).split('\\').join('/');
            } else if (entry.isDirectory()) {
                /* if path is directory, walk the directory recursively, collecting all files ... */
                const files = await walkAsync(absolutePath, {
                    /* include files only */
                    entryFilter: (ent) => ent.dirent.isFile()
                });
                /* ... and then again return path relative to project root directory */
                return files.map(file => relative(this.project.rootPath, file.path).split('\\').join('/'));
            } else {
                /* the undefined entries are filtered out below */
                console.warn(`Ignoring ${path}: not regular file or directory.`);
                return undefined;
            }
        }));
        return fileList.flat().filter(entry => entry !== undefined) as string[];
    }
}
