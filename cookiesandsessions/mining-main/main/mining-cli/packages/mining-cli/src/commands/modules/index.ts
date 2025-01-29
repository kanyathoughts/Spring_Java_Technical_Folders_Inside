import { Command, option, program } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { ModuleControllerApi, ModulePojo, VersionControllerApi } from 'mining-api-client';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { CommandProvider } from '../command-provider';
import { standardAction } from '../../lib/action-stereotypes/standard-action';
import { browseSelectAction } from '../../lib/action-stereotypes/browse-action';

@Component()
export class ModulesCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(ModuleControllerApi) private readonly moduleController: ModuleControllerApi) {}

    getCommand() {
        const command = new Command('modules')
            .description('Module related sub-commands');
        command.command('delete-all')
            .description('Delete all Modules')
            .option('--with-source', 'also delete SourceObjects')
            .action(this.deleteAll);
        return command as Command;
    }

    readonly deleteAll = standardAction(
        (options: any) => this.moduleController.deleteAllModules(this.project.projectId, !!options.withSource)
    );
}
