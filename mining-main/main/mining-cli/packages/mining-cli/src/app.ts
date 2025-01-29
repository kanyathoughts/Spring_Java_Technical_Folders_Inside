import { Component, Inject } from 'iw-ioc';
import { Command } from 'commander';
import { VersionCommand } from './commands/version';
import { ProjectsCommand } from './commands/projects';
import { DiscoveryCommand } from './commands/discovery';
import { ModulesCommand } from './commands/modules';
import { ClientsCommand } from './commands/clients';
import { JobExtensionsCommand } from './commands/job-extensions';
import { AnalysisCommand } from './commands/analysis';

@Component()
export class MiningCliApplication {

    constructor(@Inject(AnalysisCommand) private readonly analysisCommand: AnalysisCommand,
                @Inject(DiscoveryCommand) private readonly discoveryCommand: DiscoveryCommand,
                @Inject(ModulesCommand) private readonly modulesCommand: ModulesCommand,
                @Inject(ClientsCommand) private readonly clientsCommand: ClientsCommand,
                @Inject(ProjectsCommand) private readonly projectsCommand: ProjectsCommand,
                @Inject(JobExtensionsCommand) private readonly jobExtensionsCommand: JobExtensionsCommand,
                @Inject(VersionCommand) private readonly versionCommand: VersionCommand) {}

    async run(argv: string[]) {
        const app = new Command('mining-cli');

        app.addCommand(this.analysisCommand.getCommand());
        app.addCommand(this.discoveryCommand.getCommand());
        app.addCommand(this.modulesCommand.getCommand());
        app.addCommand(this.clientsCommand.getCommand());
        app.addCommand(this.projectsCommand.getCommand());
        app.addCommand(this.jobExtensionsCommand.getCommand());
        app.addCommand(this.versionCommand.getCommand());

        try {
            await app.parseAsync(argv);
        } catch (err) {
            console.error('Command failed: ', err.toString());
            process.exit(1);
        }
    }
}
