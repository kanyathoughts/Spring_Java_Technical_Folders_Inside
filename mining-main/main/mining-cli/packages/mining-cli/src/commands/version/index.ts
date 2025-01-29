import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { VersionControllerApi } from 'mining-api-client';
import { CommandProvider } from '../command-provider';
import { withDefaultErrorHandling } from '../../lib/default-error-handling';
import { standardAction } from '../../lib/action-stereotypes/standard-action';

@Component()
export class VersionCommand implements CommandProvider {

    constructor(@Inject(VersionControllerApi) private readonly versionController: VersionControllerApi) {}

    getCommand() {
        return new Command('version')
            .description('Display mining server version')
            .action(this.printVersion) as Command;
    }

    readonly printVersion = standardAction(
        () => this.versionController.getVersion(),
        (version) => console.log(version.version)
    );
}
