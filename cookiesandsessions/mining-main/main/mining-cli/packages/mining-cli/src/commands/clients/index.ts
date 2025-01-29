import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { ClientControllerApi, ClientControllerV2Api, ClientPojo } from 'mining-api-client';
import { CommandProvider } from '../command-provider';
import { standardAction } from '../../lib/action-stereotypes/standard-action';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { browseSelectAction } from '../../lib/action-stereotypes/browse-action';

@Component()
export class ClientsCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(ClientControllerApi) private readonly clientController: ClientControllerApi,
                @Inject(ClientControllerV2Api) private readonly clientControllerV2: ClientControllerV2Api) {}

    getCommand() {
        const command = new Command('clients')
            .description('Client related sub-commands');
        command.command('create <name>')
            .description('Create new Client')
            .action(this.createClient);
        command.command('delete <clientId>')
            .description('Delete Client')
            .action(this.deleteClient);
        command.command('rename <clientId> <name>')
            .description('Rename Client')
            .action(this.renameClient);
        command.command('list')
            .description('List Clients')
            .action(this.listClients);
        command.command('select <clientId>')
            .description('Change the current Client')
            .action(this.selectClient);
        return command as Command;
    }

    readonly createClient = standardAction(
        (name: string) => this.clientControllerV2.createClientV2({
            name
        })
    );

    readonly deleteClient = standardAction(
        (clientId: number) => this.clientControllerV2.deleteClient(clientId)
    );

    readonly renameClient = standardAction(
        (clientId: number, name: string) => this.clientController.updateClient(clientId, { name })
    );

    readonly selectClient = standardAction(
        (clientId: number) => this.clientController.findClientById(clientId),
        (client: ClientPojo) => this.project.clientId = client.id!
    );

    readonly listClients = browseSelectAction(
        { heading: `(active Client is prefixed with '*')`, headers: ['Id', 'Name'] },
        (page, size) => this.clientControllerV2.getAllClients(page, size),
        (client: ClientPojo) => [(client.id === this.project.clientId ? '*' : '') + client.id!, client.name!]
    );

}
