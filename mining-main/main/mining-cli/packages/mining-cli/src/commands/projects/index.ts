import { Command } from 'commander';
import { Component, Inject } from 'iw-ioc';
import { ProjectControllerApi, ProjectControllerV2Api, ProjectPojoPrototypeNaturesEnum, ProjectRoleProjectNaturesEnum } from 'mining-api-client';
import { CommandProvider } from '../command-provider';
import { standardAction } from '../../lib/action-stereotypes/standard-action';
import { terminal } from 'terminal-kit';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from '../../lib/mining-project/mining-project-config';
import { ProjectPojo } from 'mining-api-client';
import { browseSelectAction } from '../../lib/action-stereotypes/browse-action';

@Component()
export class ProjectsCommand implements CommandProvider {

    constructor(@Inject(MINING_PROJECT_CONFIG) private readonly project: MiningProjectConfig,
                @Inject(ProjectControllerApi) private readonly projectController: ProjectControllerApi,
                @Inject(ProjectControllerV2Api) private readonly projectControllerV2: ProjectControllerV2Api) {}

    getCommand() {
        const command = new Command('projects')
            .description('Project related sub-commands');
        command.command('create <name>')
            .description('Create new Project')
            .action(this.createProject);
        command.command('delete <projectId>')
            .description('Delete Project')
            .action(this.deleteProject);
        command.command('rename <projectId> <name>')
            .description('Rename Project')
            .action(this.renameProject);
        command.command('list')
            .description('List Projects')
            .action(this.listProjects);
        command.command('select <projectId>')
            .description('Change the current Project')
            .action(this.selectProject);
        return command as Command;
    }

    readonly createProject = standardAction(
        (name: string) => this.projectControllerV2.createProject({
            client: this.project.clientId,
            name,
            natures: [ ProjectRoleProjectNaturesEnum.DISCOVERY, ProjectRoleProjectNaturesEnum.MINING ] as unknown as Set<ProjectPojoPrototypeNaturesEnum>
        })
    );

    readonly deleteProject = standardAction(
        (projectId: number) => this.projectControllerV2.deleteProject(projectId)
    );

    readonly renameProject = standardAction(
        (projectId: number, name: string) => this.projectController.updateProject(projectId, { nid: projectId, client: this.project.clientId, name })
    );

    readonly selectProject = standardAction(
        (projectId: number) => this.projectController.findProjectById(projectId),
        (project: ProjectPojo) => this.project.projectId = project.id!
    );

    readonly listProjects = browseSelectAction(
        { heading: `(active Project is prefixed with '*')`, headers: ['Id', 'Name'] },
        (page, size) => this.projectControllerV2.findProjectsForClient(this.project.clientId, page, size),
        (project: ProjectPojo) => [(project.id === this.project.projectId ? '*' : '') + project.id, project.name!]
    );

}
