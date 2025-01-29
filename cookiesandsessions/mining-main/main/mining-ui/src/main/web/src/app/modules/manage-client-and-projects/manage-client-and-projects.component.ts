import { Component, OnInit, TemplateRef, ViewChild } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ConfirmDeleteModalComponent, DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { FilterType, MiningTableConfig } from '@app/shared/components/mining-table/mining-table-config.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { forkJoin, Subject } from 'rxjs';
import { ClientFormModalComponent } from '../admin-client-project/client-form-modal/client-form-modal.component';
import { ProjectFormModalComponent } from '../admin-client-project/project-form-modal/project-form-modal.component';
import { ClientRow, ProjectRow } from './client-project-row.interface';
import { MemberFormModalComponent } from '../admin-client-project/member-form-modal/member-form-modal.component';
import { AllowedTableActions, LinkType, MiningTableAction } from '@app/shared/components/mining-table/mining-table-action.interface';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { ClientControllerV2Service, ClientPojo, Member, MemberControllerService, PageMember, ProjectControllerService,
  ProjectControllerV2Service, ProjectPojo, ProjectPojoPrototype } from '@innowake/mining-api-angular-client';

const MEMBER_PAGE = 0;
const MEMBER_PAGE_SIZE = 10;
const membersKey = 'manageClientAndProject.members';
const memberKey = 'manageClientAndProject.member';

const TABLE_OPTIONS = {
    addProject: 'addProject',
    editClient: 'editClient',
    editClientAdmins: 'editClientAdmins',
    deleteClient: 'deleteClient',
    viewProject: 'viewProject',
    editProject: 'editProject',
    editMembers: 'editMembers',
    deleteProject: 'deleteProject',
    toolTip: 'tooltip',
    children: 'children'
};

@Component({
    selector: 'app-manage-client-and-projects',
    templateUrl: './manage-client-and-projects.component.html'
})
export class ManageClientAndProjectsComponent implements OnInit {
    @ViewChild('deleteClientTemplate') deleteClientContent: TemplateRef<any>;
    @ViewChild('deleteProjectTemplate') deleteProjectContent: TemplateRef<any>;
    clientList: ClientRow[] = [];
    labels: { [key: string]: { [key: string]: string } };
    dataChangeEvent: Subject<MiningTableAction> = new Subject<MiningTableAction>();
    tableConfig: MiningTableConfig = {
        columnMap: {
            name: { field: 'name', header: 'Client Projects', filterProperties: { filterType: FilterType.freeText }},
            admin: { field: 'admin', header: 'Client Admin', filterProperties: { filterType: FilterType.freeText }},
            nature: { field: 'nature', header: 'Project Nature', filterProperties: { filterType: FilterType.freeText }},
            memberCount: { field: 'memberCount', header: 'Members', toolTipField: 'members' }
        },
        paginator: true,
        rows: 10,
        serverSidePagination: false,
        loading: true,
        actions: [
            [{
                type: LinkType.DROPDOWN,
                icon: 'ellipsis',
                options: [],
                disableItem: (): boolean => false
            }],
            [{  type: LinkType.DROPDOWN,
                icon: 'ellipsis',
                options: [
                    { label: 'manageClientAndProject.viewProject', value: TABLE_OPTIONS.viewProject, disableItem: (): boolean => false },
                    { label: 'manageClientAndProject.editProjectDetails', value: TABLE_OPTIONS.editProject, disableItem: (): boolean => false },
                    { label: 'manageClientAndProject.editMembers', value: TABLE_OPTIONS.editMembers, disableItem: (): boolean => false },
                    { label: 'manageClientAndProject.deleteProject', value: TABLE_OPTIONS.deleteProject, styleClass: 'ant-helper__highlight-text',
                      disableItem: (): boolean => false }
                ],
                disableItem: (): boolean => false
            }]
        ]
    };

    projectToBeRemoved: ProjectPojoPrototype[];
    selectedClient: ClientRow;
    selectedProject: ProjectRow;

    constructor(
        private clientService: ClientControllerV2Service,
        private memberService: MemberControllerService,
        private projectServiceV1: ProjectControllerService,
        private projectServiceV2: ProjectControllerV2Service,
        private modalService: NzModalService,
        private translateService: TranslateService,
        private sanitizer: DomSanitizer,
        private messageService: NzMessageService,
        private router: Router,
        private clientProjectRelationshipService: ClientProjectRelationshipService,
        public authorizationService: KeycloakAuthorizationService,
        private labelMappingService: LabelMappingService
    ) { }

    ngOnInit(): void {
        this.clientProjectRelationshipService.setClientProjectRelationship(null);
        if (this.authorizationService.isAdmin()) {
            this.tableConfig.actions[0][0].options = [
                { label: 'manageClientAndProject.addProject', value: TABLE_OPTIONS.addProject, disableItem: () => false },
                { label: 'manageClientAndProject.editClientDetails', value: TABLE_OPTIONS.editClient, disableItem: () => false },
                { label: 'manageClientAndProject.editClientAdmins', value: TABLE_OPTIONS.editClientAdmins, disableItem: () => false },
                { label: 'manageClientAndProject.deleteClientAndProjects', value: TABLE_OPTIONS.deleteClient, styleClass: 'ant-helper__highlight-text',
                  disableItem: () => false }
            ];
        } else if (this.authorizationService.isClientAdmin()) {
            this.tableConfig.actions[0][0].options = [
                { label: 'manageClientAndProject.addProject', value: TABLE_OPTIONS.addProject, disableItem: () => false },
                { label: 'manageClientAndProject.editClientAdmins', value: TABLE_OPTIONS.editClientAdmins, disableItem: () => false }
            ];
        }
        this.getClientList();
    }

    /**
     * Fetches all clients.
     */
    getClientList(): void {
        this.clientService.getAllClients().subscribe(resp => {
            if (resp.content && resp.content.length) {
                this.getClientData(resp.content);
            }
        });
    }

    /**
     * Opens the Modal for creating new client.
     *
     * @param client client details.
     */
    async createOrUpdateClient(client?: ClientRow): Promise<void>{
        const createModal = this.getModalInstance(ClientFormModalComponent, 'clientForm.createModalTitle');
        if (client && client.id) {
          const instance = createModal.getContentComponent();
          instance.client = client;
          instance.clientLogo = await this.getClientLogo(client.id);
        }
        createModal.afterClose.subscribe((result: ClientPojo) => this.onClientCreatedOrUpdated(result, client));
    }

    /**
     * Opens the Modal for creating new Project.
     *
     * @param project project Details.
     */
    createOrUpdateProject(project?: ProjectRow): void {
        const createModal = this.getModalInstance(ProjectFormModalComponent, 'projectForm.createModalTitle');
        const instance = createModal.getContentComponent();
        if (project && project.parent) {
          instance.clientId = project.parent.id;
          instance.clientName = project.parent.name;
          instance.project = project;
        } else if (project) {
          instance.clientId = project.id;
          instance.clientName = project.name;
        }
        createModal.afterClose.subscribe((result: ProjectPojo) => this.onCreateOrUpdateProject(result, project));
    }

    /**
     * Call back method for the options defined for the mining table.
     *
     * @param row row data for the option selected.
     */
    optionSelected(row: MiningTableOptionSelected): void {
        switch (row.optionValue) {
            case TABLE_OPTIONS.addProject:
                this.createOrUpdateProject(row.data as ProjectRow);
                break;

            case TABLE_OPTIONS.editClient:
                void this.createOrUpdateClient(row.data as ClientRow);
                break;

            case TABLE_OPTIONS.deleteClient:
                this.deleteClient(row.data as ClientRow);
                break;

            case TABLE_OPTIONS.viewProject:
                this.navigateToProject(row.data as ProjectRow);
                break;

            case TABLE_OPTIONS.editProject:
                this.createOrUpdateProject(row.data as ProjectRow);
                break;

            case TABLE_OPTIONS.editMembers:
                this.editMember(row.data as ProjectRow);
                break;

            case TABLE_OPTIONS.deleteProject:
                this.deleteProject(row.data as ProjectRow);
                break;

            case TABLE_OPTIONS.toolTip:
                this.getMembers(row.data.data);
                break;

            case TABLE_OPTIONS.editClientAdmins:
                this.editClientAdmins(row.data as ClientRow);
                break;

            case TABLE_OPTIONS.children:
                this.getProjectDataForClient(row);
                break;
        }
    }

    /**
     * Fetches the data for the client.
     *
     * @param clients Array of Clients.
     */
    private getClientData(clients: ClientPojo[]) {
        clients.forEach((item: ClientPojo, index: number) => {
            const clientData: ClientRow = {
                id: item.id,
                name: item.name,
                admin: '',
                memberCount: '0 ' + this.translateService.instant(membersKey),
                members: [],
                nature: '',
                children: [],
                nonExpandableRowIndent: 1
            };

            /* Fetched the Project Count for the client */
            this.projectServiceV2.findProjectCount(item.id).subscribe(count => {
                if (count === 0) {
                    clientData.children = null;
                }
            });

            /* Fetches the Admins for the client */
            this.memberService.findClientAdmins(item.id).subscribe(admins => {
                if (admins.content && admins.content.length) {
                    clientData.admin = admins.content.map((admin: Member) => admin.firstName ? admin.firstName + ' ' + admin.lastName : admin.email).join(', ');
                }
            });

            /* Fetches member count for the client */
            this.memberService.findMemberCountForClient(item.id).subscribe(count => {
                const memberLabel = count === 1 ? memberKey : membersKey;
                clientData.memberCount = count + ' ' + this.translateService.instant(memberLabel);
            });

            this.clientList.push(clientData);

            /* Re-initiate the clientList, Need to keep the delay of 500ms to make sure data is loaded. */
            if (index === clients.length - 1) {
                setTimeout(() => {
                    this.clientList = [...this.clientList];
                    this.tableConfig.loading = false;
                }, 500);
            }
        });
    }

    /**
     * Fetches all the projects for the client.
     *
     * @param row The row that represents the client in the Mining Table
     */
    private getProjectDataForClient(row: MiningTableOptionSelected) {
        const clientId: number = row.data.id;
        this.projectServiceV1.findProjectsForClient1(clientId).subscribe((projectList) => {
            const projects = projectList;
            const projectsCount = projects.length;
            if ( ! projectsCount && row.data.expand) {
                /* Remove the "Expand" button if there are no projects under this Client. */
                row.data.children = null;
            }
            projects.forEach((item: ProjectPojo, index: number) => {
                this.tableConfig.loading = true;
                this.getProjectData(clientId, item);
                if (index === projectsCount - 1) {
                    /* Emits the event for mining table, Need to keep the delay of 500ms to make sure data is loaded. */
                    setTimeout(() => {
                        this.tableConfig.loading = false;
                        this.dataChangeEvent.next({action: AllowedTableActions.EXPAND_CHILD, id: clientId});
                    }, 500);
                }
            });
        });
    }

    /**
     * Get data for the specific project.
     *
     * @param clientId id of the client.
     * @param project project for which data needs to be fetched.
     */
    private getProjectData(clientId: number, project: ProjectPojo) {
        const projectData: ProjectRow = {
            id: project.id,
            name: project.name,
            nature: '',
            memberCount: '0 ' + this.translateService.instant(membersKey),
            admin: '',
            members: []
        };

        const index = this.clientList.findIndex(client => client.id === clientId);
        if ( ! this.clientList[index].children) {
            /* If the "Expand" button was removed for this Client, we need to add it back. */
            this.clientList[index].children = [];
        }
        this.clientList[index].children.push(projectData);
        this.clientList = [...this.clientList];

        const projectIndex = this.clientList[index].children.findIndex(item => item.id === project.id);
        /* Fetches the member count for the client */
        this.memberService.findMemberCountForProject(project.id).subscribe((count: number) => {
            const memberLabel = count === 1 ? memberKey : membersKey;
            this.clientList[index].children[projectIndex].memberCount = count + ' ' + this.translateService.instant(memberLabel);
            this.clientList = [...this.clientList];
        });

        /* Fetches the project nature */
        this.projectServiceV2.findProjectNatures(project.id).subscribe((natures: any) => {
            this.clientList[index].children[projectIndex].nature = natures ?
                natures.map((nature: ProjectPojoPrototype.NaturesEnum) => this.labelMappingService.mapLabel(LabelType.PROJECT, nature)).join(', ') : '';
            this.clientList = [...this.clientList];
        });
    }

    /**
     * Method get called when client is added/updated to render UI changes.
     *
     * @param result modal response.
     * @param client client row data.
     */
    private onClientCreatedOrUpdated(result: ClientPojo | string, client?: ClientRow) {
        if (result instanceof Object) {
            if (client && client.id) {
                const index = this.clientList.findIndex(item => item.id === client.id);
                this.clientList[index].name = result.name;
                this.clientList = [... this.clientList];
            } else {
                this.getClientData([result]);
            }
        }
    }

    /**
     * Method get called when project is added/updated to render UI changes.
     *
     * @param result modal response.
     * @param project project row data.
     */
    private onCreateOrUpdateProject(result: ProjectPojo, project: ProjectRow) {
        if (result instanceof Object) {
            if (project && project.parent) {
                const index = this.clientList.findIndex(item => item.id === project.parent.id);
                const projectIndex = this.clientList[index].children.findIndex(pro => pro.id === result.id);
                this.clientList[index].children[projectIndex].name = result.name;
                this.clientList = [... this.clientList];
            } else {
                this.getProjectData(result['clientId'], result);
            }
        } else {
            this.clientList = [... this.clientList];
        }
    }

    /**
     * Opens the confirmation modal for the deleting client.
     *
     * @param client client row data.
     */
    private deleteClient(client: ClientRow) {
        this.projectToBeRemoved = client.children;
        this.selectedClient = client;
        const deleteModal = this.getModalInstance(ConfirmDeleteModalComponent, 'deleteClientModal.modalTitle');
        const instance = deleteModal.getContentComponent();
        instance.modalContent = this.deleteClientContent;
        instance.confirmationText = 'deleteClientModal.confirmText';
        instance.confirmationButtonText = 'btnLabel.delete';
        instance.isConfirmationReq = true;
        deleteModal.afterClose.subscribe((result: string) => this.onDeleteClient(result, client));
    }

    /**
     * Method get called when client is deleted to render UI changes.
     *
     * @param result modal response.
     * @param client client row data.
     */
    private onDeleteClient(result: string, client: ClientRow) {
        if (result === DELETE_MODAL_CONFIRMED) {
            this.clientService.deleteClient(client.id).subscribe(() => {
                const index = this.clientList.findIndex(item => item.id === client.id);
                this.clientList.splice(index, 1);
                this.clientList = [... this.clientList];
                this.messageService.create('success', `${this.translateService.instant('deleteClientModal.successDelete', { clientName: client.name })}`);
            }, () => {
                this.messageService.create('error', `${this.translateService.instant('deleteClientModal.errorDelete')}`);
            });
        }
    }

    /**
     * Opens the confirmation modal for the delete client confirmation.
     *
     * @param client client row data.
     */
    private deleteProject(project: ProjectRow) {
        this.selectedProject = project;
        const deleteModal = this.getModalInstance(ConfirmDeleteModalComponent, 'deleteProjectModal.modalTitle');
        const instance = deleteModal.getContentComponent();
        instance.modalContent = this.deleteProjectContent;
        instance.confirmationText = 'manageClientAndProject.deleteProjectConfirmation';
        instance.confirmationButtonText = 'btnLabel.delete';
        instance.isConfirmationReq = true;
        deleteModal.afterClose.subscribe((result: string) => this.onDeleteProject(result, project));
    }

    /**
     * Method get called when project is deleted to render UI changes.
     *
     * @param result modal response.
     * @param project project row data.
     */
    private onDeleteProject(result: string, project: ProjectRow) {
        if (result === DELETE_MODAL_CONFIRMED) {
            this.projectServiceV2.deleteProject(project.id).subscribe(() => {
                const index = this.clientList.findIndex(item => item.id === project.parent.id);
                const projectIndex = this.clientList[index].children.findIndex(item => item.id === project.id);
                this.clientList[index].children.splice(projectIndex, 1);
                if ( ! this.clientList[index].children.length) {
                    /* Remove the "Expand" button if there are no projects under this Client. */
                    this.clientList[index].children = null;
                }
                this.clientList = [... this.clientList];
                this.messageService.create('success',
                    `${this.translateService.instant('deleteProjectModal.successDelete', { projectName: project.name })}`);
            }, () => {
                this.messageService.create('error', `${this.translateService.instant('deleteProjectModal.errorDelete')}`);
            });
        }
    }

    /**
     * Opens the modal to edit the members.
     *
     * @param project project row data.
     */
    private editMember(project: ProjectRow) {
        const memberModal = this.getMembersModalInstance(
            MemberFormModalComponent,
            'manageClientAndProject.editProjectMembers',
            { projectName: project.name }
        );
        const instance = memberModal.getContentComponent();
        instance.clientName = project.parent.id;
        instance.project = project;
        memberModal.afterClose.subscribe(() => this.onEditMember(project));
    }

    /**
     * Method get called when project members are updated to render UI changes.
     *
     * @param project project row data.
     */
    private onEditMember(project: ProjectRow) {
        this.memberService.findMembersForProject(project.id).subscribe((resp: PageMember) => {
            if (resp.content) {
                const count = resp.content.length;
                const memberLabel = count === 1 ? memberKey : membersKey;
                project.memberCount = count + ' ' + this.translateService.instant(memberLabel);
                project.members = this.getMemberArray(resp, project.id, true);
                if (project?.parent) {
                    this.onEditClientAdmins(project.parent, project);
                }
            }
        });
    }

    /**
     * Creates the modal service instance.
     *
     * @param modalData data sent to the modal.
     * @param component component for the opened modal.
     * @param title title for the modal.
     * @returns reference of the modal.
     */
    private getModalInstance(component: any, title: string, titleVal?: any): NzModalRef<any, any> {
        return this.modalService.create({
            nzTitle: this.translateService.instant(title, titleVal as object),
            nzClosable: true,
            nzMaskClosable: false,
            nzKeyboard: true,
            nzAutofocus: null,
            nzContent: component,
        });
    }

    /**
     * Gets the logo for the client.
     *
     * @param clientId id of the client.
     * @returns Promise of type string.
     */
    private async getClientLogo(clientId: number): Promise<string> {
        const result = new Promise<string>((resolve) => {
            this.clientService.getLogo(clientId).subscribe(logo => {
                resolve(this.sanitizer.bypassSecurityTrustResourceUrl(logo.toString()) as string);
            }, () => {
                resolve('');
            });
        });
        return await result;
    }

    /**
     * Navigates away to the project page.
     *
     * @param project project row data.
     */
    private navigateToProject(project: ProjectRow) {
        const clientProjectRelation = new ClientProjectRelationship(
            project.parent.id,
            project.parent.name,
            project.id,
            project.name
        );
        this.clientProjectRelationshipService.setClientProjectRelationship(clientProjectRelation);
        void this.router.navigate([RouteBuilder.buildProjectRoute(project.id, 'dashboard')]);
    }

    /**
     * Fetches members detail for the client/project.
     *
     * @param entity mining table row data.
     */
    private getMembers(entity: any) {
        if (entity.memberCount.trim().charAt(0) !== '0') {
            if ( ! entity.members.length) {
                let membersRequest;
                const nodeId: number = entity.id;
                if (entity.level > 0) {
                    membersRequest = this.memberService.findMembersForProject(nodeId, MEMBER_PAGE, MEMBER_PAGE_SIZE);
                } else {
                    membersRequest = this.memberService.findMembersForClient(nodeId, MEMBER_PAGE, MEMBER_PAGE_SIZE);
                }
                membersRequest.subscribe((resp: PageMember) => {
                    if (resp.content) {
                        entity.members = this.getMemberArray(resp, nodeId, entity.level as boolean);
                        this.dataChangeEvent.next({action: AllowedTableActions.TOOLTIP, data: entity});
                    }
                });
            } else {
                this.dataChangeEvent.next({action: AllowedTableActions.TOOLTIP, data: entity});
            }
        } else {
            entity.members = [];
            this.dataChangeEvent.next({action: AllowedTableActions.TOOLTIP, data: entity});
        }
    }

    /**
     * Opens the modal to edit the Client Admins.
     *
     * @param clientRow client row data.
     */
    private editClientAdmins(clientRow: ClientRow) {
        const memberModal = this.getMembersModalInstance(
            MemberFormModalComponent,
            'manageClientAndProject.editClientAdminMembersLabel',
            { clientName: clientRow.name });
        const instance = memberModal.getContentComponent();
        instance.client = clientRow;
        memberModal.afterClose.subscribe(() => this.onEditClientAdmins(clientRow));
    }

    /**
     * Method get called when client admins are updated to render UI changes of updating the
     * Client Admin column and members count.
     *
     * @param clientRow Client row data.
     * @param projectRow project row data.
     */
    private onEditClientAdmins(clientRow: ClientRow, projectData?: ProjectRow) {
        const clientAdmins = this.memberService.findClientAdmins(clientRow.id);
        const memCount = this.memberService.findMemberCountForClient(clientRow.id);
        forkJoin([clientAdmins, memCount]).subscribe(([admins, count]: any) => {
            if (admins.content) {
                clientRow.admin = admins.content.map((admin: Member) => admin.firstName ? admin.firstName + ' ' + admin.lastName : admin.email).join(', ');
                const memberLabel = count === 1 ? memberKey : membersKey;
                clientRow.memberCount = count + ' ' + this.translateService.instant(memberLabel);
                if (projectData) {
                    const index = clientRow.children.findIndex(item => item.id === projectData.id);
                    clientRow.children[index] = projectData;
                }
                const index = this.clientList.findIndex(item => item.id === clientRow.id);
                this.clientList[index] = clientRow;
                this.clientList = [... this.clientList];
            }
        });
    }

    /**
     * Creates the modal for the Members update.
     *
     * @param modalData data sent to the modal.
     * @param component component for the opened modal.
     * @param title title for the modal.
     * @param titleVal title value to show along with the title
     * @returns reference of the modal.
     */
    private getMembersModalInstance(component: any, title: string, titleVal?: any): NzModalRef<any, any> {
        return this.modalService.create({
            nzTitle: this.translateService.instant(title, titleVal as object),
            nzClosable: true,
            nzKeyboard: true,
            nzAutofocus: null,
            nzMaskClosable: false,
            nzContent: component,
            nzFooter: null,
            nzWidth: 800
        });
    }

    /**
     * Creates the Members string.
     *
     * @param members Member.
     * @param id project/Client id.
     * @param isProjectMember wether to fetch role detail or not.
     * @returns Array of Members.
     *
     */
    private getMemberArray(members: PageMember, id: number, isProjectMember: boolean): string[] {
        return members.content.map((member: Member) => {
            let nameString = member.firstName ? member.firstName + ' ' + member.lastName : member.email;
            if (isProjectMember) {
                const role = member.projectRoles.find(item => item.projectId === id);
                if (role && role.userRole) {
                    nameString += ' (' + role.userRole.toLowerCase().replace(/^\w/, (c) => c.toUpperCase()) + ')';
                }
            }
            return nameString;
        });
    }
}
