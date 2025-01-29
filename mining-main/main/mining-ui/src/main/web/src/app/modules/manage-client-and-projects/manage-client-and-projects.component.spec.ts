import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { ManageClientAndProjectsComponent } from './manage-client-and-projects.component';
import { TranslateModule } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { of, Subject, throwError } from 'rxjs';
import { ConfirmDeleteModalComponent, DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { ClientRow, ProjectRow } from './client-project-row.interface';
import { Router } from '@angular/router';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { MiningTableConfig } from '@app/shared/components/mining-table/mining-table-config.interface';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { HttpClient } from '@angular/common/http';
import { ClientControllerV2Service, ClientPojo, Member, MemberControllerService, ProjectControllerService, ProjectControllerV2Service, ProjectPojo } from '@innowake/mining-api-angular-client';

describe('ManageClientAndProjectsComponent', () => {
  let component: ManageClientAndProjectsComponent;
  let fixture: ComponentFixture<ManageClientAndProjectsComponent>;
  const clientServiceSpy = jasmine.createSpyObj<ClientControllerV2Service>('ClientControllerV2Service', [
    'getAllClients',
    'deleteClient',
    'getLogo'
  ]);

  const memberServiceSpy = jasmine.createSpyObj<MemberControllerService>('MemberControllerService', [
    'findClientAdmins',
    'findMemberCountForClient',
    'findMemberCountForProject',
    'findMembersForProject',
    'findMembersForClient'
  ]);

  const projectServiceV2Spy = jasmine.createSpyObj<ProjectControllerV2Service>('ProjectControllerV2Service', [
    'findProjectNatures',
    'deleteProject',
    'findProjectCount'
  ]);

  const projectServiceV1Spy = jasmine.createSpyObj<ProjectControllerService>('ProjectControllerService', [
    'findProjectsForClient1'
  ]);

  const authorizationSpy = jasmine.createSpyObj<KeycloakAuthorizationService>('KeycloakAuthorizationService', [
    'isAdmin',
    'isClientAdmin'
  ]);

  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'getContentComponent']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix']);

  const mockLogoBlob = new Blob(['aaaaa'], { type: 'plain/txt' });
  const testClient: ClientPojo = { id: 1, name: 'test client' };
  const testClientAdmin: Member = { firstName: 'test', lastName: 'Admin' };
  const testMember: Member = { firstName: 'test', lastName: 'Member', projectRoles: [] };
  const testProject: ProjectPojo = { id: 1, name: 'test project' };
  const testProjectV1: ProjectPojo = { id: 1, name: 'test project' };
  const testNature: string[] = ['nature 1', 'nature 2'];
  const TABLE_OPTIONS = {
    addProject: 'addProject',
    editClient: 'editClient',
    editClientAdmins: 'editClientAdmins',
    deleteClient: 'deleteClient',
    deleteProject: 'deleteProject',
  };
  const tableConfig: MiningTableConfig = {
    columnMap: {},
    actions: [[{
      options: [
        { label: 'manageClientAndProject.addProject', value: TABLE_OPTIONS.addProject, disableItem: (): boolean => {return false} },
        { label: 'manageClientAndProject.editClientDetails', value: TABLE_OPTIONS.editClient, disableItem: (): boolean => {return false} },
        { label: 'manageClientAndProject.editClientAdmins', value: TABLE_OPTIONS.editClientAdmins, disableItem: (): boolean => {return false} },
        { label: 'manageClientAndProject.deleteClientAndProjects', value: TABLE_OPTIONS.deleteClient, styleClass: 'ant-helper__highlight-text', disableItem: (): boolean => {return false} } 
      ]
    }]]
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ManageClientAndProjectsComponent],
      providers: [
        { provide: ClientControllerV2Service, useValue: clientServiceSpy },
        { provide: MemberControllerService, useValue: memberServiceSpy },
        { provide: ProjectControllerV2Service, useValue: projectServiceV2Spy },
        { provide: ProjectControllerService, useValue: projectServiceV1Spy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: authorizationSpy },
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
      ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        NzMessageModule,
        TranslateModule.forRoot({}),
        BrowserAnimationsModule
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ManageClientAndProjectsComponent);
    component = fixture.componentInstance;

    clientServiceSpy.getAllClients.and.returnValue(of({ content: [testClient] } as any));
    clientServiceSpy.getLogo.and.returnValue(of(mockLogoBlob as any));

    memberServiceSpy.findClientAdmins.and.returnValue(of({ content: [testClientAdmin] } as any));
    memberServiceSpy.findMemberCountForClient.and.returnValue(of(2 as any));
    memberServiceSpy.findMemberCountForProject.and.returnValue(of(2 as any));
    memberServiceSpy.findMembersForProject.and.returnValue(of({ content: [testMember] } as any));
    memberServiceSpy.findMembersForClient.and.returnValue(of({ content: [testMember] } as any));

    projectServiceV1Spy.findProjectsForClient1.and.returnValue(of([testProjectV1] as any));
    projectServiceV2Spy.findProjectNatures.and.returnValue(of(testNature as any));
    projectServiceV2Spy.findProjectCount.and.returnValue(of(2 as any));

    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next({});
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('check the table actions for Admin user', () => {
    authorizationSpy.isAdmin.and.returnValue(true);
    authorizationSpy.isClientAdmin.and.returnValue(false);
    component.ngOnInit();
    const options = tableConfig.actions[0][0].options;
    expect(component.tableConfig.actions[0][0].options.length).toEqual(options.length);
  });

  it('check the table actions for Client Admin user', () => {
    authorizationSpy.isAdmin.and.returnValue(false);
    authorizationSpy.isClientAdmin.and.returnValue(true);
    component.ngOnInit();
    const options = [tableConfig.actions[0][0].options[0], tableConfig.actions[0][0].options[2]];
    expect(component.tableConfig.actions[0][0].options.length).toEqual(options.length);
  });

  it('should get all the Client and projects', () => {
    component.clientList = [];
    component.getClientList();
    expect(component.clientList.length).toEqual(1);

    component.clientList = [];
    clientServiceSpy.getAllClients.and.returnValue(of({ content: [] } as any));
    component.getClientList();
    expect(component.clientList.length).toEqual(0);
  });

  it('should fetch data for clients', () => {
    component.clientList = [];
    const clients = [
      { id: 1, name: 'client 1' },
      { id: 2, name: 'client 2' },
    ];
    memberServiceSpy.findClientAdmins.and.returnValue(of({ content: [] } as any));
    memberServiceSpy.findMemberCountForClient.and.returnValue(of(1 as any));
    (component as any).getClientData(clients);
    expect(component.clientList.length).toEqual(2);
    expect(component.clientList[0].admin).toEqual('');
  });

  it('should fetch data for client project', () => {
    (component as any).getProjectDataForClient({ data : { id: 1}});
    expect(component.clientList[0].children.length).toEqual(1);
    projectServiceV2Spy.findProjectNatures.and.returnValue(of(false as any));
    memberServiceSpy.findMemberCountForProject.and.returnValue(of(1 as any));

    (component as any).getProjectData(1, { id: 2, name: 'test project 2' });
    expect(component.clientList[0].children.length).toEqual(2);
    expect(component.clientList[0].children[1].nature).toEqual('');
  });

  it('should open create or update client modal', () => {
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next(testClient);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);

    (component as any).createOrUpdateClient();
    expect(modalServiceSpy.create).toHaveBeenCalled();

    (component as any).createOrUpdateClient(testClient);
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should create or update the client', () => {
    const newClient: ClientPojo = { id: 2, name: 'new Client' };

    expect(component.clientList.length).toEqual(1);
    (component as any).onClientCreatedOrUpdated(false);
    expect(component.clientList.length).toEqual(1);

    (component as any).onClientCreatedOrUpdated(newClient);
    expect(component.clientList.length).toEqual(2);
    expect(component.clientList[1].name).toEqual('new Client');

    const updateClient: ClientPojo = { id: 2, name: 'new Client updated' };
    (component as any).onClientCreatedOrUpdated(updateClient, component.clientList[1]);
    expect(component.clientList.length).toEqual(2);
    expect(component.clientList[1].name).toEqual('new Client updated');
  });

  it('should open create or update project modal', () => {
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next(testProject);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);

    (component as any).createOrUpdateProject();
    expect(modalServiceSpy.create).toHaveBeenCalled();

    (component as any).createOrUpdateProject(testProject);
    expect(modalServiceSpy.create).toHaveBeenCalled();

    (component as any).createOrUpdateProject({ id: 1, name: 'project', parent: { id: 1, name: 'client' } });
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should create or update the project', () => {
    const newProject: ProjectPojo = { id: 2, name: 'new project', clientId: 1 };

    (component as any).getProjectDataForClient({ data : { id: 1}});
    (component as any).onCreateOrUpdateProject(false);
    expect(component.clientList[0].children.length).toEqual(1);

    (component as any).onCreateOrUpdateProject(newProject);
    expect(component.clientList[0].children.length).toEqual(2);
    expect(component.clientList[0].children[1].name).toEqual('new project');

    const updatedProject: ProjectPojo = { id: 2, name: 'new project update', clientId: 1 };
    (component as any).onCreateOrUpdateProject(updatedProject, { id: 2, name: 'new project', parent: { id: 1 } });
    expect(component.clientList[0].children.length).toEqual(2);
    expect(component.clientList[0].children[1].name).toEqual('new project update');
  });

  it('should open delete client modal', () => {
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);

    const client: ClientRow = { id: 1, name: 'test client', children: [] };
    (component as any).deleteClient(client);
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should delete client', () => {
    (component as any).onDeleteClient(false, testClient);
    expect(component.clientList.length).toEqual(1);

    clientServiceSpy.deleteClient.and.returnValue(throwError('test error'));
    (component as any).onDeleteClient(DELETE_MODAL_CONFIRMED, testClient);
    expect(component.clientList.length).toEqual(1);

    clientServiceSpy.deleteClient.and.returnValue(of(true as any));
    (component as any).onDeleteClient(DELETE_MODAL_CONFIRMED, testClient);
    expect(component.clientList.length).toEqual(0);
  });

  it('should open delete project modal', () => {
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);

    const project: ProjectRow = { id: 1, name: 'test project' };
    (component as any).deleteProject(project);
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should delete project', () => {
    (component as any).getProjectDataForClient({ data : { id: 1}});
    (component as any).onDeleteProject(false, testProject);
    expect(component.clientList[0].children.length).toEqual(1);

    const project: ProjectRow = { id: 1, name: 'test project', parent: testClient };
    projectServiceV2Spy.deleteProject.and.returnValue(throwError('test error'));
    (component as any).onDeleteProject(DELETE_MODAL_CONFIRMED, project);
    expect(component.clientList[0].children.length).toEqual(1);

    projectServiceV2Spy.deleteProject.and.returnValue(of(true as any));
    (component as any).onDeleteProject(DELETE_MODAL_CONFIRMED, project);
    expect(component.clientList[0].children).toBeFalsy();
  });

  it('should open edit project member modal', () => {
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next(true);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);

    const project: ProjectRow = { id: 1, name: 'test project', parent: testClient };
    (component as any).editMember(project);
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should edit project members', () => {
    const project: ProjectRow = { id: 1, name: 'test project', members: []};

    memberServiceSpy.findMembersForProject.and.returnValue(of({} as any));
    (component as any).onEditMember(project);
    expect(project.members).toEqual([]);

    memberServiceSpy.findMembersForProject.and.returnValue(of({ content: [testMember] } as any));
    (component as any).onEditMember(project);
    expect(project.members).toEqual(['test Member']);

    const editMember: Member = { firstName: 'updated', lastName: 'Member', projectRoles: [{ projectId: 1, userRole: 'MANAGER' }] };
    memberServiceSpy.findMembersForProject.and.returnValue(of({ content: [testMember, editMember] } as any));
    (component as any).onEditMember(project);
    expect(project.members).toEqual(['test Member', 'updated Member (Manager)']);
  });

  it('should not get the client logo', (done) => {
    clientServiceSpy.getLogo.and.returnValue(throwError('test error'));
    const emptyLogo = (component as any).getClientLogo();
    emptyLogo.then((result: string) => {
      expect(result).toEqual('');
      done();
    });
  });

  it('should navigate to product page', () => {
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');
    const project: ProjectRow = { id: 1, name: 'test project', parent: { id: 1, name: 'test client' } };
    (component as any).navigateToProject(project);
    expect(router.navigate).toHaveBeenCalledWith([RouteBuilder.buildProjectRoute(1, 'dashboard')]);
  });

  it('should get the members for project and client', () => {
    const entityClient: any = { id: 1, name: 'test client', memberCount: '1 member', members: [], level: 0 };
    (component as any).getMembers(entityClient);
    expect(entityClient.members).toEqual(['test Member']);

    const newMember: Member = { firstName: 'new', lastName: 'Member', projectRoles: [ { projectId: 1, userRole: 'MANAGER' }] };
    memberServiceSpy.findMembersForProject.and.returnValue(of({ content: [testMember, newMember] } as any));

    const entityProject: any = { id: 1, name: 'test project', memberCount: '1 member', members: [], level: 1, parent: testClient };
    (component as any).getMembers(entityProject);
    expect(entityProject.members).toEqual(['test Member', 'new Member (Manager)']);

    const noMember: any = { id: 1, name: 'test project', memberCount: '0 member', members: [] };
    (component as any).getMembers(noMember);
    expect(noMember.members).toEqual([]);
  });

  it('option selected should call respective methods', () => {
    const actions = [
      { optionValue: 'addProject', data: 'test data', method: 'createOrUpdateProject' },
      { optionValue: 'editClient', data: 'test data', method: 'createOrUpdateClient' },
      { optionValue: 'deleteClient', data: 'test data', method: 'deleteClient' },
      { optionValue: 'viewProject', data: 'test data', method: 'navigateToProject' },
      { optionValue: 'editProject', data: 'test data', method: 'createOrUpdateProject' },
      { optionValue: 'editMembers', data: 'test data', method: 'editMember' },
      { optionValue: 'deleteProject', data: 'test data', method: 'deleteProject' },
      { optionValue: 'tooltip', data: 'test data', method: 'getMembers' },
      { optionValue: 'editClientAdmins', data: 'test data', method: 'editClientAdmins' }
    ];

    const spiedMethods: any[] = [];
    for (const opt of actions) {
      if (!spiedMethods.includes(opt.method)) {
        spiedMethods.push(opt.method);
        spyOn((component as any), opt.method).and.callFake(() => { });
      }
      component.optionSelected(opt);
      expect((component as any)[opt.method]).toHaveBeenCalled();
    }
  });

  it('should open edit client admin modal', () => {
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next(true);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);

    const client: ClientRow = { id: 1, name: 'test client'};
    (component as any).editClientAdmins(client);
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should edit client admins', () => {
    const client: ClientRow = { id: 1, name: 'test client', admin: ''};
    memberServiceSpy.findClientAdmins.and.returnValue(of({} as any));
    memberServiceSpy.findMemberCountForClient.and.returnValue(of({} as any));
    (component as any).onEditClientAdmins(client);
    expect(client.admin).toEqual('');

    const editMember: Member = { firstName: 'Test', lastName: 'User', projectRoles: [ { projectId: 1, userRole: 'MANAGER' }] };
    memberServiceSpy.findClientAdmins.and.returnValue(of({ content: [testClient, editMember] } as any));
    memberServiceSpy.findMemberCountForClient.and.returnValue(of({} as any));
    (component as any).onEditClientAdmins(client);
    expect(client.admin).toEqual(', Test User');
  });

  it("should show proper delete confirm messages", function () {
    const clientData: ClientRow = {
      "id": 7,
      "name": "Demo Client 2",
      "admin": "",
      "memberCount": "0 Members",
      "members": [],
      "nature": "",
      "children": [
        {
          "id": 3,
          "name": "QEF Test",
          "nature": "",
          "memberCount": "0 Members",
          "admin": "",
          "members": []
        }
      ],
      "nonExpandableRowIndent": 1
    };
    spyOn((component as any), 'getModalInstance').and.callThrough();
    component.optionSelected({ optionValue: TABLE_OPTIONS.deleteClient, data: clientData });
    expect((component as any).getModalInstance).toHaveBeenCalledWith( ConfirmDeleteModalComponent, 'deleteClientModal.modalTitle');

    const projectData: ProjectRow = {
      "id": 3,
      "name": "QEF Test",
      "nature": "",
      "memberCount": "0 Members",
      "admin": "",
      "members": [],
      "parent": {
        "id": 7,
        "name": "Demo Client 2",
        "admin": "",
        "memberCount": "0 Members",
        "members": [],
        "nature": "",
        "children": [
          {
            "id": 3,
            "name": "QEF Test",
            "nature": "",
            "memberCount": "0 Members",
            "admin": "",
            "members": []
          }
        ],
        "nonExpandableRowIndent": 1,
      },
    };
    component.optionSelected({ optionValue: TABLE_OPTIONS.deleteProject, data: projectData });
    expect((component as any).getModalInstance).toHaveBeenCalledWith(ConfirmDeleteModalComponent, 'deleteProjectModal.modalTitle');
  });
});
