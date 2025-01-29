import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterModule, Router } from '@angular/router';
import { KeycloakService} from '@app/core/authentication/keycloak.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { BehaviorSubject, Subject, Observable } from 'rxjs';
import { of } from 'rxjs/internal/observable/of';

import { ProjectCardComponent } from './project-card.component';
import { MemberControllerService, PageMember, ProjectControllerV2Service, ProjectPojo, ProjectRole } from '@innowake/mining-api-angular-client';

describe('ProjectCardComponent', () => {
  let component: ProjectCardComponent;
  let fixture: ComponentFixture<ProjectCardComponent>;
  const memberServiceSpy = jasmine.createSpyObj<MemberControllerService>('MemberControllerService', ['findMemberCountForProject', 'findMembersForProject']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj('ClientProjectRelationshipService', ['setClientProjectRelationship']);
  const projectServiceSpy = jasmine.createSpyObj<ProjectControllerV2Service>('ProjectControllerV2Service', ['deleteProject']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'getContentComponent']);
  const keycloakServiceSpy = jasmine.createSpyObj<KeycloakService>('KeycloakService', ['getUserRoles', 'getUserInitials']);

  const project: ProjectPojo = {
    id: 1,
    name: 'Test Project'
  };
  const clienId = 1;
  const clientName = 'Test Client';
  let translateService: TranslateService;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ProjectCardComponent ],
      providers: [
        { provide: MemberControllerService, useValue: memberServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: ProjectControllerV2Service, useValue: projectServiceSpy },
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
        { provide: KeycloakService, useValue: keycloakServiceSpy}
      ],
      imports: [
        TranslateModule.forRoot({}),
        HttpClientTestingModule,
        NzMessageModule,
        RouterModule,
        BrowserAnimationsModule
      ]
    })
    .compileComponents();
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectCardComponent);
    translateService = TestBed.inject(TranslateService);
    component = fixture.componentInstance;
    component.project = project;
    component.clientId = clienId;
    component.clientName = clientName;
    memberServiceSpy.findMemberCountForProject.and.returnValue(of());
    memberServiceSpy.findMembersForProject.and.returnValue(of());
    projectServiceSpy.deleteProject.and.returnValue(of());
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
    expect(memberServiceSpy.findMemberCountForProject).toHaveBeenCalled();
  });

  it('should navigate to project page', () => {
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');
    component.onCardClick();
    expect(clientProjectRelationshipServiceSpy.setClientProjectRelationship).toHaveBeenCalled();
    expect(router.navigate).toHaveBeenCalledWith([RouteBuilder.buildProjectRoute(project.id, 'dashboard')]);
  });

  it('should get the project members', () => {
    memberServiceSpy.findMemberCountForProject.and.returnValue(of(3 as any));
    component.ngOnInit();
    component.loadMembers();
    expect(memberServiceSpy.findMembersForProject).toHaveBeenCalled();
  });

  it('should not get the project members when count is undefined', () => {
    memberServiceSpy.findMembersForProject.calls.reset();
    component.loadMembers();
    expect(memberServiceSpy.findMembersForProject).not.toHaveBeenCalled();
  });

  it('should not get the project members when list exist', () => {
    memberServiceSpy.findMembersForProject.calls.reset();
    component.memberList = [{ firstName: 'Test User'}];
    component.loadMembers();
    expect(memberServiceSpy.findMembersForProject).not.toHaveBeenCalled();
  });

  it('should shorten the member list when 10 and upward', waitForAsync(() => {
    memberServiceSpy.findMemberCountForProject.and.returnValue(of(15 as any));
    component.ngOnInit();
    const longPageMember: PageMember = {content: []};
    for (let i = 0; i < 9; i++) {
      longPageMember.content.push({
        firstName: 'Test member ' + i,
        projectRoles: [{
          projectId: 1,
          userRole: ProjectRole.UserRoleEnum.VIEWER
        }]
      });
    }
    memberServiceSpy.findMembersForProject.and.returnValue(of(longPageMember as any));
    component.loadMembers();
    fixture.whenStable().then(() => {
      expect(component.displayMemberList.length).toBe(10);
      expect(component.displayMemberList[9]).toBe('...');
    });
  }));

  it('Should adapt display memeber to available informations', () => {
    memberServiceSpy.findMemberCountForProject.and.returnValue(of(3 as any));
    component.ngOnInit();
    const pageMember: PageMember = { content: [
      {
        firstName: 'Test',
        lastName: 'Test',
        projectRoles: [{
          projectId: 1,
          userRole: ProjectRole.UserRoleEnum.VIEWER
        }]
      },
      {
        email: 'test@test.com',
        projectRoles: [{
          projectId: 1,
          userRole: ProjectRole.UserRoleEnum.VIEWER
        }]
      }
    ]};
    memberServiceSpy.findMembersForProject.and.returnValue(of(pageMember as any));
    component.loadMembers();
    fixture.whenStable().then(() => {
      expect(component.displayMemberList[0]).toBe('Test Test (Viewer)');
      expect(component.displayMemberList[1]).toBe('test@test.com (Viewer)');
    });
  });

  it('should open modal to edit project', () => {
    component.onEditProject();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should open modal to edit project members', () => {
    component.onEditProjectMembers();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should open modal to delete project', () => {
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    component.onDeleteProject();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should display error if some error occurred while deleting on confirming on delete project modal', () => {
    nzModalRefSpy.afterClose = new BehaviorSubject<string>(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    projectServiceSpy.deleteProject.and.returnValue(new Observable(subscriber =>
      subscriber.error(new Error('Error while deleting client.'))));
    component.onDeleteProject();
    expect(messageServiceSpy.create).toHaveBeenCalledWith('error', translateService.instant('deleteProjectModal.errorDelete'));
  });

  it('should display success message on successful deletion on confirming on delete client modal', () => {
    nzModalRefSpy.afterClose = new BehaviorSubject<string>(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    projectServiceSpy.deleteProject.and.returnValue(of(null));
    spyOn(component.projectModified, 'emit').and.callFake(() => { });
    component.onDeleteProject();
    expect(component.projectModified.emit).toHaveBeenCalled();
    expect(messageServiceSpy.create).toHaveBeenCalledWith(
      'success', translateService.instant('deleteProjectModal.successDelete', { clientName: component.project.name }));
  });
});
