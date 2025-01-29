import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { ClientCardComponent } from './client-card.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { of } from 'rxjs/internal/observable/of';
import { Observable } from 'rxjs';
import { AdminClientProjectModule } from '@app/modules/admin-client-project/admin-client-project.module';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Subject } from 'rxjs';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { KeycloakService } from '@app/core/authentication/keycloak.service';
import { ClientControllerV2Service, ClientPojo, ProjectControllerService, ProjectControllerV2Service, ProjectPojo } from '@innowake/mining-api-angular-client';

const testClient: ClientPojo = {
  id: 1,
  name: 'test client'
};

describe('ClientCardComponent', () => {
  let component: ClientCardComponent;
  let fixture: ComponentFixture<ClientCardComponent>;
  const projectServiceV2Spy = jasmine.createSpyObj<ProjectControllerV2Service>('ProjectControllerV2Service', ['findProjectCount']);
  const projectServiceV1Spy = jasmine.createSpyObj<ProjectControllerService>('ProjectControllerService', ['findProjectsForClient1']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  const clientServiceSpy = jasmine.createSpyObj<ClientControllerV2Service>('ClientControllerV2Service', ['getLogo', 'deleteClient']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'getContentComponent']);
  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('clientProjectRelationshipService',
        ['setClientProjectRelationship']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);
  const keycloakServiceSpy = jasmine.createSpyObj<KeycloakService>('KeycloakService', ['getUserRoles', 'getUserInitials']);
  const mockLogoBlob = new Blob(['aaaaa'], { type: 'plain/txt' });
  let translateService: TranslateService;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ClientCardComponent ],
      providers: [
        { provide: ProjectControllerV2Service, useValue: projectServiceV2Spy },
        { provide: ProjectControllerService, useValue: projectServiceV1Spy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: ClientControllerV2Service, useValue: clientServiceSpy },
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
        { provide: KeycloakService, useValue: keycloakServiceSpy }
      ],
      imports: [
        BrowserAnimationsModule,
        HttpClientTestingModule,
        NzMessageModule,
        AdminClientProjectModule,
        TranslateModule.forRoot({})
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ClientCardComponent);
    translateService = TestBed.inject(TranslateService);
    component = fixture.componentInstance;
    component.client = testClient;

    clientServiceSpy.getLogo.and.returnValue(of(mockLogoBlob as any));
    projectServiceV2Spy.findProjectCount.and.returnValue(of(2 as any));
    projectServiceV1Spy.findProjectsForClient1.and.returnValue(of());
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    messageServiceSpy.create.and.returnValue(null);
    fixture.detectChanges();
  });

  it('should create and get client logo and project count', waitForAsync(() => {
    expect(component).toBeTruthy();
    fixture.whenStable().then(() => {
      expect(clientServiceSpy.getLogo).toHaveBeenCalled();
      expect(projectServiceV2Spy.findProjectCount).toHaveBeenCalled();
      expect(component.projectCount).toBe(2);
    });
  }));

  it('should get the project list', () => {
    component.getProjectList();
    expect(projectServiceV1Spy.findProjectsForClient1).toHaveBeenCalled();
  });

  it('should not get the project list when already loaded', () => {
    projectServiceV1Spy.findProjectsForClient1.calls.reset();
    component.projectList = [
      { name: 'Project 1' },
      { name: 'Project 2' }
    ];
    fixture.detectChanges();
    component.getProjectList();
    expect(projectServiceV1Spy.findProjectsForClient1).not.toHaveBeenCalled();
  });

  it('should set a placeholder for project list', () => {
    projectServiceV1Spy.findProjectsForClient1.calls.reset();
    component.client = { name: 'Test client' };
    component.projectCount = 0;
    fixture.detectChanges();
    component.getProjectList();
    expect(component.projectList.length).toBe(1);
    expect(projectServiceV1Spy.findProjectsForClient1).not.toHaveBeenCalled();
  });

  it('should shorten project list when more than 10', waitForAsync(() => {
    projectServiceV2Spy.findProjectCount.and.returnValue(of(15 as any));
    component.ngOnInit();
    const longProjectList: ProjectPojo[] = [];
    for (let i = 0; i < 9; i++) {
      longProjectList.push({
        name: 'Project ' + i
      });
    }
    projectServiceV1Spy.findProjectsForClient1.and.returnValue(of(longProjectList as any));
    fixture.detectChanges();
    component.getProjectList();
    fixture.whenStable().then(() => {
      fixture.detectChanges();
      expect(component.projectList.length).toBe(10);
      expect(component.projectList[9].name).toBe('...');
    });
  }));

  it('should open the client form modal', () => {
    component.onEditClient();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should open the edit client admins form modal', () => {
    component.onEditClientAdmins();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should open the client delete modal', () => {
    nzModalRefSpy.afterClose = new Subject<string>();
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    component.onDeleteClient();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should display error if some error occurred while deleting on confirming on delete client modal', () => {
    nzModalRefSpy.afterClose = new BehaviorSubject<string>(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    clientServiceSpy.deleteClient.and.returnValue(new Observable(subscriber =>
      subscriber.error(new Error('Error while deleting client.'))));
    component.onDeleteClient();
    expect(messageServiceSpy.create).toHaveBeenCalledWith('error', translateService.instant('deleteClientModal.errorDelete'));
  });

  it('should display success message on successful deletion on confirming on delete client modal', () => {
    nzModalRefSpy.afterClose = new BehaviorSubject<string>(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    clientServiceSpy.deleteClient.and.returnValue(of(null));
    spyOn(component.clientModified, 'emit').and.callFake(() => { });
    component.onDeleteClient();
    expect(component.clientModified.emit).toHaveBeenCalled();
    expect(messageServiceSpy.create).toHaveBeenCalledWith(
      'success', translateService.instant('deleteClientModal.successDelete', { clientName: component.client.name }));
  });

  it('should navigate to the select project page', () => {
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');
    const clientProjectRelationshipParams = new ClientProjectRelationship(testClient.id, testClient.name);
    component.onCardClick();
    expect(clientProjectRelationshipServiceSpy.setClientProjectRelationship).toHaveBeenCalledWith(clientProjectRelationshipParams);
    expect(router.navigate).toHaveBeenCalledWith([RouteBuilder.buildClientRoute(testClient.id, 'projects')]);
  });
});
