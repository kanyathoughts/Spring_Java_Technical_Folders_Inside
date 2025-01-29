import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { BehaviorSubject } from 'rxjs';
import { of } from 'rxjs/internal/observable/of';
import { Subject } from 'rxjs/internal/Subject';

import { ProjectsOverviewComponent } from './projects-overview.component';
import { ProjectControllerService, ProjectPojo } from '@innowake/mining-api-angular-client';

describe('ProjectsOverviewComponent', () => {
  let component: ProjectsOverviewComponent;
  let fixture: ComponentFixture<ProjectsOverviewComponent>;
  const projectServiceSpy = jasmine.createSpyObj<ProjectControllerService>('ProjectControllerService', ['findProjectsForClient1']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'getContentComponent']);
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(0, 'TestClient', 0, 'TestProject');
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship', 'setProject']);
  const clientProjectRelationshipNullParams = {
    clientId: 0,
    getClientId: () => 0,
    getClientName: () => null as any,
    clientName: null as any
  };
  const projectList: ProjectPojo[] = [
      {
        id: 1,
        name: 'test-1'
      },
      {
        id: 2,
        name: 'test-2'
      }
    ];

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ProjectsOverviewComponent ],
      providers: [
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: ProjectControllerService, useValue: projectServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              client: { id: 1, name: 'test' }
            })
          }
        }
      ],
      imports: [
        TranslateModule.forRoot({}),
        RouterTestingModule
      ]
    })
    .compileComponents();
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));  
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectsOverviewComponent);
    component = fixture.componentInstance;
    projectServiceSpy.findProjectsForClient1.and.returnValue(of(projectList as any));
    nzModalRefSpy.afterClose = new Subject;
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should open modal for project creation', () => {
    component.openCreateModal();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should update list on successful project creation', () => {
    nzModalRefSpy.afterClose = new BehaviorSubject<string>('success');
    component.openCreateModal();
    expect(projectServiceSpy.findProjectsForClient1).toHaveBeenCalled();
  });
});
