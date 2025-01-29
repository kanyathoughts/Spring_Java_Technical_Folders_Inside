import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Component } from '@angular/core';
import { TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ActivatedRoute, convertToParamMap, Router } from '@angular/router';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule } from '@ngx-translate/core';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { of } from 'rxjs/internal/observable/of';
import { ClientProjectRelationshipService } from '../services/client-project-relationship/client-project-relationship.service';
import { ClientProjectGuard } from './client-project.guard';
import { ClientControllerService, ClientPojo, ModuleControllerService, ModulePojo, ProjectControllerService, ProjectPojo } from '@innowake/mining-api-angular-client';

describe('ClientProjectGuard', () => {
  let service: ClientProjectGuard;
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(2, 'TestClient', 2, 'TestProject');
  const ProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(2, 'TestClient', 3, 'TestProject');

  const testClient: ClientPojo = {
        id: 2,
        name: 'test Client'
    };
    const testProject: ProjectPojo = {
        id: 2,
        name: 'test Project',
        clientId: 2
    };
    const testModule: ModulePojo = {
        id: 2,
        name: 'test ModulePojo',
        projectId: 2
    }
    const testModuleProjectRelationship: ModulePojo = {
        id: 1,
        name: 'test ModulePojo',
        projectId: 1
    }
    const clientProjectRelationShipSpy = jasmine.createSpyObj<ClientProjectRelationshipService>(
        'ClientProjectRelationshipService',
        ['getClientProjectObservable', 'setClientProject', 'setProject']
    );
    const clientServiceSpy = jasmine.createSpyObj<ClientControllerService>('ClientControllerService', ['findClientById']);
    const projectServiceSpy = jasmine.createSpyObj<ProjectControllerService>('ProjectControllerService', ['findProjectById']);
    const moduleServiceSpy = jasmine.createSpyObj<ModuleControllerService>('ModuleControllerService', ['findModuleById']);
    const routerSpy = jasmine.createSpyObj<Router>('Router', ['navigate']);
    const iamSpy = jasmine.createSpyObj<IdentityAccessManagementService>('IdentityAccessManagementService', ['isAccessTokenFromUrl']);

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [MockComponent],
            imports: [
                HttpClientTestingModule,
                BrowserAnimationsModule,
                NzMessageModule,
                TranslateModule.forRoot({})
            ],
            providers:  [
                { provide: ClientControllerService, useValue: clientServiceSpy },
                { provide: ProjectControllerService, useValue: projectServiceSpy },
                { provide: ModuleControllerService, useValue: moduleServiceSpy },
                { provide: ClientProjectRelationshipService, useValue: clientProjectRelationShipSpy},
                { provide: Router, useValue: routerSpy },
                { provide: IdentityAccessManagementService, useValue: iamSpy }
            ],
        }).compileComponents();
        clientProjectRelationShipSpy.getClientProjectObservable.and.returnValue(of(new ClientProjectRelationship(1, 'client', 1, 'project')));
        clientServiceSpy.findClientById.and.returnValue(of(testClient as any));
        projectServiceSpy.findProjectById.and.returnValue(of(testProject as any));
        moduleServiceSpy.findModuleById.and.returnValue(of(testModule as any));
        routerSpy.navigate.calls.reset();
        clientProjectRelationShipSpy.setClientProject.calls.reset();
        clientProjectRelationShipSpy.setProject.calls.reset();
        moduleServiceSpy.findModuleById.calls.reset();
        iamSpy.isAccessTokenFromUrl.and.returnValue(false);
    }));

    beforeEach(() => {
      service = TestBed.inject(ClientProjectGuard);
    });

    it('should redirect to the Select Client page', waitForAsync(() => {
        let snapshot = {
            paramMap: convertToParamMap({
              projectId: 'client-test-wrong-param'
            })
        };
        service.canActivate(snapshot as any).subscribe(() => {
            expect(routerSpy.navigate).toHaveBeenCalledWith(['/clients']);
        })
    }));

    it('should resolve client route', waitForAsync(() => {
        let snapshot = {
            paramMap: convertToParamMap({
              projectId: 'client-1'
            }),
            data: {
                title: 'Test'
            }
        };
        const result = service.canActivate(snapshot as any).subscribe(() => { })
        expect(result).toBeTruthy();
    }));

    it('should resolve project route', waitForAsync(() => {
        let snapshot = {
            events: of(),
            paramMap: convertToParamMap({
                projectId: 'project-2'
            }),
            data: {
                title: 'Test'
            }
        };
        service.canActivate(snapshot as any).subscribe(() => {
            expect(clientProjectRelationShipSpy.setClientProject).toHaveBeenCalledWith(testClient, testProject);
        })
    }));

    it('should resolve else for project details', waitForAsync(() => {
      let snapshot = {
          events: of(),
          paramMap: convertToParamMap({
              projectId: 'project-2'
          }),
          data: {
              title: 'Test'
          }
      };
      service['currentClient'] = clientProjectRelationship;
      const result = service.canActivate(snapshot as any).subscribe(() => { });
      expect(result).toBeTruthy();
  }));

  it('should resolve else project route', waitForAsync(() => {
    let snapshot = {
        events: of(),
        paramMap: convertToParamMap({
            projectId: 'project-2'
        }),
        data: {
            title: 'Test'
        }
    };
    service['currentClient'] = ProjectRelationship;
    service.canActivate(snapshot as any).subscribe(() => {
        expect(clientProjectRelationShipSpy.setProject).toHaveBeenCalledWith(testProject);
    })
}));

});

@Component({
    template: ''
})
class MockComponent {
    resolvedData: any;
    constructor(private route: ActivatedRoute) {
        this.route.data.subscribe((data: { resolvedData: any}) => {
            this.resolvedData = data.resolvedData;
        });
    }
}