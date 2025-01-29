import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Component } from '@angular/core';
import { TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ActivatedRoute, convertToParamMap, Router, RouterStateSnapshot } from '@angular/router';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule } from '@ngx-translate/core';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { throwError } from 'rxjs';
import { of } from 'rxjs/internal/observable/of';
import { ClientProjectRelationshipService } from '../client-project-relationship/client-project-relationship.service';
import { ModuleResolverService } from './module-resolver.service';
import { ClientControllerService, ClientPojo, ModuleControllerService, ModulePojo, ProjectControllerService, ProjectPojo } from '@innowake/mining-api-angular-client';

describe('ModuleResolverService', () => {
    let resolver: ModuleResolverService;
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
        name: 'test Module',
        projectId: 2,
        linkHash: '85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056'
    }
  
    function fakeRouterState(url: string): RouterStateSnapshot {
      return { url } as RouterStateSnapshot;
    }

    const clientProjectRelationShipSpy = jasmine.createSpyObj<ClientProjectRelationshipService>(
        'ClientProjectRelationshipService',
        ['getClientProjectObservable', 'setClientProject']
    );
    const clientServiceSpy = jasmine.createSpyObj<ClientControllerService>('ClientControllerService', ['findClientById']);
    const projectServiceSpy = jasmine.createSpyObj<ProjectControllerService>('ProjectControllerService', ['findProjectById']);
    const moduleServiceSpy = jasmine.createSpyObj<ModuleControllerService>('ModuleControllerService', ['findModuleById', 'findModuleByLinkHash']);
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
                ModuleResolverService,
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
        moduleServiceSpy.findModuleByLinkHash.and.returnValue(of(testModule as any));
        routerSpy.navigate.calls.reset();
        clientProjectRelationShipSpy.setClientProject.calls.reset();
        moduleServiceSpy.findModuleById.calls.reset();
        moduleServiceSpy.findModuleByLinkHash.calls.reset();
        iamSpy.isAccessTokenFromUrl.and.returnValue(false);
    }));

    beforeEach(() => {
        resolver = TestBed.inject(ModuleResolverService);
    });

    it('should resolve module route with sources', waitForAsync(() => {
        let snapshot = {
            paramMap: convertToParamMap({
                projectId:'project-2',
                moduleHash: 'module-2'
            }),
            data: {
                includeSource: true
            }
        };
        let stateSnapshot = fakeRouterState('/project-1/module-85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056/details/overview'); 
        resolver.resolve(snapshot as any, stateSnapshot).subscribe(() => {
            expect(moduleServiceSpy.findModuleById).toHaveBeenCalledWith(2, 2, true);
        })
    }));

    it('should call checkIncludeSource for false', waitForAsync(() => {
        let snapshot = {
            paramMap: convertToParamMap({
                projectId:'project-2',
                moduleHash: 'module-2'
            }),
            data: {
                includeSource: false
            }
        };
        let stateSnapshot = fakeRouterState('/project-1/module-85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056/details/overview');
        moduleServiceSpy.findModuleById.and.returnValue(throwError('error'));
        resolver.resolve(snapshot as any, stateSnapshot).subscribe(() => {
            expect(moduleServiceSpy.findModuleById).toHaveBeenCalledWith(2, 2, false);
        })
    }));

    it('should call findModuleByLinkHash for linkHash', waitForAsync(() => {
        let snapshot = {
            paramMap: convertToParamMap({
                projectId:'project-2',
                moduleHash: 'module-85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056'
            }),
            data: {
                includeSource: true
            }
        };
        let stateSnapshot = fakeRouterState('/project-1/module-85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056/details/overview');
        moduleServiceSpy.findModuleByLinkHash.and.returnValue(throwError('error'));
        resolver.resolve(snapshot as any, stateSnapshot).subscribe(() => {
            expect(moduleServiceSpy.findModuleByLinkHash).toHaveBeenCalledWith(2, '85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056', false);
        })
    }));

    it('should not fetch module if identical haslink and project', waitForAsync(() => {
      resolver.previousModule = testModule;
      let snapshot = {
          paramMap: convertToParamMap({
              projectId:'project-2',
              moduleHash: 'module-85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056'
          }),
          data: {
              includeSource: false
          }
      };
      let stateSnapshot = fakeRouterState('/project-2/module-85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056/details/overview');
      resolver.resolve(snapshot as any, stateSnapshot).subscribe(() => {
          expect(moduleServiceSpy.findModuleByLinkHash).not.toHaveBeenCalled();
      })
  }));

    it('should fetch module if identical haslink as previous but different project', waitForAsync(() => {
      resolver.previousModule = testModule;
      let snapshot = {
          paramMap: convertToParamMap({
              projectId:'project-3',
              moduleHash: 'module-85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056'
          }),
          data: {
              includeSource: false
          }
      };
      let stateSnapshot = fakeRouterState('/project-3/module-85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056/details/overview');
      resolver.resolve(snapshot as any, stateSnapshot).subscribe(() => {
          expect(moduleServiceSpy.findModuleByLinkHash).toHaveBeenCalledWith(3, '85EF46B1A1B9B88CF017A9BB9EC4D5AEF8C47465596CB984956B8E2626565056', false);
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