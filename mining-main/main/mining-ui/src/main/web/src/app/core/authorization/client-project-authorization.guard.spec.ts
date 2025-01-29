import { TestBed } from '@angular/core/testing';
import { ActivatedRouteSnapshot } from '@angular/router';
import { RouterStateSnapshot } from '@angular/router';
import { AuthorizationGuard } from './authorization.guard';
import { ClientProjectGuard } from './client-project.guard';
import { ClientProjectAuthorizationGuard } from './client-project-authorization.guard';
import { of } from 'rxjs';

describe('ClientProjectAuthorizationGuard', () => {
  let service: ClientProjectAuthorizationGuard;

  beforeEach(() => {
    const authorizationGuardStub = () => ({
      canActivate: () => ({})
    });
    const clientProjectGuardStub = () => ({
      canActivate: () => ({ pipe: () => ({}) })
    });
    TestBed.configureTestingModule({
      providers: [
        ClientProjectAuthorizationGuard,
        { provide: AuthorizationGuard, useFactory: authorizationGuardStub },
        { provide: ClientProjectGuard, useFactory: clientProjectGuardStub }
      ]
    });
    service = TestBed.inject(ClientProjectAuthorizationGuard);
  });

  it('can load instance', () => {
    expect(service).toBeTruthy();
  });
  
  describe('canActivate', () => {
    it('makes expected calls', () => {
      const activatedRouteSnapshotStub: ActivatedRouteSnapshot = <any>{};
      const routerStateSnapshotStub: RouterStateSnapshot = <any>{};
      const authorizationGuardStub: AuthorizationGuard = TestBed.inject(
        AuthorizationGuard
      );
      const clientProjectGuardStub: ClientProjectGuard = TestBed.inject(
        ClientProjectGuard
      );
      const test = {};
      spyOn(authorizationGuardStub, 'canActivate').and.callThrough();
      spyOn(clientProjectGuardStub, 'canActivate').and.callThrough();
      const result = service.canActivate(activatedRouteSnapshotStub, routerStateSnapshotStub);
      expect(result).toEqual(test as any);
    });
  });
});
