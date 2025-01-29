import { TestBed, tick, fakeAsync, waitForAsync } from '@angular/core/testing';
import { Router, RouterStateSnapshot, ActivatedRouteSnapshot } from '@angular/router';

import { AuthenticationGuard } from './authentication.guard';
import { IdentityAccessManagementService } from './identity-access-management.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('AuthenticationGuard', () => {
  let authenticationGuard: AuthenticationGuard;
  let mockRouter: any;
  let mockSnapshot: RouterStateSnapshot;
  const activatedRouteSnapshotStub: ActivatedRouteSnapshot = <any>{};
  const routerStateSnapshotStub: RouterStateSnapshot = <any>{};
  const IAMServiceSpy: jasmine.SpyObj<IdentityAccessManagementService> = jasmine.createSpyObj('IAMServiceSpy', ['getAccessToken','logout']);

  beforeEach(waitForAsync(() => {
    mockRouter = {
      navigate: jasmine.createSpy('navigate')
    };
    mockSnapshot = jasmine.createSpyObj<RouterStateSnapshot>('RouterStateSnapshot', ['toString']);
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule
      ],
      providers: [
        { provide: IdentityAccessManagementService, useValue: IAMServiceSpy },
        AuthenticationGuard,
        { provide: Router, useValue: mockRouter }
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    authenticationGuard = TestBed.inject(AuthenticationGuard);
    IAMServiceSpy.getAccessToken.and.returnValues(Promise.resolve('token'));
  });

  it('should have a canActivateChild method', () => {
    expect(typeof authenticationGuard.canActivateChild).toBe('function');
  });

 it('should return true if user is authenticated and false if user is not authenticated', fakeAsync(() => {
    authenticationGuard
      .canActivateChild(activatedRouteSnapshotStub, routerStateSnapshotStub)
      .subscribe(result => expect(result).toBeTruthy());
  }));
});
