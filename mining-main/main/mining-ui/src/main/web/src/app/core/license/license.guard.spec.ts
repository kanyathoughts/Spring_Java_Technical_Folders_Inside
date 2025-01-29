import { TestBed } from '@angular/core/testing';
import { ActivatedRouteSnapshot } from '@angular/router';
import { Router } from '@angular/router';
import { LicenseExpiryService } from './license-expiry.service';
import { LicenseGuard } from './license.guard';
import { of, ReplaySubject } from 'rxjs';

describe('LicenseGuard', () => {
  let service: LicenseGuard;
  const isLicenseExpired = new ReplaySubject();
  beforeEach(() => {
    const routerStub = () => ({ navigateByUrl: (string: any) => ({}) });
    const isLicenseExpireVal = true;
   isLicenseExpired.next(isLicenseExpireVal);
    const LicenseExpiryServiceSpy = jasmine.createSpyObj<LicenseExpiryService>('LicenseExpiryService', ['isLicenseExpired']);
    TestBed.configureTestingModule({
      providers: [
        LicenseGuard,
        { provide: Router, useFactory: routerStub },
        { provide: LicenseExpiryService, useValue: { isLicenseExpired } }
      ]
    });
    service = TestBed.inject(LicenseGuard);
    isLicenseExpired.next(false);
  });
  it('can load instance', () => {
    expect(service).toBeTruthy();
  });

  describe('canActivate', () => {
    it('makes expected calls', () => {
      const activatedRouteSnapshotStub: ActivatedRouteSnapshot = {
        data: {
          title: 'test'
        }
      } as any;
      const routerStub: Router = TestBed.inject(Router);
      spyOn(routerStub, 'navigateByUrl').and.callThrough();
      service.canActivate(activatedRouteSnapshotStub);
      isLicenseExpired.next(false);
      expect(routerStub.navigateByUrl).toHaveBeenCalled();
    });
  });
});
