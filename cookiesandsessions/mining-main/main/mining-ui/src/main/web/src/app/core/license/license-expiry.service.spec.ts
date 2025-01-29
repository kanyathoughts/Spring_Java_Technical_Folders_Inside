import { TestBed } from '@angular/core/testing';

import { LicenseExpiryService } from './license-expiry.service';
import { of } from 'rxjs';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { TranslateService } from '@ngx-translate/core';
import { HttpClient } from '@angular/common/http';
import { LicenseExpirationInfo } from '@innowake/mining-api-angular-client';

describe('LicenseExpiryService', () => {
  let licenseExpiryService: LicenseExpiryService;
  const notificationService  = jasmine.createSpyObj<NzNotificationService>('NzNotificationService', ['error','warning']);
  const translateService = jasmine.createSpyObj<TranslateService>('TranslateService', ['instant']);
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'skipErrorHandler', 'get']);
  let licenseInfo: LicenseExpirationInfo = {days:1, expiryDate:'12-01-2022', neverExpires:false};
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        LicenseExpiryService,
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: NzNotificationService, useValue: notificationService},
        { provide: TranslateService, useValue: translateService}
      ]
    });
    httpServiceSpy.get.and.returnValue(of({
      days: 2,
      expiryDate: "10/12/2022",
      neverExpires: true
    }) as any);
    notificationService.warning.and.returnValue(true as any);
    httpServiceSpy.disableApiPrefix.and.returnValue(httpServiceSpy);
    httpServiceSpy.skipErrorHandler.and.returnValue(httpServiceSpy);
    licenseExpiryService = TestBed.inject(LicenseExpiryService);
  });

  it('should be created', () => {
    expect(licenseExpiryService).toBeTruthy();
  });

  it('should call checkExpiry function', () => {
    licenseExpiryService.checkExpiry();
    licenseExpiryService.expiryDate = licenseInfo.expiryDate;
    licenseExpiryService.isLicenseExpired.next(false);
    httpServiceSpy.get.and.returnValue(of(  {days: -1 ,
      expiryDate: "10/12/2022",
      neverExpires: false}) as any);
    expect(httpServiceSpy.get).toHaveBeenCalled();
  }); 
});
