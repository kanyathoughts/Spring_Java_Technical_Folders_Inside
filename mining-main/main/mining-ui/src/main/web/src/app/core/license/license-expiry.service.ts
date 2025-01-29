import { Injectable } from '@angular/core';
import { LicenseExpirationInfo } from '@innowake/mining-api-angular-client';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { TranslateService } from '@ngx-translate/core';
import { ReplaySubject } from 'rxjs';
import { Logger } from '@app/core';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { getBasePath } from '../utils/base-path.utils';

const log = new Logger('License Expiry');

@Injectable({
  providedIn: 'root'
})
export class LicenseExpiryService {
  isLicenseExpired: ReplaySubject<boolean> = new ReplaySubject<boolean>();
  expiryDate: string;

  private http: HttpClient;

  constructor(
    private httpClient: HttpClient,
    private nzNotification: NzNotificationService,
    private translateService: TranslateService
  ) {
    this.http = this.httpClient.disableApiPrefix().skipErrorHandler();
  }

  checkExpiry(): void {
    this.http.get<LicenseExpirationInfo>(`${getBasePath()}/api/license-expiry-info`).subscribe((response: LicenseExpirationInfo) => {
      this.expiryDate = response.expiryDate;
      if (response.days !== null && (response.days > 0 && response.days <= 30)) {
        this.isLicenseExpired.next(false);
        this.nzNotification.warning(
          this.translateService.instant('licenseNotification.expireSoonTitle') as string,
          this.translateService.instant('licenseNotification.expireSoonMessage', { expiryDate: response.expiryDate }) as string,
          { nzDuration: 0 });
        return true;
      } else if (response.neverExpires === false && response.days <= 0) {
        this.isLicenseExpired.next(true);
        return false;
      }
    }, (error: HttpErrorResponse) => {
      log.error(error.message);
    });
  }
}
