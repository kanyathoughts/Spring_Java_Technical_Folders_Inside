import { Component } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { LicenseExpiryService } from '@app/core/license/license-expiry.service';
import { NzResultStatusType } from 'ng-zorro-antd/result';

@Component({
  selector: 'app-error',
  templateUrl: './error.component.html'
})
export class ErrorComponent {
  errorId: string;
  title: string;
  subTitle: string;
  status: NzResultStatusType;
  constructor(
    public licenseExpiryService: LicenseExpiryService,
    private route: ActivatedRoute
  ) {
    this.route.paramMap.subscribe(result => {
      const details = this.getErrorConfiguration(result['params'].lincenseId as string);
      this.title = details.title;
      this.subTitle = details.subTitle;
      this.status = details.status;
    });
  }

  private getErrorConfiguration(errorId: string) {
    const errorConfigurations = {
      licenseExpired: {
        title: 'licenseNotification.expiredTitle',
        subTittle: 'licenseNotification.expiredMessage',
        status: '403'
      },
      unauthorizedAccess: {
        title: 'unauthorizedAccessTitle',
        subTittle: 'unauthorizedAccessMessage',
        status: '403'
      },
      loggedOut: {
        title: 'loggedOutPage.tittle',
        subTitle: 'loggedOutPage.subTitle',
        status: '403'
      }
    };
    return errorConfigurations[errorId] || errorConfigurations['unauthorizedAccess'];;
  }
}
