import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { FeatureToggleService } from './feature-toggle/feature-toggle.service';
import { environment } from '@env/environment';
import { StateKey } from '@app/shared/interfaces/state-maintainance.interface';
import { Observable, ReplaySubject } from 'rxjs';
import { AnnotationPojo, ModulePojo } from '@innowake/mining-api-angular-client';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { Logger } from '@app/core';

enum RequestType {
  SHOW_MODULE = 'SHOW_MODULE',
  SHOW_ANNOTATION = 'SHOW_ANNOTATION',
}

const log = new Logger('EclipseDeepLinkService');

@Injectable({
  providedIn: 'root'
})
export class DeepLinkService {

  featureToggleObserver: ReplaySubject<boolean>;

  defaultPort = environment.deepLinkPort;

  http: HttpClient;

  constructor(
    private httpService: HttpClient,
    private messageService: NzMessageService,
    private translateService: TranslateService,
    private featureToggleService: FeatureToggleService,
  ) {
    this.http = this.httpService.disableApiPrefix().skipErrorHandler();
  }

  /**
   * Check if the feature toggle for the deeplink functionality is activated or not.
   * The call to the Api is done only the first time this method is called but can be retrieve several time thanks to the ReplayObservable
   * @returns An observable containing a boolean set to true if the Deeplink feature toggle is activated
   */
  featureIsActive(): Observable<boolean> {
    if ( ! this.featureToggleObserver) {
      this.featureToggleObserver = new ReplaySubject<boolean>();
      this.featureToggleService.isActive('eclipseDeepLink').subscribe(resp=> {
        this.featureToggleObserver.next(resp);
      });
    }
    return this.featureToggleObserver;
  }

  /**
   * Fetch the port to use to communicate with the local eclipse instance.
   * @returns the port number
   */
  getPort(): number {
    const portValue = sessionStorage.getItem(StateKey.DeepLinkPortKey);
    return portValue == null ? this.defaultPort : Number(JSON.parse(portValue));
  }

  /**
   * Get the local url to reach eclipse server instance
   * @returns the url string
   */
  getURL(): string {
    return 'http://localhost:' + this.getPort();
  }

  /**
   * Pings the url of the local eclipse instance
   * @returns a boolean promise whereas eclipse is available or not
   */
  heartbeat(): Promise<boolean> {
    return this.http.head(this.getURL()).toPromise()
      .then(() => true).catch(() => false);
  }

  /**
   * Ask the local eclipse instance to open the file contening the module
   * @param module which should be displayed in eclipse
   */
  showModuleInEclipse(module: ModulePojo): void {
    if (module.path == null) {
      this.messageService.error(`${this.translateService.instant('eclipseDeeplink.undefinedPathError')}`);
      log.error('The path of the module is undefined: ' + module);
      return;
    }
    const request = { 'projectId': module.projectId, 'path': module.path };
    const data = { 'requestType': RequestType.SHOW_MODULE, 'request': JSON.stringify(request) };

    this.http.post(this.getURL(), data).subscribe();
  }

  /**
   * Open the file contening the annotation
   * @param annotation the annotation to be open in eclipse
   */
  showAnnotationIneclipse(annotation: AnnotationPojo): void {
    const request = {
      'projectId': annotation.projectId,
      'recordId': annotation.uid
    };
    const data = { 'requestType': RequestType.SHOW_ANNOTATION, 'request': JSON.stringify(request) };
    this.http.post(this.getURL(), data).subscribe();
  }
}
