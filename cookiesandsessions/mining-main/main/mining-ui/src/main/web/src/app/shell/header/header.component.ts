import { ChangeDetectorRef, Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { KeycloakService } from '@app/core/authentication/keycloak.service';
import { NzModalService } from 'ng-zorro-antd/modal';
import { SettingsComponent } from '@app/shared/components/settings/settings.component';
import { TranslateService } from '@ngx-translate/core';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { Subscription } from 'rxjs';
import { ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { HttpClient } from '@angular/common/http';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { LocalShowcaseMode } from '@app/core/utils/local-showcase.util';

@Component({
  selector: 'app-header',
  templateUrl: './header.component.html'
})
export class HeaderComponent implements OnInit, OnDestroy {

  projectId: number;
  username: string;
  clientId: number;
  currentClient: ClientProjectRelationship;
  isAdmin: boolean;
  isClientAdmin: boolean;
  showSettings: boolean;
  routeProjectSelection: string;
  showHide: string;
  showJobDetails = true;
  aboutMiningModalflag = false;
  versionLabel: string;
  copyRightYear: number;
  selectedValue: string;
  hideSelection = false;
  localShowcaseMode = new LocalShowcaseMode();

  readonly basePath = getBasePath();
  readonly apiLicensesUrl = this.basePath + '/innoWake-Third-Party-Licenses.html';
  readonly webLicensesUrl = this.basePath + '/innoWake-Third-Party-Licenses-Frontend.html';

  private clientProjectSubscription: Subscription;
  private scrollEventSubscription: Subscription;

  constructor(
    public keycloakService: KeycloakService,
    private IAMService: IdentityAccessManagementService,
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private deepLinkService: DeepLinkService,
    private authorizationService: KeycloakAuthorizationService,
    private modalService: NzModalService,
    private translateService: TranslateService,
    private changeDetector: ChangeDetectorRef,
    private scrollEventService: ScrollEventService,
    private httpClient: HttpClient
  ) {
    this.deepLinkService.featureIsActive().subscribe(resp => {
      if (resp) {
        this.showSettings = true;
      }
    });
  }

  get isLocalShowcaseActive(): boolean {
    return this.localShowcaseMode.isInLocalShowcaseMode;
  }

  get routeToConfigureView(): string {
    return 'project-' + this.projectId + '/configuration/view';
  }

  ngOnInit(): void {
    this.username = this.IAMService.getUsername();
    this.scrollEventSubscription = this.scrollEventService.getScrollObservable().subscribe((scrollPosition) => {
      this.showHide = 'mining-header-' + scrollPosition;
    });
    this.clientProjectSubscription = this.clientProjectRelationshipService.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.currentClient = response;
      if (this.currentClient) {
        this.routeProjectSelection = RouteBuilder.buildClientRoute(this.currentClient.getClientId(), 'projects');
        this.projectId = this.currentClient.getProjectId();
      }
      // Avoid error "NG0100: Expression has changed after it was checked" for the breadcrumb values in the template
      this.changeDetector.detectChanges();
    });
    this.isAdmin = this.authorizationService.isAdmin();
    this.isClientAdmin = this.authorizationService.isClientAdmin();
    this.copyRightYear = (new Date()).getFullYear();
    const client = this.httpClient.disableApiPrefix();
    client.get<object>(getBasePath() + '/api/v1/version').subscribe((version: object) => {
      this.versionLabel = version['version'];
    });
  }

  /**
   * handles openeing and closing the modal..
   */
  handleMiningModal(flag: boolean): void {
    this.aboutMiningModalflag = flag;
  }

  settings(): void {
    this.modalService.create({
      nzTitle: this.translateService.instant('settings'),
      nzAutofocus: null,
      nzMaskClosable: false,
      nzClosable: true,
      nzKeyboard: true,
      nzContent: SettingsComponent
    });
  }

  logout(): void {
    this.IAMService.logout(this.httpClient);
  }

  /**
   * returns the initials of the current user.
   * @return current user initials.
   */
  public getUserInitials(): string {
    if (this.keycloakService.getUserInitials() !== '') {
      return this.keycloakService.getUserInitials();
    } else {
      return this.username ? this.username[0].toUpperCase() : '';
    }
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
    this.scrollEventSubscription?.unsubscribe();
  }
}
