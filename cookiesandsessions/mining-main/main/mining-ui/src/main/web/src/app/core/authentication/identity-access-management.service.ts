import { Injectable } from '@angular/core';
import { KeycloakService } from './keycloak.service';
import { OauthtokenService } from './oauthtoken.service';
import { Router } from '@angular/router';
import { StateMaintainenceService } from '../services/state-maintenance/state-maintainence.service';
import { RoutesRecognized } from '@angular/router';
import { ACCESS_TOKEN, getSearchParamValueFromPath, isAccessTokenPresentInPath } from '../utils/access-token-util';
import { EclipseService } from './eclipse.service';
import { HttpClient } from '@angular/common/http';

/**
 * This service provides methods required for the Authentication/Authorization and
 * information provided is on the bases of the Authentication mechanism being used
 * keyClaok or OAuth.
 */
@Injectable({
  providedIn: 'root'
})
export class IdentityAccessManagementService {

  /** The currentPath is the url path i.e. it does not contain the protocol, domain name and the anchor identifier '#'. */
  private currentPath: string = undefined;

  constructor(
    private oauthTokenService: OauthtokenService,
    private eclipseService: EclipseService,
    private router: Router,
    private keycloakService: KeycloakService,
    private stateMaintainenceService: StateMaintainenceService
  ) {
    this.updateCurrentPath();
  }

  /**
   * returns the Access token if the user is logged in.
   * @param url URL string.
   * @returns Promise for accessToken.
   */
  public async getAccessToken(): Promise<string> {
    if (this.keycloakService.useKeycloackAuth) {
      return this.keycloakService.getToken();
    }
    if (this.eclipseService.isEclipseView) {
      return await this.eclipseService.getAccessToken();
    }
    const accessToken = getSearchParamValueFromPath(this.currentPath, ACCESS_TOKEN);
    if (accessToken) {
      return accessToken;
    }
    return this.oauthTokenService.getAccessToken();
  }

  /**
   * Logouts the user from system and redirects to login page
   * @param options is type of keyCloackLogoutOption passed as parameter to logout method
   */
  public logout(http: HttpClient): void {
    localStorage.clear();
    if (this.keycloakService.useKeycloackAuth) {
      this.keycloakService.logout(http);
    } else {
      this.stateMaintainenceService.clearAllTheSavedStates();
      void this.router.navigate(['/login'], { replaceUrl: true });
    }
  }

  /**
   * returns the username of the current user.
   */
  public getUsername(): string {
    if (this.keycloakService.useKeycloackAuth) {
      return this.keycloakService.getUsername();
    } else {
      return this.oauthTokenService.getUsername();
    }
  }

  /**
   * returns the userId of the current user, In case of default Auth it will always return null.
   * @returns userId of current user or null
   */
  public getUserId(): string {
    if (this.keycloakService.useKeycloackAuth) {
      return this.keycloakService.getUserId();
    } else {
      return null;
    }
  }

  /**
   * returns boolean if the current url has access-token
   */
  isAccessTokenFromUrl(): boolean {
    return isAccessTokenPresentInPath(this.currentPath);
  }

  private updateCurrentPath(): void {
    this.currentPath = this.router.url;
    this.router.events.subscribe(event => {
      if (event instanceof RoutesRecognized) {
        this.currentPath = event.url;
      }
    });
  }
}
