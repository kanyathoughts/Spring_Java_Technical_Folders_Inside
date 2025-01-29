import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Logger } from '../services/logger.service';
import { getBasePath } from '../utils/base-path.utils';
import { Observable, of } from 'rxjs';
import { catchError, switchMap } from 'rxjs/operators';
import { KeycloakAuth } from '@app/shared/interfaces/keycloak-auth.interface';
import { KeycloakMiningRole } from '@app/shared/interfaces/keycloak-mining-role.interface';
import { Router } from '@angular/router';

const log = new Logger('KeyclaokService');
export const KEYCLOAK_TOKEN_PLACEHOLDER = 'keycloak';

/**
 * This service holds all the methods used Authentication using KeyCloak.
 * KeyCloak provides us the better way to Authenticate users
 */
@Injectable({
  providedIn: 'root'
})
export class KeycloakService {

  useKeycloackAuth = false;
  keyCloakAuthURL = '';
  keyCloakRealm = '';
  keyCloakClient = '';

  private instance: KeycloakAuth;

  constructor(private router: Router) {}

  get isAvailable(): boolean {
    return !! this.instance;
  }

  /**
   * Check if the user is logged in and redirect the user to the loggin page if not.
   * @param http HttpClient must be passed as a parameter to avoid circular dependencies
   * @param redirectPath The path in mining ui where the user will be redirected (after the #)
   * @returns an obeservable returning true if the user is logged in
   */
  checkLogin(httpClient: HttpClient, redirectPath: string): Observable<boolean> {
    const http = httpClient.skipErrorHandler();
    return http.get(getBasePath() + '/api/v1/auth/login').pipe(
      switchMap((authData: KeycloakAuth) => {
        this.instance = authData;
        return of(true);
      }), catchError(error => {
        if (error.status === 401) {
          window.open(getBasePath() + '/api/v1/auth/ui?context=' + redirectPath, '_self');
        }
        return of(false);
      })
    );
  }

  /**
   * Logs out the user from the keyCloak server.
   * @param http HttpClient must be passed as a parameter to avoid circular dependencies
   */
  logout(httpClient: HttpClient): void {
    this.instance = null;
    const http = httpClient.skipErrorHandler();
    http.get(getBasePath() + '/api/v1/auth/logout').subscribe(() => {
      this.logoutFromKeycloak();
    }, (error) => {
      if (error.status === 401) {
        void this.router.navigate(['/error/loggedOut']);
      }
      log.error(error.message);
    });
  }


  /**
   * Gets the token for the authenticated user and calls refreshToken if new token is required.
   * @returns KeyCloak Token.
   */
  async getToken(): Promise<string> {
    return new Promise((resolve) => resolve(KEYCLOAK_TOKEN_PLACEHOLDER));
  }

  /**
   * Gets the logged in user name.
   * @return name of the user.
   */
  getUsername(): string {
    let userName = '';
    if (this.instance) {
      userName = this.instance.user_logon_name;
    }
    return userName;
  }

  /**
   * Gets the logged in user id.
   * @return id of the user.
   */
  getUserId(): string {
    let userId: string = null;
    if (this.instance) {
      userId = this.instance.subject;
    }
    return userId;
  }

  /**
   * Gets current user roles
   */
  getUserRoles(): KeycloakMiningRole[] {
    return this.instance.mining_roles;
  }

  /**
   * Extracts and returns user initials from userProfile object.
   * @returns string value.
   */
  getUserInitials(): string {
    if (this.instance) {
      const firstName = this.instance.user_given_name ? this.instance.user_given_name[0] : '';
      const lastName = this.instance.user_family_name ?  this.instance.user_family_name[0] : '';
      return firstName.toUpperCase() + lastName.toUpperCase();
    }
    return '';
  }

  private logoutFromKeycloak() {
    if (this.keyCloakAuthURL && this.keyCloakRealm) {
      const redirectUrl = getBasePath() + '/#/clients';
      const url = `${this.keyCloakAuthURL}/realms/${this.keyCloakRealm}/protocol/openid-connect/logout?`
      + `client_id=${this.keyCloakClient}&post_logout_redirect_uri=${redirectUrl}`;
      window.open(url, '_self');
    } else {
      void this.router.navigate(['/error/loggedOut']);
    }
  }
}
