import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';

import { Observable, of, from } from 'rxjs';
import { IdentityAccessManagementService } from './identity-access-management.service';
import { switchMap } from 'rxjs/operators';
import { KEYCLOAK_TOKEN_PLACEHOLDER, KeycloakService } from './keycloak.service';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class AuthenticationGuard  {
  constructor(private IAMService: IdentityAccessManagementService, private keycloakService: KeycloakService, private httpClient: HttpClient) {}
  /**
   * CanActivate checks if the current request by the user has appropriate token else redirects to the Login page
   * @param route The activated route of the current request
   * @param state The router state snapshot of the current request
   */
  canActivateChild(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
    return from(this.IAMService.getAccessToken())
    .pipe(
      switchMap((token: string) => {
        if (token) {
          if (token === KEYCLOAK_TOKEN_PLACEHOLDER) {
            return this.keycloakService.checkLogin(this.httpClient, state.url);
          }
          return of(true);
        }else {
          this.IAMService.logout(this.httpClient);
        }
        return of(false);
      })
    );
  }
}
