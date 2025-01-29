import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { KeycloakService } from '../authentication/keycloak.service';
import { ClientProjectRelationshipService } from '../services/client-project-relationship/client-project-relationship.service';
import { KeycloakAuthorizationService } from './keycloak-authorization.service';
import { isAccessTokenPresentInPath } from '../utils/access-token-util';
import { Observable, of } from 'rxjs';
import { ProjectRole } from '@innowake/mining-api-angular-client';

/**
 * This guard provide a protection for routes restricted by User's Role
 */
@Injectable({
  providedIn: 'root'
})
export class AuthorizationGuard  {
  private currentClient: ClientProjectRelationship;

  constructor(
    private router: Router,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private keycloakAuthorization: KeycloakAuthorizationService,
    private keycloakService: KeycloakService
  ) {
    this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.currentClient = response;
    });
  }

  canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
    /* If access token is present in the url and server is running in default authentication (i.e. not in IAM profile)
    then navigate the user to the requested URL. Otherwise if the miningCurrentClient information is not present in the
    local storage, then the url request would fail even if provided with the valid access token.
    In case of invalid access token, user is redirected to error page with message "Unauthorized access due to invalid access-token." */
    if (isAccessTokenPresentInPath(state.url) && ! this.keycloakService.isAvailable) {
      return of(true);
    }
    // Check authorization for pages that need rights on clients but not for projects
    if (next.data.clientAuthorization) {
      const clientId: number = this.currentClient ? this.currentClient.getClientId() : null;
      if (next.data.clientAuthorization === ProjectRole.UserRoleEnum.ADMIN && this.keycloakAuthorization.isClientAdmin(clientId)) {
        return of(true);
      } else if (this.keycloakAuthorization.hasAccessToClientProjects(clientId)) {
        return of(true);
      } else {
        void this.router.navigate(['clients']);
        return of(false);
      }
    }
    // Role based authorization, take the role defined on the last nested route
    let role: ProjectRole.UserRoleEnum = next.data.role;
    let nextChild = next.firstChild;
    while (nextChild) {
      if (nextChild.data.role) {
        role = nextChild.data.role;
      }
      nextChild = nextChild.firstChild;
    }
    if (role) {
      if ( ! this.currentClient) {
        void this.router.navigate(['clients']);
        return of(false);
      }
      if (this.keycloakAuthorization.hasUserRole(this.currentClient, role)) {
        // We cannot directly return the expression in the if-statement above.
        // That would cause the user to be redirected to an empty page if they're not authorized.
        return of(true);
      }
    }
    // void this.router.navigate(['clients']);
    return of(true);
  }

  canActivateChild(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
    return this.canActivate(next, state);
  }
}
