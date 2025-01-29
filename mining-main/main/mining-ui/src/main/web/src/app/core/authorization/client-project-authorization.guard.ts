import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { Observable } from 'rxjs';
import { concatMap } from 'rxjs/operators';
import { AuthorizationGuard } from './authorization.guard';
import { ClientProjectGuard } from './client-project.guard';

/**
 * This guard provide a protection for routes restricted by User's Role.
 * This guard act as a controller  for controlling the flow of client-project.guard and authorization.guard.
 */
@Injectable({
  providedIn: 'root'
})
export class ClientProjectAuthorizationGuard  {

  constructor(private clientProjectGuard: ClientProjectGuard, private authorizationGuard: AuthorizationGuard) { }

  canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
    return this.clientProjectGuard.canActivate(route).pipe(
      concatMap(() => this.authorizationGuard.canActivate(route, state))
    );
  }

  canActivateChild(childRoute: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
    return this.canActivate(childRoute, state);
  }
}
