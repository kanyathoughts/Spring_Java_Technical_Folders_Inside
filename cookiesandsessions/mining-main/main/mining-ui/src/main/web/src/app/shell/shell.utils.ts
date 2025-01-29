import { Routes, Route } from '@angular/router';
import { AuthenticationGuard } from '@app/core';
import { ProjectShellComponent } from './project-shell/project-shell.component';
import { BaseHeaderShellComponent } from './base-shell/base-header-shell.component';
import { ClientProjectAuthorizationGuard } from '@app/core/authorization/client-project-authorization.guard';
import { LicenseGuard } from '@app/core/license/license.guard';
import { NavigationGuard } from '@app/core/pendingChanges/navigationGuard';

/**
 * Provides helper methods to create routes.
 */
export class Shell {

  /**
   * Creates routes using the project shell component and authentication.
   * @param routes The routes to add.
   * @return The new route using shell as the base.
   */
  static projectShellChildRoutes(routes: Routes): Route {
    return {
        path: '',
        component: ProjectShellComponent,
        children: routes,
        canDeactivate: [NavigationGuard],
        canActivateChild: [ClientProjectAuthorizationGuard]
      };
  }

  /**
   * Creates routes using the header shell component and authentication.
   * @param routes The routes to add.
   * @return The new route using shell as the base.
   */
  static headerShellChildRoutes(routes: Routes): Route {
    return {
      path: '',
      component: BaseHeaderShellComponent,
      children: routes,
      canActivate: [LicenseGuard],
      canDeactivate: [NavigationGuard],
      canActivateChild: [AuthenticationGuard],
    };
  }
}
