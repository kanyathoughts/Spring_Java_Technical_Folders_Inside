/* eslint-disable */
import { ActivatedRouteSnapshot, DetachedRouteHandle, RouteReuseStrategy } from '@angular/router';
import { Injectable } from '@angular/core';

/**
 * A route strategy allowing for explicit route reuse.
 * Used as a workaround for https://github.com/angular/angular/issues/18374
 * To reuse a given route, add `data: { reuse: true, shell: 'my-shell' }` to the route definition.
 */
@Injectable()
export class RouteReusableStrategy extends RouteReuseStrategy {
  private pathsToIgnore: string[] = ['code-viewer'];

  public shouldDetach(route: ActivatedRouteSnapshot): boolean {
    return false;
  }

  public store(route: ActivatedRouteSnapshot, detachedTree: DetachedRouteHandle | null): void {}

  public shouldAttach(route: ActivatedRouteSnapshot): boolean {
    return false;
  }

  public retrieve(route: ActivatedRouteSnapshot): DetachedRouteHandle | null {
    return null;
  }

  public shouldReuseRoute(future: ActivatedRouteSnapshot, curr: ActivatedRouteSnapshot): boolean {
    if (this.shouldIgnoreDynamicPath(future.routeConfig?.path) && this.shouldIgnoreDynamicPath(curr.routeConfig?.path)) {
      return false;
    }
    return future.routeConfig === curr.routeConfig || future.data.reuse;
  }

  private shouldIgnoreDynamicPath(path: string | undefined): boolean {
    return !!path && this.pathsToIgnore.some(dynamicPath => path.includes(dynamicPath));
  }
}
