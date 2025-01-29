import { HttpEvent, HttpHandler, HttpInterceptor, HttpRequest } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { NavigationStart, Router } from '@angular/router';
import { Observable, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';

/**
 * Cancel pending HTTP request on navigation
 */
@Injectable()
export class CancelRequestOnNavigationInterceptor implements HttpInterceptor {

  private navigationStart = new Subject<void>();

  constructor(router: Router) {
    router.events.pipe(filter(event => event instanceof NavigationStart)).subscribe(() => {
      this.navigationStart.next();
    });
  }

  /**
   * Intercept HTTP requests and insert the interruption based on navigation.
   * @param req the HttpRequest which is being intercepted
   * @param next used to return request with the manipulated final url and required request headers
   * @returns the initial request with the cancel on navigation mechanism
   */
  intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    return next.handle(req).pipe(takeUntil(this.navigationStart.asObservable()));
  }
}
