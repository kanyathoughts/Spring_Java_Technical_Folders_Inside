import { Injectable } from '@angular/core';
import { HttpEvent, HttpInterceptor, HttpHandler, HttpRequest, HttpErrorResponse } from '@angular/common/http';
import { Observable } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { Router } from '@angular/router';
import { IdentityAccessManagementService } from '../authentication/identity-access-management.service';
import { EclipseService } from '../authentication/eclipse.service';
import { NzMessageRef, NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';

/**
 * Adds a default error handler to all requests.
 */
@Injectable()
export class ErrorHandlerInterceptor implements HttpInterceptor {

  private sessionExpiredMessageRef: NzMessageRef;

  constructor(
    private router: Router,
    private IAMService: IdentityAccessManagementService,
    private eclipseService: EclipseService,
    private messageService: NzMessageService,
    private translateService: TranslateService
  ) {}

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    return next.handle(request).pipe(catchError((error: HttpErrorResponse) => this.errorHandler(error)));
  }

  private errorHandler(response: HttpErrorResponse): Observable<HttpEvent<any>> {
    if (response.status === 401) {
      if (this.IAMService.isAccessTokenFromUrl() || this.eclipseService.isEclipseView) {
        void this.router.navigate(['/error']);
      } else if ( ! this.sessionExpiredMessageRef) {
        this.sessionExpiredMessageRef = this.messageService.error(this.translateService.instant('messageService.sessionExpired') as string,
          { nzDuration: 0 });
        this.sessionExpiredMessageRef.onClose.subscribe(() => this.sessionExpiredMessageRef = null);
      }
    }
    throw response;
  }
}
