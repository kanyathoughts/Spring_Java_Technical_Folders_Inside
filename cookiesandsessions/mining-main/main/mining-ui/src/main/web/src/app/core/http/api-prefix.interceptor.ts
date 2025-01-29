import { Injectable } from '@angular/core';
import { HttpEvent, HttpInterceptor, HttpHandler, HttpRequest, HttpXsrfTokenExtractor } from '@angular/common/http';
import { Observable, from } from 'rxjs';
import { environment } from '@env/environment';
import { IdentityAccessManagementService } from '../authentication/identity-access-management.service';
import { switchMap } from 'rxjs/operators';
import { KEYCLOAK_TOKEN_PLACEHOLDER } from '../authentication/keycloak.service';

const AUTHORIZATION = 'authorization';
/**
 * Prefixes all requests not starting with `http[s]` with `environment.serverUrl`.
 */
@Injectable()
export class ApiPrefixInterceptor implements HttpInterceptor {
  constructor(private IAMService: IdentityAccessManagementService,
    private tokenExtractor: HttpXsrfTokenExtractor) { }
  /**
   * Intercept method used to create the final url with header parameters and user details
   * based on the type of request.
   * @param request the HttpRequest which is being intercepted
   * @param next used to return request with the manipulated final url and required request headers
   */
  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    return from(this.IAMService.getAccessToken())
      .pipe(
        switchMap((token: string) => {
          let finalUrl = request.url;
          const containsLegacyAuth = finalUrl.includes('legacy-auth');
          const headers = {
            'X-Requested-With': 'XMLHttpRequest'
          };
          if (! token && ! containsLegacyAuth) {
            headers[AUTHORIZATION] = 'Basic ' + btoa('mining-plugin-client' + ':' + 'mining-client-secret');
            headers['Content-Type'] = 'application/x-www-form-urlencoded';
          } else if (token !== KEYCLOAK_TOKEN_PLACEHOLDER) {
            headers[AUTHORIZATION] = 'Bearer ' + token;
          }
          if (environment.production) {
            const manipulated: URL = new URL(finalUrl);
            finalUrl = manipulated.pathname;
          }
          // Insert header for XSRF security check
          if (!(request.method === 'GET' || request.method === 'HEAD')) {
            const cookieheaderName = 'X-XSRF-TOKEN';
            const csrfToken = this.tokenExtractor.getToken();
            if (csrfToken !== null && !request.headers.has(cookieheaderName)) {
              headers[cookieheaderName] = csrfToken;
            }
          }
          request = request.clone({
            url: finalUrl,
            withCredentials: true,
            setHeaders: headers
          });
          return next.handle(request);
        })
      );
  }
}
