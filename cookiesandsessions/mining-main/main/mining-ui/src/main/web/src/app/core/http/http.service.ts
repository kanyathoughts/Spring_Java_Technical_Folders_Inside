import { Inject, Injectable, InjectionToken, Injector, Optional, Type } from '@angular/core';
import { HttpClient, HttpEvent, HttpInterceptor, HttpHandler, HttpRequest, HttpHeaders, HttpContext, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { ErrorHandlerInterceptor } from './error-handler.interceptor';
import { CacheInterceptor } from './cache.interceptor';
import { ApiPrefixInterceptor } from './api-prefix.interceptor';
import { CancelRequestOnNavigationInterceptor } from './cancel-request-on-navigation.interceptor';
import { SetSerializationInterceptor } from './set-serialization.interceptor';

// HttpClient is declared in a re-exported module, so we have to extend the original module to make it work properly
// (see https://github.com/Microsoft/TypeScript/issues/13897)
declare module '@angular/common/http' {

  // Augment HttpClient with the added configuration methods from HttpService, to allow in-place replacement of
  // HttpClient with HttpService using dependency injection
  export interface HttpClient {

    /**
     * Enables caching for this request.
     * @param forceUpdate Forces request to be made and updates cache entry.
     * @return The new instance.
     */
    cache(forceUpdate?: boolean): HttpClient;

    /**
     * Skips default error handler for this request.
     * @return The new instance.
     */
    skipErrorHandler(): HttpClient;

    /**
     * Do not use API prefix for this request.
     * @return The new instance.
     */
    disableApiPrefix(): HttpClient;

    /**
     * Disable the interceptor cancelling requests on navigation
     * @return The new instance
     */
    disableCancelOnNavigation(): HttpClient
  }

}


// From @angular/common/http/src/interceptor: allows to chain interceptors
class HttpInterceptorHandler implements HttpHandler {
  constructor(private next: HttpHandler, private interceptor: HttpInterceptor) { }

  handle(request: HttpRequest<any>): Observable<HttpEvent<any>> {
    return this.interceptor.intercept(request, this.next);
  }
}

/**
 * Allows to override default dynamic interceptors that can be disabled with the HttpService extension.
 * Except for very specific needs, you should better configure these interceptors directly in the constructor below
 * for better readability.
 *
 * For static interceptors that should always be enabled (like ApiPrefixInterceptor), use the standard
 * HTTP_INTERCEPTORS token.
 */
export const HTTP_DYNAMIC_INTERCEPTORS = new InjectionToken<HttpInterceptor>('HTTP_DYNAMIC_INTERCEPTORS');

/**
 * Extends HttpClient with per request configuration using dynamic interceptors.
 */
@Injectable()
export class HttpService extends HttpClient {
  constructor(
    private httpHandler: HttpHandler,
    private injector: Injector,
    @Optional() @Inject(HTTP_DYNAMIC_INTERCEPTORS) private interceptors: HttpInterceptor[] = []
  ) {
    super(httpHandler);

    if ( ! this.interceptors) {
      // Configure default interceptors that can be disabled here
      this.interceptors = [
        this.injector.get(ApiPrefixInterceptor),
        this.injector.get(ErrorHandlerInterceptor),
        this.injector.get(CancelRequestOnNavigationInterceptor),
        this.injector.get(SetSerializationInterceptor)
      ];
    }
  }

  cache(forceUpdate?: boolean): HttpClient {
    const cacheInterceptor = this.injector.get(CacheInterceptor).configure({ update: forceUpdate });
    return this.addInterceptor(cacheInterceptor);
  }

  skipErrorHandler(): HttpClient {
    return this.removeInterceptor(ErrorHandlerInterceptor);
  }

  disableApiPrefix(): HttpClient {
    return this.removeInterceptor(ApiPrefixInterceptor);
  }

  disableCancelOnNavigation(): HttpClient {
    return this.removeInterceptor(CancelRequestOnNavigationInterceptor);
  }

  disableSetSerialization(): HttpClient {
    return this.removeInterceptor(SetSerializationInterceptor);
  }

  // Override the original method to wire interceptors when triggering the request.
  request(first: string|HttpRequest<any>, url?: string, options: {
    body?: any,
    headers?: HttpHeaders|{[header: string]: string | string[]},
    context?: HttpContext,
    observe?: 'body'|'events'|'response',
    params?: HttpParams|
          {[param: string]: string | number | boolean | ReadonlyArray<string|number|boolean>},
    reportProgress?: boolean,
    responseType?: 'arraybuffer'|'blob'|'json'|'text',
    withCredentials?: boolean,
  } = {}): Observable<any> {
    const handler = this.interceptors.reduceRight(
      (next, interceptor) => new HttpInterceptorHandler(next, interceptor),
      this.httpHandler
    );
    if (typeof first === 'string') {
      return new HttpClient(handler).request(first, url, options);
    } else {
      return new HttpClient(handler).request(first);
    }
  }

  private removeInterceptor(interceptorType: Type<any>): HttpService {
    return new HttpService(
      this.httpHandler,
      this.injector,
      this.interceptors.filter(i => ! (i instanceof interceptorType))
    );
  }

  private addInterceptor(interceptor: HttpInterceptor): HttpService {
    return new HttpService(this.httpHandler, this.injector, this.interceptors.concat([interceptor]));
  }
}
