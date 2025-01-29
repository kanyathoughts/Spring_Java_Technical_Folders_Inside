import { HttpEvent, HttpHandler, HttpInterceptor, HttpRequest } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

/**
 * Set<T> are by design not serializable but in our case the openAPI generated code is using Set in object sent in POST request body
 * If we don't transform the Set to an array the Set will be serialized into an empty object.
 */
@Injectable()
export class SetSerializationInterceptor implements HttpInterceptor {

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    let body = request.body;
    if (body && typeof body === 'object') {
      if (Object.keys(body as object).length) {
        Object.keys(body as object).forEach((key: string) => {
          const value = body[key];
          if (body[key] instanceof Set) {
            body[key] = Array.from(value as Set<any>);
          }
        });
      } else if (body instanceof Set) {
        body = Array.from(body);
      }
    }
    request = request.clone({
      body
    });
    return next.handle(request);
  }

}
