import { HTTP_INTERCEPTORS, HttpEvent, HttpRequest, HttpResponse } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';
import { SetSerializationInterceptor } from './set-serialization.interceptor';
import { Observable, of } from 'rxjs';

describe('SetSerializationInterceptor', () => {

  let setSerializationInterceptor: SetSerializationInterceptor;

  const url = '/rest/endpoint';
  const mockHandler = {
    handle: (req: HttpRequest<any>): Observable<HttpEvent<any>> => of(new HttpResponse(req))
  };

  function createInterceptor() {
    setSerializationInterceptor = new SetSerializationInterceptor();
    return setSerializationInterceptor;
  }

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        {
          provide: HTTP_INTERCEPTORS,
          useFactory: createInterceptor,
          multi: true
        },
      ]
    });
    TestBed.inject(HTTP_INTERCEPTORS);
  });

  it('should sanityze body containing a Set', (done) => {
    const spy = spyOn(mockHandler, 'handle');
    const request = new HttpRequest('GET', url).clone({
      body:  new Set(['val1', 'val2', 'val3', 'val4'])
    });
    setSerializationInterceptor.intercept(request, mockHandler);
    const expectedRequest = request.clone({ body: ['val1', 'val2', 'val3', 'val4']})
    expect(spy).toHaveBeenCalledWith(expectedRequest);
    done();
  });

  it('should sanityze body containing a Set property', (done) => {
    const spy = spyOn(mockHandler, 'handle');
    const request = new HttpRequest('GET', url).clone({
      body:  {
        description: 'string property',
        setProperty: new Set(['val1', 'val2', 'val3', 'val4'])
      }
    });
    setSerializationInterceptor.intercept(request, mockHandler);
    const expectedRequest = request.clone({ body: {description: 'string property',
    setProperty:['val1', 'val2', 'val3', 'val4']}})
    expect(spy).toHaveBeenCalledWith(expectedRequest);
    done();
  });
});
