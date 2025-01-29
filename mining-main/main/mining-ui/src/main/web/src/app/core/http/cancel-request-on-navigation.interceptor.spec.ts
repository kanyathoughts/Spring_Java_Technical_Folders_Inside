import { HttpClient, HTTP_INTERCEPTORS } from "@angular/common/http";
import { HttpClientTestingModule, HttpTestingController } from "@angular/common/http/testing";
import { fakeAsync, TestBed, tick } from "@angular/core/testing";
import { Router } from "@angular/router";
import { RouterTestingModule } from "@angular/router/testing";
import { CancelRequestOnNavigationInterceptor } from "./cancel-request-on-navigation.interceptor";

describe('CancelRequestOnNavigation', () => {
  let http: HttpClient;
  let interceptor: CancelRequestOnNavigationInterceptor;
  let router: Router;

  function createInterceptor() {
    interceptor = new CancelRequestOnNavigationInterceptor(router);
    return interceptor;
  }

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule, RouterTestingModule.withRoutes([])],
      providers: [
        {
          provide: HTTP_INTERCEPTORS,
          useFactory: createInterceptor,
          multi: true
        },
      ]
    });

    http = TestBed.inject(HttpClient);
    router = TestBed.inject(Router);
    TestBed.inject(HTTP_INTERCEPTORS);
  });

  it('should interupt the observable on navigation', fakeAsync(() => {
    const interceptSpy = spyOn(interceptor, 'intercept').and.callThrough();
    const navigationStartSpy = spyOn((interceptor as any).navigationStart, 'next').and.callThrough();
    http.get('http://domain.com/v1/api/something').subscribe();
    tick();
    router.navigate(['']);
    tick();
    expect(interceptSpy).toHaveBeenCalled();
    expect(navigationStartSpy).toHaveBeenCalled();
  }));
})