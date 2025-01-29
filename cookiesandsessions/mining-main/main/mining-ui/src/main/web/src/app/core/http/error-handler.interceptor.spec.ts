import { TestBed, waitForAsync } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { HTTP_INTERCEPTORS, HttpClient } from '@angular/common/http';
import { Router } from '@angular/router';

import { ErrorHandlerInterceptor } from './error-handler.interceptor';
import { RouterTestingModule } from '@angular/router/testing';
import { IdentityAccessManagementService } from '../authentication/identity-access-management.service';
import { ErrorComponent } from '../../modules/error-module/error.component';
import { EclipseService } from '../authentication/eclipse.service';

describe('ErrorHandlerInterceptor', () => {
  let errorHandlerInterceptor: ErrorHandlerInterceptor;
  let http: HttpClient;
  let httpMock: HttpTestingController;
  let router: Router;
  const identityAccessManagementServiceSpy = jasmine.createSpyObj('identityAccessManagementService', ['logout', 'isAccessTokenFromUrl']);
  const eclipseServiceDummy = { 'isEclipseView': false, getAccessToken() { return Promise.resolve("DUMMY") } };
  const messageServiceSpy = jasmine.createSpyObj('NzMessageService', ['error']);
  const translateServiceSpy = jasmine.createSpyObj('TranslateService', ['instant']);

  function createInterceptor() {
    errorHandlerInterceptor = new ErrorHandlerInterceptor(router, identityAccessManagementServiceSpy, eclipseServiceDummy, messageServiceSpy, translateServiceSpy);
    return errorHandlerInterceptor;
  }

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule, RouterTestingModule.withRoutes([{ path: 'home', component: ErrorComponent }])],
      providers: [
        {
          provide: HTTP_INTERCEPTORS,
          useFactory: createInterceptor,
          multi: true
        },
        { provide: IdentityAccessManagementService, useValue: identityAccessManagementServiceSpy },
        { provide: EclipseService, useValue: eclipseServiceDummy }
      ]
    });

    http = TestBed.inject(HttpClient);
    httpMock = TestBed.inject(HttpTestingController);
    router = jasmine.createSpyObj('router', ['navigate']);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should catch error and call error handler', waitForAsync(() => {
    TestBed.inject(HTTP_INTERCEPTORS);
    const interceptSpy = spyOn(errorHandlerInterceptor, 'intercept').and.callThrough();
    http.get('/toto').subscribe(
      () => fail('should error'),
      () => {
        expect(interceptSpy).toHaveBeenCalled();
      }
    );

    httpMock.expectOne({}).flush(null, {
      status: 404,
      statusText: 'error'
    });
  }));

  it('router should call error page on 401 if token is from url', () => {
    identityAccessManagementServiceSpy.isAccessTokenFromUrl.and.returnValue(true);
    eclipseServiceDummy.isEclipseView = false;
    http.get('/toto').subscribe(
      () => fail('should error'),
      () => {
        expect(router.navigate).toHaveBeenCalledWith(['/error']);
      }
    );
    httpMock.expectOne({}).flush(null, {
      status: 401,
      statusText: 'error'
    });
  });

  it('router should call error page on 401 if in Eclipse view', () => {
    identityAccessManagementServiceSpy.isAccessTokenFromUrl.and.returnValue(false);
    eclipseServiceDummy.isEclipseView = true;
    http.get('/toto').subscribe(
      () => fail('should error'),
      () => {
        expect(router.navigate).toHaveBeenCalledWith(['/error']);
      }
    );
    httpMock.expectOne({}).flush(null, {
      status: 401,
      statusText: 'error'
    });
  });

  it('router should call login page on 401 if token not from url and not in Eclipse view', () => {
    identityAccessManagementServiceSpy.isAccessTokenFromUrl.and.returnValue(false);
    eclipseServiceDummy.isEclipseView = false;
    const mockMessageRef = jasmine.createSpyObj('NzMessageRef', ['onClose']);
    messageServiceSpy.error.and.returnValue(mockMessageRef);
    http.get('/toto').subscribe(
      () => fail('should error'),
      () => {
        expect(messageServiceSpy.error).toHaveBeenCalled();
        expect(router.navigate).not.toHaveBeenCalled();
      }
    );
    httpMock.expectOne({}).flush(null, {
      status: 401,
      statusText: 'error'
    });
  });
});
