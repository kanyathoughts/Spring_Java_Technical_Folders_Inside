import { TestBed, fakeAsync, tick } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController, TestRequest } from '@angular/common/http/testing';
import { HTTP_INTERCEPTORS, HttpClient } from '@angular/common/http';

import { ApiPrefixInterceptor } from './api-prefix.interceptor';
import { environment } from '@env/environment';
import { IdentityAccessManagementService } from '../authentication/identity-access-management.service';

describe('ApiPrefixInterceptor', () => {
  let http: HttpClient;
  let httpMock: HttpTestingController;
  const IAMServiceSpy: jasmine.SpyObj<IdentityAccessManagementService> = jasmine.createSpyObj('IAMServiceSpy', ['getAccessToken']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        {
          provide: HTTP_INTERCEPTORS,
          useClass: ApiPrefixInterceptor,
          multi: true
        },
        {
          provide: IdentityAccessManagementService,
          useValue: IAMServiceSpy
        },
      ]
    });

    http = TestBed.inject(HttpClient);
    httpMock = TestBed.inject(HttpTestingController);
    IAMServiceSpy.getAccessToken.and.returnValue(Promise.resolve('token'));
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should call token url if token is present', fakeAsync(() => {
    environment.production = false;
    http.get('http://domain.com/v1/api/authenticate/something').subscribe();

    tick();
    const request: TestRequest = httpMock.expectOne({ url: 'http://domain.com/v1/api/authenticate/something' });
    expect(request).not.toBeNull();
    expect(request.request.headers.has('authorization')).toBeTruthy();
    expect(request.request.headers.get('authorization')).toBe('Bearer token');
  }));

  it('should manipulate url.', fakeAsync(() => {
    environment.production = true;
    http.get('http://domain.com/v1/api/something').subscribe();
    tick();
    httpMock.expectOne({ url: '/v1/api/something' });
    environment.production = false;
  }));

  it('should tets the Delete Request', fakeAsync(() => {
    environment.production = true;
    http.delete('http://domain.com/v1/api/something').subscribe();
    tick();
    httpMock.expectOne({ url: '/v1/api/something' });
    environment.production = false;
  }));
});
