import { Type } from '@angular/core';
import { TestBed, tick, fakeAsync } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { HttpClient, HttpInterceptor } from '@angular/common/http';

import { HttpService } from './http.service';
import { HttpCacheService } from './http-cache.service';
import { ErrorHandlerInterceptor } from './error-handler.interceptor';
import { CacheInterceptor } from './cache.interceptor';
import { ApiPrefixInterceptor } from './api-prefix.interceptor';
import { RouterTestingModule } from '@angular/router/testing';
import { CancelRequestOnNavigationInterceptor } from './cancel-request-on-navigation.interceptor';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { SetSerializationInterceptor } from './set-serialization.interceptor';

describe('HttpService', () => {
  let httpCacheService: HttpCacheService;
  let http: HttpClient;
  let httpMock: HttpTestingController;
  let interceptors: HttpInterceptor[];
  const messageServiceSpy = jasmine.createSpyObj('NzMessageService', ['error']);
  const translateServiceSpy = jasmine.createSpyObj('TranslateService', ['instant']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule, RouterTestingModule],
      providers: [
        ErrorHandlerInterceptor,
        SetSerializationInterceptor,
        CacheInterceptor,
        ApiPrefixInterceptor,
        HttpCacheService,
        CancelRequestOnNavigationInterceptor,
        {
          provide: HttpClient,
          useClass: HttpService
        },
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: TranslateService, useValue: translateServiceSpy }
      ]
    });

    http = TestBed.inject(HttpClient);
    httpMock = TestBed.inject(HttpTestingController as Type<HttpTestingController>);
    httpCacheService = TestBed.inject(HttpCacheService);

    const realRequest = http.request;
    spyOn(HttpService.prototype, 'request').and.callFake(
      function(this: any, method: string, url: string, options?: any) {
        interceptors = this.interceptors;
        return realRequest.call(this, method, url, options);
      }
    );
  });

  afterEach(() => {
    httpCacheService.cleanCache();
    httpMock.verify();
  });

  it('should use error handler, API prefix and no cache by default', fakeAsync(() => {
    // Act
    const request = http.get('/toto');

    // Assert
    request.subscribe(() => {
      expect(http.request).toHaveBeenCalled();
      expect(interceptors.some(i => i instanceof ApiPrefixInterceptor)).toBeTruthy();
      expect(interceptors.some(i => i instanceof ErrorHandlerInterceptor)).toBeTruthy();
      expect(interceptors.some(i => i instanceof CacheInterceptor)).toBeFalsy();
    });

    tick();
    httpMock.expectOne({}).flush({});
  }));

  it('should use cache', fakeAsync(() => {
    // Act
    const request = http.cache().get('/toto');

    // Assert
    request.subscribe(() => {
      expect(interceptors.some(i => i instanceof ApiPrefixInterceptor)).toBeTruthy();
      expect(interceptors.some(i => i instanceof ErrorHandlerInterceptor)).toBeTruthy();
      expect(interceptors.some(i => i instanceof CacheInterceptor)).toBeTruthy();
    });

    tick();
    httpMock.expectOne({}).flush({});
  }));

  it('should skip error handler', fakeAsync(() => {
    // Act
    const request = http.skipErrorHandler().get('/toto');

    // Assert
    request.subscribe(() => {
      expect(interceptors.some(i => i instanceof ApiPrefixInterceptor)).toBeTruthy();
      expect(interceptors.some(i => i instanceof ErrorHandlerInterceptor)).toBeFalsy();
      expect(interceptors.some(i => i instanceof CacheInterceptor)).toBeFalsy();
    });

    tick();
    httpMock.expectOne({}).flush({});
  }));

  it('should not use API prefix', () => {
    // Act
    const request = http.disableApiPrefix().get('/toto');

    // Assert
    request.subscribe(() => {
      expect(interceptors.some(i => i instanceof ApiPrefixInterceptor)).toBeFalsy();
      expect(interceptors.some(i => i instanceof ErrorHandlerInterceptor)).toBeTruthy();
      expect(interceptors.some(i => i instanceof CacheInterceptor)).toBeFalsy();
    });
    httpMock.expectOne({}).flush({});
  });
});
