import { fakeAsync, TestBed, tick } from '@angular/core/testing';

import { KeycloakService } from './keycloak.service';
import { IdentityAccessManagementService } from './identity-access-management.service';
import { RouterTestingModule } from '@angular/router/testing';
import { OauthtokenService } from './oauthtoken.service';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Router } from '@angular/router';
import { ErrorComponent } from '../../modules/error-module/error.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { of } from 'rxjs';

declare global {
  interface Window {
    java__callback__authenticate_eclipse_view(): Promise<string>;
  }
}

describe('IdentityAccessManagementService', () => {
  let keycloakService: KeycloakService;
  let service: IdentityAccessManagementService;
  let router: Router;
  let httpClient: HttpClient;
  const messageServiceSpy = jasmine.createSpyObj('NzMessageService', ['error']);
  const translateServiceSpy = jasmine.createSpyObj('TranslateService', ['instant']);
  const keycloakServiceSpy = jasmine.createSpyObj('KeycloakService', ['logout', 'getUsername']);
  const oauthTokenServiceSpy = jasmine.createSpyObj('OauthtokenService', ['setOauthToken', 'getUsername', 'getAccessToken']);
  
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        RouterTestingModule.withRoutes([{ path: 'home', component: ErrorComponent }])
      ],
      providers: [
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: TranslateService, useValue: translateServiceSpy },
        { provide: KeycloakService, useValue:  keycloakServiceSpy },
        { provide: OauthtokenService, useValue:  oauthTokenServiceSpy }
      ]
    });
    service = TestBed.inject(IdentityAccessManagementService);
    keycloakService = TestBed.inject(KeycloakService);
    router = TestBed.inject(Router);
    httpClient = TestBed.inject(HttpClient);
    window.java__callback__authenticate_eclipse_view = undefined;
    oauthTokenServiceSpy.getAccessToken.and.returnValue('token');
  });

  afterAll(() => {
    window.java__callback__authenticate_eclipse_view = null;
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should return the token from IAM Service', async () => {
    const token = await service.getAccessToken();
    expect(token).toEqual('token');
  });

  it('should return the username from IAM Service', () => {
    service.getUsername();
    expect(oauthTokenServiceSpy.getUsername).toHaveBeenCalled();
  });

  it('should logout user from IAM Service', () => {
    spyOn(router, 'navigate').and.returnValue(of(true).toPromise());
    service.logout(httpClient);
    expect(router.navigate).toHaveBeenCalled();
  });

  it('access token is obtained from url positive', fakeAsync(() => {
    router.navigate(['home'], { queryParams: { 'access-token': 'c8960c7a-6df5-4f60-8f18-249268d9a581' } });
    tick();
    expect(service.isAccessTokenFromUrl()).toEqual(true);
  }));

  it('access token is obtained from url negative', fakeAsync(() => {
    router.navigate(['home']);
    tick();
    expect(service.isAccessTokenFromUrl()).toEqual(false);
  }));
});
