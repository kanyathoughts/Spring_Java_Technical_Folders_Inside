import { TestBed } from '@angular/core/testing';
import { KEYCLOAK_TOKEN_PLACEHOLDER, KeycloakService } from './keycloak.service';
import { RouterTestingModule } from '@angular/router/testing';
import { of, throwError } from 'rxjs';
import { environment } from '@env/environment';
import { KeycloakAuth } from '@app/shared/interfaces/keycloak-auth.interface';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { HttpClient } from '@angular/common/http';
import { Router } from '@angular/router';

describe('KeyCloakService', () => {
  let service: KeycloakService;
  let router: Router;
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'skipErrorHandler', 'get']);

  const authInstance: KeycloakAuth = {
    now: Date.now().toString(),
    subject: 'userId',
    access_token_issued: '',
    access_token_expires:'',
    session_id: '',
    session_created: '',
    session_accessed: '',
    session_expires: '',
    user_email: 'test@test.com',
    user_family_name: '',
    user_full_name: '',
    user_given_name: 'Test',
    user_logon_name: 'Test',
    mining_roles: [
      {client: 1, project: 1, value: 'admin'}
    ]
  }

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        RouterTestingModule.withRoutes([])
      ],
      providers: [
        { provide: HttpClient, useValue: httpServiceSpy }
      ]
    });
    service = TestBed.inject(KeycloakService);
    router = TestBed.inject(Router);
    httpServiceSpy.skipErrorHandler.and.returnValue(httpServiceSpy);
    environment.keyCloakConfigUrl = 'Test';
  });

  afterAll(() => {
    environment.keyCloakConfigUrl = '';
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should check login with success', done => {
    httpServiceSpy.get.and.returnValue(of(authInstance));
    const obs = service.checkLogin(httpServiceSpy, '');
    obs.subscribe(result => {
      expect(result).toBeTrue();
      done();
    });
  });

  it('should check login with error', done => {
    // We can't test the 401 code as setting the windows loation breaks the test execution
    httpServiceSpy.get.and.returnValue(throwError({status: 404}));
    const obs = service.checkLogin(httpServiceSpy, '');
    obs.subscribe(result => {
      expect(result).toBeFalse();
      done();
    });
  });

  it('should get the username of the logged in user', () => {
    (service as any).instance = authInstance;
    let userName = service.getUsername();
    expect(userName).toEqual('Test');
  });

  it('should logout the user', async () => {
    httpServiceSpy.get.and.returnValue(of(true));
    spyOn((service as any), 'logoutFromKeycloak').and.callFake(() => { });
    service.logout(httpServiceSpy);
    expect((service as any).logoutFromKeycloak).toHaveBeenCalled();
  });

  it('should check for token', async () => {
    const token = await service.getToken();
    expect(token).toEqual(KEYCLOAK_TOKEN_PLACEHOLDER);
  });

  it('should get the initials of logged in user', () => {
    (service as any).instance = authInstance;
    (service as any).instance.user_given_name = 'First';
    (service as any).instance.user_family_name = 'First';
    expect(service.getUserInitials()).toEqual('FF');

    (service as any).instance.user_given_name = null;
    (service as any).instance.user_family_name = 'First';
    expect(service.getUserInitials()).toEqual('F');

    (service as any).instance.user_given_name = 'First';
    (service as any).instance.user_family_name = null;
    expect(service.getUserInitials()).toEqual('F');
  });
});
