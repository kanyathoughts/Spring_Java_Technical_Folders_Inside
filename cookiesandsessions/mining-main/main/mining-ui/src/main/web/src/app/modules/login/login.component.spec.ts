import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { TranslateModule } from '@ngx-translate/core';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';

import { CoreModule, AuthenticationService} from '@app/core';
import { LoginComponent } from './login.component';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { throwError, of } from 'rxjs';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { LegacyAuthControllerService, VersionControllerService } from '@innowake/mining-api-angular-client';

describe('LoginComponent', () => {
  let component: LoginComponent;
  let fixture: ComponentFixture<LoginComponent>;
  let httpMock: HttpTestingController;
  const oauthServiceSpy: jasmine.SpyObj<OauthtokenService> = jasmine.createSpyObj<OauthtokenService>('OauthtokenService', ['setOauthToken']);
  const legacyAuthSpy: jasmine.SpyObj<LegacyAuthControllerService> = jasmine.createSpyObj<LegacyAuthControllerService>('LegacyAuthControllerService', ['login']);
  const authenticationService: AuthenticationService = new AuthenticationService(oauthServiceSpy, legacyAuthSpy);
  const versionServiceSpy = jasmine.createSpyObj<VersionControllerService>('VersionControllerService', ['getVersion']);
  const oAuthToken: any = {
    access_token: 'ffb3eff8-53a7-4153-bff1',
    token_type: 'bearer',
    username: 'test-admin'
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        TranslateModule.forRoot(),
        ReactiveFormsModule,
        CoreModule,
        HttpClientTestingModule
      ],
      declarations: [LoginComponent],
      providers: [
        {provide: AuthenticationService, useValue: authenticationService},
        {provide: OauthtokenService, useValue: oauthServiceSpy},
        {provide: VersionControllerService, useValue: versionServiceSpy}
      ]
    }).compileComponents();
    httpMock = TestBed.inject(HttpTestingController);
    legacyAuthSpy.login.and.returnValue(of(oAuthToken as any));
    versionServiceSpy.getVersion.and.returnValue(of([] as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LoginComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
    component.login();
    expect(component.isLoading).toBeFalsy();
  });

  describe('login error', () => {
    beforeEach(() => {
      fixture = TestBed.createComponent(LoginComponent);
      component = fixture.componentInstance;
      legacyAuthSpy.login.and.returnValue(throwError('Test Error'));
      fixture.detectChanges();
    });

    it('should throw error on login', () => {
      expect(component).toBeTruthy();
      component.login();
      expect(component.error).toBe('Test Error');
    });
  });
});
