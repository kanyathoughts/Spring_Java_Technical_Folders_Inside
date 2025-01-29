import { TestBed } from '@angular/core/testing';

import { OauthtokenService } from './oauthtoken.service';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { ErrorComponent } from '../../modules/error-module/error.component';

describe('OauthtokenService', () => {
  let oauthTokenService: OauthtokenService;
  let httpMock: HttpTestingController;

  const oAuthToken: any = {
    access_token: 'ffb3eff8-53a7-4153-bff1',
    tokenType: 'bearer',
    username: 'test-admin'
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [],
      declarations: [ErrorComponent]
    });
    httpMock = TestBed.inject(HttpTestingController);
    oauthTokenService = TestBed.inject(OauthtokenService);
  });

  it('should be created', () => {
    const service: OauthtokenService = TestBed.inject(OauthtokenService);
    expect(service).toBeTruthy();
  });

  it('Test to set oauth token in the local storage', () => {
    const service: OauthtokenService = TestBed.inject(OauthtokenService);
    service.setOauthToken(oAuthToken);
    expect(service.getAccessToken()).toEqual(oAuthToken.access_token);
    expect(service.getUsername()).toBe('test-admin');
  });
});
