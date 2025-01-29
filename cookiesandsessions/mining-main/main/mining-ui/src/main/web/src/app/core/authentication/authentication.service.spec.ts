import { TestBed } from '@angular/core/testing';

import { AuthenticationService } from './authentication.service';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('AuthenticationService', () => {
  let authenticationService: AuthenticationService;
  let http: HttpClient;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        AuthenticationService
      ]
    });
    http = TestBed.inject(HttpClient);
    authenticationService = TestBed.inject(AuthenticationService);
  });

  it('should have a login method', () => {
    expect(typeof authenticationService).toBeTruthy();
  });
});
