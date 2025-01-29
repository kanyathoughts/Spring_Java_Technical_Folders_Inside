import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed } from '@angular/core/testing';
import { of, throwError } from 'rxjs';
import { EclipseService } from '../authentication/eclipse.service';
import { KeycloakService } from '../authentication/keycloak.service';
import { initializeKeycloak } from './keycloak.initializer';
import { environment } from '@env/environment';

describe('KeyclaokInitializer', () => {
    const keycloakServiceSpy = jasmine.createSpyObj('KeycloakService', ['checkLogin']);
    let keycloakService: KeycloakService;
    let eclipseService: EclipseService;
    let httpClient: HttpClient;

    beforeEach(() => {
        TestBed.configureTestingModule({
            imports: [
                HttpClientTestingModule,
            ],
            providers: [
                { provide: KeycloakService, useValue: keycloakServiceSpy },
            ]
        });
        keycloakService = TestBed.inject(KeycloakService);
        eclipseService = TestBed.inject(EclipseService);
        httpClient = TestBed.inject(HttpClient);
        eclipseService.isEclipseView = false;
        environment.keyCloakConfigUrl = '/test';
    });

    it('should use keycloak', done => {
        spyOn(httpClient, 'get').and.returnValue(of(true));
        keycloakServiceSpy.checkLogin.and.returnValue(of(true));
        const obs = initializeKeycloak(keycloakService, eclipseService, httpClient);
        obs().subscribe(() => {
          expect(keycloakService.checkLogin).toHaveBeenCalled();
          done();
        });
    });

    it('should reject the promise', done => {
      spyOn(httpClient, 'get').and.returnValue(throwError({}));
      const obs = initializeKeycloak(keycloakService, eclipseService, httpClient);
      obs().subscribe((result) => {
        expect(result).toBeFalse();
        done();
      });
    });

    it('should skip keycloack for eclipse web views', done => {
      eclipseService.isEclipseView = true;
      const getSpy = spyOn(httpClient, 'get');
      const obs = initializeKeycloak(keycloakService, eclipseService, httpClient);
      obs().subscribe((result) => {
        expect(getSpy).not.toHaveBeenCalled();
        expect(result).toBeTrue();
        done();
      });
    });
});
