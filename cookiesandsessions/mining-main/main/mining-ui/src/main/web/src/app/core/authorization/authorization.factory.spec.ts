import { TestBed } from '@angular/core/testing';
import { KeycloakService } from '../authentication/keycloak.service';
import { authorizationFactory } from './authorization.factory';
import { KeycloakAuthorizationService } from './keycloak-authorization.service';

describe('AuthorizationFactory', () => {
    let service: any;
    const keycloakServiceSpy = jasmine.createSpyObj('KeycloakService', ['isAvailable', 'getUserRoles']);

    it('should use NoAuthorizationService', () => {
        keycloakServiceSpy.isAvailable = false;
        TestBed.configureTestingModule({
            providers: [
                { provide: KeycloakService, useValue: keycloakServiceSpy },
                {
                    provide: KeycloakAuthorizationService,
                    useFactory: authorizationFactory,
                    deps: [KeycloakService]
                }
            ]
        });
        service = TestBed.inject(KeycloakAuthorizationService);
        expect(service.constructor.name).toBe('NoAuthorizationService');
    });

    it('should use AuthorizationService', () => {
        keycloakServiceSpy.isAvailable = true;
        keycloakServiceSpy.getUserRoles.and.returnValue([]);
        TestBed.configureTestingModule({
            providers: [
                { provide: KeycloakService, useValue: keycloakServiceSpy },
                {
                    provide: KeycloakAuthorizationService,
                    useFactory: authorizationFactory,
                    deps: [KeycloakService]
                }
            ]
        });
        service = TestBed.inject(KeycloakAuthorizationService);
        expect(service.constructor.name).toBe('KeycloakAuthorizationService');
    });
});
