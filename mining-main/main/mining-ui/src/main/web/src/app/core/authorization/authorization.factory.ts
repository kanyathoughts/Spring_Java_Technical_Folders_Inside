import { KeycloakService } from '../authentication/keycloak.service';
import { KeycloakAuthorizationService } from './keycloak-authorization.service';
import { NoAuthorizationService } from './no-authorization.service';

/**
 * This factory method decide whether the application uses Authorization service or not
 * @param keycloak the keycloak service
 * @returns instance of NoAuthorization Or KeyCloakAuthorization service.
 */
export const  authorizationFactory = (keycloak: KeycloakService): NoAuthorizationService | KeycloakAuthorizationService =>{
	if (keycloak.isAvailable) {
    const authService = new KeycloakAuthorizationService();
    authService.setUserRoles(keycloak.getUserRoles());
		return authService;
	} else {
		return new NoAuthorizationService();
	}
};
