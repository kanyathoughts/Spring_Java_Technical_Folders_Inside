import { environment } from '@env/environment';
import { EclipseService } from '../authentication/eclipse.service';
import { getBasePath } from '../utils/base-path.utils';
import { KeycloakService } from '../authentication/keycloak.service';
import { Observable, of } from 'rxjs';
import { switchMap, catchError } from 'rxjs/operators';
import { HttpClient } from '@angular/common/http';

export const initializeKeycloak = (
  keycloak: KeycloakService,
  eclipse: EclipseService,
  http: HttpClient
  ): () => Observable<any> => {
  if ( ! eclipse.isEclipseView && ! location.href.includes('/error/')) {
      return () => {
        const url = environment.keyCloakConfigUrl ? getBasePath() + environment.keyCloakConfigUrl : null;
        return http.get(url).pipe(
          switchMap((response) => {
            keycloak.useKeycloackAuth = true;
            keycloak.keyCloakAuthURL = response['auth-server-url'] || '';
            keycloak.keyCloakRealm = response['realm'] || '';
            keycloak.keyCloakClient = response['client-id'] || '';
            return keycloak.checkLogin(http, location.hash.replace('#', ''));
          }), catchError(() => of(false))
        );
      };
    }
    return () => of(true);
  };
