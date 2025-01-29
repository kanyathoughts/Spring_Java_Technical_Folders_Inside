import { Router, RouterStateSnapshot } from '@angular/router';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { of } from 'rxjs/internal/observable/of';
import { KeycloakService } from '../authentication/keycloak.service';
import { ClientProjectRelationshipService } from '../services/client-project-relationship/client-project-relationship.service';
import { StateMaintainenceService } from '../services/state-maintenance/state-maintainence.service';
import { AuthorizationGuard } from './authorization.guard';
import { KeycloakAuthorizationService } from './keycloak-authorization.service';
import { ProjectRole } from '@innowake/mining-api-angular-client';

describe('AuthorizationGuard', () => {
  let authorizationGuard: AuthorizationGuard;
  const mockRouteSnapshot = jasmine.createSpyObj<RouterStateSnapshot>('RouterStateSnapshot', ['toString']);
  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const routerSpy = jasmine.createSpyObj<Router>('Router', ['navigate']);
  const authorizationServiceSpy = jasmine.createSpyObj<KeycloakAuthorizationService>('KeycloakAuthorizationService',
    ['hasAccessToClientProjects', 'hasUserRole', 'isAdmin', 'isClientAdmin']);
  const keycloakServiceSpy = jasmine.createSpyObj<KeycloakService>('KeycloakService', ['isAvailable']);
  const routeStateMock: any = { snapshot: {}, url: '/home', data: {}};
  const clientProjectRelatiomshipMock: ClientProjectRelationship = new ClientProjectRelationship(1, 'Client test', 2, 'Project Test');
  const stateMaintainenceServiceSpy = jasmine.createSpyObj<StateMaintainenceService>('StateMaintainenceService', ['clearStateOnProjectChange']);
  const clientProjectRelationshipService = new ClientProjectRelationshipService(stateMaintainenceServiceSpy);

  beforeEach(() => {
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of());
    authorizationServiceSpy.hasAccessToClientProjects.and.returnValue(true);
    authorizationServiceSpy.hasUserRole.and.returnValue(true);
    authorizationServiceSpy.isAdmin.and.returnValue(false);
    authorizationGuard = new AuthorizationGuard(routerSpy, clientProjectRelationshipService, authorizationServiceSpy, keycloakServiceSpy);
  });

  it('should have a canActivate method', () => {
    expect(typeof authorizationGuard.canActivate).toBe('function');
  });

  xit('should redirect when current client is undefined', () => {
    clientProjectRelationshipService.setClientProjectRelationship(null);
    authorizationGuard.canActivate(routeStateMock, mockRouteSnapshot);
    expect(routerSpy.navigate).toHaveBeenCalledWith(['clients']);
  });

  xit('Should redirect if neither role or client authorization is specified', () => {
    const clientProjectMock = new ClientProjectRelationship(1, 'Client test');
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectMock));
    authorizationGuard = new AuthorizationGuard(routerSpy, clientProjectRelationshipServiceSpy, authorizationServiceSpy, keycloakServiceSpy);
    const routeState: any = { snapshot: {}, url: '/home', data: {}};
    authorizationGuard.canActivate(routeState, mockRouteSnapshot);
    expect(routerSpy.navigate).toHaveBeenCalled();
  });

  it('Should authorize page needing only client rights', () => {
    const clientProjectMock = new ClientProjectRelationship(1, 'Client test');
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectMock));
    authorizationGuard = new AuthorizationGuard(routerSpy, clientProjectRelationshipServiceSpy, authorizationServiceSpy, keycloakServiceSpy);
    const routeState: any = { snapshot: {}, url: '/home', data: { clientAuthorization: ProjectRole.UserRoleEnum.VIEWER }};
    authorizationGuard.canActivate(routeState, mockRouteSnapshot);
    expect(authorizationServiceSpy.hasAccessToClientProjects).toHaveBeenCalledWith(1);
  });

  it('Should authorize page for client admin', () => {
    const clientProjectMock = new ClientProjectRelationship(1, 'Client test');
    authorizationServiceSpy.isClientAdmin.and.returnValue(true);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectMock));
    authorizationGuard = new AuthorizationGuard(routerSpy, clientProjectRelationshipServiceSpy, authorizationServiceSpy, keycloakServiceSpy);
    const routeState: any = { snapshot: {}, url: '/home', data: { clientAuthorization: ProjectRole.UserRoleEnum.ADMIN }};
    expect(authorizationGuard.canActivate(routeState, mockRouteSnapshot)).toBeTruthy();
  });

  it('Should redirect if the user has not rights for the client scope', () => {
    const clientProjectMock = new ClientProjectRelationship(1, 'Client test');
    authorizationServiceSpy.hasAccessToClientProjects.and.returnValue(false);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectMock));
    authorizationGuard = new AuthorizationGuard(routerSpy, clientProjectRelationshipServiceSpy, authorizationServiceSpy, keycloakServiceSpy);
    const routeState: any = { snapshot: {}, url: '/home', data: { clientAuthorization: ProjectRole.UserRoleEnum.VIEWER }};
    authorizationGuard.canActivate(routeState, mockRouteSnapshot);
    expect(routerSpy.navigate).toHaveBeenCalled();
  });

  it('Should redirect if role is specified but current client is empty', () => {
    authorizationServiceSpy.hasUserRole.calls.reset();
    authorizationGuard = new AuthorizationGuard(routerSpy, clientProjectRelationshipServiceSpy, authorizationServiceSpy, keycloakServiceSpy);
    const routeState: any = { snapshot: {}, url: '/home', data: { role: ProjectRole.UserRoleEnum.EDITOR }};
    authorizationGuard.canActivate(routeStateMock, mockRouteSnapshot);
    authorizationGuard.canActivate(routeState, mockRouteSnapshot);
    expect(routerSpy.navigate).toHaveBeenCalled();
  });

  it('Should use authorize page using user role set in route', () => {
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelatiomshipMock));
    authorizationGuard = new AuthorizationGuard(routerSpy, clientProjectRelationshipServiceSpy, authorizationServiceSpy, keycloakServiceSpy);
    const routeState: any = { snapshot: {}, url: '/home', data: { role: ProjectRole.UserRoleEnum.EDITOR }};
    authorizationGuard.canActivate(routeState, mockRouteSnapshot);
    expect(authorizationServiceSpy.hasUserRole).toHaveBeenCalledWith(clientProjectRelatiomshipMock, ProjectRole.UserRoleEnum.EDITOR );
  });

  it('Should use authorize page using user role set in child route', () => {
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelatiomshipMock));
    authorizationGuard = new AuthorizationGuard(routerSpy, clientProjectRelationshipServiceSpy, authorizationServiceSpy, keycloakServiceSpy);
    const routeState: any = { snapshot: {}, url: '/home', data: {}, firstChild: {data: { role: ProjectRole.UserRoleEnum.MANAGER }}};
    authorizationGuard.canActivate(routeState, mockRouteSnapshot);
    expect(authorizationServiceSpy.hasUserRole).toHaveBeenCalledWith(clientProjectRelatiomshipMock, ProjectRole.UserRoleEnum.MANAGER );
  });
});
