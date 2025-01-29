import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed } from '@angular/core/testing';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { KeycloakAuthorizationService } from './keycloak-authorization.service';
import { KeycloakMiningRole } from '@app/shared/interfaces/keycloak-mining-role.interface';
import { ProjectRole } from '@innowake/mining-api-angular-client';

describe('AuthorizationService', () => {
  let service: KeycloakAuthorizationService;

  describe('Admin role', () => {
    const listUserRole: KeycloakMiningRole[] = [
      { client: null, project: null, value:'admin' }
    ];

    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          HttpClientTestingModule
        ],
        providers: [
          KeycloakAuthorizationService
        ]
      });
      service = TestBed.inject(KeycloakAuthorizationService);
      service.setUserRoles(listUserRole);
    });

    it('should be created', () => {
      expect(service).toBeTruthy();
    });

    it('should be admin', () => {
      const result = service.isAdmin();
      expect(result).toBeTrue();
    });

    it('should be client admin by hierarchy', () => {
      const result = service.isClientAdmin(1);
      expect(result).toBeTrue();
    });

    it('should have user role by hierarchy 1', () => {
      const result = service.hasUserRole(new ClientProjectRelationship(1, 'Admin Client', 1), ProjectRole.UserRoleEnum.MANAGER);
      expect(result).toBeTrue();
    });

    it('should have project nature by hierarchy', () => {
      const resultMining = service.hasProjectNature(1, ProjectRole.ProjectNaturesEnum.MINING);
      expect(resultMining).toBeTrue();
      const resultDiscovery = service.hasProjectNature(1, ProjectRole.ProjectNaturesEnum.DISCOVERY);
      expect(resultDiscovery).toBeTrue();
      const resultDiscoLight = service.hasProjectNature(1, ProjectRole.ProjectNaturesEnum.DISCOVERY_LIGHT);
      expect(resultDiscoLight).toBeTrue();
    });
  });

  describe('Client admin role', () => {
    const listUserRole: KeycloakMiningRole[] = [
      { client: 1, project: 1, value: 'admin' },
      { client: 1, project: 1, value: 'manager' },
      { client: 1, project: 1, value: 'client-admin'}
    ];

    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          HttpClientTestingModule
        ],
        providers: [
          KeycloakAuthorizationService
        ]
      });
      service = TestBed.inject(KeycloakAuthorizationService);
      service.setUserRoles(listUserRole);
    });

    it('should not be admin', () => {
      const result = service.isAdmin();
      expect(result).toBeFalse();
    });

    it('should be client admin', () => {
      const result = service.isClientAdmin(1);
      expect(result).toBeTrue();
    });

    it('should not be client admin', () => {
      const result = service.isClientAdmin(2);
      expect(result).toBeFalse();
    });

    it('should have access to client projects being a client admin', () => {
      const result = service.hasAccessToClientProjects(1);
      expect(result).toBeTrue();
    });

    it('should have user role by hierarchy 2', () => {
      const result = service.hasUserRole(new ClientProjectRelationship(1, 'Admin Role Client', 1), ProjectRole.UserRoleEnum.MANAGER);
      expect(result).toBeTrue();
    });
  });

  describe('Project user role and nature', () => {
    const listUserRole: KeycloakMiningRole[] = [
      { client: 1, project: 1, value: 'viewer' },
      { client: 1, project: 1, value: 'mining' },
      { client: 1, project: 2, value: 'editor' },
      { client: 1, project: 2, value: 'discovery' },
      { client: 1, project: 3, value: 'manager' },
      { client: 1, project: 3, value: 'discovery_lite' }
    ];

    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          HttpClientTestingModule
        ],
        providers: [
          KeycloakAuthorizationService
        ]
      });
      service = TestBed.inject(KeycloakAuthorizationService);
      service.setUserRoles(listUserRole);
    });

    it('should have user role', () => {
      const result = service.hasUserRole(new ClientProjectRelationship(1, 'Viewer', 1), ProjectRole.UserRoleEnum.VIEWER);
      expect(result).toBeTrue();
    });

    it('should have user role by hierarchy 3', () => {
      const result = service.hasUserRole(new ClientProjectRelationship(1, 'Editor', 3), ProjectRole.UserRoleEnum.EDITOR);
      expect(result).toBeTrue();
    });

    it('should not have user role', () => {
      const result = service.hasUserRole(new ClientProjectRelationship(1, 'Manager', 2), ProjectRole.UserRoleEnum.MANAGER);
      expect(result).toBeFalse();
    });

    it('should not have project role', () => {
      const resultUserRole = service.hasUserRole(new ClientProjectRelationship(1, 'Not Manager', 4), ProjectRole.UserRoleEnum.MANAGER);
      expect(resultUserRole).toBeFalse();
      const resultProjectNature = service.hasProjectNature(4, ProjectRole.ProjectNaturesEnum.MINING);
      expect(resultProjectNature).toBeFalse();
    });

    it('should have project nature', () => {
      const result = service.hasProjectNature(1, ProjectRole.ProjectNaturesEnum.MINING);
      expect(result).toBeTrue();
    });

    it('should have access to client projects if the user has corresponding role for that clientId', () => {
      /* The keycloak user role with {client-1-} exists, so access to client-1 's projects must be granted. */
      const result = service.hasAccessToClientProjects(1);
      expect(result).toBeTrue();
    });

    it('should not have access to client projects if the user does not have corresponding role for that clientId', () => {
      /* None of the keycloak user roles match {client-2-}, so access to client-2 's projects must be denied. */
      const result = service.hasAccessToClientProjects(2);
      expect(result).toBeFalse();
    });
  });
});
