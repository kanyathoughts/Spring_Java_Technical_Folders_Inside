import { Injectable } from '@angular/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { AuthorizationService } from './authorization-service.interface';
import { KeycloakMiningRole } from '@app/shared/interfaces/keycloak-mining-role.interface';
import { EntityId, ProjectRole } from '@innowake/mining-api-angular-client';

/**
 * This service provides authorization mechanismes based on user roles and project nature.
 */
@Injectable()
export class KeycloakAuthorizationService implements AuthorizationService {

  private roles: KeycloakMiningRole[];

  /**
   * Set Keycloak user roles
   * @param roles Keycloak user roles
   */
  setUserRoles(roles: KeycloakMiningRole[]): void {
    this.roles = roles;
  }

  /**
   * Returns whether the current user is admin or not
   * @return boolean value
   */
  isAdmin(): boolean {
    return this.roles.findIndex(role => role.client === null && role.project === null && role.value === ProjectRole.UserRoleEnum.ADMIN.toLowerCase()) >= 0;
  }

  /**
   * Returns whether the current user is admin for a client or not
   * @param clientId client Id for which the rights are checked. If no clientId, this methods checks if the user is admin for any client.
   * @return boolean value
   */
  isClientAdmin(clientId?: EntityId): boolean {
    if (this.isAdmin()) {
      return true;
    }
    const clientAdminRole = ProjectRole.UserRoleEnum.CLIENT_ADMIN.toLowerCase().replace('_', '-');
    if ( ! clientId) {
      return this.roles.filter(role => role.client && role.value === clientAdminRole).length > 0;
    }
    return this.roles.findIndex(role => role.client === clientId && role.value === clientAdminRole) !== -1;
  }

  /**
   * Returns whether the current user has a given role for a project or not
   * @param currentClient current client
   * @param userRole role minimum requested
   * @return boolean value
   */
  hasUserRole(currentClient: ClientProjectRelationship, requestedUserRole: ProjectRole.UserRoleEnum): boolean {
    const clientId = currentClient.getClientId();
    const projectId = currentClient.getProjectId();
    if (this.isClientAdmin(clientId)) {
      return true;
    }

    const currentUserProjectRole = this.roles.filter(val => val.project === projectId);
    if (! currentUserProjectRole || currentUserProjectRole.length === 0) {
      return false;
    }

    switch (requestedUserRole) {
      case ProjectRole.UserRoleEnum.VIEWER:
        return true ;
      case ProjectRole.UserRoleEnum.EDITOR:
        const index = currentUserProjectRole.findIndex(role =>
          role.value === ProjectRole.UserRoleEnum.EDITOR.toLowerCase() || role.value === ProjectRole.UserRoleEnum.MANAGER.toLowerCase()
        );
        return index >= 0;
      case ProjectRole.UserRoleEnum.MANAGER:
        return currentUserProjectRole.findIndex(role => role.value === ProjectRole.UserRoleEnum.MANAGER.toLowerCase()) >= 0;
      default:
        return false;
    }
  }

  /**
   * Check if the user has right for a project related to the given client id
   * @param clientId Client id for which the roles must be related
   * @return boolean value
   */
  hasAccessToClientProjects(clientId: number): boolean {
    if (this.isClientAdmin(clientId)) {
      return true;
    }
    return this.roles.findIndex(role => role.client === clientId) >= 0;
  }

  /**
   * Returns whether the current user has authorization for a project and a given project nature or not
   * @param projectId project Id for which the authorization is checked
   * @param projectNature Project nature for which the authorization is checked
   * @return boolean value
   */
  hasProjectNature(projectId: number, projectNature: ProjectRole.ProjectNaturesEnum): boolean {
    if (this.isAdmin()) {
      return true;
    }
    const projectRoles = this.roles.filter(val => val.project === projectId);
    if (! projectRoles || projectRoles.length === 0) {
      return false;
    }
    return projectRoles.findIndex(val => val.value === projectNature.toLowerCase()) >= 0;
  }
}
