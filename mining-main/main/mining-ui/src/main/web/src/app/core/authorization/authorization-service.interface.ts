import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ProjectRole } from '@innowake/mining-api-angular-client';

/**
 * This interface ensure that all Authorisation services implement the same methods
 */
export interface AuthorizationService {
    isAdmin(): boolean;
    isClientAdmin(clientId: number): boolean;
    hasUserRole(currentClient: ClientProjectRelationship, userRole: ProjectRole.UserRoleEnum): boolean;
    hasProjectNature(projectId: number, projectNature: ProjectRole.ProjectNaturesEnum): boolean;
    hasAccessToClientProjects(clientId: number): boolean;
}
