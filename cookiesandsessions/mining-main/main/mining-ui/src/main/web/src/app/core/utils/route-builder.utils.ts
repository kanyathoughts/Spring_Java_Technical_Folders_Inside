import { EntityId } from '@innowake/mining-api-angular-client';

/**
 * Utility class to build Mining routes
 */
export class RouteBuilder {
    /**
     * Build a route related to a module
     * @param projectId project id of the module
     * @param module linkHash or id of the module
     * @param pathEnd end of the route to build
     * @returns target route string
     */
    static buildModuleRoute(projectId: number, module: EntityId, pathEnd: string): string {
        return '/project-' + projectId + '/module-' + module + '/' + pathEnd;
    }

    /**
     * Build a route related to a project
     * @param projectId project id for the target route
     * @param pathEnd end of the route to build
     * @returns target route string
     */
    static buildProjectRoute(projectId: number, pathEnd: string): string {
        return '/project-' + projectId + '/' + pathEnd;
    }

    /**
     * Build a route related to a client
     * @param clientId client id for the target route
     * @param pathEnd end of the route to build
     * @returns target route string
     */
    static buildClientRoute(clientId: number, pathEnd: string): string {
        return '/client-' + clientId + '/' + pathEnd;
    }

    /**
     * Generates the uri for the External Routes
     * @param projectId project id for the target route
     * @param pathEnd end of the route to build
     * @returns target route string
     */
    static buildCustomTableRoute(projectId: number, pathEnd: string): string {
        return '/project-' + projectId + '/custom-table/' + pathEnd;
    }

    /**
     * Generates the uri for the External Routes
     * @param projectId project id for the target route
     * @param pathEnd end of the route to build
     * @returns target route string
     */
    static buildExternalRoute(projectId: number, pathEnd: string): string {
        return '/project-' + projectId + '/external/' + pathEnd;
    }

    /**
     * Build a route related to the details of a module
     * @param projectId project id of the module
     * @param module linkHash or id of the module
     * @param pathEnd end of the route to build
     * @returns target route string
     */
    static buildDetailsRoute(projectId: number, module: EntityId, pathEnd: string): string {
        return '/project-' + projectId + '/module-' + module + '/details/' + pathEnd;
    }
}
