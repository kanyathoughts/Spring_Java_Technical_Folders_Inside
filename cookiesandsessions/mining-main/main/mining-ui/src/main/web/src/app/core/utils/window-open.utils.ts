import { EntityId } from '@innowake/mining-api-angular-client';
import { RouteBuilder } from './route-builder.utils';

/**
 * method for opening in new tab
 * @param  projectId current project id
 * @param  moduleId current module id
 * @param  path where routing will be done
 * @param  window global window object
 * @returns route that has to be navigated
 */
export const openInNewTab = (projectId: number, module: EntityId, path: string, window: Window): any => {
    const pathName: string = location.pathname;
    let route: string;
    if (module === null) {
        route = RouteBuilder.buildProjectRoute(projectId, path);
    } else {
        route = RouteBuilder.buildModuleRoute(projectId, module, path);
    }
    const fullPath = (pathName.length !== 1) ? pathName + '#' + route : '/#' + route;
    return window.open(fullPath);
};
