import { environment } from '@env/environment';

export const  getBasePath =(): string => {
    if (environment.serverUrl) {
        return environment.serverUrl;
    }
    const path = location.origin + location.pathname;
    return path.endsWith('/') ? path.slice(0, -1) : path;
};
