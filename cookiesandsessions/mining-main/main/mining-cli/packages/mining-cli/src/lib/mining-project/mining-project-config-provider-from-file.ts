
import { access, readFile, writeFile } from 'fs-extra';
import { IOC } from 'iw-ioc';
import { asValue } from 'awilix';

import { resolve, join, dirname } from 'path';
import { MiningProjectConfig, MINING_PROJECT_CONFIG } from './mining-project-config';

export const PROJECT_FILE_NAME = '.mining-cli-project';

type Empty<T> = {
    [P in keyof T]-?: T[P] | undefined;
};

const EMPTY_PROJECT: Empty<MiningProjectConfig> = {
    server: undefined,
    clientId: undefined,
    projectId: undefined,
    rootPath: undefined
};

export async function createSampleProjectFile(projectDir: string): Promise<void> {
    const project: Partial<MiningProjectConfig> = {
        clientId: 1,
        projectId: 1,
        server: {
            url: 'http://localhost:8080',
            accessToken: ''
        }
    };
    await writeFile(resolve(projectDir, PROJECT_FILE_NAME), JSON.stringify(project, undefined, 2));
}

export async function loadProjectFile(projectDir: string): Promise<boolean> {
    const projectFile = await locateProjectFile(resolve(projectDir));
    if (projectFile === undefined) {
        IOC.getRootContainer().register(MINING_PROJECT_CONFIG, asValue(EMPTY_PROJECT));
        return false;
    }
    try {
        const fileContent = await readFile(projectFile, 'utf8');
        const project: MiningProjectConfig = JSON.parse(fileContent);
        project.rootPath = resolve(dirname(projectFile));
        IOC.getRootContainer().register(MINING_PROJECT_CONFIG, asValue(wrapProjectSoItSavesAutomatically(projectFile, project)));
        return true;
    } catch (err) {
        console.error('Unable to read project file', projectFile);
        console.error(err);
        process.exit(1);
    }
}

async function locateProjectFile(searchPath: string): Promise<string | undefined> {
    try {
        const filePath = join(searchPath, PROJECT_FILE_NAME);
        await access(filePath);
        return filePath;
    } catch (err) {
        const parentPath = resolve(searchPath, '..');
        if (parentPath === searchPath) {
            return undefined;
        } else {
            return locateProjectFile(parentPath);
        }
    }

}

/** Wrap project object so its contents are saved automatically to disk each time it is modified */
function wrapProjectSoItSavesAutomatically<T>(projectFile: string, project: T): T {
    const wrapped: any = {};

    Object.getOwnPropertyNames(EMPTY_PROJECT).forEach(propertyName => {
        Object.defineProperty(wrapped, propertyName, {
            enumerable: true,
            configurable: false,
            get: () => (project as any)[propertyName],
            set: (v) => {
                (project as any)[propertyName] = v;
                writeFile(projectFile, JSON.stringify(project, undefined, 2), 'utf8');
            }
        });
    });

    return wrapped;
}