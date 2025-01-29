const IS_LOCAL_SHOWCASE_MODE = 'configuration.view.isLocalShowcaseMode';
const IS_LOCAL_ON = 'TRUE';
const IS_LOCAL_OFF = 'FALSE';

export class LocalShowcaseMode {
    get isInLocalShowcaseMode(): boolean {
        return localStorage.getItem(IS_LOCAL_SHOWCASE_MODE) === IS_LOCAL_ON;
    }

    set isInLocalShowcaseMode(v: boolean) {
        localStorage.setItem(IS_LOCAL_SHOWCASE_MODE, v ? IS_LOCAL_ON : IS_LOCAL_OFF);
    }
}


