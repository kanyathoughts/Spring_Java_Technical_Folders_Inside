import { EntityId } from '@innowake/mining-api-angular-client';

/**
 * Interface for modal window parameters
 */
export interface ModalWindowParams {
    id?: EntityId;
    key?: string;
    value?: any;
    type?: FormTypes;
    options?: any[];
    label?: string;
    class?: string;
    deleteFeature?: boolean;
    openInEclipseFeature?: boolean;
}

export enum FormTypes {
    COMBO_BOX = 'COMBO_BOX',
    TEXT = 'TEXT'
}
