/**
 * Custom DynamicDialogConfig to add a call back method post save
 */
export declare class CustomDynamicDialogConfig {
    data?: any;
    header?: string;
    width?: string;
    closeOnEscape?: boolean;
    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    saveCallBack(updatedParams?: any): void;
    deleteCallBack?(): void;
    openInEclipseCallback?(): void;
}
