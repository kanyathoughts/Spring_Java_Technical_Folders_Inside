/**
 * Defines the labels for the parent component that uses shared-annotation-editor in the App.
 */
export enum AnnotationEditor {
    WEB_ANNOTATION_EDITOR = 'web',
    CODEVIEWER_ANNOTATION_EDITOR = 'codeviewer',
    ECLIPSE_EDITOR ='eclipse'
}
/**
 * Defines response of shared-annotation-editor form.
 */
export enum FormResult {
    Saved,
    Canceled,
    Deleted,
    Disabled,
    Error
}
/**
 * Interface for FormResponse of shared-annottaion-editor.
 */
export interface FormResponse<T> {
    result: FormResult,
    returnedObject?: T,
}

export const EDITOR_WIDTH = {
    editorWIthAnnotations: '85vw',
    editorWithoutAnnotations: '34vw'
};
