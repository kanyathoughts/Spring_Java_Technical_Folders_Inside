import { AnnotationPojo, EntityId } from '@innowake/mining-api-angular-client';
import { CodeAnnotationEditorComponent } from './code-annotation/code-annotation-editor.component';
/**
 * Type Interface for the Annotation position array we are using in the {MiningModuleAnnotationEditorComponent}
 * for determining the position of the Annotation in th sidebar.
 */
export interface AnnotationPosition {
    id: number;
    top: number;
    initialTop: number;
}

/**
 * Type Interface for properties need to be defined for the Code Annotation custom element we have created for
 * displaying the Annotation in the sidebar.
 */
export interface AnnotationElementData {
    annotation: AnnotationPojo;
    borderColor?: string;
    typeLabel?: string;
    stateLabel?: string;
    moduleId: EntityId;
    moduleName?: string;
    projectId: number;
    viewZoneId?: string;
    modulePath: string;
    isBusinessVariable?: boolean;
    functionalBlockDetails?: FunctionalBlockDetails;
    isEdit?: boolean;
    canCopyAnnotation?: boolean;
}

/**
 * Type Interface for properties need to be defined for the Code Annotation for
 * Add/Update/Deleting the Annotation.
 */
export interface AnnotationDetails {
    initialDecorators: string[],
    lineNumberStart: number,
    lineNumberEnd: number,
    codeAnnotationComponent: CodeAnnotationEditorComponent
}

export interface FunctionalBlockDetails {
    listOfFunctionalGroupNames: string,
    functionalGroupsAssociatedToAnnotation: any
}

export interface AnnotationsFunctionalGroups {
    uid?: string;
    name?: string;
    description?: string;
    generatedFrom?: {
        annotationId?: number;
    };
    children?: {
        content?: AnnotationsFunctionalGroups[];
    };
}
