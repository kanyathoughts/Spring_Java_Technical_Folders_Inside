import { AnnotationPojo } from '@innowake/mining-api-angular-client';

/**
 * The utility class which contains the static methods for getting
 * previous and next state.
 */
export class StateChange {

    /**
     * Returns the array of next states for the current state.
     * @param state current state.
     */
    public static getNext(state: AnnotationPojo.StateEnum): AnnotationPojo.StateEnum[] {
        switch (state) {
            case AnnotationPojo.StateEnum.CANDIDATE: {
                return [AnnotationPojo.StateEnum.IN_ANALYSIS];
            }
            case AnnotationPojo.StateEnum.IN_ANALYSIS: {
                return [AnnotationPojo.StateEnum.FOR_REVIEW, AnnotationPojo.StateEnum.REJECTED];
            }
            case AnnotationPojo.StateEnum.FOR_REVIEW: {
                return [AnnotationPojo.StateEnum.APPROVED, AnnotationPojo.StateEnum.INVALID];
            }
            case AnnotationPojo.StateEnum.INVALID:
            case AnnotationPojo.StateEnum.REJECTED:
            case AnnotationPojo.StateEnum.APPROVED:
                return [];
            default:
                    throw new Error('Undefined state ' + state + ' for retrieving the next states');
        }
    }

    /**
     * Returns the previous state for the current state.
     * @param state current state.
     */
    public static getPrevious(state: AnnotationPojo.StateEnum): AnnotationPojo.StateEnum {
        switch (state) {
            case AnnotationPojo.StateEnum.CANDIDATE:
                return;
            case AnnotationPojo.StateEnum.IN_ANALYSIS:
                return AnnotationPojo.StateEnum.CANDIDATE;
            case AnnotationPojo.StateEnum.REJECTED:
            case AnnotationPojo.StateEnum.FOR_REVIEW: {
                return AnnotationPojo.StateEnum.IN_ANALYSIS;
            }
            case AnnotationPojo.StateEnum.INVALID:
            case AnnotationPojo.StateEnum.APPROVED: {
                return AnnotationPojo.StateEnum.FOR_REVIEW;
            }
            default:
                 throw new Error('Undefined state ' + state + ' for retrieving the previous state');
        }
    }
}

