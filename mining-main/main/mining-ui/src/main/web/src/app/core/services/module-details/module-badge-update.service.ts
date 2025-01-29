import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';

@Injectable({
    providedIn: 'root'
})
export class ModuleBadgeUpdateService {
    annotationDataDictionarySub: Subject<{operation: string, count: number}> = new Subject<{operation: string, count: number}>();
    moduleToBeReviewed: Subject<string> = new Subject<string>();

    /**
     * method to update the badge count for annotation , data dictionary from other component
     * @param  removeAnnotationDataDictionary - operation decided which one to update annotation or data dictionary
     */
    updateAnnotationDataDictionary(updateBadgeCountObject: {operation: string, count: number}): void {
        this.annotationDataDictionarySub.next(updateBadgeCountObject);
    }

    /**
     * method to update the badages count whenever there is deletion and update in the annotation , data dictionary or dependency
     * @returns Subject string
     */
    getAnnotationDataDictionary(): Subject<{operation: string, count: number}> {
        return this.annotationDataDictionarySub;
    }

    /**
     * method to update the Warning badge for modules component
     * @returns Subject string
     */
    getModuleToBeReviewed(): Subject<string> {
        return this.moduleToBeReviewed;
    }

    /**
     * method to pass the Warning badge for modules component
     * @param moduleUpdated The badge name to check the condition
     */
    updateModuleToBeReviewed(moduleUpdated: string): void {
        this.moduleToBeReviewed.next(moduleUpdated);
    }
}
