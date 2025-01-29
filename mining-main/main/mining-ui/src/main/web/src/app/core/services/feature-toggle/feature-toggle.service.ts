import { Injectable } from '@angular/core';
import { Logger } from '@app/core';
import { Observable, Subscriber } from 'rxjs';
import { Feature, FeatureControllerService } from '@innowake/mining-api-angular-client';

const logger = new Logger('FeatureToggleService');

/**
 * Service class for Feature Toggling.
 */
@Injectable({
    providedIn: 'root'
})
export class FeatureToggleService {
    constructor(private featureControllerService: FeatureControllerService) {}

    /**
     * Method to check if a given feature is active or not.
     * @param featureId ID of the feature.
     */
    isActive(featureId: string): Observable<boolean> {
        const source: Observable<boolean> = new Observable((subscriber: Subscriber<boolean>) => {
            this.featureControllerService.findFeatureById(featureId).subscribe((response: Feature) => {
                subscriber.next(response.enabled);
                subscriber.complete();
            }, () => {
                logger.error('Error while fetching feature for ID: ', featureId);
                subscriber.next(false);
                subscriber.complete();
            });
        });
        return source;
    }
}
