import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot } from '@angular/router';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { Observable, of } from 'rxjs';

/**
 * A guard to ensure that the user is accessing a feature that exists and is active.
 */
@Injectable({
    providedIn: 'root'
})
export class FeaturesGuard  {

    constructor(private features: FeatureToggleService) { }

    /**
     * CanActivate checks if the user is trying to access feature
     * that exists and is active or not.
     * @param route The activated route of the current request.
     */
    canActivate(route: ActivatedRouteSnapshot): Observable<boolean> {
        if (route.data.requiredFeature) {
            return this.features.isActive(`${route.data.requiredFeature}`);
        }
        return of(true);
    }
}
