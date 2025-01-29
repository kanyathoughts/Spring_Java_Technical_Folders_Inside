import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router } from '@angular/router';
import { Observable, of } from 'rxjs';
import { LicenseExpiryService } from './license-expiry.service';

/**
 * A guard to ensure that the user is accessing a feature that exists and is active.
 */
@Injectable({
    providedIn: 'root'
})
export class LicenseGuard  {

    constructor(private licenseExpiryService: LicenseExpiryService, private router: Router) {
    }

    /**
     * CanActivate checks if the user is trying to access feature
     * that exists and is active or not.
     * @param route The activated route of the current request.
     * @returns observable boolean value for error page
     */
    canActivate(route: ActivatedRouteSnapshot): Observable<boolean> {
        this.licenseExpiryService.isLicenseExpired.subscribe((result: boolean) => {
            if (result) {
                if (route.data.title === 'Error') {
                    return of(true);
                } else {
                    void this.router.navigateByUrl('/error/licenseExpired');
                    return of(false);
                }
            } else {
                return of(true);
            }
        });
        return of(true);
    }
}
