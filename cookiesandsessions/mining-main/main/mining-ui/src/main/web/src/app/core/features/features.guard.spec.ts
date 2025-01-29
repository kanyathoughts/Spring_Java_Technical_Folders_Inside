import { TestBed, waitForAsync } from '@angular/core/testing';
import { FeaturesGuard } from './features.guard';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { of } from 'rxjs';
import { Router, RouterStateSnapshot } from '@angular/router';

describe('FeaturesGuard', () => {
    let featuresGuard: FeaturesGuard;
    const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);
    const mockSnaphot: any = {
        data: {
            requiredFeature: 'testFeature'
        }
    };
    const mockInvalidSnaphot: any = {
        data: {}
    };
    const mockRouteSnapshot = jasmine.createSpyObj<RouterStateSnapshot>('RouterStateSnapshot', ['toString']);
    const routerSpy = jasmine.createSpyObj<Router>('Router', ['navigate']);

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            providers: [
                FeaturesGuard,
                {
                    provide: FeatureToggleService,
                    useValue: featureToggleServiceSpy
                },
                {
                  provide: Router,
                  useValue: routerSpy
                }
            ]
        });
    }));

    beforeEach(() => {
        featuresGuard = TestBed.inject(FeaturesGuard);
        featureToggleServiceSpy.isActive.and.returnValues(of(true), of(false));
    });

    it('should create the function', () => {
        expect(featuresGuard).toBeTruthy();
        expect(typeof featuresGuard.canActivate).toBe('function');
    });

    it('should return appropriate values from the service', () => {
        featuresGuard.canActivate(mockInvalidSnaphot).subscribe(response => {
            /* This is expected to be true as some data do not require features */
            expect(response).toBeTruthy();
        });

        /* The following return values are related to the return values of the feature toggle service spy */
        featuresGuard.canActivate(mockSnaphot).subscribe(response => {
            expect(response).toBeTruthy();
        });

        featuresGuard.canActivate(mockSnaphot).subscribe(response => {
            expect(response).toBeFalsy();
        });
    });
});
