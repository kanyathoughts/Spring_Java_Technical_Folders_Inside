import { TestBed, waitForAsync } from '@angular/core/testing';
import { FeatureToggleService } from './feature-toggle.service';
import { of, throwError } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Feature, FeatureControllerService } from '@innowake/mining-api-angular-client';

describe('FeatureToggleService', () => {
    const enabledFeature: Feature = {
        customProperties: {},
        description: 'enabled_description',
        enabled: true,
        id: Feature.IdEnum.INCREMENTAL_SCAN,
        recordId: 'enabled_record_id'
    };
    const disbaledFeature: Feature = {
        customProperties: {},
        description: 'disabled_description',
        enabled: false,
        id: Feature.IdEnum.CODE_VIEWER_HYPERLINKING,
        recordId: 'disabled_record_id'
    };
    const featureControllerServiceSpy: jasmine.SpyObj<FeatureControllerService> = jasmine.createSpyObj<FeatureControllerService>('FeatureControllerService',
                                                                                                                                ['findFeatureById']);
    let featureToggleService: FeatureToggleService;
    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            imports: [ HttpClientTestingModule ],
            providers: [
                FeatureToggleService,
                { provide: FeatureControllerService, useValue: featureControllerServiceSpy }
            ]
        });
    }));

    beforeEach(() => {
        featureToggleService = TestBed.inject(FeatureToggleService);
        featureControllerServiceSpy.findFeatureById.and.returnValues(of(enabledFeature as any),
                                                                        of(disbaledFeature as any),
                                                                        of(disbaledFeature as any),
                                                                        throwError(new Error('test_error')));
    });

    it('should return values', async() => {
        expect(featureToggleService).toBeTruthy();

        featureToggleService.isActive('discoveryIntegration').subscribe(result => {
            expect(result).toBeTruthy();
        });

        featureToggleService.isActive('storeModuleDescription').subscribe(result => {
            expect(result).toBeFalsy();
        });

        featureToggleService.isActive('eclipseDeepLink').subscribe(result => {
            expect(result).toBeFalsy();
        });

        featureToggleService.isActive('invalidFeatureId').subscribe(result => {
            expect(result).toBeFalsy();
        });
    });
});
