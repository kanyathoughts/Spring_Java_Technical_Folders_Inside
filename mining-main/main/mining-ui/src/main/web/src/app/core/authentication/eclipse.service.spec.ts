import { TestBed } from '@angular/core/testing';
import { EclipseService } from './eclipse.service';

declare global {
    interface Window {
        java__callback__authenticate_eclipse_view(): Promise<string>;
        java__callback__authenticate_eclipse_view_spy: jasmine.SpyObj<any>;
    }
}

describe('EclipseService', () => {
    let service: EclipseService;
    window.java__callback__authenticate_eclipse_view_spy = jasmine.createSpyObj('javacallback', ['spyMethod']);

    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [],
        providers: []
      });
      window.java__callback__authenticate_eclipse_view = 
        () => {
            window.java__callback__authenticate_eclipse_view_spy.spyMethod();
            return Promise.resolve('dummy_access_token');
        };
      service = TestBed.inject(EclipseService);
    });

    afterAll(() => {
        window.java__callback__authenticate_eclipse_view_spy = null;
        window.java__callback__authenticate_eclipse_view = null;
    });

    it('should set isEclipseView property to true if ui is in eclipse browser', () => {
        expect(service).toBeTruthy();
        expect(service.isEclipseView).toBeTrue();
    });

    it('should set isEclipseView property to false if ui is not in eclipse browser', () => {
        window.java__callback__authenticate_eclipse_view = undefined;
        const eclipseService = new EclipseService();
        expect(eclipseService.isEclipseView).toBeFalse();
    });

    it('should get access token from java callback if in eclipse view', async() => {
        service.isEclipseView = true;
        expect(await service.getAccessToken()).toEqual('dummy_access_token');
    });

    it('should return null token if not in eclipse view', async() => {
        service.isEclipseView = false;
        expect(await service.getAccessToken()).toBeFalsy();
    });

    it('should return the access token as null if error occurs while execution of callback', async() => {
        window.java__callback__authenticate_eclipse_view = 
        () => {
            window.java__callback__authenticate_eclipse_view_spy.spyMethod();
            throw new Error('unknown error occurred while execution of callback');
        };
        const eclipseService = new EclipseService();
        expect(await eclipseService.getAccessToken()).toBeFalsy();
    });
});
