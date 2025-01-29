import { TestBed, ComponentFixture, waitForAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TranslateModule } from '@ngx-translate/core';
import { CoreModule, I18nService } from '@app/core';
import { AppComponent } from './app.component';
import { NzAlertModule } from 'ng-zorro-antd/alert';
import { LicenseExpiryService } from './core/license/license-expiry.service';
import { of } from 'rxjs';

describe('AppComponent', () => {
  let fixture: ComponentFixture<AppComponent>;
  let component: AppComponent;
  const i18nServiceSpy: jasmine.SpyObj<I18nService> = jasmine.createSpyObj<I18nService>('I18nService', ['init', 'destroy']);
  const licenseExpirySpy: jasmine.SpyObj<LicenseExpiryService> = jasmine.createSpyObj<LicenseExpiryService>('LicenseExpiryService', ['checkExpiry']);
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [RouterTestingModule.withRoutes([]), TranslateModule.forRoot(), CoreModule, NzAlertModule],
      declarations: [AppComponent],
      providers: [
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: LicenseExpiryService, useValue: licenseExpirySpy }
      ]
    }).compileComponents();
    licenseExpirySpy.checkExpiry.and.returnValue(of({days: 365}) as any);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(AppComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create the app', waitForAsync(() => {
    component.ngOnInit();
    expect(component).toBeTruthy();
  }));

  it('should destroy app', () => {
    component.ngOnDestroy();
    expect(i18nServiceSpy.destroy).toHaveBeenCalled();
  });
});
