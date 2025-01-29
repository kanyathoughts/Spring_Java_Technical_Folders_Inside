import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { ErrorComponent } from './error.component';
import { ActivatedRoute, convertToParamMap, Router } from '@angular/router';
import { of } from 'rxjs';
import { LicenseExpiryService } from '@app/core/license/license-expiry.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { TranslateService, TranslateModule } from '@ngx-translate/core';

describe('ErrorComponent', () => {
    let fixture: ComponentFixture<ErrorComponent>;
    let component: ErrorComponent;
    const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>('NzNotificationService', ['create', 'warning']);
    const licenseExpirySpy: jasmine.SpyObj<LicenseExpiryService> = jasmine.createSpyObj<LicenseExpiryService>('LicenseExpiryService', ['checkExpiry']);

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            providers: [
                ErrorComponent,
                { provide: LicenseExpiryService, useValue: licenseExpirySpy},
                { provide: ActivatedRoute, useValue: { paramMap: of(convertToParamMap({ lincenseId: 'licenseExpired' })) } },
                { provide: NzNotificationService, useValue: notificationSpy },
                TranslateService
            ],
            imports: [
                HttpClientTestingModule,
                TranslateModule.forRoot({}),
            ],
            declarations: [ErrorComponent]
        }).compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(ErrorComponent);
        component = fixture.componentInstance;
        component.licenseExpiryService.expiryDate = new Date().toDateString();
        fixture.detectChanges();
    });

    it('should create error component', () => {
        expect(component).toBeTruthy();
    });
});
