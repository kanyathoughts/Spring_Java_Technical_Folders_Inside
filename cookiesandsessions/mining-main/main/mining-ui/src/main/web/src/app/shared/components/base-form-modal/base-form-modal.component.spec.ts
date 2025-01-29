import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import { BaseFormModalComponent } from './base-form-modal.component';

describe('BaseFormModalComponent', () => {
    let component: BaseFormModalComponent;
    let fixture: ComponentFixture<BaseFormModalComponent>;
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
    const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [BaseFormModalComponent],
            providers: [
                { provide: NzMessageService, useValue: messageServiceSpy},
                { provide: NzModalRef, useValue: nzModalRefSpy }
            ],
            imports: [
                NzModalModule,
                NzMessageModule,
                BrowserAnimationsModule
            ]
        }).compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(BaseFormModalComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should close modal on cancel', () => {
        component.onCancel();
        expect(nzModalRefSpy.close).toHaveBeenCalledWith('cancel');
    });

    it('should close modal with message', () => {
        component.closeModal('success', 'Test message', true);
        expect(nzModalRefSpy.close).toHaveBeenCalledWith(true);
        expect(messageServiceSpy.create).toHaveBeenCalledWith('success', 'Test message');
    });
});
