import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NzModalRef } from 'ng-zorro-antd/modal';

import { ConfirmDeleteModalComponent, DELETE_MODAL_CONFIRMED } from './confirm-delete-modal.component';

describe('ConfirmDeleteModalComponent', () => {
  let component: ConfirmDeleteModalComponent;
  let fixture: ComponentFixture<ConfirmDeleteModalComponent>;
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ConfirmDeleteModalComponent ],
      providers: [
        { provide: NzModalRef, useValue: nzModalRefSpy },
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConfirmDeleteModalComponent);
    component = fixture.componentInstance;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should close modal on cancel', () => {
    component.onCancel();
    expect(nzModalRefSpy.close).toHaveBeenCalledWith('cancel');
  });

  it('should close modal on confirm', () => {
    component.onConfirm();
    expect(nzModalRefSpy.close).toHaveBeenCalledWith(DELETE_MODAL_CONFIRMED);
  });
});
