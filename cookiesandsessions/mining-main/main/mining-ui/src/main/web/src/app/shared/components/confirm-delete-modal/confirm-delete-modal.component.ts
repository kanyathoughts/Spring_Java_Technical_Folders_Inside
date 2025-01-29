import { Component, Input, TemplateRef } from '@angular/core';
import { NzModalRef } from 'ng-zorro-antd/modal';

export const DELETE_MODAL_CONFIRMED = 'confirmed';

@Component({
  selector: 'mn-confirm-delete-modal',
  templateUrl: './confirm-delete-modal.component.html'
})
export class ConfirmDeleteModalComponent {

  @Input() modalContent: TemplateRef<any>;

  @Input() confirmationText: string;

  @Input() confirmationButtonText: string;

  @Input() isConfirmationReq: boolean;

  agreed = false;

  constructor(
    private modal: NzModalRef,
  ) { }

  /**
   * Handles user click on the cancel btn
   */
  onCancel(): void {
    this.modal.close('cancel');
  }

  /**
   * Handles user click on the delete btn
   */
  onConfirm(): void {
    this.modal.close(DELETE_MODAL_CONFIRMED);
  }
}
