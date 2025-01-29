import { Component } from '@angular/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';

@Component({
    selector: 'mn-base-form-modal',
    template: ''
})
export class BaseFormModalComponent {

    mode: 'edit' | 'create' = 'create';

    constructor(
        public modal: NzModalRef,
        public messageService: NzMessageService,
    ) {
    }

    /**
     * Method called by the cancel button
     */
    onCancel(): void {
        this.modal.close('cancel');
    }

    /**
     * Close the modal with a message to give a feedback on the result of the form
     * @param result output of the form, defines the type of message displayed
     * @param message text to display
     */
    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    closeModal(result: 'success' | 'error' | 'warning', message: string, data: any): void {
        this.messageService.create(result, message);
        this.modal.close(data);
    }
}
