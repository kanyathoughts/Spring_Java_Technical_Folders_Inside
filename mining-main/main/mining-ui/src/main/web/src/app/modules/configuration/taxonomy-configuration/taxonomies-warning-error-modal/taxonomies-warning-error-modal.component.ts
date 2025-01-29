import { Component, Input, OnInit } from '@angular/core';
import { TaxonomyModalService } from '@app/modules/configuration/taxonomy-configuration/taxonomy-modal.service';
import { BaseFormModalComponent } from '@app/shared/components/base-form-modal/base-form-modal.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { WarningCancel } from '../taxonomy-reporting.model';

@Component({
  selector: 'taxonomies-error-warning-modal',
  templateUrl: './taxonomies-warning-error-modal.component.html',
})
export class TaxonomiesErrorWarningModalComponent extends BaseFormModalComponent implements OnInit {
  @Input() closeImport: () => void;
  warningMessage: any;

  constructor(
    public modal: NzModalRef,
    public messageService: NzMessageService,
    private taxonomyModalService: TaxonomyModalService
  ) {
    super(modal, messageService);
  }

  ngOnInit(): void {
    this.warningMessage = this.taxonomyModalService.getValidationResponse();
  }
  /**
   * on click of cancel for warning pop up
   */
  onCancelWarning(): void {
    this.taxonomyModalService.triggerCancelWarningSubject(WarningCancel.WARNING_CANCEL);
    this.closeImport();
  }
  /**
   * on click of continue for warning pop
   */
  onContinueWarning(): void {
    this.taxonomyModalService.triggerCancelWarningSubject(WarningCancel.WARNING_CONTINUE);
  }
  /**
   * method to download warning logs
   */
  onDownloadWarningLog(): void {
    this.taxonomyModalService.downloadLog();
  }
}
