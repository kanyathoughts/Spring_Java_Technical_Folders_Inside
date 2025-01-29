import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { BaseFormModalComponent } from '@app/shared/components/base-form-modal/base-form-modal.component';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { Subscription, throwError } from 'rxjs';
import { TaxonomiesErrorWarningModalComponent } from '../taxonomies-warning-error-modal/taxonomies-warning-error-modal.component';
import { TaxonomyModalService } from '../taxonomy-modal.service';
import { WarningCancel } from '../taxonomy-reporting.model';
import { last, switchMap } from 'rxjs/operators';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { JobControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';

const taxonomyStartImportKey = 'taxonomyReportingComponent.startImport';

@Component({
  selector: 'taxonomies-modal',
  templateUrl: './taxonomies-modal.component.html',
})

export class TaxonomiesModalComponent extends BaseFormModalComponent implements OnInit, OnDestroy {
  @Input() downloadTemplate: () => void;
  fileList: File[] = [];
  projectId: number;
  showMessage = false;
  toggleCancelBtn = true;
  toggleImportBtn = true;
  loadingStateValidation = false;
  toggleContentSection = true;
  disableCancel = false;
  taxonomyImportLink: string;
  alertType: 'error' | 'warning' | 'success' | 'info';
  validationBtnTxt: string;
  errorMessage: string;
  importSubscription: Subscription;
  importWarningSubscription: Subscription;
  validationCancellationSubscription: Subscription;
  showAll: boolean;
  showWarning = false;
  showPercent = 0;
  constructor(
    private jobController: JobControllerService,
    private jobManagerService: JobManagerService,
    public modal: NzModalRef,
    private modalService: NzModalService,
    public messageService: NzMessageService,
    private taxonomyModalService: TaxonomyModalService,
    private translateService: TranslateService,
    private taxonomyControllerService: TaxonomyControllerService
  ) {
    super(modal, messageService);
  }

  beforeUpload = (file: File): boolean => {
    this.fileList = this.fileList.concat(file);
    return false;
  };

  ngOnInit(): void {
    this.showAll = true;
    this.taxonomyImportLink = this.taxonomyModalService.taxonomyImportLink;
    this.projectId = this.taxonomyModalService.getProjectId();
    this.validationBtnTxt = this.translateService.instant(taxonomyStartImportKey);
    this.importWarningSubscription = this.taxonomyModalService.cancelWarningSubject.subscribe((response: string) => {
      if (response === WarningCancel.WARNING_CANCEL) {
        this.onValidationCancel();
        this.toggleContentSection = true;
      }
      if (response === WarningCancel.WARNING_CONTINUE) {
        this.toggleImportBtn = false;
        this.toggleContentSection = true;
        this.validationBtnTxt = this.translateService.instant('taxonomyReportingComponent.import');
        this.onImportContinue();
      }
    });

    this.validationCancellationSubscription = this.taxonomyModalService.taxonomyModalEventEmitter.subscribe(
      (response: string) => {
        const isValidationStart = this.taxonomyModalService.isImportValidationStarted();
        if (response === WarningCancel.POP_UP_CLOSED && this.importSubscription && isValidationStart) {
          this.importSubscription.unsubscribe();
          this.taxonomyModalService.setStartValidation(false);
          this.messageService.warning(`${this.translateService.instant('taxonomyReportingComponent.importCancelled')}`);
        }
      }
    );
  }

  /**
   * method to validate and import file
   */
  onValidation(): void {
    this.toggleContentSection = false;
    this.showPercent = 0;
    this.validationBtnTxt = this.translateService.instant('taxonomyReportingComponent.importBtnValidationTxt');
    this.toggleCancelBtn = ! this.toggleCancelBtn;
    this.loadingStateValidation = ! this.loadingStateValidation;
    this.showWarning = false;
    let validationJobId: string;
    this.taxonomyModalService.setStartValidation(true);
    this.showAll = false;
    this.modal.updateConfig({
      nzClosable: true,
      nzKeyboard: true,
      nzMaskClosable: false,
    });
    this.importSubscription = this.taxonomyControllerService.validateImportTaxonomy(this.projectId, this.fileList[0])
      .pipe(
        switchMap((response) => {
          if (response['overallResult'] === 'ERROR') {
            return throwError(response);
          } else {
            validationJobId = response + '';
            return this.jobManagerService.register({
              jobId: validationJobId, foreground: true, cancellable: true,
            }).status$;
          }
        }), last(),
        switchMap(() => this.jobController.getJobResult(validationJobId)))
      .subscribe((response: object) => {
        this.handleTaxonomyImportValidation(response);
      }, (error: object) => {
        this.loadingStateValidation = ! this.loadingStateValidation;
        this.handleErrorCase(error);
      });
  }

  /**
   * cancelling Validation operation
   */
  onValidationCancel(): void {
    this.taxonomyModalService.setStartValidation(false);
    this.toggleContentSection=true;
    this.showAll = true;
    if (this.importSubscription) {
      this.alertType = null; // for making Import button enabled
      this.loadingStateValidation = false;
      this.toggleCancelBtn = ! this.toggleCancelBtn; // for cancel button to toggle
      this.validationBtnTxt = this.translateService.instant(taxonomyStartImportKey);
      this.importSubscription.unsubscribe();
      this.showMessage = false;
      this.messageService.warning(`${this.translateService.instant('taxonomyReportingComponent.validationCancel')}`);
    } else {
      this.onCancel();
    }
  }

  /**
   * this works for stops button and validating the property by changing according to requirement.
   */
  onStop(): void {
    this.importSubscription.unsubscribe();
    this.showWarning = true;
    this.showAll = true;
    this.toggleImportBtn = true;
    this.loadingStateValidation = false;
    this.disableCancel = false;
    this.toggleCancelBtn = true;
    this.toggleContentSection = true;
    this.validationBtnTxt = this.translateService.instant(taxonomyStartImportKey);
    this.messageService.warning(`${this.translateService.instant('taxonomyReportingComponent.importStopped')}`);
    this.modal.updateConfig({
      nzClosable: true,
      nzKeyboard: true,
      nzMaskClosable: false,
    });
  }


  /**
   * method to download error logs
   */
  onDownloadLog(): void {
    this.taxonomyModalService.downloadLog();
  }
  /**
   * method to disabling enabling button based on the condition
   * @returns boolean
   */
  disableImportBtn(): boolean {
    return (this.fileList.length === 0 && this.toggleImportBtn) || this.alertType === 'error';
  }

  ngOnDestroy(): void {
    if (this.importWarningSubscription) {
      this.importWarningSubscription.unsubscribe();
    }
  }

  /**
   * Convert import validation result to message String.
   * @param response Result of the import validation.
   * @param msgLimit Maximum number of messages to print for each marker type.
   * @returns Formatted multi-line message.
   */
  private responseToMessage(response: any, msgLimit: number): string {
    let msg = 'Result: ' + response.overallResult;

    const lines = new Map<string, { cnt: number, str: string }>();
    for (const marker of response.markers) {
      let msgs = lines.get(marker.markerType as string);
      if ( ! msgs) {
        msgs = { cnt: 0, str: '' };
        lines.set(marker.markerType as string, msgs);
      }
      if (msgs.cnt++ < msgLimit) {
        msgs.str += '\nLine ' + marker.lineNumber + ': ' + marker.markerText;
      }
    }

    const msgKeys = new Set<string>();
    for (const prioKey of ['ERROR', 'WARNING']) {
      if (lines.has(prioKey)) {
        msgKeys.add(prioKey);
      }
    }
    for (const msgKey of lines.keys()) {
      msgKeys.add(msgKey);
    }

    for (const msgKey of msgKeys) {
      const msgs = lines.get(msgKey);
      msg += '\n\n' + msgKey + ' messages:' + msgs.str;
      if (msgs.cnt > msgLimit) {
        msg += '\n... +' + (msgs.cnt - msgLimit);
      }
    }

    return msg;
  }



  private handleErrorCase(response: object) {
    this.toggleCancelBtn = ! this.toggleCancelBtn;
    this.taxonomyModalService.setValidationResponse(response);
    this.alertType = 'error';
    this.showMessage = true;
    this.validationBtnTxt = this.translateService.instant(taxonomyStartImportKey);
    this.errorMessage = this.responseToMessage(response, 100);
    this.showAll = true;
  }

  private handleTaxonomyImportValidation(response: object): void {
    this.jobManagerService.percent.subscribe(res => {
      this.showPercent = this.showPercent + res;
    });
    const { overallResult, markers } = response as any;
    this.loadingStateValidation = ! this.loadingStateValidation;
    const isValidationStart = this.taxonomyModalService.isImportValidationStarted();
    if (isValidationStart) {
      if (overallResult === 'WARNING' && markers && markers.length > 0) {
        this.taxonomyModalService.setValidationResponse(response);
        const modal = this.modalService.create<TaxonomiesErrorWarningModalComponent>({
          nzTitle: this.translateService.instant('taxonomyReportingComponent.importAssignmentsTitleWarning'),
          nzMaskClosable: false,
          nzKeyboard: true,
          nzAutofocus: null,
          nzClosable: true,
          nzContent: TaxonomiesErrorWarningModalComponent
        });
        const instance = modal.getContentComponent();
        instance.closeImport = this.onCancel.bind(this);
        this.toggleContentSection = false;
      } else if (overallResult === 'ERROR' && markers && markers.length > 0) {
        this.handleErrorCase(response);
      } else {
        this.showMessage = false;
        this.validationBtnTxt = this.translateService.instant('taxonomyReportingComponent.import');
        this.toggleImportBtn = false;
        this.onImportContinue();
      }
    }
  }

  /**
   * method to continue import
   */
  private onImportContinue(): void {
    this.taxonomyModalService.setStartValidation(false);
    this.loadingStateValidation = true;
    this.disableCancel = ! this.disableCancel;
    this.modal.updateConfig({
      nzClosable: false,
      nzKeyboard: false,
      nzMaskClosable: false,
    });
    this.taxonomyModalService.triggerCancelWarningSubject(WarningCancel.ON_IMPORT);
    this.importSubscription = this.taxonomyControllerService.importTaxonomy(this.projectId, this.fileList[0])
      .pipe(switchMap((jobId: string[]) => this.jobManagerService.register({
        jobId: jobId.toString(), foreground: true, cancellable: true
      }).status$),
        last())
      .subscribe(() => {
        this.taxonomyModalService.triggerCancelWarningSubject(WarningCancel.TAXONOMY_IMPORTED);
        this.loadingStateValidation = false;
        this.disableCancel = ! this.disableCancel;
        this.onCancel();
      });
  }
}
