import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { BaseFormModalComponent } from '@app/shared/components/base-form-modal/base-form-modal.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { Subscription, from } from 'rxjs';
import { last, map, switchMap } from 'rxjs/operators';
import { TranslateService } from '@ngx-translate/core';
import { Logger } from '@app/core';
import { AnnotationControllerService, JobControllerService } from '@innowake/mining-api-angular-client';
const log = new Logger('AnnotationsImportModalComponent');

export interface AnnotationImportResult {
  errors?: string[];
  totalLines?: number;
}

@Component({
  selector: 'app-annotations-import-modal',
  templateUrl: './annotations-import-modal.component.html'
})
export class AnnotationsImportModalComponent extends BaseFormModalComponent implements OnInit, OnDestroy {

  @Input() projectId: number;
  showPercent = 0;
  showInitialContent = true;
  loading = false;
  finished = false;
  errorMessage: string;
  importResult: AnnotationImportResult;
  fileList: File[] = [];
  importSubscription: Subscription;
  titleIconType = 'loading';
  alertType: 'error' | 'warning' | 'success';
  finalMessage: string;
  tileIconClass = 'annotations-modal__load-icon';

  constructor(private jobController: JobControllerService,
    private jobManagerService: JobManagerService,
    public modal: NzModalRef,
    public messageService: NzMessageService,
    private annotationController: AnnotationControllerService,
    private translateService: TranslateService) {
    super(modal, messageService);
  }
  ngOnDestroy(): void {
    if (this.importSubscription) {
      this.importSubscription.unsubscribe();
    }
  }

  ngOnInit(): void {
    this.showInitialContent = true;
  }

  /**
   * method to  import file
   */
  onImport(): void {
    this.showInitialContent = false;
    this.loading = true;
    let importJobId: string;
    this.jobManagerService.percent.subscribe(res => {
      this.showPercent = res;
    });
    this.importSubscription = this.annotationController.importAnnotations(this.projectId, this.fileList[0])
      .pipe(switchMap(jobId => {
        importJobId = jobId as unknown as string;
        return this.jobManagerService.register({
          jobId: jobId as unknown as string, foreground: true, cancellable: true
        }).status$;
      })
        , last(),
        switchMap(() => this.jobController.getJobResult(importJobId)),
        switchMap((blob: Blob) => from(blob.text())),
        map((text) => JSON.parse(text)))
      .subscribe((response: AnnotationImportResult) => {
        this.importResult = response;
        const totalCount = response.totalLines;
        const errorCount = response.errors.length;
        if (errorCount === 0) {
          /* import is completely successful */
          this.titleIconType = 'check-circle';
          this.alertType = 'success';
          this.finalMessage = this.translateService.instant('annotationReporting.importSuccess');
          this.tileIconClass = 'annotations-modal__success-icon';
        } else if (errorCount < totalCount) {
          /* import is partially successful */
          this.alertType = 'warning';
          this.titleIconType = 'warning';
          this.finalMessage = this.translateService.instant('annotationReporting.importPartialSuccess');
          this.tileIconClass = 'annotations-modal__warning-icon';
        } else {
          /* import is totally unsuccessful */
          this.alertType = 'error';
          this.titleIconType = 'close-circle';
          this.finalMessage = this.translateService.instant('annotationReporting.importFailure');
          this.tileIconClass = 'annotations-modal__error-icon';
        }

        if (errorCount > 0) {
          /* just to show 50 limited error lines in UI */
          this.errorMessage = this.importResult.errors.slice(0, 50).join('\n');
          if (this.importResult.errors.length > 50) {
            this.errorMessage += '\n...';
          }
        }
      }, (error) => {
        log.error(`System error Occured: ${error}`);
        /* could add some message that something went wrong retry*/
        this.finalMessage = this.translateService.instant('annotationReporting.importJobFailed');
        this.titleIconType = 'close-circle';
        this.tileIconClass = 'annotations-modal__error-icon';
        this.alertType = 'error';
        /* set some dummy values */
        this.importResult = { totalLines: 0, errors: [] };
      }, () => {
        this.loading = false;
        this.finished = true;
      });
  }

  onDownloadLog(): void {
    const downloadText = JSON.stringify(this.importResult.errors, null, '\t');
    const link = document.createElement('a');
    const mimeType = 'text/plain';
    link.setAttribute('download', 'log');
    link.setAttribute('href', 'data:' + mimeType + ';charset=utf-8,' + encodeURIComponent(downloadText));
    link.click();
  }

  beforeUpload = (file: File): boolean => {
    this.fileList = this.fileList.concat(file);
    return false;
  };

  closeModal(): void {
    this.modal.destroy('closed');
  }

  onDone(result: string): void {
    this.modal.destroy(result);
  }

}
