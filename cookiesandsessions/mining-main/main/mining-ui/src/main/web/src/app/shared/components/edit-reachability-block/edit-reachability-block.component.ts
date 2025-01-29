import { Component, Input, OnInit } from '@angular/core';
import { BaseEditFunctionalBlockComponent } from '../base-edit-functional-block/base-edit-functional-block.component';
import { UntypedFormBuilder } from '@angular/forms';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { FunctionalBlockControllerService, JobControllerService, JobInformation } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { HttpErrorResponse } from '@angular/common/http';
import { RemoteJob } from '@app/core/services/job-manager/job-manager-service.interface';
import { WaitModalComponent } from '../wait-modal/wait-modal.component';

@Component({
  selector: 'app-edit-reachability-block',
  templateUrl: '../base-edit-functional-block/base-edit-functional-block.component.html'
})
export class EditReachabilityBlockComponent extends BaseEditFunctionalBlockComponent implements OnInit {

  static readonly WAIT_MODAL_DELAY = 10000;

  @Input() reachabilityBlockUid: any;
  @Input() description: string;

  functionalBlock: { uid: string; name: string; description: string };

  modalRefWait: NzModalRef;

  generateJob: RemoteJob;

  constructor(fb: UntypedFormBuilder,
    modalRef: NzModalRef,
    functionalBlockController: FunctionalBlockControllerService,
    translateService: TranslateService,
    messageService: NzMessageService,
    featureToggleService: FeatureToggleService,
    private modalService: NzModalService,
    private jobControllerService: JobControllerService,
    public jobManagerService: JobManagerService,
    private graphQlControllerService: GraphQlControllerService) {
      super(fb, modalRef, functionalBlockController, translateService, messageService, featureToggleService);
  }

  ngOnInit(): void {
    super.ngOnInit();
    this.getFuntionalBlock();
    this.modalRef.afterClose?.subscribe(() => {
      this.modalRefWait?.close();
    });
    this.jobManagerService.jobNotification?.subscribe((jobInformation: JobInformation) => {
      this.handleJobUpdate(jobInformation);
    });
  }

  onSave(): void {
    this.saveUpdate(this.functionalBlock);
  }

  handleCancel(): void {
    if (this.isGenerating) {
      this.jobControllerService.cancelJob(this.generateJob.jobId).subscribe(() => {
        this.isGenerating = false;
      });
    }
    super.handleCancel();
  }

  successMessage(): string {
    return this.translateService.instant('reachability.editSuccess');
  }

  errorMessage(): string {
    return this.translateService.instant('reachability.editError');
  }

  showGenerate(): boolean {
    return this.functionalBlock && this.genAiTranslateActive;
  }

  generateDescription(): void {
    this.isGenerating = true;
    this.jobControllerService.submitJobExtension(this.projectId, 'generate-single-reachability-block-description', {
      'uids': [this.functionalBlock.uid], 'generateModuleDescriptions': ['' + this.generateModuleDescriptions]
    }).subscribe({
      next: (resp) => {
        setTimeout(() => {
          if (this.isGenerating && this.isVisible) {
            this.createWaitModal();
          }
        }, EditReachabilityBlockComponent.WAIT_MODAL_DELAY);
        const remoteJob = {
          jobId: resp as unknown as string,
          autoDownloadResult: false,
          foreground: true,
          cancellable: true
        };
        this.generateJob = this.jobManagerService.register(remoteJob);
      },
      error: (error: HttpErrorResponse) => {
        this.modalRefWait?.close();
        this.messageService.error(this.translateService.instant('job.jobSubmitError') + error.message);
        this.isGenerating = false;
        this.isGenerationSuccessful = false;
      }
    });
  }

  getTooltipText(): string {
    return this.translateService.instant('genAI.contentWarning');
  }

  showGenerateModuleDescriptionsCheckbox(): boolean {
    return true;
  }

  private handleJobUpdate(jobInformation: JobInformation): void {
    if (this.generateJob && this.generateJob.jobId === jobInformation?.jobId) {
      switch(jobInformation.status) {
        case JobInformation.StatusEnum.SUCCESS:
          this.jobControllerService.getJobResult(jobInformation.jobId).subscribe((result) => {
            this.modalRefWait?.close();
            this.isGenerating = false;
            if ( ! result || ! result.object ) {
              this.messageService.error(this.translateService.instant('genAI.noDescriptionGenerated') as string);
              this.isGenerationSuccessful = false;
              return;
            }
            this.formGroup.get('description').setValue(JSON.parse(result.object as string).description);
            this.formGroup.markAsDirty();
            this.isGenerationSuccessful = true;
          });
          break;
        case JobInformation.StatusEnum.FAILURE:
        case JobInformation.StatusEnum.CANCELED:
        case JobInformation.StatusEnum.TIMEOUT:
          this.isGenerating = false;
          this.isGenerationSuccessful = false;
          this.modalRefWait?.close();
          break;
      }
    }
  };

  private createWaitModal(): void {
    this.modalRefWait = this.modalService.create({
      nzContent: WaitModalComponent,
      nzWidth: '400px',
      nzClosable: false,
      nzFooter: [
        {
          label: this.translateService.instant('genAI.waitModalStay'),
          onClick: () => {
            this.modalRefWait.close();
          }
        },
        {
          label: this.translateService.instant('genAI.waitModalClose'),
          type: 'primary',
          onClick: () => {
            this.modalRefWait.close();
            this.isVisible = false;
            this.modalRef?.destroy();
          }
        }
      ]
    });
  }

  private getFuntionalBlock(): void {
    const requestQuery = {
      'query': `{
        functionalBlocks(
          projectId:  ${this.projectId},
          filterObject: { content_uid: { eq: "${this.reachabilityBlockUid}" } }
        ) {
          content {
            uid
            name
            description
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: any) => {
      if (response && response?.data?.functionalBlocks) {
        const functionalBlocksRecords: any[] = response?.data?.functionalBlocks?.content;
        this.functionalBlock = functionalBlocksRecords[0];
        this.formGroup.get('name').setValue(this.functionalBlock.name);
        if (this.description && this.description !== this.functionalBlock.description) {
          this.formGroup.get('description').setValue(this.description);
          this.formGroup.markAsDirty();
        } else {
          this.formGroup.get('description').setValue(this.functionalBlock.description);
        }
      }
    });
  }

}
