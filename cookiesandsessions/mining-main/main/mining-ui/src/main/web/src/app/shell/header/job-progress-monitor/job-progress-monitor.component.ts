import { ChangeDetectorRef, Component, OnDestroy, OnInit, TemplateRef, ViewChild } from '@angular/core';
import { Subject, Subscription, from } from 'rxjs';
import { JobDetails, JOB_TYPE } from '@app/shared/interfaces/job-details.interface';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { HttpErrorResponse } from '@angular/common/http';
import { Logger } from '@app/core';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { ConfirmDeleteModalComponent, DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { delay, map, switchMap } from 'rxjs/operators';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { Router } from '@angular/router';
import { TaxonomyPropagationService } from '@app/core/services/taxonomy-propagation.service';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { TaxonomyPropagationComponent } from '@app/shared/taxonomy-propagation/taxonomy-propagation/taxonomy-propagation.component';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { JobControllerService, JobInformation, PagedJobInformation } from '@innowake/mining-api-angular-client';
import { EditReachabilityBlockComponent } from '@app/shared/components/edit-reachability-block/edit-reachability-block.component';
import { RESULT_EDITED } from '@app/shared/components/base-edit-functional-block/base-edit-functional-block.component';

export const reachabilityBlockChanged = new Subject<string>();

const log = new Logger('JobProgressMonitorComponent');
const badgeType = 'unordered-list';

@Component({
  selector: 'job-progress-monitor',
  templateUrl: './job-progress-monitor.component.html'
})
export class JobProgressMonitorComponent implements OnInit, OnDestroy {
  @ViewChild('jobNotificationTemplate') jobNotificationTemplate: TemplateRef<any>;
  @ViewChild('removeModalTemplate') removeModalContent: TemplateRef<any>;
  jobsDetails: JobDetails[] = [];
  pageIndex = 0;
  perPage = 30;
  totalJobs = 0;
  refreshSubscription: Subscription;
  jobNotification: JobDetails = {};
  loadingJobs = false;
  showJobNotification = true;
  statusArray: any = [];
  successCount = 0;
  warningCount = 0;
  runningCount = 0;
  errorCount = 0;
  runningStateArray: any = [];
  type = badgeType;
  color = '';
  backgroundClass = '';
  count = 0;
  theme = '';
  showLogModal = false;
  logDetails: { title: string, logs: string, id: string } = { title: '', logs: '', id: '' };
  loadingLogButton = false;
  toBeRemovedJob = '';
  removeAllJobs = false;
  isMenuVisible = false;
  keepOpen = false;
  closingSubscription: Subscription;
  isJobListAvailable = false;
  modulePropagationModal: NzModalRef;

  constructor(
    private jobControllerService: JobControllerService,
    public authorizationService: KeycloakAuthorizationService,
    private jobManagerService: JobManagerService,
    private nzNotificationService: NzNotificationService,
    private translateService: TranslateService,
    private modalService: NzModalService,
    private messageService: NzMessageService,
    private taxonomyPropagation: TaxonomyPropagationService,
    private userCustomizableTable: CustomizableTableColumnService,
    public router: Router,
    private cd: ChangeDetectorRef) { }

  ngOnInit(): void {
    this.getJobInfo();
    const jobNotificationMap = new Map<string, Record<'id' | 'status', string>>();
    this.jobManagerService.jobNotification.next(null);
    this.jobManagerService.jobFormat.subscribe(format => {
      if (format === 'ARTIFICIAL_EDGE') {
        this.showJobNotification = false;
      }
    });
    this.refreshSubscription = this.jobManagerService.jobNotification.subscribe(res => {
      if (res && res.jobName) {
        this.jobNotification = this.jobManagerService.getJobDetail(res);
        this.setRunningStatusDetails(res);
        const jobInfo = jobNotificationMap.get(res.jobId);
        if (jobInfo) {
          if (res.status !== jobInfo.status) {
            this.nzNotificationService.remove(jobNotificationMap.get(res.jobId).id);
            this.setJobNotification(jobNotificationMap, res);
          } else {
            const job = this.jobsDetails.find(job => job.jobId === res.jobId);
            if (job) {
              job.stepDescription = res.stepDescription;
            }
          }
        } else {
          this.setJobNotification(jobNotificationMap, res);
        }
      }
    });
  }

  /**
   * Shows the job logs when the button is clicked.
   * @param job details of the job.
   */
  showLogs(job: JobDetails): void {
    const title = job.jobDescription;
    this.jobControllerService.getJobLog(job.jobId).subscribe((logs: { [key: string]: string }) => {
      job.loadingLogButton = false;
      job.disableActionButton = true;
      this.logDetails.id = job.jobId;
      this.logDetails.title = this.translateService.instant('showLogModal.logTitle', { title });
      Object.values(logs).forEach((logValues) => {
        this.logDetails.logs += logValues;
      });
      this.showLogModal = true;
    }, (error: HttpErrorResponse) => {
      log.error(error);
      job.loadingLogButton = false;
      job.disableActionButton = true;
      this.showLogModal = true;
      this.logDetails.title = '';
      this.logDetails.logs = this.translateService.instant('showLogModal.errorMessage');
    });
  }

  /**
   * Closes the log modal.
   */
  closeLogModal(): void {
    this.jobsDetails.forEach(element => {
      if (element.jobId === this.logDetails.id) {
        element.disableActionButton = false;
      }
    });
    this.showLogModal = false;
  }

  /**
   * Method for the Pagination
   * @param current is the current page number.
   */
  paginate(current: number): void {
    this.pageIndex = current ? current - 1 : this.pageIndex;
    this.getJobInfo();
    document.getElementById('jobsContentRef')?.scrollTo(0, 0);
  }

  /**
   * Gets the visibility state of the job item popover.
   * @param state state of the popover.
   */
  getPopoverVisibilityState(state: boolean): void {
    this.statusArray = [];
    this.showJobNotification = !state;
    this.resetCounters();
    this.paginate(1);
    if (this.runningStateArray.length <= 0) {
      // Timeout to wait until the badge anymation is over
      setTimeout(() => {
        this.color = '';
        this.theme = '';
        this.type = badgeType;
      }, 500);
    } else {
      this.type = 'loading';
      this.color = 'job-monitor-header__primary-color';
      this.theme = 'outline';
    }
    // This is necessary to avoid error "NG0100: Expression has changed after it was checked"
    this.cd.detectChanges();
  }

  /**
   * check whether the JobInfo is still in running state or not
   */
  checkJobState(jobInfo: JobInformation): boolean {
    return this.runningStateArray && this.runningStateArray.includes(jobInfo.jobId);
  }

  /**
   * Perform the respective action as per status of the job.
   * @param job the job details.
   */
  performAction(label: string, job: JobDetails): void {
    switch (label) {
      case 'Show Log':
        job.loadingLogButton = true;
        this.showLogs(job);
        break;
      case 'Download Again':
        this.jobManagerService.downloadFile(job.jobId);
        break;
      case 'Show Result':
        if (job.jobDescription === 'Generate Reachability Block Description') {
          this.showEditReachabilityBlockModal(job);
        } else if (JOB_TYPE[job.jobDescription].routeType === 'module') {
          void this.router.navigateByUrl(RouteBuilder.buildModuleRoute(job.projectId, job.moduleId, JOB_TYPE[job.jobDescription].urlParams as string));
        } else {
          void this.router.navigateByUrl(RouteBuilder.buildProjectRoute(job.projectId, JOB_TYPE[job.jobDescription].urlParams as string));
        }
        break;
      case 'Review Now':
        this.reviewTaxonomyPropagationJob(job);
        break;
      default:
        this.cancelJob(job);
    }
  }

  /**
   * reviews TaxonomyPropagation Job.
   * @param job the job details.
   */
  reviewTaxonomyPropagationJob(job: JobDetails): void {
    this.jobControllerService.getJobResult(job.jobId as unknown as string).pipe(
      switchMap((blob: Blob) => from(blob.text())),
      map((res: string) => JSON.parse(res))).subscribe(jobResult => {
        this.modulePropagationModal = this.modalService.create<TaxonomyPropagationComponent>({
          nzTitle: this.translateService.instant('taxonomyPropagation.taxonomyPropagationModalTitle'),
          nzKeyboard: true,
          nzClosable: true,
          nzCentered: true,
          nzWidth: '80%',
          nzMaskClosable: false,
          nzWrapClassName: 'vertical-center-modal',
          nzContent: TaxonomyPropagationComponent
        });
        const instance = this.modulePropagationModal.getContentComponent();
        instance.affectedModules = jobResult;
        instance.usage = Usages.MODULETABLE;
        instance.graphQlType = 'modules';
        instance.projectId = this.taxonomyPropagation.currentProjectId;
        instance.pageType = TypeName.PAGEMODULE;
        instance.moduleIdForPreFiltering = jobResult?.object[1]?.map((modules: Record<string, unknown>) => modules.moduleId);
        instance.internalDataPoints = [this.userCustomizableTable.dataPointsList.find(y => y.name === 'id')];
        instance.allowSelectDeselectRows = true;
        instance.selectedTaxonomyNames = this.translateService.instant('taxonomyPropagation.selectedTaxonomiesInfo',
        { selectedTaxonomiesString: this.taxonomyPropagation.selectedTaxonomyName });
      },
        error => {
          console.error('Error converting Blob to JSON:', error);
        }
      );
  }

  /**
   * Cancels the running job.
   * @param job details of the running job.
   */
  cancelJob(job: JobDetails): void {
    job.loadingLogButton = true;
    this.jobControllerService.cancelJob(job.jobId).subscribe(() => {
      job.loadingLogButton = false;
      job.disableActionButton = true;
    }, () => {
      job.loadingLogButton = false;
      job.disableActionButton = true;
    });
  }

  /**
   * refactor the array if the job status change from running
   */
  spliceJobRunningStatus(jobInfo: JobInformation): string {
    this.runningStateArray.splice(this.runningStateArray.indexOf(jobInfo.jobId), 1);
    this.statusArray.forEach((element: any, index: any) => {
      if (element.id === 3) {
        if (this.runningCount === 0 || this.runningCount < 0 || this.runningCount === 1) {
          this.statusArray.splice(index, 1);
        }
        return element.name = 'RUNNING :' + --this.runningCount;
      }
    });
    return '';
  }

  /**
   * setting badge count and sorting based on priorities
   */
  setBadgeCount(): void {
    if (this.statusArray && this.statusArray.length) {
      let flag = true;
      this.type = badgeType;
      this.color = '';
      this.count = 0;
      this.statusArray = this.statusArray.sort((first: any, second: any) => 0 - (first.id < second.id ? -1 : 1));
      this.statusArray.forEach((element: any) => {
        if (flag) {
          switch (element.id) {
            case 1:
              this.type = 'close-circle';
              this.color = 'job-monitor-header__error-color';
              this.theme = 'fill';
              this.count = this.errorCount;
              flag = false;
              break;
            case 2:
              this.type = 'warning';
              this.color = 'job-monitor-header__warning-color';
              this.theme = 'fill';
              this.count = this.warningCount;
              flag = false;
              break;
            case 3:
              this.type = 'loading';
              this.color = 'job-monitor-header__primary-color';
              this.theme = 'outline';
              this.count = this.runningCount;
              flag = false;
              break;
            case 4:
              this.type = 'check-circle';
              this.color = 'job-monitor-header__success-color';
              this.theme = 'fill';
              this.count = this.successCount;
              flag = false;
              break;
            default:
              this.type = badgeType;
              this.color = 'job-monitor-header__text-color';
              this.theme = 'outline';
              this.count = 0;
              break;
          }
          return 0;
        }
      });
    }
  }

  /**
   *removeJobFromHistory deletes selected job from history
   *@param jobInfo gives the jobInfo to be deleted
   *@param removeAll gives the flag to remove all or to remove single job
   */
  removeJobsFromHistory(jobInfo: JobInformation, removeAll: boolean): void {
    this.removeAllJobs = removeAll;
    if (!removeAll) {
      this.toBeRemovedJob = jobInfo.jobDescription;
    }
    const removeModal = this.modalService.create<ConfirmDeleteModalComponent>({
      nzTitle: removeAll ? this.translateService.instant('jobProgressMonitor.clearAllJobsHeader') :
        this.translateService.instant('Remove' + ' ' + this.toBeRemovedJob + '?'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzOkLoading: true,
      nzContent: ConfirmDeleteModalComponent
    });
    const instance = removeModal.getContentComponent();
    instance.modalContent = this.removeModalContent;
    instance.confirmationButtonText = removeAll ? 'jobProgressMonitor.clearBtnLbl' : 'jobProgressMonitor.RemovebtnLbl';
    instance.isConfirmationReq = false;
    removeModal.afterClose.subscribe((result) => {
      if (result === DELETE_MODAL_CONFIRMED) {
        const removeJobsReq = removeAll ? this.jobControllerService.deleteJob() : this.jobControllerService.deleteJob1(jobInfo.jobId);
        removeJobsReq.subscribe(() => {
          if (removeAll) {
            this.jobsDetails = [];
          } else {
            const selectedJobDetailIndex = this.jobsDetails.findIndex((job) => job.jobId === jobInfo.jobId);
            if (selectedJobDetailIndex > -1) {
              this.jobsDetails.splice(selectedJobDetailIndex, 1);
            }
            this.paginate(1);
          }
          this.messageService.create('success', `${this.translateService.instant(removeAll ? 'jobProgressMonitor.removeHistorySuccess' :
            'jobProgressMonitor.successDelete')}`);
        }, () => {
          this.messageService.create('error', `${this.translateService.instant(removeAll ? 'jobProgressMonitor.errorclearHistory' :
            'jobProgressMonitor.errorDelete')}`);
        });
      }
    });
  }

  /**
   * Open the Job progress Monitor popover
   */
  openJobProgressMonitor(): void {
    this.isMenuVisible = true;
    // we need to cancel any ongoing closing event otherwise it overrides the opening
    this.cancelCloseJobProgressMonitor();
  }

  /**
   * Triggers the event closing the Job Progress Monitor after a delay of 500ms
   */
  closeJobProgressMonitor(): void {
    if (!this.keepOpen) {
      const closingSubject = new Subject();
      // Using 500 ms to have a behavior similar to the other actions in the app header
      this.closingSubscription = closingSubject.pipe(delay(500)).subscribe(() => {
        this.isMenuVisible = false;
      });
      closingSubject.next(null);
      // complete the subject so we don't have to unsubscribe to it if not canceled
      closingSubject.complete();
    }
  }

  /**
   * Toggle the state keeping the menu open even if the mouse leaves it (ie : the dropdown menu inside the popover)
   */
  toggleKeepMenuOpen(dropdownOpen: boolean): void {
    // After switching back to false we need to trigger the close event in case the mouse already left the menu
    // Otherwise the menu stays indefinitely open as the mouseleave event already got triggered,
    // if the user is still in the menu the nouseenter event on the popover will cancel the closing
    if (! dropdownOpen) {
      this.closeJobProgressMonitor();
    }
    this.keepOpen = dropdownOpen;
  }

  /**
   * Cancel the closing event is happening during the closing delay
   */
  cancelCloseJobProgressMonitor(): void {
    this.closingSubscription?.unsubscribe();
  }

  ngOnDestroy(): void {
    this.refreshSubscription?.unsubscribe();
  }

  private showEditReachabilityBlockModal(job: JobDetails) {
    this.jobControllerService.getJobResult(job.jobId).subscribe((result) => {
      this.modalService.openModals
        .filter(modal => modal.getContentComponent() instanceof EditReachabilityBlockComponent)
        .forEach(modal => modal.destroy());
      const editReachabilityBlockModal = this.modalService.create<EditReachabilityBlockComponent>({
        nzTitle: this.translateService.instant('reachability.editModalTitle'),
        nzClosable: true,
        nzMaskClosable: false,
        nzWrapClassName: 'vertical-center-modal',
        nzClassName: 'functional-analysis-tree-component__edit-window',
        nzKeyboard: true,
        nzContent: EditReachabilityBlockComponent,
        nzWidth: 600
      });
      const instance = editReachabilityBlockModal.getContentComponent();
      const resultObj = JSON.parse(result.object as string);
      instance.projectId = job.projectId;
      instance.reachabilityBlockUid = resultObj.uid;
      instance.description = resultObj.description;
      instance.isGenerationSuccessful = true;
      editReachabilityBlockModal.afterClose.subscribe((result) => {
        if (result === RESULT_EDITED) {
          reachabilityBlockChanged.next(resultObj.uid as string);
        }
      });
    });
  }

  private getJobInfo(): void {
    this.loadingJobs = true;
    this.jobControllerService.getJobInformations(this.perPage, '', this.pageIndex).subscribe((jobs: PagedJobInformation) => {
      this.totalJobs = jobs.totalElements;
      this.jobsDetails = [];
      jobs.content.forEach((element: JobInformation) => {
        const jobDetail = this.jobManagerService.getJobDetail(element);
        if (element.messages.length) {
          jobDetail.details = element.messages;
        }
        this.setRunningStatusDetails(element);
        const jobIndex = this.jobsDetails.findIndex((job) => job.jobId === jobDetail.jobId);
        if (jobIndex === -1) {
          this.jobsDetails.push(jobDetail);
        } else {
          this.jobsDetails[jobIndex] = jobDetail;
        }
        this.loadingJobs = false;
      });

      this.isJobListAvailable = this.jobsDetails && this.jobsDetails.length ? true : false;
      if (this.showJobNotification) {
        this.setBadgeCount();
      } else if (this.runningStateArray.length <= 0) {
        this.color = '';
        this.theme = '';
        this.type = badgeType;
      }
      this.loadingJobs = false;
    }, (error: HttpErrorResponse) => {
      this.loadingJobs = false;
      log.error(error);
    });
  }

  /**
   * storing the values based on the JOB status on status array
   */
  private setRunningStatusDetails(jobInfo: JobInformation): JobDetails {
    switch (jobInfo.status) {
      case 'SUCCESS':
        if (jobInfo.resultStatus?.severity === 'OK') {
          if (this.checkJobState(jobInfo)) {
            if (this.successCount > 0) {
              this.statusArray.map((element: any) => {
                if (element.id === 4) {
                  element.name = 'Finished Successfully :' + ++this.successCount;
                }
              });
            } else {
              this.statusArray.push({ 'id': 4, 'name': 'Finished Successfully :' + ++this.successCount });
            }
            this.spliceJobRunningStatus(jobInfo);
          }
        } else if (jobInfo.resultStatus?.severity === 'ERROR') {
          if (this.checkJobState(jobInfo)) {
            if (this.errorCount > 0) {
              this.statusArray.map((element: any) => {
                if (element.id === 1) {
                  element.name = 'ERROR :' + ++this.errorCount;
                }
              });
            } else {
              this.statusArray.push({ 'id': 1, 'name': 'ERROR :' + this.errorCount++ });
            }
            this.spliceJobRunningStatus(jobInfo);
          }
        } else if (jobInfo.resultStatus?.severity === 'WARNING' && this.checkJobState(jobInfo)) {
          if (this.warningCount > 0) {
            this.statusArray.map((element: any) => {
              if (element.id === 2) {
                element.name = 'WARNING :' + ++this.warningCount;
              }
            });
          } else {
            this.statusArray.push({ 'id': 2, 'name': 'WARNING :' + this.warningCount++ });
          }
          this.spliceJobRunningStatus(jobInfo);
        }
        break;
      case 'FAILURE':
        if (this.checkJobState(jobInfo)) {
          if (this.errorCount > 0) {
            this.statusArray.map((element: any) => {
              if (element.id === 1) {
                element.name = 'FAILURE :' + ++this.errorCount;
              }
            });
          } else {
            this.statusArray.push({ 'id': 1, 'name': 'FAILURE :' + this.errorCount++ });
          }
          this.spliceJobRunningStatus(jobInfo);
        }
        break;
      case 'SCHEDULED':
        this.statusArray.push({ 'id': 5, 'name': 'SCHEDULED :0' });
        break;
      case 'RUNNING':
        if (!this.checkJobState(jobInfo)) {
          if (this.runningCount > 0) {
            this.statusArray.map((element: any) => {
              if (element.id === 3) {
                this.runningStateArray[this.runningCount] = (jobInfo.jobId);
                element.name = 'RUNNING :' + ++this.runningCount;
              }
            });
          } else {
            this.statusArray.push({ 'id': 3, 'name': 'RUNNING :' + ++this.runningCount });
            this.runningStateArray[0] = (jobInfo.jobId);
          }
        }
        break;
      case 'CANCELED':
        if (this.checkJobState(jobInfo)) {
          this.statusArray.push({ 'id': 5, 'name': 'CANCELED :0' });
          this.spliceJobRunningStatus(jobInfo);
        }
        break;
      case 'CANCEL_REQUESTED':
        if (this.checkJobState(jobInfo)) {
          this.statusArray.push({ 'id': 5, 'name': 'CANCEL_REQUESTED :0' });
          this.spliceJobRunningStatus(jobInfo);
        }
        break;
    }
    return jobInfo;
  }

  private resetCounters() {
    this.count = 0;
    this.successCount = 0;
    this.errorCount = 0;
    this.runningCount = 0;
    this.warningCount = 0;
  }

  private setJobNotification(jobNotificationMap: Map<string, Record<'id' | 'status', string>>, res: JobInformation): void {
    const jobDuration = res.status === 'FAILURE' ? 0 : 3000;
    if (this.showJobNotification) {
      jobNotificationMap.set(res.jobId, {
        id: this.nzNotificationService.template(this.jobNotificationTemplate as TemplateRef<Record<string, any>>,
          { nzData: this.jobNotification, nzKey: this.jobNotification.jobId, nzDuration: jobDuration }).messageId,
        status: res.status
      });
    }
    this.getJobInfo();
  }
}
