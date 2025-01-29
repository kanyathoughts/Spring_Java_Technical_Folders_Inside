import { Injectable } from '@angular/core';
import { BehaviorSubject, forkJoin, from, interval, Observable, Subject, Subscription } from 'rxjs';
import { RemoteJob } from './job-manager-service.interface';
import { catchError, tap } from 'rxjs/operators';
import { WarningCancel } from '@app/modules/configuration/taxonomy-configuration/taxonomy-reporting.model';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { JOB_TYPE, JobDetails } from '@app/shared/interfaces/job-details.interface';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { GraphUtility } from '@app/modules/graph/dependency/utils/dependency-graph-utility';
import { JobControllerService, JobInformation } from '@innowake/mining-api-angular-client';

type PartialWithRequired<T, K extends keyof T> = Partial<Omit<T, K>> & Pick<T, K>;
const ANNOTATIONS_IMPORT_JOB_NAME = 'Annotation Csv Import';
export const REFRESH_INTERVAL = 2000;
export const LOCAL_STORAGE_KEY = 'remoteJobs';

/* states in which we consider a job "failed" */
const FAILED_JOB_STATES = [
  JobInformation.StatusEnum.FAILURE,
  JobInformation.StatusEnum.TIMEOUT,
  JobInformation.StatusEnum.CANCELED
];

/* states in which we consider a job "finished" */
const FINISHED_JOB_STATES = [
  ...FAILED_JOB_STATES,
  JobInformation.StatusEnum.SUCCESS
];

const RUNNING_JOB_STATES = [
  JobInformation.StatusEnum.UNKNOWN,
  JobInformation.StatusEnum.SCHEDULED,
  JobInformation.StatusEnum.RUNNING,
  JobInformation.StatusEnum.CANCEL_REQUESTED
];

const CALL_CHAIN_EXPORTER_TOKEN = 'call-chain-exporter';
@Injectable({
  providedIn: 'root'
})
export class JobManagerService {

  readonly jobs$: Observable<RemoteJob[]> = new BehaviorSubject([]);
  percent: BehaviorSubject<number> = new BehaviorSubject<number>(0);
  jobNotification: BehaviorSubject<JobInformation> = new BehaviorSubject<JobInformation>({});
  jobStatus = new Subject<boolean>();
  jobResult = new Subject();
  jobFormat = new Subject<string>();
  modulePropagationModal: NzModalRef;
  private jobSubject = this.jobs$ as BehaviorSubject<RemoteJob[]>;
  private jobs: RemoteJob[] = [];
  private refreshSubscription: Subscription | undefined;

  constructor(
    private readonly jobService: JobControllerService,
    private iamService: IdentityAccessManagementService,
    private jobController: JobControllerService,
    private translateService: TranslateService
  ) {
    this.loadStoredJobs();
    this.clearFinished();
    this.refresh();
  }

  static isJobRunning(status: JobInformation.StatusEnum): boolean {
    return ! FINISHED_JOB_STATES.includes(status);
  }

  /**
   * Registers the new job for next processing and updated job list is persisted in the localStorage
   *
   * @param remoteJob object with submitted jobId and other properties for further processing wrt the Job status
   * @returns the updated remoteJob object
   */
  register(remoteJob: PartialWithRequired<RemoteJob, 'jobId'>): RemoteJob {
    const remoteJobPrivate = remoteJob as RemoteJob;

    /* apply defaults */
    remoteJobPrivate.autoDownloadResult = remoteJobPrivate.autoDownloadResult ?? false;
    remoteJobPrivate.cancellable = remoteJobPrivate.cancellable ?? false;
    remoteJobPrivate.foreground = remoteJobPrivate.foreground ?? false;
    remoteJobPrivate.status$ = new BehaviorSubject<JobInformation.StatusEnum>(JobInformation.StatusEnum.UNKNOWN);
    this.jobs.push(remoteJobPrivate);
    this.updateJobs(this.jobs);
    this.refresh(remoteJob?.details?.get('exportFormat'));
    return remoteJobPrivate;
  }

  /**
   * Clears all the localstorage jobs with SUCCESS, TIMEOUT, FAILURE, CANCELED status
   */
  clearFinished(): void {
    this.updateJobs(this.jobs.filter(job => {
      if (job.jobInfo && FINISHED_JOB_STATES.includes(job.jobInfo?.status)) {
        /* ensure subscribers to this job's status are unsubscribed before the job is removed from the list of active jobs */
        job.status$.complete();
        return false;
      }
      return true;
    }));
  }

  /**
   * Refresh the jobInformation for all jobs with status UNKNOWN, SCHEDULED, RUNNING, CANCELREQUESTED at REFRESH_INTERVAL
   * Downloads the result for the jobs with SUCCESS status that have autoDownloadResult set to true
   * Displays an error message for the jobs with FAILURE, TIMEOUT or CANCELED status
   * Removes completed jobs from the list of jobs and persists the updated jobs info in the localstorage
   * @param exportFormat takes up type of export such as ARTIFICIAL_EDGE OR DEPENDENCY_GRAPH
   */
  refresh(exportFormat?: string): void {
    let haveRunningJobs = false;
    /* use forkJoin() to request all job information in parallel, then wait until all requests are finished */
    forkJoin(
      this.jobs.map(job => this.jobService.getJobInformation(job.jobId)
        .pipe(tap((jobInfo: JobInformation) => {
          if (exportFormat !== GraphUtility.ARTIFICIAL_EDGE.toUpperCase()) {
            this.jobNotification.next(jobInfo);
          } else if (exportFormat === GraphUtility.ARTIFICIAL_EDGE.toUpperCase()) {
            this.jobFormat.next(exportFormat);
            if (jobInfo.status === JobInformation.StatusEnum.SUCCESS) {
              this.jobNotification.next(jobInfo);
            }
          }
          if (jobInfo.jobDescription === WarningCancel.VALIDATING_TAXONOMY_ASSIGN || jobInfo.jobDescription === WarningCancel.IMPORT_TAXONOMY_ASSIGN) {
            this.percent.next(Math.round((jobInfo.processedWorkUnits / jobInfo.totalWorkUnits) * (50)));
          }
          if(jobInfo.jobName === ANNOTATIONS_IMPORT_JOB_NAME){
            this.percent.next(Math.round((100 * jobInfo.processedWorkUnits) / jobInfo.totalWorkUnits));
          }
          job.jobInfo = jobInfo;
          if (RUNNING_JOB_STATES.includes(job.jobInfo.status)) {
            haveRunningJobs = true;
          } else if (job.jobInfo.status === JobInformation.StatusEnum.SUCCESS && job.autoDownloadResult) {
            this.downloadFile(jobInfo.jobId);
          }
          job.status$.next(job.jobInfo.status);
          if ( ! JobManagerService.isJobRunning(job.jobInfo.status)) {
            job.status$.complete();
          }
        }))
        .pipe(catchError(async (err) => {
          console.warn('unable to retrieve information for job', job.jobId, await err);
          /* remove the failed job from the list */
          this.jobs.splice(this.jobs.indexOf(job), 1);
        })))
    )
      /* subscribe to the result of forkJoin() - i.e. run after all jobs have been refreshed */
      .subscribe(() => {
        this.clearFinished();
        if (haveRunningJobs && ! this.refreshSubscription) {
          this.refreshSubscription = interval(REFRESH_INTERVAL).subscribe(() => this.refresh());
        } else if ( ! haveRunningJobs && this.refreshSubscription) {
          this.refreshSubscription.unsubscribe();
          this.refreshSubscription = undefined;
        }
      });
  }

  /**
   * Registers the new job for next processing and updated job list is persisted in the localStorage
   *
   * @param exportFormat takes up type of export such as ARTIFICIAL_EDGE OR DEPENDENCY_GRAPH
   * @param projectId is the projectId
   * @param requestObject parameters
   */
  jobExtension(exportFormat: string, projectId: number, requestObject: { [key: string]: string[] }): void {
    this.jobController.submitJobExtension(projectId, this.translateService.instant('callChainExtensionType') + '', requestObject).subscribe((jobId: any) => {
      /* The JobId is any as it's returned as CharArray from Backend, which needs to be string in futher steps */
      if (jobId) {
        const detailsMap = new Map<string, string>();
        detailsMap.set('exportFormat', exportFormat);
        const remoteJob = {
          jobId,
          uiToken: CALL_CHAIN_EXPORTER_TOKEN,
          foreground: true,
          cancellable: true,
          autoDownloadResult: (exportFormat === 'DEPENDENCY_GRAPH' || exportFormat === 'ARTIFICIAL_EDGE') ? false : true,
          details: detailsMap
        };
        this.register(remoteJob);
      }
    });
  }

  /**
   * Subscribe to the job notifications. If a dependency-graph job finishes successfully, we want to set {@linkplain dependencyGraph}
   * in order to display it on the UI.
   */
  subscribeToJobNotification(): void {
    this.jobNotification.subscribe(notification => {
      if ( ! notification || notification?.status !== JobInformation.StatusEnum.SUCCESS) {
        if ([JobInformation.StatusEnum.CANCELED, JobInformation.StatusEnum.FAILURE, JobInformation.StatusEnum.TIMEOUT,
        JobInformation.StatusEnum.UNKNOWN].includes(notification?.status)) {
          this.jobStatus.next(false);
        }
        return;
      }
      const remoteJob = this.getRemoteJob(notification.jobId);
      const exportFormat = remoteJob?.details.get('exportFormat');
      if (remoteJob && remoteJob.details && (exportFormat === 'DEPENDENCY_GRAPH' || exportFormat === GraphUtility.ARTIFICIAL_EDGE.toUpperCase())) {
        this.jobController.getJobResult(notification.jobId).subscribe((result: Blob) => {
          this.jobResult.next(result);
        });
      }
      this.jobStatus.next(false);
    });
  }

  /**
   * Returns a RemoteJob for a job ID.
   * @param jobId ID of the job
   * @returns RemoteJob with the specified ID or undefined if no such job exists.
   */
  getRemoteJob(jobId: string): RemoteJob {
    return this.jobs.find(j => j.jobId === jobId);
  }

  /**
   * Formats the job information into the required format for job list and notification.
   *
   * @param jobInfo job information received when a job is started.
   * @returns the formatted job details.
   */
  getJobDetail(jobInfo: JobInformation): JobDetails {
    const jobCircleIcon = 'close-circle';
    const jobTextColorClass = 'job-monitor-header__text-color';
    const jobErrorTextColorClass = 'job-monitor-header__error-color';
    let jobDetails: JobDetails = {};
    switch (jobInfo.status) {
      case 'SUCCESS':
        jobDetails = {
          jobIcon: 'check-circle',
          stepDescription: 'Finished successfully',
          actionLabel: 'Show Log',
          colorClass: 'job-monitor-header__success-color',
          time: jobInfo.finishTime,
          listingPriority: 2
        };
        if (jobInfo.resultStatus?.severity === 'ERROR') {
          jobDetails = {
            jobIcon: jobCircleIcon,
            stepDescription: jobInfo.resultStatus.message,
            actionLabel: 'Show Log',
            colorClass: jobErrorTextColorClass,
            time: jobInfo.finishTime,
            listingPriority: 2
          };
        } else if (jobInfo.resultStatus?.severity === 'WARNING') {
          jobDetails = {
            jobIcon: 'warning',
            stepDescription: jobInfo.resultStatus.message,
            actionLabel: 'Show Log',
            colorClass: 'job-monitor-header__warning-color',
            time: jobInfo.finishTime,
            listingPriority: 2
          };
        }
        jobDetails.resultButton = jobInfo.resultStatus && ! jobInfo.resultStatus.internalResult
          ? this.getResultButtonLabel(jobInfo)
          : null;
        break;
      case 'FAILURE':
        jobDetails = {
          jobIcon: jobCircleIcon,
          stepDescription: jobInfo.resultStatus?.message ? jobInfo.resultStatus.message : 'Failed',
          actionLabel: 'Show Log',
          colorClass: jobErrorTextColorClass,
          time: jobInfo.finishTime,
          listingPriority: 2
        };
        break;
      case 'SCHEDULED':
        jobDetails = {
          jobIcon: 'clock-circle',
          stepDescription: 'Scheduled',
          actionLabel: 'Cancel',
          colorClass: jobTextColorClass,
          time: jobInfo.scheduledStartTime,
          listingPriority: 2
        };
        break;
      case 'RUNNING':
        jobDetails = {
          jobIcon: 'loading',
          stepDescription: jobInfo.stepDescription,
          actionLabel: 'Stop',
          colorClass: 'job-monitor-header__primary-color',
          time: jobInfo.startTime,
          listingPriority: 1
        };
        break;
      case 'CANCELED':
        jobDetails = {
          jobIcon: jobCircleIcon,
          stepDescription: 'Stopped',
          actionLabel: 'Show Log',
          colorClass: jobTextColorClass,
          time: jobInfo.submitTime,
          listingPriority: 2
        };
        break;
      case 'CANCEL_REQUESTED':
        jobDetails = {
          jobIcon: jobCircleIcon,
          stepDescription: 'Stopping...',
          actionLabel: 'Stopping',
          disableActionButton: true,
          colorClass: 'job-monitor-header__text-color',
          listingPriority: 2
        };
        break;
      case 'TIMEOUT':
        jobDetails = {
          jobIcon: 'field-time',
          stepDescription: 'Timeout',
          actionLabel: 'Show Log',
          colorClass: jobErrorTextColorClass,
          time: jobInfo.scheduledStartTime,
          listingPriority: 2
        };
        break;
    }
    return {
      jobDescription: jobInfo.jobName,
      jobId: jobInfo.jobId,
      finishTime: jobInfo.finishTime,
      status: jobInfo.status,
      userName: jobInfo.userName,
      jobIcon: jobDetails.jobIcon,
      stepDescription: jobDetails.stepDescription,
      actionLabel: jobDetails.actionLabel,
      colorClass: jobDetails.colorClass,
      jobStartedBy: jobInfo.userName,
      time: jobDetails.time ? dateFormatter(jobDetails.time as unknown as Date) : '',
      jobType: '',
      details: {},
      downloadLink: {},
      loadingLogButton: false,
      disableActionButton: jobDetails.disableActionButton ? jobDetails.disableActionButton : false,
      listingPriority: jobDetails.listingPriority,
      resultButton: this.getResultButtonLabelToDisplay(jobInfo, jobDetails?.resultButton),
      projectId: jobInfo.projectId,
      moduleId: jobInfo.moduleId
    };
  }

  /**
   * Loads the jobs from the local storage for further processing
   * Downloads the job file based on job ID.
   * @param jobId job ID.
   */
  downloadFile(jobId: string): void {
    let downloadUrl = `${getBasePath()}/api/v1/jobs/${encodeURIComponent(String(jobId))}/result`;
    from(this.iamService.getAccessToken()).subscribe((token) => {
      if (token !== null) {
        downloadUrl += `?access_token=${token}`;
      }
      const downloadLink = document.createElement('a');
      downloadLink.href = downloadUrl;
      downloadLink.click();
    });
  }

  /**
   * Invokes job to export the data for the supported formats.
   *
   * @param format format for export.
   * @param options Additional parameters for the export job.
   * @param projectId project id for which export job is invoked.
   */
  invokeExportJob(format: string, options: object, projectId: number): void {
    this.jobController.submitJobExtensionV2(projectId, format, options, undefined).subscribe(jobId => {
      const remoteJob = {
        jobId: jobId as unknown as string,
        uiToken: 'export-options-' + format,
        autoDownloadResult: true,
        foreground: true
      };
      this.register(remoteJob);
    });
  }


  private loadStoredJobs(): void {
    const rmtJobs: RemoteJob[] = JSON.parse(localStorage.getItem(LOCAL_STORAGE_KEY));
    this.jobs = rmtJobs ?? [];
    this.jobs.forEach(job => {
      job.status$ = new BehaviorSubject(job.jobInfo?.status ?? JobInformation.StatusEnum.UNKNOWN);
    });
    this.jobSubject.next(this.jobs);
  }

  private updateJobs(jobs: RemoteJob[]) {
    this.jobs = jobs;
    this.jobSubject.next(jobs);
    const job = this.jobs.map((jobs: RemoteJob) =>
    ({
      autoDownloadResult: jobs.autoDownloadResult,
      cancellable: jobs.cancellable,
      jobId: jobs.jobId,
      foreground: jobs.foreground,
      jobInfo: jobs.jobInfo,
      status$: '',
      uiToken: jobs.uiToken
    })
    );
    localStorage.setItem(LOCAL_STORAGE_KEY, JSON.stringify(job));
  }

  private getResultButtonLabel(jobInfo: JobInformation): string {
    if (jobInfo.resultStatus.hasCollectableResult && jobInfo.jobName !== 'Taxonomy Propagation Module Identification') {
      return this.translateService.instant('jobProgressMonitor.downloadResult');
    } else {
      return this.translateService.instant('jobProgressMonitor.navigateResult');
    }
  }

  private getResultButtonLabelToDisplay(jobInfo: JobInformation, resultButton: string): string {
    if (resultButton && JOB_TYPE[jobInfo.jobName]) {
      if ((JOB_TYPE[jobInfo.jobName].routeType === 'module' && jobInfo.moduleId)
        || (JOB_TYPE[jobInfo.jobName].routeType === 'project' && jobInfo.projectId)
        || JOB_TYPE[jobInfo.jobName].showModal) {
        return resultButton;
      }
    } else if (resultButton && ! JOB_TYPE[jobInfo.jobName] && jobInfo.resultStatus?.hasCollectableResult) {
      return resultButton;
    } else {
      return null;
    }
  }
}
