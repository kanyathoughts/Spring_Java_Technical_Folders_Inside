import { HttpHeaders, HttpResponse } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { fakeAsync, TestBed, tick } from '@angular/core/testing';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { TranslateModule } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { of, throwError } from 'rxjs';
import { RemoteJob } from './job-manager-service.interface';
import { JobManagerService, REFRESH_INTERVAL } from './job-manager.service';
import { JobControllerService, JobInformation } from '@innowake/mining-api-angular-client';

describe('JobManagerService', () => {
  let service: JobManagerService;

  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'success',
    'loading',
    'remove',
    'error'
  ]);
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', [
    'getJobInformation', 'submitJobExtensionV2' , 'submitJobExtension'
  ]);
  const iamTokenServiceSpy: jasmine.SpyObj<IdentityAccessManagementService> = jasmine.createSpyObj('oauthtokenServiceSpy', ['getAccessToken']);
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        TranslateModule.forRoot({}),
        HttpClientTestingModule
      ],
      providers: [
        JobManagerService,
        {
          provide: IdentityAccessManagementService,
          useValue: iamTokenServiceSpy
        },
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy }
      ]
    })
    jobControllerServiceSpy.getJobInformation.and.returnValue(of({}) as any);
    messageServiceSpy.success.and.returnValue('test' as any);
    iamTokenServiceSpy.getAccessToken.and.returnValue(Promise.resolve("token"));
  });

  beforeEach(() => {
    localStorage.clear();
    service = TestBed.inject(JobManagerService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should register job', () => {
    let jobs: RemoteJob[] = [];
    service.jobs$.subscribe(j => jobs = j);

    service.register({
      jobId: 'someJobId',
      cancellable: true,
      foreground: true,
      autoDownloadResult: false
    });
    expect(jobs).toBeDefined();
  });

  it('should remove job when finished', fakeAsync(() => {
    let jobs: RemoteJob[] = [];
    service.jobs$.subscribe(j => jobs = j);

    jobControllerServiceSpy.getJobInformation.and.returnValue(of({ jobId: 'someJobId', status: 'RUNNING' }) as any);

    service.register({
      jobId: 'someJobId'
    });

    tick();

    expect(jobs).toContain(jasmine.objectContaining({
      jobId: 'someJobId'
    }));

    /* job is now "done" */
    jobControllerServiceSpy.getJobInformation.and.returnValue(of({ jobId: 'someJobId', status: 'SUCCESS' }) as any);

    /* "wait" until jobManager refreshes the job */
    tick(REFRESH_INTERVAL);

    expect(jobs.length).toBe(0);

  }));

  it('should remove job when retrieving job status fails', fakeAsync(() => {
    let jobs: RemoteJob[] = [];
    service.jobs$.subscribe(j => jobs = j);

    jobControllerServiceSpy.getJobInformation.and.returnValue(of({ jobId: 'someJobId', status: 'RUNNING' }) as any);

    service.register({
      jobId: 'someJobId'
    });

    tick();

    expect(jobs).toContain(jasmine.objectContaining({
      jobId: 'someJobId'
    }));

    jobControllerServiceSpy.getJobInformation.and.returnValue(throwError('request failed'));

    /* "wait" until jobManager refreshes the job */
    tick(REFRESH_INTERVAL);

    expect(jobs.length).toBe(0);

  }));

  it('should show error message when job fails', fakeAsync(() => {
    jobControllerServiceSpy.getJobInformation.and.returnValue(of({ jobId: 'someJobId', status: 'FAILURE' }) as any);

    service.register({
      jobId: 'someJobId'
    });

    tick();
  }));

  it('should NOT retrieve job result automatically when autoDownloadResult is not given', fakeAsync(() => {
    jobControllerServiceSpy.getJobInformation.and.returnValue(of({ jobId: 'someJobId', status: 'SUCCESS' }) as any);

    service.register({
      jobId: 'someJobId',
    });

    tick();

    expect(jobControllerServiceSpy.getJobInformation as any).toHaveBeenCalled();
  }));

  it('should get job details', () => {
    const jobInfo: JobInformation = {
      "jobId": "1dcfbcd3-c9ab-4f6d-8f58-ce505c689d82",
      "userName": "admin",
      "jobDescription": "Exporting Call Chains",
      "stepDescription": "Creating Call Chains",
      "submitTime": "2022-09-29T13:59:03.959Z",
      "scheduledStartTime": "2022-09-29T13:59:03.960Z",
      "startTime": "2022-09-29T13:59:03.968Z",
      "finishTime": null,
      "eta": null,
      "status": "RUNNING",
      "resultStatus": null,
      "totalWorkUnits": 0,
      "processedWorkUnits": 0,
      "messages": []
    }
    const runningResult = service.getJobDetail(jobInfo);
    expect(runningResult.stepDescription).toBe('Creating Call Chains');

    jobInfo.status = 'FAILURE';
    const failureResult = service.getJobDetail(jobInfo);
    expect(failureResult.stepDescription).toBe('Failed');

    jobInfo.status = 'SCHEDULED';
    const scheduledResult = service.getJobDetail(jobInfo);
    expect(scheduledResult.stepDescription).toBe('Scheduled');

    jobInfo.status = 'CANCELED';
    const canceledResult = service.getJobDetail(jobInfo);
    expect(canceledResult.stepDescription).toBe('Stopped');

    jobInfo.status = 'CANCEL_REQUESTED';
    const cancelRequestedResult = service.getJobDetail(jobInfo);
    expect(cancelRequestedResult.stepDescription).toBe('Stopping...');

    jobInfo.status = 'SUCCESS';
    jobInfo.resultStatus = {
      severity: 'OK',
      message: 'Success'
    };
    const successOkResult = service.getJobDetail(jobInfo);
    expect(successOkResult.stepDescription).toBe('Finished successfully');
    jobInfo.resultStatus = {
      severity: 'ERROR',
      message: 'Error'
    };
    const successErrorResult = service.getJobDetail(jobInfo);
    expect(successErrorResult.stepDescription).toBe('Error');
    jobInfo.resultStatus = {
      severity: 'WARNING',
      message: 'Warning'
    };
    const successWarningResult = service.getJobDetail(jobInfo);
    expect(successWarningResult.stepDescription).toBe('Warning');
  });
});
