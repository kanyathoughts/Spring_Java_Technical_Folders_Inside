import { HttpClient } from '@angular/common/http';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { BehaviorSubject, of, Subject } from 'rxjs';

import { GraphmlExportComponent } from './graphml-export.component';
import { DataPointControllerService, JobControllerService } from '@innowake/mining-api-angular-client';

describe('GraphmlExportComponent', () => {
  let component: GraphmlExportComponent;
  let fixture: ComponentFixture<GraphmlExportComponent>;

  const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj('DataPointControllerService', ['getDataPointsForType']);
  const JobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', ['submitJobExtension']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['success', 'loading', 'remove', 'error']);
  const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>('NzNotificationService', ['create']);
  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GraphmlExportComponent ],
      imports :[TranslateModule.forRoot({})],
      providers: [ 
        { provide: DataPointControllerService, useValue: dataPointControllerServiceSpy }, 
        { provide: JobControllerService, useValue: JobControllerServiceSpy },
        { provide: NzMessageService, useValue: messageServiceSpy },
        TranslateService,
        { provide: NzNotificationService, useValue: notificationSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },

    ],
    })
    .compileComponents();
    let status = {
      jobId: 'someJobId',
      cancellable: true,
      foreground: true,
      label: 'some label',
      autoDownloadResult: false,
      status$: new Subject()
    };
    const percentage = new BehaviorSubject<number>(10);
    JobControllerServiceSpy.submitJobExtension.and.returnValue(of('test' as any));
    jobManagerServiceSpy.register.and.returnValue(status as any);
    jobManagerServiceSpy.percent = percentage;
  });

  beforeEach(() => {
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of([] as any));
    fixture = TestBed.createComponent(GraphmlExportComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test downloadGraphML', () => {
    component.dataPoints = [{
      label: 'text',
      id: '1',
      checked: true
    }]
    component.downloadGraphML();
    expect(component.isDownloading).toBeTruthy();
  });

  it('should test closeGraphMLdrawer', () => {
    component.closeGraphMLdrawer();
    expect(component.drawerRef).toBeNull();
  });
});
