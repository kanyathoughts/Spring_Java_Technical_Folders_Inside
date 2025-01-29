import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import { BehaviorSubject, Observable, Subject, Subscription } from 'rxjs';
import { of } from 'rxjs/internal/observable/of';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TaxonomiesModalComponent } from './taxonomies-modal.component';
import { EventEmitter } from '@angular/core';
import { TaxonomyModalService } from '../taxonomy-modal.service';
import { WarningCancel } from '../taxonomy-reporting.model';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { JobControllerService, JobInformation, TaxonomyControllerService } from '@innowake/mining-api-angular-client';

const validationResponse = { overallResult: "SUCCESS", markers: [{ 'test': 'testing' }] };

describe('TaxonomiesModalComponent', () => {
  let component: TaxonomiesModalComponent;
  let fixture: ComponentFixture<TaxonomiesModalComponent>;
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
  const jobInfoSuccess: JobInformation = { status: JobInformation.StatusEnum.SUCCESS };
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'success',
    'warning',
    'remove',
    'error',
    'loading',
  ]);
  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);
  const taxonomiesServiceSpy = jasmine.createSpyObj('TaxonomyModalService',
    ['validateImportedFile', 'triggerCancelWarningSubject', 'setStartValidation', 'isImportValidationStarted', 'downloadLog', 'getProjectId', 'setValidationResponse']);
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', ['getJobInformation', 'getJobResult']);
  taxonomiesServiceSpy.cancelWarningSubject = new Subject();
  let response = {object:{
    className: "innowake.mining.shared.model.TaxonomyImportValidationResult",
    overallResult: "NONE",
    markers: [{ 'test': 'testing' }]
  }};
  const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
  taxonomiesServiceSpy.taxonomyModalEventEmitter = new EventEmitter();
  const taxonomyControllerServiceSpy = jasmine.createSpyObj<TaxonomyControllerService>('TaxonomyControllerService', ['validateImportTaxonomy', 'importTaxonomy']);
  beforeEach(
    waitForAsync(() => {
      TestBed.configureTestingModule({
        declarations: [TaxonomiesModalComponent],
        imports: [TranslateModule.forRoot({}),
          NzModalModule, NzMessageModule,
          HttpClientTestingModule,
          BrowserAnimationsModule],
        providers: [
          { provide: NzMessageService, useValue: messageServiceSpy },
          { provide: NzModalRef, useValue: nzModalRefSpy },
          { provide: TaxonomyModalService, useValue: taxonomiesServiceSpy },
          { provide: NzMessageService, useValue: messageServiceSpy },
          { provide: TaxonomyControllerService, useValue: taxonomyControllerServiceSpy },
          { provide: JobControllerService, useValue: jobControllerServiceSpy },
          { provide: JobManagerService, useValue: jobManagerServiceSpy },
          TranslateService,
        ],
      }).compileComponents();
      let status = {
        "status$": of({
          "_isScalar": false,
          "closed": false,
          "isStopped": true,
          "hasError": false,
          "_value": "SUCCESS"
        })
      };
      const percentage = new BehaviorSubject<number>(10);

      jobManagerServiceSpy.register.and.returnValue(status as any);
      jobManagerServiceSpy.percent = percentage;
      jobControllerServiceSpy.getJobResult.and.returnValues(of(blob) as any);
      jobControllerServiceSpy.getJobInformation.and.returnValues(of(jobInfoSuccess as any));
      messageServiceSpy.success.and.returnValue(null);
      messageServiceSpy.warning.and.returnValue(null);
      messageServiceSpy.loading.and.returnValue(null);
      taxonomiesServiceSpy.triggerCancelWarningSubject.and.returnValue(of(WarningCancel.WARNING_CONTINUE));
      taxonomiesServiceSpy.validateImportedFile.and.returnValue(of(WarningCancel.WARNING_CANCEL));
      taxonomiesServiceSpy.isImportValidationStarted.and.returnValue(true);
      taxonomiesServiceSpy.downloadLog.and.returnValue(null);
      taxonomyControllerServiceSpy.validateImportTaxonomy.and.returnValue(of("b6da1c8b-67f1-45b1-89b5-ab495a574989" as any));
      taxonomyControllerServiceSpy.importTaxonomy.and.returnValue(of(validationResponse as any));
      
    })
  );

  beforeEach(() => {
    fixture = TestBed.createComponent(TaxonomiesModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next({});
  });

  it('should create', async(() => {
    expect(component).toBeTruthy();
  }));

  afterEach(() => {
    fixture.destroy();
  });

  it('should test onInit method with response Warning Cancel', () => {
    taxonomiesServiceSpy.cancelWarningSubject.next('Warning Cancel');
    const unsubscribe = taxonomiesServiceSpy.cancelWarningSubject.subscribe((response: string) => {
      expect(response).toBe('Warning Cancel');
      expect(component.toggleContentSection).toBeTruthy();
    });
    unsubscribe.unsubscribe();
  });

  it('should test onInit method with response Warning Continue', () => {
    taxonomiesServiceSpy.cancelWarningSubject.next(WarningCancel.WARNING_CONTINUE);
    const unsubscribe = taxonomiesServiceSpy.cancelWarningSubject.subscribe((response: string) => {
      expect(response).toBe(WarningCancel.WARNING_CONTINUE);
      expect(component.toggleImportBtn).toBeFalsy();
    });
    unsubscribe.unsubscribe();
  });

  it('should test onInit method with response Warning Continue', () => {
    taxonomiesServiceSpy.taxonomyModalEventEmitter.next(WarningCancel.POP_UP_CLOSED);
    component.importSubscription = true as any;
    expect(messageServiceSpy.warning).toHaveBeenCalled();
  });


  it('should test onValidationCancel method', async(() => {
    component.importSubscription = new Subscription();
    component.onValidationCancel();
    const response = messageServiceSpy.warning('Import Cancelled');
    expect(response).toBe(null);
  }));



  it('should test onValidation method  when validationBtn is equal to Start Import', () => {
    component.validationBtnTxt = 'Start Import';
    taxonomyControllerServiceSpy.importTaxonomy.and.returnValue(of(validationResponse as any));
    component.onValidation();
    expect(component.validationBtnTxt).toBeDefined();
  });

  it('should test onValidation method when validation response is warning', () => {
    let response = {"object":{
      overallResult: "WARNING", markers: [{ 'test': 'testing' }]
    }}
    const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
    component.validationBtnTxt = '';
    jobControllerServiceSpy.getJobResult.and.returnValues(of(blob) as any);
    component.onValidation();
    expect(component.toggleContentSection).toBeFalse();
  });

  it('should test onValidation method  when validation response is error', () => {
    let response = {object:{
      className: "innowake.mining.shared.model.TaxonomyImportValidationResult",
      overallResult: "ERROR",
      markers: [{ 'test': 'testing' }]
    }};
    const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
    jobControllerServiceSpy.getJobResult.and.returnValues(of(blob) as any);
    component.validationBtnTxt = '';
    component.onValidation();
    expect(component.showAll).toBeFalse();
  });


  it('should test onValidation method  when validation response is Success', () => {
    let response = {object:{
      className: "innowake.mining.shared.model.TaxonomyImportValidationResult",
      overallResult: "NONE",
      markers: [{ 'test': 'testing' }]
    }};
    const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
    jobControllerServiceSpy.getJobResult.and.returnValues(of(blob) as any);
    component.validationBtnTxt = '';
    component.onValidation();
    expect(component.toggleImportBtn).toBeFalse();
  });

  it('should test disableImportBtn method', () => {
    component.alertType = 'error';
    const response = component.disableImportBtn();
    expect(response).toBeTruthy();
  });

  it('should test onDownloadLog method', () => {
    component.onDownloadLog();
    expect(taxonomiesServiceSpy.downloadLog).toHaveBeenCalled();
  });

  it('should test onDownloadLog method', () => {
    taxonomyControllerServiceSpy.importTaxonomy.and.returnValue(of(validationResponse as any));
    component.onValidation();
    expect(component.loadingStateValidation).toBeFalse();
    expect(component.showMessage).toBeFalse();
  });

  it('should test onStop method', () => {
     component.importSubscription = new Subscription();
    component.onStop();
    expect(component.validationBtnTxt).toBe('taxonomyReportingComponent.startImport');
  });

  it('should test valadate method', () => {
    let value = { obj: "value" };
    expect(component.beforeUpload(value as any)).toBeFalsy();
  });

  it('should test valadate method', () => {
    (component as any).handleErrorCase(validationResponse);
    expect((component as any).responseToMessage(validationResponse as any, 1)).toBeDefined();
  });
});
