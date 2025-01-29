import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AnnotationImportResult, AnnotationsImportModalComponent } from './annotations-import-modal.component';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { NzModalModule, NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BehaviorSubject } from 'rxjs/internal/BehaviorSubject';
import { of, throwError } from 'rxjs';
import { JobControllerService, AnnotationControllerService, JobInformation } from '@innowake/mining-api-angular-client';

describe('AnnotationsImportModalComponent', () => {
  let component: AnnotationsImportModalComponent;
  let fixture: ComponentFixture<AnnotationsImportModalComponent>;
  let jobControllerService = jasmine.createSpyObj<JobControllerService>('JobControllerService', [
    'getJobResult',
    'getJobInformation'
  ]);
  let jobManagerService = jasmine.createSpyObj<JobManagerService>('JobManagerService', [
    'percent',
    'register'
  ]);
  let messageService: NzMessageService = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'success',
    'warning',
    'remove',
    'error',
    'loading',
  ]);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
  let annotationControllerService = jasmine.createSpyObj<AnnotationControllerService>('AnnotationControllerService', ['importAnnotations']);

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AnnotationsImportModalComponent ],
      imports: [TranslateModule.forRoot({}),
        NzModalModule, NzMessageModule,
        HttpClientTestingModule,
        BrowserAnimationsModule],
      providers: [
        { provide: NzMessageService, useValue: messageService },
          { provide: NzModalRef, useValue: nzModalRefSpy },
          { provide: AnnotationControllerService, useValue: annotationControllerService },
          { provide: NzMessageService, useValue: messageService },
          { provide: JobControllerService, useValue: jobControllerService },
          { provide: JobManagerService, useValue: jobManagerService },
          TranslateService,
      ]
    })
    .compileComponents();
    /* commmon test setup */
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
    jobManagerService.register.and.returnValue(status as any);
    jobManagerService.percent = percentage;
    const jobInfoSuccess: JobInformation = { status: JobInformation.StatusEnum.SUCCESS };
    jobControllerService.getJobInformation.and.returnValues(of(jobInfoSuccess as any));
    annotationControllerService.importAnnotations.and.returnValue(of("b6da1c8b-67f1-45b1-89b5-ab495a574989" as any));

  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AnnotationsImportModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with correct default values', () => {
    expect(component.showInitialContent).toBeTrue();
    expect(component.loading).toBeFalse();
    expect(component.finished).toBeFalse();
    expect(component.titleIconType).toEqual('loading');
  });

  it('should test on import returning success',  (done) => {
    
    /* specific to component */
    let response: AnnotationImportResult = {};
    response.totalLines = 4
    response.errors = [];
    const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
    jobControllerService.getJobResult.and.returnValues(of(blob) as any);
    
    component.onImport();
    expect(component.showInitialContent).toBeFalsy();
    expect(component.loading).toBeTruthy();
    expect(jobManagerService.register).toHaveBeenCalled();
    expect(jobControllerService.getJobResult).toHaveBeenCalled();
    setTimeout(() => {
      expect(component.importResult.errors.length).toBe(0);
      expect(component.loading).toBeFalsy();
      expect(component.finished).toBeTruthy();
      expect(component.titleIconType).toBe('check-circle');
      expect(component.alertType).toBe('success');
      expect(component.finalMessage).toBe('annotationReporting.importSuccess');
      done();
    }, 1000);
  });

  it('should test on import returning warning',  (done) => {
    
    /* specific to component */
    let response: AnnotationImportResult = {};
    response.totalLines = 4
    response.errors = ['line 1','line 2'];
    const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
    jobControllerService.getJobResult.and.returnValues(of(blob) as any);
    
    component.onImport();
    expect(component.showInitialContent).toBeFalsy();
    expect(component.loading).toBeTruthy();
    expect(jobManagerService.register).toHaveBeenCalled();
    expect(jobControllerService.getJobResult).toHaveBeenCalled();
    setTimeout(() => {
      expect(component.importResult.errors.length).toBe(2);
      expect(component.errorMessage).toBe('line 1\nline 2');
      expect(component.titleIconType).toBe('warning');
      expect(component.alertType).toBe('warning');
      expect(component.finalMessage).toBe('annotationReporting.importPartialSuccess');
      done();
    }, 1000);
  });

  it('should test on import returning error',  (done) => {
    
    /* specific to component */
    let response: AnnotationImportResult = {};
    response.totalLines = 2
    response.errors = ['line 1','line 2'];
    const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
    jobControllerService.getJobResult.and.returnValues(of(blob) as any);
    
    component.onImport();
    expect(component.showInitialContent).toBeFalsy();
    expect(component.loading).toBeTruthy();
    expect(jobManagerService.register).toHaveBeenCalled();
    expect(jobControllerService.getJobResult).toHaveBeenCalled();
    setTimeout(() => {
      expect(component.importResult.errors.length).toBe(2);
      expect(component.errorMessage).toBe('line 1\nline 2');
      expect(component.titleIconType).toBe('close-circle');
      expect(component.alertType).toBe('error');
      expect(component.finalMessage).toBe('annotationReporting.importFailure');
      done();
    }, 1000);
  });

  it('should test on import returning system error',  (done) => {
    
    /* specific to component */
    jobControllerService.getJobResult.and.returnValues(throwError({status: 404}));
    
    component.onImport();
    expect(component.showInitialContent).toBeFalsy();
    expect(component.loading).toBeTruthy();
    expect(jobManagerService.register).toHaveBeenCalled();
    expect(jobControllerService.getJobResult).toHaveBeenCalled();
    setTimeout(() => {
      expect(component.titleIconType).toBe('close-circle');
      expect(component.alertType).toBe('error');
      expect(component.finalMessage).toBe('annotationReporting.importJobFailed');
      expect(component.importResult).toBeDefined();
      expect(component.importResult.totalLines).toBe(0);
      expect(component.importResult.errors).toBeDefined();
      done();
    }, 1000);
  });

  it('should test on import with more than 50 error items',  (done) => {
    
    /* specific to component */
    let response: AnnotationImportResult = {};
    response.totalLines = 4
    response.errors = [];
    for(let i = 1; i <= 52; i++) {
      response.errors.push(i.toString());
    }
    const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
    jobControllerService.getJobResult.and.returnValues(of(blob) as any);
    
    component.onImport();
    expect(component.showInitialContent).toBeFalsy();
    expect(component.loading).toBeTruthy();
    expect(jobManagerService.register).toHaveBeenCalled();
    expect(jobControllerService.getJobResult).toHaveBeenCalled();
    setTimeout(() => {
      expect(component.importResult.errors.length).toBe(response.errors.length);
      expect(component.errorMessage).toContain('...');
      expect(component.errorMessage).toContain('1\n');
      expect(component.errorMessage).toContain('50\n');
      expect(component.errorMessage).not.toContain('51');
      done();
    }, 1000);
  });

});
