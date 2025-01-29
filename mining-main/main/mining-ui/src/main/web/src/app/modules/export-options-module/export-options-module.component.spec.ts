import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { ExportOptionsModuleComponent, EXPORT_TOKEN } from './export-options-module.component';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { Observable, of, Subject } from 'rxjs';
import { HttpTestingController, HttpClientTestingModule } from '@angular/common/http/testing';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { RemoteJob } from '@app/core/services/job-manager/job-manager-service.interface';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { ExportDownloadService } from '../../core/services/export-download/export-download.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NzModalModule } from 'ng-zorro-antd/modal';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { UntypedFormBuilder } from '@angular/forms';
import { ExportParameter } from './export-options-module.interface';
import { CustomPropertyMetadata, ExportFormatDescription, IoControllerService, JobControllerService } from '@innowake/mining-api-angular-client';


describe('ExportOptionsModuleComponent', () => {
  let component: ExportOptionsModuleComponent;
  let fixture: ComponentFixture<ExportOptionsModuleComponent>;
  let httpMock: HttpTestingController;
  const formBuilder: UntypedFormBuilder = new UntypedFormBuilder();

  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['success', 'loading', 'remove', 'error']);
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(0, 'TestClient', 0, 'TestProject');
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService', [
    'getClientProjectObservable',
    'setClientProjectRelationship'
  ]);
  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', [
    'register'
  ], [
    'jobs$'
  ]);
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', [
    'submitJobExtensionV2'
  ]);
  const exportFormats: ExportFormatDescription[] = [
    {
      "id": "csv",
      "description": "CSV Data",
      "extensionType": "EXPORT_EXTENSION",
      "requiredRole": "VIEWER",
      "requiredNature": "MINING",
      "parameterDescriptions": [],
      "showOnExportPage": { "show": true, "category": "test", "label": "CSV Data" },
      "uploadDescription": {
        "name": "Test File",
        "description": "Test the file export functionality",
        "required": false,
        "accept": "image/jpg, image/jpeg, image/png",
        "supported": true
      }
    },
    {
      "id": "jpeg",
      "description": "CSV Data",
      "extensionType": "EXPORT_EXTENSION",
      "requiredRole": "VIEWER",
      "requiredNature": "MINING",
      "parameterDescriptions": [],
      "showOnExportPage": { "show": true, "category": "", "label": "CSV Data" },
      "uploadDescription": {
        "name": "Test File",
        "description": "Test the file export functionality",
        "required": false,
        "accept": "image/jpg, image/jpeg, image/png",
        "supported": true
      }
    }
  ]
  const ioControllerSpy: jasmine.SpyObj<IoControllerService> = jasmine.createSpyObj<IoControllerService>('IoControllerService',
  ['getExportFormats']);
  const authServiceSpy = new NoAuthorizationService();
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ExportOptionsModuleComponent ],
      imports: [
        NzModalModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({})
      ],
      providers: [
        NzDrawerService,
        TranslateService,
        UntypedFormBuilder,
        ExportDownloadService,
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: IoControllerService, useValue: ioControllerSpy },
        { provide: KeycloakAuthorizationService, useValue: authServiceSpy }
      ]
    })
    .compileComponents();
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));

    /* this awkwardness is per the documentation: https://jasmine.github.io/tutorials/spying_on_properties */
    (Object.getOwnPropertyDescriptor(jobManagerServiceSpy, 'jobs$').get as jasmine.Spy).and.returnValue(of([]));

    jobControllerServiceSpy.submitJobExtensionV2.and.returnValue(of('some-job-id') as Observable<any>);

    messageServiceSpy.loading.and.returnValue({ messageId: 'some-message-id', onClose: new Subject() })
    messageServiceSpy.success.and.returnValue({ messageId: 'some-message-id', onClose: new Subject() })
    messageServiceSpy.error.and.returnValue({ messageId: 'some-message-id', onClose: new Subject() })
    ioControllerSpy.getExportFormats.and.returnValue(of(exportFormats as any));
    spyOn(authServiceSpy, 'hasUserRole').and.returnValue(true);
    spyOn(authServiceSpy, 'hasProjectNature').and.returnValue(true);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ExportOptionsModuleComponent);
    component = fixture.componentInstance;
    httpMock = TestBed.inject(HttpTestingController);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should have job of format', async () => {
    const jobObservable: Observable<Array<Partial<RemoteJob>>> = of([
      {
        jobId: 'foo',
        uiToken: EXPORT_TOKEN + 'csv'
      }
    ]);
    (Object.getOwnPropertyDescriptor(jobManagerServiceSpy, 'jobs$').get as jasmine.Spy).and.returnValue(jobObservable);

    const hasJob = await component.hasRunningJobOfFormat('csv').toPromise();
    expect(hasJob).toBeTrue();
  });

  it('should NOT have job of format', async () => {
    const jobObservable: Observable<Array<Partial<RemoteJob>>> = of([
      {
        jobId: 'foo',
        uiToken: EXPORT_TOKEN + 'excel'
      }
    ]);
    (Object.getOwnPropertyDescriptor(jobManagerServiceSpy, 'jobs$').get as jasmine.Spy).and.returnValue(jobObservable);

    const hasJob = await component.hasRunningJobOfFormat('csv').toPromise();
    expect(hasJob).toBeFalse();
  });

  it('should request export of format and display success', () => {
    component.exportToFormat('some-test-format', {});

    const request = httpMock.expectOne(getBasePath() + '/api/v1/projects/0/export/some-test-format');

    expect(messageServiceSpy.loading).toHaveBeenCalled();

    request.flush(new Blob(), {
      status: 200,
      statusText: 'OK',
      headers: {
        'content-disposition': 'attachment; filename=some_file_name.csv'
      }
    });

    expect(messageServiceSpy.success).toHaveBeenCalled();
  });

  it('should request export of format and display error', () => {
    component.exportToFormat('some-test-format', {});

    const request = httpMock.expectOne(getBasePath() + '/api/v1/projects/0/export/some-test-format');

    expect(messageServiceSpy.loading).toHaveBeenCalled();

    request.flush(new Blob(), {
      status: 500,
      statusText: 'FAIL, lol',
      headers: {
        'content-disposition': 'attachment; filename=some_file_name.csv'
      }
    });

    expect(messageServiceSpy.error).toHaveBeenCalled();
  });

  it('should call exportExtensions', () => {
    const groupDetail = {
      'extensionType': 'EXPORT_EXTENSION',
      'id': 'csv',
      'label': 'test',
      'parameterDescriptions': [{}],
      'uploadDescription': {
        'name': 'Test File',
        'description': 'Test the file export functionality',
        'required': false,
        'accept': 'image/jpg, image/jpeg, image/png',
        'supported': false
      }
    }
    spyOn(component,'exportExtensions').and.callThrough();
    component.exportExtensions(groupDetail as any);
    expect(component.exportExtensions).toHaveBeenCalled();
  });

  it('should call else exportExtensions', () => {
    const groupDetail = {
      'extensionType': 'JOB_EXTENSION',
      'id': 'csv',
      'label': 'test',
      'parameterDescriptions': [{}],
      'uploadDescription': {
        'name': 'Test File',
        'description': 'Test the file export functionality',
        'required': false,
        'accept': 'image/jpg, image/jpeg, image/png',
        'supported': true
      }
    }
    spyOn(component,'exportExtensions').and.callThrough();
    component.exportExtensions(groupDetail as any);
    expect(component.exportExtensions).toHaveBeenCalled();
  });

  it('should submit export form', () => {
    component.exportForm = formBuilder.group({
      'repeaterString' : [['Repeater1', 'Repeater2']],
      'stringParameter' : ['Test string']
    });
    const exportRepeaterJobData = [{
      name: 'repeaterString',
      label: 'repeaterString',
      dataType: CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST,
      inputName: 'repeaterString',
      id: 'csv',
      jobLabel: 'test',
      extensionType: 'JOB_EXTENSION'
    }];
    const exportStringJobData = [{
      name: 'stringParameter',
      label: 'stringParameter',
      dataType: CustomPropertyMetadata.DataTypeEnum.STRING,
      inputName: 'stringParameter',
      id: 'csv',
      jobLabel: 'test',
      extensionType: 'JOB_EXTENSION'
    }];
    const exportRepeaterExportData = [{
      name: 'repeaterString',
      label: 'repeaterString',
      dataType: CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST,
      inputName: 'repeaterString',
      id: 'csv',
      jobLabel: 'test',
      extensionType: 'EXPORT_EXTENSION'
    }];
    const exportStringExportData = [{
      name: 'stringParameter',
      label: 'stringParameter',
      dataType: CustomPropertyMetadata.DataTypeEnum.STRING,
      inputName: 'stringParameter',
      id: 'csv',
      jobLabel: 'test',
      extensionType: 'EXPORT_EXTENSION'
    }];

    spyOn(component, 'exportToFormat').and.callThrough();
    component.exportData = exportRepeaterExportData as ExportParameter[];
    component.onSubmit();
    expect(component.exportToFormat).toHaveBeenCalled();
    component.exportData = exportStringExportData as ExportParameter[];
    component.onSubmit();
    expect(component.exportToFormat).toHaveBeenCalled();

    spyOn((component as any), 'invokeExportJob').and.callThrough();
    component.exportData = exportRepeaterJobData as ExportParameter[];
    component.onSubmit();
    expect((component as any).invokeExportJob).toHaveBeenCalled();
    component.exportData = exportStringJobData as ExportParameter[];
    component.onSubmit();
    expect((component as any).invokeExportJob).toHaveBeenCalled();
  });

  it('should cancel form', () => {
    component.onCancel();
    expect(component.isExportCancel).toBeFalsy();
    expect(component.isExportSave).toBeFalsy();
  });

  it('should tests canSubmitForm method', () => {
    component.exportForm = formBuilder.group({
      stringParameter: 'Test Description'
    });
    expect(component.canSubmitForm()).toBe(component.exportForm.valid && component.exportForm.dirty);
  });

  it('should check export based on extension type', () => {
    spyOn((component as any), 'exportToFormat').and.callThrough();
    (component as any).triggerExport(ExportFormatDescription.ExtensionTypeEnum.EXPORT_EXTENSION, 'xls', 'excel', {});
    expect((component as any).exportToFormat).toHaveBeenCalled();
    spyOn((component as any), 'invokeExportJob').and.callThrough();
    (component as any).triggerExport(ExportFormatDescription.ExtensionTypeEnum.JOB_EXTENSION, 'csv', 'csv', {});
    expect((component as any).invokeExportJob).toHaveBeenCalled();
  });
});
