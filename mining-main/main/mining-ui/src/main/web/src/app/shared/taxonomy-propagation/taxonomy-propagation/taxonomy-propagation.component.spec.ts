import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import { BehaviorSubject, Observable, Subject, Subscription } from 'rxjs';
import { of } from 'rxjs/internal/observable/of';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { EventEmitter } from '@angular/core';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { TaxonomyPropagationComponent } from './taxonomy-propagation.component';
import { JobControllerService, JobInformation, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { CustomizableTableQueryParameter } from '@app/core/services/user-customizable-table/customizable-table-query-parameters.interface';
import { update } from 'lodash';
import { subscribe } from 'graphql';

const validationResponse = { overallResult: "SUCCESS", markers: [{ 'test': 'testing' }] };

describe('TaxonomyPropagationComponent', () => {
  let component: TaxonomyPropagationComponent;
  let fixture: ComponentFixture<TaxonomyPropagationComponent>;
  const nzModalRefStub = { 
    close: () => { },
    afterClose: {
      subscribe: () => { }
    },
    updateConfig: () => { },
  };
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
  let response = {object:{
    className: "innowake.mining.shared.model.TaxonomyImportValidationResult",
    overallResult: "NONE",
    markers: [{ 'test': 'testing' }]
  }};
  const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
  taxonomiesServiceSpy.taxonomyModalEventEmitter = new EventEmitter();
  const taxonomyControllerServiceSpy = jasmine.createSpyObj<TaxonomyControllerService>('TaxonomyControllerService', ['validateImportTaxonomy', 'importTaxonomy']);
  
  const userCustomizableTableServiceSpy = jasmine.createSpyObj<CustomizableTableColumnService>('CustomizableTableColumnService', [
    'getSelectedDataPoints',
    'setColumnIdsSelection'
  ]);
  const identifiedModules: Array<Record<string, any>> = [
    { moduleId: 1 },
    { moduleId: 2 },
    { moduleId: 3 },
  ];
  
  beforeEach(
    waitForAsync(() => {
      TestBed.configureTestingModule({
        declarations: [TaxonomyPropagationComponent],
        imports: [TranslateModule.forRoot({}),
          NzModalModule, NzMessageModule,
          HttpClientTestingModule,
          BrowserAnimationsModule],
        providers: [
          { provide: NzMessageService, useValue: messageServiceSpy },
          { provide: NzModalRef, useValue: nzModalRefStub },
          { provide: TaxonomyControllerService, useValue: taxonomyControllerServiceSpy },
          { provide: JobControllerService, useValue: jobControllerServiceSpy },
          { provide: JobManagerService, useValue: jobManagerServiceSpy },
          { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
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
      taxonomiesServiceSpy.isImportValidationStarted.and.returnValue(true);
      taxonomiesServiceSpy.downloadLog.and.returnValue(null);
      taxonomyControllerServiceSpy.validateImportTaxonomy.and.returnValue(of("b6da1c8b-67f1-45b1-89b5-ab495a574989" as any));
      taxonomyControllerServiceSpy.importTaxonomy.and.returnValue(of(validationResponse as any));
      userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of([]));
      userCustomizableTableServiceSpy.setColumnIdsSelection.and.returnValue(null);
    })
  );

  beforeEach(() => {
    fixture = TestBed.createComponent(TaxonomyPropagationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', async(() => {
    expect(component).toBeTruthy();
  }));

  it('should return true if all identified modules are in the start modules', () => {
    const startModules: number[] = [1, 2, 3];
    const result = component['checkIfOnlyStartModulesIdentified'](identifiedModules, startModules);
    expect(result).toBe(true);
  });

  it('should return false if not all identified modules are in the start modules', () => {
    const startModules: number[] = [1, 2];
    const result = component['checkIfOnlyStartModulesIdentified'](identifiedModules, startModules);
    expect(result).toBe(false);
  });

  it('should return false if identified modules is empty', () => {
    const startModules: number[] = [1, 2, 3];
    const result = component['checkIfOnlyStartModulesIdentified']([], startModules);
    expect(result).toBe(false);
  });

  it('should return false if identified modules is null', () => {
    const startModules: number[] = [1, 2, 3];
    const result = component['checkIfOnlyStartModulesIdentified'](null, startModules);
    expect(result).toBe(false);
  });

  it('should return false if identified modules length is not equal to start modules length', () => {
    const startModules: number[] = [1, 2, 3, 4];
    const result = component['checkIfOnlyStartModulesIdentified'](identifiedModules, startModules);
    expect(result).toBe(false);
  });

  it('should handle parent component table', () => {
    const params: CustomizableTableQueryParameter = {
      filter: [],
      page: 1,
      sort: 'asc'
    };
    component.parentTableParams = params;
    component.parentDefaultColumns = ['Column1', 'Column2'];
    component.parentSelectedColumns = ['Column1'];

    spyOn(component['parametersService'], 'setParameters');

    component['handleParentComponentTable']();
    expect(component['parametersService'].setParameters).toHaveBeenCalledWith(params.filter, params.page, params.sort);
    expect(component['userCustomizableTable'].setColumnIdsSelection).toHaveBeenCalledWith(component.parentSelectedColumns);
    expect(component['userCustomizableTable'].defaultColumnsToShow).toEqual(component.parentDefaultColumns);
  });
});
