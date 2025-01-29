import { HttpClientTestingModule } from '@angular/common/http/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { SharedModule } from '@app/shared';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import {Location} from '@angular/common';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { HttpClient } from '@angular/common/http';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ModuleErrorsComponent } from './module-errors.component';
import { AnnotationControllerService, DataDictionaryControllerService, FeatureControllerService, ModuleControllerService, ProjectControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { of } from 'rxjs';

describe('ModuleErrorsComponent', () => {
  let component: ModuleErrorsComponent;
  let fixture: ComponentFixture<ModuleErrorsComponent>;
  const i18nServiceSpy = { language: 'en-US' };
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'get', 'request']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
    'GraphQlControllerService',
    ['graphQl']
  );
  const jobManagerServiceSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['register']);
  const dataDictionaryControllerServiceSpy: jasmine.SpyObj<DataDictionaryControllerService> = jasmine.createSpyObj('DataDictionaryControllerService', [
    'updateDataDictionaryEntry', 'deleteDataDictionaryEntry', 'findDataDictionaryEntryByRecordId'
  ]);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [RouterTestingModule,
        SharedModule,
        TranslateModule.forRoot({}),
        HttpClientTestingModule,
        RouterTestingModule,
        BrowserAnimationsModule
      ],
      declarations: [ModuleErrorsComponent],
      schemas: [CUSTOM_ELEMENTS_SCHEMA],
      providers: [
        Location,
        TranslateService,
        FeatureControllerService,
        NumberFormatter,
        ModuleControllerService,
        AnnotationControllerService,
        ProjectControllerService,
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        {provide: DataDictionaryControllerService, useValue: dataDictionaryControllerServiceSpy},
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        TaxonomyControllerService
      ]
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
    jobManagerServiceSpy.register.and.returnValue(status as any);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleErrorsComponent);
    component = fixture.componentInstance;
    component.projectId = 1;
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should update isErrorMarker when showCfgMetaDataDrawer is called', () => {
    const event = true;
    component.showCfgMetaDataDrawer(event);
    expect(component.isErrorMarker).toBe(event);
  });

  it('should open side panel code view and emit row selection', () => {
    const rowData: any = {
      location: {
        rootRelativeOffset: 0
      },
      errorText: 'Sample error text'
    };
    component.openSidePanelCodeView(rowData);
    expect(component.isErrorMarker).toBe(true);
    expect(component.rowSelection.next).toHaveBeenCalled;
  });
});
