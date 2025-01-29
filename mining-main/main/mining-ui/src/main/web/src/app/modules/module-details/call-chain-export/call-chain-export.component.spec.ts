import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync, tick, fakeAsync } from '@angular/core/testing';
import { UntypedFormBuilder } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { I18nService } from '@app/core';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { Observable, Subject, of, throwError } from 'rxjs';
import { CallChainExportComponent } from './call-chain-export.component';
import { ElementRef, NO_ERRORS_SCHEMA } from '@angular/core';
import { ModuleValueChange } from './call-chain-export.interface';
import { ModuleDetails } from '@app/shared/components/module-listing/module-listing.interface';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { JobControllerService, ModuleControllerService } from '@innowake/mining-api-angular-client';
import { WindowToken } from '@app/core/utils/window';
import { model } from 'model-test-data';
class MockElementRef implements ElementRef {
    nativeElement = { class: 'end-select input' };
}

describe('CallChainExportComponent', () => {
    let component: CallChainExportComponent;
    let mockWindow: any;
    let openedUrl = '';
    let fixture: ComponentFixture<CallChainExportComponent>;
    let translateService: TranslateService;
    const i18nServiceSpy = { language: 'en-US' };
    const listModule = {
        "content": [{}],
        "pageable": {
            "sort": {
                "empty": true,
                "sorted": false,
                "unsorted": true
            },
            "offset": 0,
            "pageNumber": 0,
            "pageSize": 10,
            "paged": true,
            "unpaged": false
        },
        "totalElements": 0,
        "last": true,
        "totalPages": 0,
        "size": 10,
        "number": 0,
        "sort": {
            "empty": true,
            "sorted": false,
            "unsorted": true
        },
        "numberOfElements": 0,
        "first": true,
        "empty": true
    };

    const aggregationResult = model.aggregationResult;

    const graphQlResponse = { data: {
        modules: [
            {id: '2001', name: 'Module1'},
            {id: '2002', name: 'Module2'},
            {id: '2003', name: 'Module3'}
        ]
    }};

    const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);
    const jobControllerSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj<JobControllerService>('JobControllerService', ['getJobInformation', 'submitJobExtension', 'cancelJob']);
    const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj<ModuleControllerService>('ModuleControllerService', ['getAggregatedValues2']);
    const labelMappingServiceSpy: jasmine.SpyObj<LabelMappingService> = jasmine.createSpyObj('LabelMappingService', ['mapLabel'])
    const modalServiceSpy: jasmine.SpyObj<NzModalService> = jasmine.createSpyObj('NzModalService', ['create']);
    const messageServiceSpy: jasmine.SpyObj<NzMessageService> = jasmine.createSpyObj('NzMessageService', ['error']);
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'close', 'destroy']);
    const jobManagerSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['jobs$', 'jobNotification', 'getRemoteJob', 'register', 'jobExtension', 'subscribeToJobNotification'], ['jobs$']);
    const currentClient: ClientProjectRelationship = new ClientProjectRelationship(1, 'test', 1, 'test');
    const relationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService', ['getClientProjectObservable']);
    const moduleDetails: ModuleDetails = {
        "modulesId": [
            '11495',
            '11470',
            '6168'
        ],
        "modulesName": [
            "P446681",
            "P446681",
            "P446681"
        ],
        "selectedOption": "name",
        "repeatedModuleIds": [
            [
                11495,
                11470,
                6168
            ]
        ]
    }
    const modalRefStub = {
        afterClose() {
          return of(moduleDetails);
        }
      };

    const modalStub = { close: () => modalRefStub };

    class MockJobManagerService {
        jobNotification() {
            return of('SUCCESS');
        }
    }
    beforeEach(waitForAsync(() => {
        mockWindow = {
            get location() {
                return {
                    href: 'http://localhost:8081/#/browse-modules/1/1/1/explore',
                    hash: '#/browse-modules/1/1/1/explore'
                };
            },
            open: (sUrl: any) => {
                openedUrl = sUrl;
            }
        };
        mockWindow.open.bind(mockWindow);
        TestBed.configureTestingModule({
            declarations: [CallChainExportComponent],
            schemas: [NO_ERRORS_SCHEMA],
            providers: [
                UntypedFormBuilder,
                NzMessageService,
                NumberFormatter,
                TranslateService,
                {provide: JobControllerService, useValue: jobControllerSpy},
                {provide: FeatureToggleService, useValue: featureToggleServiceSpy},
                {provide: ModuleControllerService, useValue: moduleControllerServiceSpy},
                { provide: LabelMappingService, useValue: labelMappingServiceSpy },
                { provide: NzModalService, useValue: modalServiceSpy },
                { provide: NzMessageService, useValue: messageServiceSpy },
                { provide: I18nService, useValue: i18nServiceSpy },
                { provide: NzModalRef, useValue: modalStub },
                { provide: JobManagerService, useValue: jobManagerSpy },
                { provide: ElementRef, useValue: new MockElementRef() },
                {provide: ClientProjectRelationshipService, useValue: relationshipServiceSpy},
                { provide: WindowToken, useValue: mockWindow },
            ],
            imports: [
                NzDropDownModule,
                RouterTestingModule,
                HttpClientTestingModule,
                BrowserAnimationsModule,
                TranslateModule.forRoot({}),
            ],
        }).compileComponents();
        TestBed.compileComponents();
        featureToggleServiceSpy.isActive.and.returnValue(of(false));
        (Object.getOwnPropertyDescriptor(jobManagerSpy, 'jobs$').get as jasmine.Spy).and.returnValue(of([]));
        jobControllerSpy.submitJobExtension.and.returnValue(of('test' as any));
        jobControllerSpy.getJobInformation.and.returnValue(of('some-job-id') as Observable<any>);
        moduleControllerServiceSpy.getAggregatedValues2.and.returnValues(of(aggregationResult as any), of([] as any));
        jobControllerSpy.cancelJob.and.returnValues(of({} as any));
        modalServiceSpy.create.and.returnValue(nzModalRefSpy);
        relationshipServiceSpy.getClientProjectObservable.and.returnValue(of(currentClient as any));
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(CallChainExportComponent);
        component = fixture.componentInstance;
        translateService = TestBed.inject(TranslateService);
        nzModalRefSpy.afterClose = new Subject();
        nzModalRefSpy.afterClose.next(of(moduleDetails));
        jobManagerSpy.jobResult = new Subject;
        jobManagerSpy.jobResult.next(null);
        jobManagerSpy.jobStatus = new Subject;
        jobManagerSpy.jobStatus.next(null);
        fixture.detectChanges();
    });

    it('should create component', () => {
        expect(component).toBeTruthy();
    });

    it('should test direction value', () => {
        component.setDefault();
        expect(component.directionValue.value).toEqual(['OUT']);
    });

    it('should test moduleChange method', () => {
        let event: ModuleValueChange = {
            startModule: ['Module1', 'Module2'],
            endModule: ['-1'],
            moduleDetails: [{label: 'Module1', value: [2001]}],
            selectedRadio: 'name'

        }
        component.moduleChange(event);
        expect(component.selectedStartRadio).toBe('name');
    });

    it('should test subscribeToJobNotification', () => {
        jobManagerSpy.jobResult.next(true);
        jobManagerSpy.jobStatus.next(false);
        component.subscribeToJobNotification();
        expect(component.disableBtn).toBeFalsy();
        expect(component.loadingBtn).toBeFalsy();
    });

    it('should test startCallChain', () => {
        component.startCallChainJob('DEPENDENCY_GRAPH');
        expect(component.loadingBtn).toBe(true);
    });

    it('should test startCallChain else case', () => {
        component.startCallChainJob('CALL_CHAIN_GRAPH');
        expect(component.disableBtn).toBeTruthy();
        expect(component.loadingBtn).toBeFalsy();
    });

    it('should test setModuleTypeOptions error case', () => {
        component.hiddenFromCallChainLoading = true;
        moduleControllerServiceSpy.getAggregatedValues2.and.returnValues(throwError(new Error('Test Error')));
        component.ngOnInit();
        expect(component.hiddenFromCallChainLoading).toBeFalsy();
    });

    it('should do nothing if there is no active job', () => {
        component['exportJob'] = undefined;
        component.cancelCallChainJob();
        expect(jobControllerSpy.cancelJob).not.toHaveBeenCalled();
    });

    it('should cancel an active job', fakeAsync(() => {
        component.cancelCallChainJob();
        tick();
        expect(component.isJobCancelDisabled).toBeTruthy();
        expect(component.onCancelLoading).toBeFalsy();
    }));

    it('should open module in new browser tab', () => {
      let btnType = "Error";
      component.projectId = 3;
      component.redirectToModule(btnType);
      expect(openedUrl).toContain('modules?preFilter=');
    });
})
