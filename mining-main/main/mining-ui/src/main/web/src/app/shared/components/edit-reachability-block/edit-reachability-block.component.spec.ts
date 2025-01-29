import { ComponentFixture, TestBed } from "@angular/core/testing";
import { BehaviorSubject, of, throwError } from "rxjs";
import { EditReachabilityBlockComponent } from "./edit-reachability-block.component";
import { ReactiveFormsModule } from "@angular/forms";
import { TranslateModule } from "@ngx-translate/core";
import { NzMessageService } from "ng-zorro-antd/message";
import { NzModalRef, NzModalService } from "ng-zorro-antd/modal";
import { FunctionalBlockControllerService, FunctionalBlockPojoPrototype, JobControllerService, JobInformation } from "@innowake/mining-api-angular-client";
import { GraphQlControllerService } from "@app/core/services/graphql.service";
import { BaseEditFunctionalBlockComponent, RESULT_EDITED } from "../base-edit-functional-block/base-edit-functional-block.component";
import { FeatureToggleService } from "@app/core/services/feature-toggle/feature-toggle.service";
import { JobManagerService } from "@app/core/services/job-manager/job-manager.service";

describe('EditReachabilityBlockComponent', () => {
  let component: EditReachabilityBlockComponent;
  let fixture: ComponentFixture<EditReachabilityBlockComponent>;

  const functionalBlockController: jasmine.SpyObj<FunctionalBlockControllerService> = jasmine.createSpyObj('FunctionalBlockControllerService', [
    'generateReachabilityBlockDescription',
    'updateFunctionalBlock'
    
  ]);
  const nzMessageServiceStub: jasmine.SpyObj<NzMessageService> = jasmine.createSpyObj<NzMessageService>
    ('NzMessageService', ['create', 'error']); 

  const graphQlControllerService: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService', [
      'graphQl'
    ]);
  const nzModalSpy: jasmine.SpyObj<NzModalRef> = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['destroy']);
  const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);

  const jobControllerServiceSpy = jasmine.createSpyObj('JobControllerService', [ 'submitJobExtension', 'getJobInformation', 'getJobResult', 'cancelJob' ]);
  const modalServiceSpy = jasmine.createSpyObj('NzModalService', [ 'create' ]);
  const jobManagerServiceSpy = jasmine.createSpyObj('JobManagerService', [ 'register' ], [ 'jobNotification' ]);

  const functionalBlockMock: { uid: string; name: string; description: string } = {
    uid:'f35erfa5DF4efd',
    name: 'Mocked Functional Block',
    description: 'Test Data'
  };
  
  const array = ['a','b','c','d','e','f','g']

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [EditReachabilityBlockComponent, BaseEditFunctionalBlockComponent],
      imports: [ReactiveFormsModule, TranslateModule.forRoot({})],
      providers: [
        { provide: NzModalRef, useValue: nzModalSpy},
        { provide: FunctionalBlockControllerService, useValue: functionalBlockController },
        { provide: NzMessageService, useValue: nzMessageServiceStub },
        { provide: GraphQlControllerService, useValue: graphQlControllerService },
        { provide: FeatureToggleService, useValue: featureToggleServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy},
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy}
      ]
    });
    functionalBlockController.updateFunctionalBlock.and.returnValue(of({} as any));
    functionalBlockController.generateReachabilityBlockDescription.and.returnValue(of (array as any));
    graphQlControllerService.graphQl.and.returnValue(of({} as any));
    featureToggleServiceSpy.isActive.and.returnValue(of(false));
    jobControllerServiceSpy.submitJobExtension.and.returnValue(of({jobId: '123'} as any));
    fixture = TestBed.createComponent(EditReachabilityBlockComponent);
    component = fixture.componentInstance;
    component.projectId = 1;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should have fetched the functional block', () => {
    graphQlControllerService.graphQl.and.returnValue(of({ data: { functionalBlocks: { content: [functionalBlockMock]}}} as any));
    component.reachabilityBlockUid = functionalBlockMock.uid;
    component.ngOnInit();
    expect(component.functionalBlock).toBe(functionalBlockMock);
  });

  it('should save the functional block with success', () => {
    component.functionalBlock = functionalBlockMock;
    component.formGroup.get('name').setValue('Updated Name');
    component.formGroup.get('description').setValue('Updated Description');
    component.formGroup.markAsDirty();
    component.onSave();
    const expectedRequest: FunctionalBlockPojoPrototype =
    {
      name: 'Updated Name',
      description: 'Updated Description'
    };
    expect(functionalBlockController.updateFunctionalBlock).toHaveBeenCalledWith(1, functionalBlockMock.uid, expectedRequest);
    expect(nzMessageServiceStub.create).toHaveBeenCalledWith('success', 'reachability.editSuccess')
    expect(nzModalSpy.destroy).toHaveBeenCalledWith(RESULT_EDITED);
  });

  it('should try to save the functional block with error', () => {
    component.functionalBlock = functionalBlockMock;
    functionalBlockController.updateFunctionalBlock.and.returnValue(throwError(() => new Error('500')))
    component.onSave();
    expect(functionalBlockController.updateFunctionalBlock).toHaveBeenCalled();
    expect(nzMessageServiceStub.create).toHaveBeenCalledWith('error', 'reachability.editError')
  });

  it('should submit job to generate reachability block description with module descriptions', () => {
    component.functionalBlock = functionalBlockMock;
    component.generateModuleDescriptions = true;
    component.generateDescription();
    expect(jobControllerServiceSpy.submitJobExtension).toHaveBeenCalledWith(1, 'generate-single-reachability-block-description', {
      'uids': [functionalBlockMock.uid], 'generateModuleDescriptions': ['true']});
  });

  it('should submit job to generate reachability block description without module descriptions', () => {
    component.functionalBlock = functionalBlockMock;
    component.generateModuleDescriptions = false;
    component.generateDescription();
    expect(jobControllerServiceSpy.submitJobExtension).toHaveBeenCalledWith(1, 'generate-single-reachability-block-description', {
      'uids': [functionalBlockMock.uid], 'generateModuleDescriptions': ['false']});
  });

  it('should show error message when submitting job fails', () => {
    component.functionalBlock = functionalBlockMock;
    jobControllerServiceSpy.submitJobExtension.and.returnValue(throwError(() => new Error('500')));
    component.generateDescription();
    expect(nzMessageServiceStub.error).toHaveBeenCalledWith('job.jobSubmitError500')
    expect(component.isGenerating).toBeFalse();
  });

  it('should get job result when job is successful', () => {
    const jobId = '123';
    const generatedDescription = 'generated description';
    component.generateJob = {
      jobId: jobId,
      autoDownloadResult: false,
      foreground: true,
      cancellable: true,
      label: 'Test Job',
      status$: new BehaviorSubject(JobInformation.StatusEnum.RUNNING)
    };

    const subject: BehaviorSubject<BehaviorSubject<JobInformation>> = new BehaviorSubject<BehaviorSubject<JobInformation>>(
      new BehaviorSubject<JobInformation>({
      jobId: jobId,
      status: JobInformation.StatusEnum.SUCCESS
    }));
    Object.defineProperty(jobManagerServiceSpy, 'jobNotification', subject);

    jobControllerServiceSpy.getJobResult.and.returnValue(of({
      object: '{"description": "' + generatedDescription + '"}'
    }));

    component.ngOnInit();

    expect(jobControllerServiceSpy.getJobResult).toHaveBeenCalledWith(jobId);
    expect(component.formGroup.get('description').value).toBe(generatedDescription);
    expect(component.isGenerating).toBeFalse();
    expect(component.isGenerationSuccessful).toBeTrue();
  });

  it('should handle failing job', () => {
    const jobId = '321';
    component.generateJob = {
      jobId: jobId,
      autoDownloadResult: false,
      foreground: true,
      cancellable: true,
      label: 'Test Job',
      status$: new BehaviorSubject(JobInformation.StatusEnum.RUNNING)
    };

    const subject: BehaviorSubject<BehaviorSubject<JobInformation>> = new BehaviorSubject<BehaviorSubject<JobInformation>>(
      new BehaviorSubject<JobInformation>({
      jobId: jobId,
      status: JobInformation.StatusEnum.FAILURE
    }));
    Object.defineProperty(jobManagerServiceSpy, 'jobNotification', subject);

    component.ngOnInit();

    expect(component.formGroup.get('description').value).toBe('');
    expect(component.isGenerating).toBeFalse();
    expect(component.isGenerationSuccessful).toBeFalse();
  });

  it('should cancel job on cancel button', () => {
    const jobId = '123';
    component.generateJob = {
      jobId: jobId,
      autoDownloadResult: false,
      foreground: true,
      cancellable: true,
      label: 'Test Job',
      status$: new BehaviorSubject(JobInformation.StatusEnum.RUNNING)
    };

    component.isGenerating = true;

    jobControllerServiceSpy.cancelJob.and.returnValue(of({}));

    component.handleCancel();
    expect(jobControllerServiceSpy.cancelJob).toHaveBeenCalledWith(jobId);
  });
   
});
