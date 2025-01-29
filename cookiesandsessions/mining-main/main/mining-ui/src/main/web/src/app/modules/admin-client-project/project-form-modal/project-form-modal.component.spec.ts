import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { UntypedFormBuilder } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import { throwError } from 'rxjs';
import { of } from 'rxjs/internal/observable/of';
import { ProjectFormModalComponent } from './project-form-modal.component';
import { ClientControllerV2Service, PagedClientPojo, ProjectControllerService, ProjectControllerV2Service, ProjectPojoPrototype, ProjectRole } from '@innowake/mining-api-angular-client';

describe('ProjectFormModalComponent', () => {
  let component: ProjectFormModalComponent;
  let fixture: ComponentFixture<ProjectFormModalComponent>;
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);
  const clientControllerV2Spy = jasmine.createSpyObj<ClientControllerV2Service>('ClientControllerV2Service', ['getAllClients']);
  const projectControllerSpy = jasmine.createSpyObj<ProjectControllerService>('ProjectControllerService', ['updateProject']);
  const projectControllerV2Spy = jasmine.createSpyObj<ProjectControllerV2Service>(
    'ProjectControllerV2Service',
    ['createProject', 'findProjectNatures', 'changeProjectNatures']
  );
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const clientList: PagedClientPojo = {
    content: [
      { name: 'Client Test 1' },
      { name: 'Client Test 2' }
    ]
  };

  const mockProjectNature: ProjectRole.ProjectNaturesEnum[] = [
    ProjectRole.ProjectNaturesEnum.MINING,
    ProjectRole.ProjectNaturesEnum.DISCOVERY
  ];

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ProjectFormModalComponent ],
      imports: [
        TranslateModule.forRoot({}),
        NzModalModule,
        NzMessageModule,
        HttpClientTestingModule,
        BrowserAnimationsModule
      ],
      providers: [
        { provide: NzMessageService, useValue: messageServiceSpy},
        { provide: NzModalRef, useValue: nzModalRefSpy },
        { provide: ClientControllerV2Service, useValue: clientControllerV2Spy },
        { provide: ProjectControllerService, useValue: projectControllerSpy },
        { provide: ProjectControllerV2Service, useValue: projectControllerV2Spy },
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        TranslateService,
        UntypedFormBuilder
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectFormModalComponent);
    component = fixture.componentInstance;
    clientControllerV2Spy.getAllClients.and.returnValue(of(clientList as any));
    projectControllerSpy.updateProject.and.returnValue(of('result' as any, waitForAsync));
    projectControllerV2Spy.createProject.and.returnValue(of('result' as any, waitForAsync));
    projectControllerV2Spy.findProjectNatures.and.returnValue(of(mockProjectNature as any));
    projectControllerV2Spy.changeProjectNatures.and.returnValue(of());
    component.selectedProjects = [];
    component.selectedProjects.push(ProjectPojoPrototype.NaturesEnum.DISCOVERY);
    let fb : UntypedFormBuilder = new UntypedFormBuilder(); 
    component.projectNatures.push(fb.control({ value: true, disabled: true }));
  });

  it('should create', waitForAsync(() => {
    expect(component).toBeTruthy();
    fixture.detectChanges();
    fixture.whenStable().then(() => {
      expect(component.selectedProjects.length).toBe(mockProjectNature.length);
      expect(clientControllerV2Spy.getAllClients).toHaveBeenCalled();
      expect(component.clientList.length).toBe(2);
    });
  }));

  it('should use parameter for client if present', () => {
    component.clientId = 1;
    component.clientName = 'Test client';
    component.ngOnInit();
    expect(component.clientList.length).toBe(1);
    expect(component.formClientId).toBe(1);
  });

  it('should be on edit mode', waitForAsync(() => {
    component.project = {id: 1, name: 'Test project' };
    component.ngOnInit();
    expect(component.mode).toBe('edit');
    expect(component.projectName).toBe('Test project');
    /* expect(projectControllerV2Spy.findProjectNatures).toHaveBeenCalled(); 
    fixture.whenStable().then(() => {
      expect(component.selectedProjects.length).toBe(Object.keys(ProjectPojoPrototype.NaturesEnum).length);
    }); */
  }));

  it('should submit on create mode', waitForAsync(() => {
    component.projectForm.get('name').setValue('Test project');
    component.onSubmit();
    expect(projectControllerV2Spy.createProject).toHaveBeenCalled();
    fixture.whenStable().then(() => {
      expect(nzModalRefSpy.close).toHaveBeenCalled();
    });
  }));

  it('should submit on create mode with error', waitForAsync(() => {
    projectControllerV2Spy.createProject.and.returnValue(throwError({}));
    component.projectForm.get('name').setValue('Test project');
    component.onSubmit();
    fixture.whenStable().then(() => {
      expect(nzModalRefSpy.close).toHaveBeenCalled();
    });
  }));

  it('should submit on edit mode', waitForAsync(() => {
    projectControllerSpy.updateProject.and.returnValue(of({id: 1, name: 'Test project updated' } as any));
    component.project = {id: 1, name: 'Test project' };
    component.clientId = 1;
    component.clientName = 'Test client';
    component.projectNatures.markAsDirty();
    component.ngOnInit();
    component.onSubmit();
    expect(projectControllerSpy.updateProject).toHaveBeenCalled();
    fixture.whenStable().then(() => {
      expect(component.project.name).toBe('Test project updated' );
      /* expect(projectControllerV2Spy.changeProjectNatures).toHaveBeenCalled(); */
    });
  }));

  it('should submit with error on project update', waitForAsync(() => {
    projectControllerSpy.updateProject.and.returnValue(throwError({}));
    component.project = {id: 1, name: 'Test project' };
    component.clientId = 1;
    component.clientName = 'Test client';
    component.ngOnInit();
    component.onSubmit();
    fixture.whenStable().then(() => {
      expect(nzModalRefSpy.close).toHaveBeenCalled();
    });
  }));

  it('should submit with error on project nature update', waitForAsync(() => {
    projectControllerSpy.updateProject.and.returnValue(of({id: 1, name: 'Test project updated' } as any));
    projectControllerV2Spy.changeProjectNatures.and.returnValue(throwError({}));
    component.project = {id: 1, name: 'Test project' };
    component.clientId = 1;
    component.clientName = 'Test client';
    component.ngOnInit();
    component.projectNatures.markAsDirty();
    component.onSubmit();
    fixture.whenStable().then(() => {
      expect(nzModalRefSpy.close).toHaveBeenCalled();
    });
  }));
});
