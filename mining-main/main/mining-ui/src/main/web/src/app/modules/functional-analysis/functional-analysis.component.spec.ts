import { ComponentFixture, TestBed } from '@angular/core/testing';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { FunctionalAnalysisComponent } from './functional-analysis.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { of } from 'rxjs/internal/observable/of';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { FunctionalBlockControllerService, FunctionalBlockPojoPrototype, JobControllerService, JobInformation } from '@innowake/mining-api-angular-client';
import { Apollo } from 'apollo-angular';
import { ModulesGQL } from '@app/graphql/generated/generated';
import { NzBadgeModule } from 'ng-zorro-antd/badge';
import { NzButtonModule } from 'ng-zorro-antd/button';
import { NzIconModule } from 'ng-zorro-antd/icon';
import { NoopAnimationsModule } from '@angular/platform-browser/animations';
import { Subject } from 'rxjs';
import { DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { ActivatedRoute } from '@angular/router';

describe('FunctionalAnalysisComponent', () => {
  let component: FunctionalAnalysisComponent;
  let fixture: ComponentFixture<FunctionalAnalysisComponent>;
  const i18nServiceSpy = { language: 'en-US' };
  const jobInfoSuccess: JobInformation = { status: JobInformation.StatusEnum.SUCCESS };
  const graphQlData = {
    "data": {
      "functionalBlocks": {
        "content": []
      }
    }
  } as any;
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
    "empty": true,
    "data": {
      "modules": {
        "content": [{}]
      }
    }
  };
  let response = {
    object: {
      className: "innowake.mining.shared.model.TaxonomyImportValidationResult",
      overallResult: "NONE",
      markers: [{ 'test': 'testing' }]
    }
  };
  const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
  let activatedRoute: ActivatedRoute;
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['error']);
  const graphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>
    ('GraphQlControllerService', ['graphQl']);
  const jobManagerServiceSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['register']);
  const jobControllerServiceSpy = jasmine.createSpyObj<JobControllerService>('JobControllerService', ['getJobInformation', 'getJobResult', 'cancelJob', 'getJobInformations', 'getJobLog', 'getJobResult', 'submitJobExtension', 'submitJobExtensionV2']);
  const moduleGqlServiceSpy: jasmine.SpyObj<ModulesGQL> = jasmine.createSpyObj<ModulesGQL>
    ('ModulesGQL',
      ['fetch']);
  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
    const functionalBlockController: jasmine.SpyObj<FunctionalBlockControllerService> = jasmine.createSpyObj('FunctionalBlockControllerService', [
      'createFunctionalBlock'
    ]);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'close', 'getContentComponent']);

  beforeEach(() => {
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of({ getprojectId: 1 } as any));
    graphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData as any));
  });
  jobManagerServiceSpy.register.and.returnValue(status as any);
  jobControllerServiceSpy.getJobResult.and.returnValues(of(blob) as any);
  jobControllerServiceSpy.getJobInformation.and.returnValues(of(jobInfoSuccess as any));
  moduleGqlServiceSpy.fetch.and.returnValue(of(listModule as any));
  nzModalRefSpy.getContentComponent.and.returnValue({});
  modalServiceSpy.create.and.returnValue(nzModalRefSpy);
  beforeEach((() => {
    TestBed.configureTestingModule({
      declarations: [FunctionalAnalysisComponent],
      imports: [TranslateModule.forRoot({}), RouterTestingModule.withRoutes([]), HttpClientTestingModule, NzBadgeModule, NzButtonModule, NzIconModule, NoopAnimationsModule],
      providers: [TranslateService, NumberFormatter,
        Apollo,
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceStub },
        { provide: ModulesGQL, useValue: moduleGqlServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService()},
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: FunctionalBlockControllerService, useValue: functionalBlockController },
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: {
              queryParams: {}
            }
          }
        }
      ]
    })
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(FunctionalAnalysisComponent);
    component = fixture.componentInstance;
    activatedRoute = TestBed.inject(ActivatedRoute);
    component.projectId = 1
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test onReset', () => {
    component.onReset();
    expect(component.selectedModuleIds.length).toBe(0);
  });

  it('should test isButtonDisabled', () => {
    component.selectedModuleIds = ['1', '2'];
    const disable = component.isButtonDisabled();
    expect(disable).toBe(false);
  });

  it('should test updateTaxonomySelection', () => {
    const event = [{
      taxonomyId: 1000,
      taxonomyTitle: 'test'
    }];
    component.updateTaxonomySelection(event as any);
    expect(component.selectedTaxonomyIds.length).toBe(1);
  });

  it('should trigger backend when there is atleast 1 character', () => {
    component.onModuleNameSearch('test');
    expect(moduleGqlServiceSpy.fetch).toHaveBeenCalled();
  });

  it('should set empty listSearchModule and not call backend when search input is empty', () => {
    component.onModuleNameSearch('');
    moduleGqlServiceSpy.fetch.calls.reset();
    expect(component.listSearchModule).toEqual([]);
  });

  it('should display badge dot when isFiltersActive is true', () => {
    component.isFiltersActive = true;
    fixture.detectChanges();
    const badgeDot = fixture.nativeElement.querySelector('.ant-badge-dot');
    expect(badgeDot).not.toBeNull();
  });

  it('should not display badge dot when isFiltersActive is false', () => {
    fixture.detectChanges();
    const badgeDot = fixture.nativeElement.querySelector('.ant-badge-dot');
    expect(badgeDot).toBeNull();
  });

  it('should enable remove button if annotationsData is not empty', () => {
    const annotationsData = new Map<string, NzTreeNodeOptions>([
      ['1', { title: 'test' } as NzTreeNodeOptions]
    ]);
    component['getFGDetails'] = [{
      uid: '1', type: 'test', parent: {} as NzTreeNodeOptions
    }];
    component.handleRemoveLabel(annotationsData);
    expect(component.disableRemoveButton).toBe(false);
  });

  it('should disable remove button if annotationsData is empty', () => {
    const annotationsData = new Map<string, NzTreeNodeOptions>();
    component.handleRemoveLabel(annotationsData);
    expect(component.disableRemoveButton).toBe(true);
  });

  it('should open modal to remove Annotation', () => {
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    component.removeAnnotations();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should set reachabilityIds correctly', () => {
    activatedRoute.snapshot.queryParams = {
      filterApplied: JSON.stringify({
        reachabilityIds: [1, 2, 3]
      })
    };

    component.ngOnInit();
    fixture.detectChanges();

    expect(component.selectReachabilityIds.map(String)).toEqual(['1', '2', '3']);
  });

  it('should set blockNameSearch correctly', () => {
    activatedRoute.snapshot.queryParams = {
      filterApplied: JSON.stringify({
        blockNameSearch: 'testBlock'
      })
    };

    component.ngOnInit();
    fixture.detectChanges();

    expect(component.blockNameSearch).toEqual('testBlock');
  });

  it('should set taxonomyIds correctly', () => {
    activatedRoute.snapshot.queryParams = {
      filterApplied: JSON.stringify({
        taxonomyIds: [4, 5]
      })
    };

    component.ngOnInit();
    fixture.detectChanges();

    expect(component.selectedTaxonomyIds).toEqual([4, 5]);
  });

  it('should set moduleIds correctly', () => {
    activatedRoute.snapshot.queryParams = {
      filterApplied: JSON.stringify({
        moduleIds: [6, 7]
      })
    };

    component.ngOnInit();
    fixture.detectChanges();

    expect(component.selectedModuleIds.map(String)).toEqual(['6', '7']);
  });

  it('should set ddIds correctly', () => {
    activatedRoute.snapshot.queryParams = {
      filterApplied: JSON.stringify({
        ddIds: [8, 9]
      })
    };

    component.ngOnInit();
    fixture.detectChanges();

    expect(component.selectedDDIds.map(String)).toEqual(['8', '9']);
  });

  it('should set all filter attributes correctly', () => {
    activatedRoute.snapshot.queryParams = {
      filterApplied: JSON.stringify({
        reachabilityIds: [1, 2, 3],
        blockNameSearch: 'testBlock',
        taxonomyIds: [4, 5],
        moduleIds: [6, 7],
        ddIds: [8, 9]
      })
    };

    component.ngOnInit();
    fixture.detectChanges();

    expect(component.selectReachabilityIds.map(String)).toEqual(['1', '2', '3']);
    expect(component.blockNameSearch).toEqual('testBlock');
    expect(component.selectedTaxonomyIds).toEqual([4, 5]);
    expect(component.selectedModuleIds.map(String)).toEqual(['6', '7']);
    expect(component.selectedDDIds.map(String)).toEqual(['8', '9']);
    expect(component.isFiltersActive).toBeTrue();
  });
});
