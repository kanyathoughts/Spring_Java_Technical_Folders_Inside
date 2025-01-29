import { ComponentFixture, TestBed } from '@angular/core/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { BlockDetailsComponent } from './block-details.component';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor, I18nService } from '@app/core';
import { HttpClient, HttpXsrfTokenExtractor } from '@angular/common/http';
import { of } from 'rxjs';
import { WindowToken } from '@app/core/utils/window';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Column } from '@app/shared/components/mining-table/mining-table-config.interface';
import { Configuration, DataPointControllerService, JobControllerService, MiningDataPointDefinitionWithPath, TaxonomyAssignmentsGetResponse, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { CustomizableTableCoreComponent } from '@app/shared/components/customizable-table-core/customizable-table-core.component';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { TypeName } from '@app/shared/interfaces/datapoints-labels.interface';
import { model } from 'model-test-data';
import { ReachabilityService } from '../utils/reachability.service';

describe('BlockDetailsComponent', () => {
  let component: BlockDetailsComponent;
  let fixture: ComponentFixture<BlockDetailsComponent>;

  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'skipErrorHandler', 'get', 'request']);
  const i18nServiceSpy = { language: 'en-US' };
  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('ClientProjectRelationshipService',
    ['currentClient', 'getClientProjectObservable']);
    const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');
    let taxonomyService = new TaxonomyControllerService(httpServiceSpy, 'http://localhost', new Configuration());
    const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj(
        'DataPointControllerService',
        ['getDataPointsForType']
    );
    const columns = model.columns;

    const initialDataPoints: MiningDataPointDefinitionWithPath[] = model.initialDataPoints;
  const customTable: jasmine.SpyObj<CustomizableTableCoreComponent> = jasmine.createSpyObj('CustomizableTableCoreComponent', [
    'initialize'
  ]);
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
    'GraphQlControllerService',
    ['graphQl']
  );
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', ['getJobInformation']);
  const reachabilityServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('ReachabilityService', ['updateOutdatedBlock']);
  customTable.initialDataPoints = initialDataPoints;
  const taxonomy: TaxonomyAssignmentsGetResponse = {
    "moduleCount": 3,
    "taxonomies": [
      { "state": "ALL" },
      { "state": "NONE" },
      { "state": "SOME" },
      { "state": "SOME" }
    ]
  };
  const graphQl = {
    "data": {
      "modules": {
        "content": [
          {
            "name": "&PARAM1",
            "linkHash": "1EF4567F3E4F4F77D1F29261F57CAD630DA49332522267E913B8CEB3C71DCE4D",
            "objectTypeLink": {
              "technologyLink": "RESOURCE",
              "typeLink": "FILE",
              "storageLink": "FILE"
            },
            "metricsDate": "2022-09-02 10:53",
            "id": 554,
            "requiresReview": false,
            "inCodebase": false,
            "identificationLink": "MISSING",
          },
          {
            "name": "CICS-TC",
            "linkHash": "DC259F8EB2CEDBBE71FC36C22D90E7EFCCEEEF50DA5CF324D0AD91124B955393",
            "objectTypeLink": {
              "technologyLink": "CSD",
              "typeLink": "TRANSACTION",
              "storageLink": "FILE_SECTION"
            },
            "metricsDate": "2022-09-02 10:53",
            "id": 556,
            "requiresReview": false,
            "inCodebase": false,
            "identificationLink": "MISSING",
          }
        ],
        "totalElements": 143,
        "size": 30
      }
    }
  };
  const moduleIds = {
    "data": {
      "functionalBlocks": {
        "content": [
          {
            "resolvedModuleParts": [
              {
                "module": {
                  "id": 1775
                }
              }
            ]
          }
        ]
      }
    }
  };
  let mockWindow: any;
  let openedUrl = '';
  beforeEach(async () => {
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
    await TestBed.configureTestingModule({
      declarations: [BlockDetailsComponent, CustomizableTableCoreComponent],
      providers: [
        NumberFormatter,
        TranslateService,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        HttpXsrfTokenExtractor,
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide : I18nService, useValue: i18nServiceSpy },
        { provide: TaxonomyControllerService, useValue: taxonomyService },
        { provide: DataPointControllerService, useValue: dataPointControllerServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: ReachabilityService, useValue: reachabilityServiceSpy }
      ],
      schemas: [ NO_ERRORS_SCHEMA ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        AntDesignImportsModule,
        TranslateModule.forRoot({}),
      ],
    }).compileComponents();
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(initialDataPoints as any));
    httpServiceSpy.disableApiPrefix.and.returnValue(httpServiceSpy);
    httpServiceSpy.get.and.returnValue(of('99.9.99-TRUNK-MINING-SNAPSHOT'));
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    jobControllerServiceSpy.getJobInformation.and.returnValue(of({ jobId: 'someJobId', status: 'RUNNING' }) as any);
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockDetailsComponent);
    component = fixture.componentInstance;
    component.customTable = customTable;
    clientProjectRelationshipServiceSpy.currentClient = clientProjectRelationship;
    const ids: string[] = ['2345', '5678'];
    const reachabilityDetails = {
        "pageTitle": "UNISYSJCL",
        "blockId": "afcff2b9-bfb7-45f4-820e-401a3e1fd9f7",
        "mergeParent": false,
        "outdated": false,
        "deleted": false
      }
    localStorage.setItem(`1-reachabilityIds`, JSON.stringify(ids));
    localStorage.setItem(`1-reachabilityDetails`, JSON.stringify(reachabilityDetails));
    component.pageType = TypeName.PAGEREACHABILITY;
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test switch columns', () => {
    spyOn((component as any), 'changeColumnOrder').and.callThrough();
    spyOn((component as any), 'swapDefaultColumnIndex').and.callThrough();
    component['changeColumnOrder'](columns as Column[]);
    expect(component.columnForRowSelection).toEqual('lowerBoundModuleName');
    expect(component['swapDefaultColumnIndex']).toHaveBeenCalled();
  });

  it('should test getAssignmentData', () => {
    spyOn((component as any), 'assignTaxonomies').and.callThrough();
    spyOn(taxonomyService, 'getAssignedTaxonomyByModule').and.returnValue(of(taxonomy as any));
    let ids = new Set<number>([1, 23, 4]);
    component['getAssignmentData'](ids);
    expect(component['assignTaxonomies']).toHaveBeenCalled();
  });

  it('should test when single block id', () => {
    const ids: string[] = ['2345'];
    localStorage.setItem(`1-reachabilityIds`, JSON.stringify(ids));
    spyOn((component as any), 'getReachabilityBlock').and.callThrough();
    component.ngOnInit();
    expect(component['getReachabilityBlock']).toHaveBeenCalled();
  });

  it('should test fetchAssociatedModulesForRB', () => {
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(moduleIds as any));
    component['fetchAssociatedModulesForRB']();
    expect(component.moduleIdArray).toEqual([1775]);
  });
});
