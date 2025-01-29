import { ComponentFixture, TestBed } from '@angular/core/testing';
import { DnaAnalysisComponent } from './dna-analysis.component';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { BehaviorSubject, of, throwError } from 'rxjs';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { DNAAnalysisService } from '@app/core/services/dna-analysis.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { NzMessageService } from 'ng-zorro-antd/message';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { SharedModule } from '@app/shared';
import { FormsModule } from '@angular/forms';
import { RemoteJob } from '@app/core/services/job-manager/job-manager-service.interface';
import { DiscoveryControllerService, JobInformation } from '@innowake/mining-api-angular-client';

describe('DnaAnalysisComponent', () => {
  let component: DnaAnalysisComponent;
  let fixture: ComponentFixture<DnaAnalysisComponent>;
  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);
  const authServiceSpy = new NoAuthorizationService();
  const dnaCardData = {
    clusterings: [{
      clusters: [{ moduleCount: 2, description: '' }],
      algorithm: { Clustering: 'Louvain', Sequencer: 'Cobol Methods', Similarity: 'Weighted Levenshtein' },
      options: [{ name: 'similarity threshold', title: 'Similarity Threshold', value: '0.85' }]
    }]
  } as any;
  const dnaAnalysis = {
    "filterData": {
        "filters": {
            "cluster": 0
        },
        "filterString": "COBOL Methods"
    },
    "chartTableConfig": {
        "columnMap": {
            "ModuleName": {
                "field": "moduleName",
                "header": "moduleName",
                "columnAction": {
                    "type": "hyperlink"
                },
                "displayAs": "link"
            },
            "Path": {
                "field": "path",
                "header": "path"
            },
            "Cluster": {
                "field": "cluster",
                "header": "cluster"
            }
        },
        "paginator": true,
        "rows": 30,
        "pageChanger": true,
        "serverSidePagination": false,
        "showTotalCount": true,
        "loading": false
    },
    "requestQuery": {
        "query": "query { dnaModulesInCluster(projectId: 4, algorithm: \"COBOL Methods\", clusterIndex: 0) { module { id name path } clusterIndex } }"
    }
};
  const graphQl = {
    "data": {
      "dnaModulesInCluster": [
        {
          module: {
            id: 1,
            name: 'test',
            path: 'test Path'
          },
          clusterIndex: 1
        }
      ]
    }
  };
  const dnaCardList = [{
    "chartData": [
        {
            "index": 0,
            "key": "Cluster 0: 34.78% (16 modules):0",
            "value": 16
        },
        {
            "index": 1,
            "key": "Cluster 1: 32.61% (15 modules):1",
            "value": 15
        },
        {
            "index": 2,
            "key": "Cluster 2: 28.26% (13 modules):2",
            "value": 13
        },
        {
            "index": -1,
            "key": "Cluster Unassigned: 4.35% (2 modules):-1",
            "value": 2
        }
    ],
    "options": [
        {
            "title": "Similarity",
            "value": "Weighted Levenshtein"
        },
        {
            "title": "Clustering",
            "value": "Louvain"
        },
        {
            "name": "maxLevels",
            "title": "Maximum Levels",
            "value": "5"
        },
        {
            "name": "maxIterations",
            "title": "Maximum Iterations",
            "value": "10"
        },
        {
            "name": "defaultTolerance",
            "title": "Default Tolerance",
            "value": "0.000100"
        },
        {
            "name": "minDNALength",
            "title": "Minimum DNA Length",
            "value": "5"
        },
        {
            "name": "similarity threshold",
            "title": "Similarity Threshold",
            "value": "0.85"
        }
    ],
    "title": "COBOL Methods",
    "clusterModuleCount": 46,
    "assignedModuleCount": 44,
    "clustersLength": 3,
    "chartFilterData": {
        "filterArgs": [
            "index"
        ]
    }
}];
const chartDataValue = [{
  "id": 18931,
  "moduleName": "'BABKREU'",
  "path": "src/cobol/programs/BABKCMP/'BABKREU'.cbl",
  "cluster": 1
}];
const jobId = '5d2e70fb-afe0-42b7-abe8-edb1f1d3ed1d'
const remoteJob: RemoteJob = {
  jobId,
  label: 'test job',
  foreground: false,
  cancellable: false,
  autoDownloadResult: false,
  status$: new BehaviorSubject<JobInformation.StatusEnum>(JobInformation.StatusEnum.UNKNOWN)
} 
  const discoveryControllerService: jasmine.SpyObj<DiscoveryControllerService> = jasmine.createSpyObj('DiscoveryControllerService', ['modelDNAForSelectedTimestamp', 'listTheTimestampsOfDNASnapshots', 'discoverDNA']);
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(0, 'TestClient', 0, 'TestProject');
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const i18nServiceSpy = { language: 'en-US' };
  const dnaAnalysisSpy: jasmine.SpyObj<DNAAnalysisService> = jasmine.createSpyObj('DNAAnalysisService',
  ['openChartDetailsTable', 'createChartData', 'formattedDnaTableData']);
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService',
    ['graphQl']);
  const labelMappingServiceSpy: jasmine.SpyObj<LabelMappingService> = jasmine.createSpyObj('LabelMappingService', ['mapLabel'])
  beforeEach((() => {
    TestBed.configureTestingModule({
      declarations: [DnaAnalysisComponent],
      imports: [
        FormsModule,
        TranslateModule.forRoot({}),
        SharedModule,
        BrowserAnimationsModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({}),
        RouterTestingModule.withRoutes([])
      ],
      providers: [TranslateService,
        NumberFormatter,
        { provide: DiscoveryControllerService, useValue: discoveryControllerService },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: LabelMappingService, useValue: labelMappingServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: authServiceSpy },
        { provide: DNAAnalysisService, useValue: dnaAnalysisSpy },
        HttpTestingController,
        NzMessageService,
        HttpClient,
        HttpHandler
      ]
    }).compileComponents();
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    discoveryControllerService.modelDNAForSelectedTimestamp.and.returnValue(of(dnaCardData as any));
    discoveryControllerService.listTheTimestampsOfDNASnapshots.and.returnValue(of(['1680178969327'] as any));
    discoveryControllerService.discoverDNA.and.returnValue(of([jobId] as any));
    jobManagerServiceSpy.register.and.returnValue(remoteJob);
    dnaAnalysisSpy.openChartDetailsTable.and.returnValue(dnaAnalysis as any);
    dnaAnalysisSpy.createChartData.and.returnValue(dnaCardList as any);
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DnaAnalysisComponent);
    component = fixture.componentInstance;
    component.projectId = 1;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test ngOnInit', () => {
    component.ngOnInit();
    expect(component.dnaChartsData).toBeDefined();
  });

  it('should test ngOnInit when there is error', () => {
    component.ngOnInit();
    expect(component.loadState).toBeDefined();
  });
  
  it('should open chart details', () => {
    component.openChartDetailsTable({cluster: '0'} as any, 0);
    expect(component.showChartDetails).toBeTrue();
  });

  it('should open chart details when clicked on Chart header', () => {
    component.openChartDetailsTable({cluster: undefined, index: undefined}, 0);
    expect(component.showChartDetails).toBeTrue();
  });

  it('should verify that data is present in selectedClusterDetails', () => {
    component.openChartDetailsTable({cluster: 0, index: 1}, 0);
    expect(component.selectedClusterDetails.length).toBe(1);
  });

  it('should verify that selectedClusterDetails is empty when clicked on Chart header for openChartDetailsTable', () => {
    component.openChartDetailsTable({cluster: undefined, index: undefined}, 0);
    expect(component.selectedClusterDetails.length).toBe(0);
  });

  it('should verify that selectedClusterDetails is empty when no cluster is selected for getChartData', () => {
    component.getChartData('Cobol Methods', undefined);
    expect(component.selectedClusterDetails.length).toBe(0);
  });

  xit('should not open chart details', () => {
    component.openChartDetailsTable({cluster: '0'} as any, 1);
    expect(component.showChartDetails).toBeTruthy();
  });

  it('should analyse DNA', () => {
    component.analyzeDNA();
    expect(discoveryControllerService.discoverDNA).toHaveBeenCalled();
  });

  it('should navigate to default DNA URL when there is no query params', () => {
    (component as any).navigateToDnaUrl();
    expect(component.showChartDetails).toBeFalse();
  });

  it('should test snapshot selection', () => {
    spyOn((component as any), 'navigateToDnaUrl');
    component.addSnapshotToUrl('1680178969327');
    expect((component as any).navigateToDnaUrl).toHaveBeenCalledWith({snapshot: '1680178969327'});

    component.selectedDnaSnapshot = '2780178969328'
    component.addSnapshotToUrl();
    expect((component as any).navigateToDnaUrl).toHaveBeenCalledWith({snapshot: '2780178969328'});
  });
});
