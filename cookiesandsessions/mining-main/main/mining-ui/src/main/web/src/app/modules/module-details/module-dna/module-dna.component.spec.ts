import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { TranslateModule } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ModuleDNAComponent } from './module-dna.component';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { ChartCardClickHandlerService } from '@app/core/services/chart-card-click-handler.service';
import { of } from 'rxjs';
import { NzMessageService } from 'ng-zorro-antd/message';
import { DNAAnalysisService } from '@app/core/services/dna-analysis.service';

describe('ModuleDNAComponent', () => {
  let component: ModuleDNAComponent;
  let fixture: ComponentFixture<ModuleDNAComponent>;

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

  const i18nServiceSpy = { language: 'en-US' };
  const clickServiceSpy = jasmine.createSpyObj<ChartCardClickHandlerService>('ChartCardClickHandlerService', ['getFinalClick', 'addListenerToChart', 'getElementArgs']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['error']);
  const dnaAnalysisSpy: jasmine.SpyObj<DNAAnalysisService> = jasmine.createSpyObj('DNAAnalysisService',
    ['openChartDetailsTable']);
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ModuleDNAComponent],
      imports: [TranslateModule.forRoot(), HttpClientTestingModule],
      providers: [NumberFormatter,
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: ChartCardClickHandlerService, useValue: clickServiceSpy },
        { provide: DNAAnalysisService, useValue: dnaAnalysisSpy },
        { provide: NzMessageService, useValue: messageServiceSpy }]
    }).compileComponents();
    clickServiceSpy.getFinalClick.and.returnValue(of({ event: 'element', cardTitle: 'Cobol Methods' } as any));
    clickServiceSpy.getElementArgs.and.returnValue({
      data: { data: { 'index': 1, key: "Cluster 1: 100% (5 modules):1", value: 15 } }
    });
    dnaAnalysisSpy.openChartDetailsTable.and.returnValue(dnaAnalysis as any);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleDNAComponent);
    component = fixture.componentInstance;
    component.dnaCardData = {
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
      "title": "Cobol Methods",
      "clusterModuleCount": 46,
      "assignedModuleCount": 44,
      "clustersLength": 3
    } as any;
    fixture.detectChanges();
  });

  it('should create', () => {
    component.ngOnInit();
    expect(component).toBeTruthy();

    component.dnaCardData = {
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
      "title": "COBOL Skeleton",
      "clusterModuleCount": 46,
      "assignedModuleCount": 44,
      "clustersLength": 1
    } as any;
    component.ngOnInit();
    expect(component).toBeTruthy();
  });

  it('should generate filter obj', () => {
    component.chartDataFilters.filters = {};
    component.generateFilterObjAndEmitEvent();
    expect(component.isModuleList).toBeFalsy();
    component.generateFilterObjAndEmitEvent('moduleList');
    expect(component.isModuleList).toBeTruthy();
  });

  it('should close chart details', () => {
    component.closeChartDetails();
    expect(component.showChartDetails).toBeFalse();
  });
});
