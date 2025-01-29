import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpTestingController } from '@angular/common/http/testing';
import { TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { NzModalModule } from 'ng-zorro-antd/modal';
import { DNAAnalysisService } from './dna-analysis.service';
import { I18nService } from './i18n/i18n.service';
import { LabelMappingService } from './label-mapping.service';
import { CfgSupportedTypeService } from './cfg-supported-type.service';

const dnaCardData = {
  clusterings: [
    {
      "algorithm": {
        "Sequencer": "COBOL Methods",
        "Similarity": "Weighted Levenshtein",
        "Clustering": "Louvain"
      },
      "options": [
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
          "value": "11"
        },
        {
          "name": "similarity threshold",
          "title": "Similarity Threshold",
          "value": "0.85"
        }
      ],
      "clusters": [{
        "clusterIndex": 0,
        "algorithm": {
          "Sequencer": "COBOL Methods",
          similarity: "Weighted Levenshtein",
          clustering: "Louvain"
        }
      },
      {
        "clusterIndex": 2,
        "algorithm": {
          "Sequencer": "COBOL Methods",
          similarity: "Weighted Levenshtein",
          clustering: "Louvain"
        }
      },
      {
        "clusterIndex": 1,
        "algorithm": {
          "Sequencer": "COBOL Methods",
          similarity: "Weighted Levenshtein",
          clustering: "Louvain"
        }
      }
      ]
    },
    {
      "algorithm": {
        "Sequencer": "COBOL Methods",
        "Similarity": "Weighted Levenshtein",
        "Clustering": "Louvain"
      },
      "options": [
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
          "value": "10"
        },
        {
          "name": "similarity threshold",
          "title": "Similarity Threshold",
          "value": "0.85"
        }
      ],
      "clusters": [{}]
    }
  ],
  moduleCount: 10
}

describe('DNAAnalysisService', () => {
  let service: DNAAnalysisService;
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const cfgSupportedTypeServiceSpy = jasmine.createSpyObj<CfgSupportedTypeService>('CfgSupportedTypeService', ['checkIfSupported']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        NzModalModule,
        NzMessageModule,
        BrowserAnimationsModule,
        TranslateModule.forRoot({})
      ],
      providers: [NumberFormatter,
        HttpTestingController,
        HttpClient,
        HttpHandler,
        TranslateService,
        I18nService,
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: CfgSupportedTypeService, useValue: cfgSupportedTypeServiceSpy }
      ]
    });
    service = TestBed.inject(DNAAnalysisService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should check DNA list for similar module', () => {
    const dnaCardList = service.createChartData(dnaCardData);
    expect(dnaCardList.length).toBe(2);
    expect(dnaCardList[0].chartData[1].index).toBe(1);
  });

  it('should create chart data', () => {
    const clusters = [{
                  "clusterIndex": 1,
                  "algorithm": {
                      "Sequencer": "COBOL Methods",
                      similarity: "Weighted Levenshtein",
                      clustering: "Louvain"
                  }
              },
              {
                  "clusterIndex": 2,
                  "algorithm": {
                      "Sequencer": "COBOL Skeleton",
                      similarity: "Weighted Levenshtein",
                      clustering: "Louvain"
                  }
              }
          ];
    service.createChartData(dnaCardData, clusters);
  });


  it('should open chart details', () => {
    const dnaCardData = [{
      moduleCount: 4,
      clustersLength: 1,
      clusterModuleCount: 2,
      title: 'Cobol Methods',
      options: [{ name: 'similarity threshold', title: 'Similarity Threshold', value: '0.85' }],
      chartData: [{ value: 1, key: 'Cluster size: 100% (2 modules)' }, { value: 2, key: 'Unassigned: 60% (5 modules)' }],
      assignedModuleCount: 12
    }];
    service.openChartDetailsTable({ clusterIndex: 1 }, dnaCardData[0] as any);
    const chartFilter = service.openChartDetailsTable({ clusterIndex: 1 }, dnaCardData[0] as any);
    expect(chartFilter).toBeUndefined();
  });

  it('should test disableTableAction', () => {
    const data = {
      "module": {
        "name": "MMRS71D1",
        "linkHash": "898A2AE42B76CED0EAF4BEA649A5C28EF41DF8CB86A44481DBD2A9D00CD9D3B9",
        "path": "src/cobol/programs/MMRS71D1.cbl",
        "id": 433,
        "objectTypeLink": {
          "storageLink": "FILE"
        },
        "inCodebase": true,
        "identificationLink": "IDENTIFIED"
      },
      "clusterIndex": -1,
      "id": 0,
      "metricsDate": "N/A",
      "level": 0,
      "expand": false,
      "key": "parent-0",
      "sourceCodeAvailable": true,
      "identification": "IDENTIFIED"
    };
    const disableCodeViewer = service.disableTableAction('code-viewer', data);
    expect(disableCodeViewer).toEqual({disableButton: false, toolTip: (service as any).translateService.instant('iconToolTip.codeViewer')});
    
    cfgSupportedTypeServiceSpy.checkIfSupported.and.returnValue(true);
    const disableCFG = service.disableTableAction('control-flow', data);
    expect(disableCFG).toEqual({disableButton: false, toolTip: (service as any).translateService.instant('cfgNotAvailable')});
  });
});
