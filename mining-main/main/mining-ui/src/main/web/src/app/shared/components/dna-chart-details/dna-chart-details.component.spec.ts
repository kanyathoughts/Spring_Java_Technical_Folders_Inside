import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpTestingController } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { I18nService } from '@app/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { NzModalModule } from 'ng-zorro-antd/modal';
import { of } from 'rxjs';
import { DnaChartDetailsComponent } from './dna-chart-details.component';
import { WindowToken } from '@app/core/utils/window';
import { MiningTableOptionSelected } from '../mining-table/mining-table-option-selected.interface';
import { UntypedFormBuilder } from '@angular/forms';
import { DiscoveryControllerService } from '@innowake/mining-api-angular-client';
import { DNAAnalysisService } from '@app/core/services/dna-analysis.service';

describe('DnaChartDetailsComponent', () => {
  let component: DnaChartDetailsComponent;
  let fixture: ComponentFixture<DnaChartDetailsComponent>;
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

  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService',
    ['graphQl']);
  const discoveryControllerService: jasmine.SpyObj<DiscoveryControllerService> = jasmine.createSpyObj('DiscoveryControllerService', ['updateDnaCommunity']);
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const dnaAnalysisSpy = jasmine.createSpyObj<DNAAnalysisService>('DNAAnalysisService', ['disableTableAction'])
  let mockWindow: any;
  let openedUrl = '';
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
      declarations: [DnaChartDetailsComponent],
      providers: [
        NumberFormatter,
        HttpTestingController,
        HttpHandler,
        TranslateService,
        I18nService,
        UntypedFormBuilder,
        { provide: DiscoveryControllerService, useValue: discoveryControllerService },
        { provide: WindowToken, useValue: mockWindow },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: DNAAnalysisService, useValue: dnaAnalysisSpy }
      ],
      imports: [
        NzModalModule,
        NzMessageModule,
        BrowserAnimationsModule,
        TranslateModule.forRoot({})
      ]
    }).compileComponents();
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DnaChartDetailsComponent);
    component = fixture.componentInstance;
    component.isModuleList =  true;
    component.filters = {cluster: '1'}
    fixture.detectChanges();
  });

 it('should check for changes', () => {
    component.ngOnChanges();
    expect(component.additionalGraphQlParams['clusterIndex']).toBe(1);
  });

  it('should close chart details', () => {
    spyOn(component.showChartDetails, 'emit');
    component.closeChartDetails();
    expect(component.showChartDetails.emit).toHaveBeenCalled();
  });

  it('should test enableEdit', () => {
    component.enableEdit();
    expect(component.disableEdit).toBeFalsy();
  });

  it('should return route path to module detail page', () => {
    component.projectId = 1;
    const testValue: MiningTableOptionSelected = {
      optionValue: "code-viewer",
      data: {
        module: {
          recordId: "#222:79",
          customProperties: {},
          id: 637
        }
      }
    };
    component.handleSelectedOption(testValue);
    expect(openedUrl).toContain('code-viewer');
  });
});
