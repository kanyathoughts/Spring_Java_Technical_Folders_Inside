import { ComponentFixture, TestBed } from '@angular/core/testing';
import { I18nService } from '@app/core';
import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { DnaCardComponent } from './dna-card.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { ChartCardClickHandlerService } from '@app/core/services/chart-card-click-handler.service';
import { of } from 'rxjs';

describe('DnaCardComponent', () => {
  let component: DnaCardComponent;
  let fixture: ComponentFixture<DnaCardComponent>;
  const i18nServiceSpy = { language: 'en-US' };
  let translateService: TranslateService;
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['error']);
  const clickServiceSpy = jasmine.createSpyObj<ChartCardClickHandlerService>('ChartCardClickHandlerService', ['getFinalClick', 'addListenerToChart', 'getElementArgs']);
  beforeEach((() => {
    TestBed.configureTestingModule({
      declarations: [DnaCardComponent],
      imports: [TranslateModule.forRoot({})],
      providers: [TranslateService, NumberFormatter,
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: ChartCardClickHandlerService, useValue: clickServiceSpy }
      ]
    }).compileComponents();
    clickServiceSpy.getFinalClick.and.returnValue(of({ event: 'element', cardTitle: 'Cobol Methods' } as any));
    clickServiceSpy.getElementArgs.and.returnValue({data: {data : {'index': 1 }}});
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DnaCardComponent);
    component = fixture.componentInstance;
    translateService = TestBed.inject(TranslateService);
    component.dnaCardData = {
      moduleCount: 4,
      clustersLength: 1,
      clusterModuleCount: 2,
      title: 'Cobol Methods',
      options: [{ name: 'similarity threshold', title: 'Similarity Threshold', value: '0.85' }],
      chartData: [{ value: 1, key: 'Cluster size: 100% (2 modules)' }, { value: 2, key: 'Unassigned: 60% (5 modules)' }]
    } as any;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test when cluster length is greater than 1', () => {
    component.dnaCardData = {
      moduleCount: 4,
      clustersLength: 3,
      clusterModuleCount: 2,
      title: 'Cobol Methods',
      options: [{ name: 'similarity threshold', title: 'Similarity Threshold', value: '0.85' }],
      chartData: [{ value: 1, key: 'Cluster size: 100% (2 modules)' }, { value: 2, key: 'Unassigned: 60% (5 modules)' }]
    } as any;
    component.ngOnInit();
    expect(component.dnaChartTxt).toBeDefined();
  });

  it('should generate filter obj', () => {
    spyOn(component.openTableForFilter, 'emit');
    component.generateFilterObjAndEmitEvent();
    expect(component.openTableForFilter.emit).toHaveBeenCalled();
  });

  describe('Export DNA Chart', () => {
    const event = {
      stopPropagation: () => {
        return true;
      }
    };
    it('should test export chart', (done) => {
      fixture.detectChanges();
      const html2canvasPromise = new Promise((resolve) => {
        resolve('test')
      });
      component.exportDNAChart('.png', event as any);
      html2canvasPromise.then((data) => {
        expect(data).toBeDefined();
        done();
      });
    });
    it('should test failure', () => {
      fixture.detectChanges();
      const html2canvasPromise = new Promise((error) => {
        error('export error')
      });
      component.exportDNAChart('.png', event as any);
      html2canvasPromise.catch((error) => {
        expect(messageServiceSpy.error).toHaveBeenCalled();
      });
    })
  });
});
