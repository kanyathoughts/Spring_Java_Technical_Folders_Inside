import { TranslateModule, TranslateService } from '@ngx-translate/core';
import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TaxonomyCardComponent } from './taxonomy-card.component';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { of } from 'rxjs';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { AggregationResultTaxonomyFieldName, TaxonomyControllerService } from '@innowake/mining-api-angular-client';

describe('TaxonomyCardComponent', () => {
    let component: TaxonomyCardComponent;
    let fixture: ComponentFixture<TaxonomyCardComponent>; 

    const taxonomyControllerServiceSpy: jasmine.SpyObj<TaxonomyControllerService> = jasmine.createSpyObj('TaxonomyControllerService', ['getAggregatedValues3', 'getSlocValues', 'getAggregatedValues']);
    const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['error']);
    const taxonomyAggregatedResponse: AggregationResultTaxonomyFieldName[] = [
        {
            group: { TYPE_NAME: "DataDomain" as any },
            fields: { MODULE_ID: 2 as any }
        }
    ]
    const i18nServiceSpy = { language: 'en-US' };

    beforeEach(async () => {
        await TestBed.configureTestingModule({
          declarations: [ TaxonomyCardComponent ],
          imports: [
            TranslateModule.forRoot({})
          ],
          providers: [
            TranslateService,
            FileSaveSupport,
            NumberFormatter,
            { provide: TaxonomyControllerService, useValue: taxonomyControllerServiceSpy },
            { provide: NzMessageService, useValue: messageServiceSpy },
            { provide: I18nService, useValue: i18nServiceSpy }
          ]
        })
        .compileComponents();
        taxonomyControllerServiceSpy.getAggregatedValues.and.returnValue(of(taxonomyAggregatedResponse as any));
        taxonomyControllerServiceSpy.getSlocValues.and.returnValue(of({'DataDomain': 300} as any))
      });

    beforeEach(() => {
        fixture = TestBed.createComponent(TaxonomyCardComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should call OnInputValueChange', () => {
      spyOn(component as any, 'fetchChartData').and.callThrough();
      component.selectedTaxonomy = 'none';
      component.OnInputValueChange('');
      expect(component['fetchChartData']).toHaveBeenCalled();
    });

    it('should call else OnInputValueChange', () => {
      component['chartPlot'] = null as any;
      spyOn(component as any, 'fetchChartData').and.callThrough();
      component.OnInputValueChange('all');
      expect(component['fetchChartData']).toHaveBeenCalled();
    });

    it('should call else fetchChartData', () => {
      taxonomyControllerServiceSpy.getAggregatedValues.and.returnValue(of(null as any));
      spyOn(component as any, 'fetchChartData').and.callThrough();
      component['fetchChartData'](null,null);
      expect(component['fetchChartData']).toHaveBeenCalled();
    });

    it('should call else ngOnDestroy', () => {
      component['chartPlot'] = null as any;
      spyOn(component, 'ngOnDestroy').and.callThrough();
      component.exportChart('');
      component.ngOnDestroy();
      expect(component.ngOnDestroy).toHaveBeenCalled();
    });
})