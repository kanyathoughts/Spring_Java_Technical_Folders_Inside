import { TestBed } from '@angular/core/testing';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { MetricsTableType } from '../../modules/metrics/shared/components/metrics-card/metrics-card.interface';
import { ChartDetailConfigService } from './chart-details-config.service';
import { of } from 'rxjs';
describe('ChartDetailConfigService', () => {
  let service: ChartDetailConfigService;
  beforeEach(() => {
    const LabelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['mapLabel']);
    TestBed.configureTestingModule({
      providers: [
        ChartDetailConfigService,
        { provide: LabelMappingService, useValue:  LabelMappingServiceSpy}
      ]
    });
    service = TestBed.inject(ChartDetailConfigService);
    LabelMappingServiceSpy.mapLabel.and.returnValue(of(  "label") as any);
  });
  it('can load instance', () => {
    expect(service).toBeTruthy();
  });

  it('should test with DNATable', () => {
    service.getChartDataConfigByTableType(MetricsTableType.DNATable , 1);
    expect(service.getChartDataConfigByTableType(MetricsTableType.DNATable , 1)).toBeTruthy()
  });

  it('should pass switch case  of ModuleDetailsTable', () => {
    service.getChartDataConfigByTableType(MetricsTableType.ModuleDetailsTable , 1);
    expect(service.getChartDataConfigByTableType(MetricsTableType.ModuleDetailsTable , 1)).toBeTruthy()
  });
  it('should pass switch case  of UtilitiesTable', () => {
    service.getChartDataConfigByTableType(MetricsTableType.UtilitiesTable , 1);
    expect(service.getChartDataConfigByTableType(MetricsTableType.UtilitiesTable , 1)).toBeTruthy()
   
  });
  it('should pass switch case  of InterfacesTable', () => {
    service.getChartDataConfigByTableType(MetricsTableType.InterfacesTable , 1);
    expect(service.getChartDataConfigByTableType(MetricsTableType.InterfacesTable , 1)).toBeTruthy()
   
  });
  it('should pass switch case  of SqlDetailsTable', () => {
    service.getChartDataConfigByTableType(MetricsTableType.SqlDetailsTable , 1);
    expect(service.getChartDataConfigByTableType(MetricsTableType.SqlDetailsTable , 1)).toBeTruthy()
   
  });
  it('should pass switch case  of IMSTable', () => {
    service.getChartDataConfigByTableType(MetricsTableType.IMSTable , 1);
    expect(service.getChartDataConfigByTableType(MetricsTableType.IMSTable , 1)).toBeTruthy()
  });
  it('should pass switch case  of default', () => {
    service.getChartDataConfigByTableType(null , 1);
    expect(service.getChartDataConfigByTableType(null, 1)).toBeTruthy()
    
  });
});
