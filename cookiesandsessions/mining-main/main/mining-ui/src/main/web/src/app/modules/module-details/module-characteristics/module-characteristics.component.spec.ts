import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule } from '@ngx-translate/core';
import { ModuleCharacteristicsComponent } from './module-characteristics.component';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { of } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { HttpClient } from '@angular/common/http';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { FeatureControllerService } from '@innowake/mining-api-angular-client';

describe('ModuleCharacteristicsComponent', () => {
  let component: ModuleCharacteristicsComponent;
  let fixture: ComponentFixture<ModuleCharacteristicsComponent>;

  const i18nServiceSpy = { language: 'en-US' };
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'get']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ModuleCharacteristicsComponent],
      imports: [TranslateModule.forRoot(), HttpClientTestingModule],
      providers: [NumberFormatter,
        FeatureToggleService, FeatureControllerService,
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        { provide: I18nService, useValue: i18nServiceSpy }]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleCharacteristicsComponent);
    component = fixture.componentInstance;
    component.module={
      metricsDate:'2021-04-20T08:15:51.614'
    };
    fixture.detectChanges();
  });

  it('should create', () => {
    component.ngOnChanges();
    expect(component).toBeTruthy();
  });

  it('should get number of lines of comments in module', () => {
    spyOn(TestBed.inject(FeatureToggleService), 'isActive').and.returnValue(of(true));
    expect(component.getNumberOfLinesOfComment()).toEqual('0');

    component.module = {
      sourceMetrics: {
        commentLines: 4,
        codeLines: 10
      },
    };
    expect(component.getNumberOfLinesOfComment()).toEqual('4 (40%)');
  });
});
