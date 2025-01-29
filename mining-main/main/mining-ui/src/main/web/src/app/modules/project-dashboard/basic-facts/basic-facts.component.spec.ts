import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { of } from 'rxjs';
import { BasicFactsComponent } from './basic-facts.component';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '@app/shared';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NzCollapseModule } from 'ng-zorro-antd/collapse';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { WindowToken } from '@app/core/utils/window';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';
import { NzSkeletonModule } from 'ng-zorro-antd/skeleton';
import { NzStatisticModule } from 'ng-zorro-antd/statistic';
import { ModuleControllerService, ModuleStatisticsResponse } from '@innowake/mining-api-angular-client';

describe('BasicFactsComponent', () => {
  let component: BasicFactsComponent;
  let clientProjectRelationship: ClientProjectRelationship;
  let moduleStatisticsResponse: ModuleStatisticsResponse;
  let fixture: ComponentFixture<BasicFactsComponent>;
  let mockWindow: any;

  const moduleControllerServiceSpy = jasmine.createSpyObj('moduleControllerServiceSpy', ['getModuleStatistics']);
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any))
  moduleControllerServiceSpy.getModuleStatistics.and.returnValue(of(moduleStatisticsResponse as any));
  const i18nServiceSpy = { language: 'en-US' };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [FormsModule, SharedModule, NzSkeletonModule, NzStatisticModule, BrowserAnimationsModule, NzCollapseModule, TranslateModule.forRoot({}), HttpClientTestingModule, RouterTestingModule],
      declarations: [BasicFactsComponent],
      providers: [
        { provide: ClientProjectRelationship, useValue: clientProjectRelationship },
        TranslateService,
        NumberFormatter,
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
        { provide: I18nService, useValue: i18nServiceSpy }
      ]
    }).compileComponents();
    fixture = TestBed.createComponent(BasicFactsComponent);
    component = fixture.componentInstance;
    component.clientProjectRelationship = clientProjectRelationship;
    clientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject', '27/11/2021' as any);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    moduleControllerServiceSpy.getModuleStatistics.and.returnValue(of([]));
    fixture.detectChanges()
  }));


  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test for no data', () => {
    moduleControllerServiceSpy.getModuleStatistics.and.returnValue(of([]));
    component.ngOnInit();
    expect(component.showSkeleton).toBeFalse();
  });

  it('should test for module with errors', () => {
    const moduleStats: ModuleStatisticsResponse = {
      count: 10,
      sourceCodeLineCount: 1000,
      technologies: null,
      withErrorsCount: 5
    };
    moduleControllerServiceSpy.getModuleStatistics.and.returnValue(of(moduleStats as any));
    component.ngOnInit();
    fixture.detectChanges()
    expect(component.errorCount.toString()).toEqual(moduleStats.withErrorsCount.toString());
    expect(component.hasError).toBeTruthy();
  });

  it('should test for module with null errors', () => {
    const moduleStats: ModuleStatisticsResponse = {
      count: 10,
      sourceCodeLineCount: 1000,
      technologies: null,
      withErrorsCount: null
    };
    let debugElement: DebugElement = fixture.debugElement;
    moduleControllerServiceSpy.getModuleStatistics.and.returnValue(of(moduleStats as any));
    component.ngOnInit();
    fixture.detectChanges()
    expect(component.hasError).toBeFalsy();
    let input = debugElement.query(By.css(".error-card-template__text"));
    expect(input).toBeNull();
  });

  it('should test else part for the clientProjectRelationship service', () => {
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(null as any));
    component.ngOnInit();
    expect(component.showSkeleton).toBeTruthy();
  });
});
