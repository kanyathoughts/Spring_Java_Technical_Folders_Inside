import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { of } from 'rxjs';
import { ApplicationDecompositionComponent } from './application-decomposition.component';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { ReferenceControllerService, TaxonomyControllerService, TaxonomyPojo } from '@innowake/mining-api-angular-client';
import { HttpClient } from '@angular/common/http';

describe('ApplicationDecompositionCompoenent', () => {
  let component: ApplicationDecompositionComponent;
  let fixture: ComponentFixture<ApplicationDecompositionComponent>;
  let taxonomyControllerServiceSpy: TaxonomyControllerService;

  const i18nServiceSpy = { language: 'en-US' };
  const taxonomy: TaxonomyPojo[] = [
    {
      type: { name: "Program Type"},
      name: "Batch",
      taxonomyReferenceCount: 4
    },
    {
      type: { name: "Program Type"},
      name: "UI",
      taxonomyReferenceCount: 2
    },
    {
      type: { name: "Program Type"},
      name: "Library",
      taxonomyReferenceCount: 7
    }
  ]
  const taxonomyDetails = [{ taxonomyId: '1', taxonomyTitle: 'Test' }];

  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(0, 'TestClient', 0, 'TestProject');
  const clientProjectRelationshipServiceStub: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const metricsFilterServiceStub: jasmine.SpyObj<MetricsFilterService> = jasmine.createSpyObj<MetricsFilterService>
    ('MetricsFilterService', ['getMetricsTaxonomyFilter']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [ApplicationDecompositionComponent],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({}),
      ],
      providers: [
        TaxonomyControllerService,
        ReferenceControllerService,
        HttpClient,
        {
          provide: ClientProjectRelationshipService,
          useValue: clientProjectRelationshipServiceStub,
        },{
          provide : MetricsFilterService,
          useValue: metricsFilterServiceStub
        },
        NumberFormatter,
        { provide: I18nService, useValue: i18nServiceSpy }
      ]
    }).compileComponents();
    taxonomyControllerServiceSpy = TestBed.inject(TaxonomyControllerService);
    clientProjectRelationshipServiceStub.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    metricsFilterServiceStub.getMetricsTaxonomyFilter.and.returnValue(of(taxonomyDetails) as any);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ApplicationDecompositionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('can load instance', () => {
    expect(component).toBeTruthy();
    spyOn(taxonomyControllerServiceSpy, 'findAllTaxonomies').and.returnValue(of(taxonomy as any));
    component.ngOnInit();
    expect(taxonomyControllerServiceSpy.findAllTaxonomies).toHaveBeenCalled();
    component.loadState = LoaderState.loading;
  });

  describe('should fetch application decomposition data', () => {

    it('should test metrics card for data', () => {
      spyOn(taxonomyControllerServiceSpy, 'findAllTaxonomies').and.returnValue(of(taxonomy as any));
      (component as any).fetchApplicationDecompositionData(1,[13]);
      expect(taxonomyControllerServiceSpy.findAllTaxonomies).toHaveBeenCalled();
      expect(component.metricCardList[0].title).toBe('metrics.applicationDecomposition.cardTitle');
    });

    it('should test metrics card for no data', () => {
      spyOn(taxonomyControllerServiceSpy, 'findAllTaxonomies').and.returnValue(of([] as any));
      (component as any).fetchApplicationDecompositionData(1, [], []);
      expect(taxonomyControllerServiceSpy.findAllTaxonomies).toHaveBeenCalled();
      expect(component.metricCardList[0].title).toBe('metrics.applicationDecomposition.noDataCardTitle');
    });

    xit('should test generateFilterQuery when there is not filter selected', () => {
      const generateFilterQueryResult = component.generateFilterQuery({}, '', [12]).filterObject;
      expect(generateFilterQueryResult).toEqual({'content_taxonomies_id': {in: [12] as any}});
    });

    it('should test generateFilterQuery when there is filter selected', () => {
      const generateFilterQueryResult = component.generateFilterQuery({}, 'test: test', [12]).filterObject;
      expect(generateFilterQueryResult).toEqual({'content_taxonomies_id': {in: [12] as any}});
    });

  });
});