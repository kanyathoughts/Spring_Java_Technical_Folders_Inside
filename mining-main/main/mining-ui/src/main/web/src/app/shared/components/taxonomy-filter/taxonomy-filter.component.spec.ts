import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { TaxonomyFilterComponent } from './taxonomy-filter.component';
import { of } from 'rxjs';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule } from '@ngx-translate/core';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { TaxonomyControllerService, TaxonomyPojo } from '@innowake/mining-api-angular-client';
const taxonomyDetails = [{taxonomyId: '1', taxonomyTitle: 'Test'}]

describe('TaxonomyFilterComponent', () => {
  let component: TaxonomyFilterComponent;
  let fixture: ComponentFixture<TaxonomyFilterComponent>;
  let taxonomyControllerServiceSpy: TaxonomyControllerService;

  const taxonomy: TaxonomyPojo[] =[{
    "uid": "#122:196",
    "customProperties": {
      "TaxonomyCustomProperties": [
        {
          "name": "customTaxonomyProperty",
          "value": "A value for the custom Taxonomy property",
          "dataType": "STRING"
        }
      ]
    },
    "id": 1,
    "name": "Employee domain",
    "projectId": 1,
    "type": {
      "name": "DataDomain",
      "projectId": 1,
      "category": {
        "name": "Business Taxonomies",
        "projectId": 1,
        "id": 4
      }
    },
    "taxonomyReferenceCount": 2
  },
  {
    "uid": "#123:196",
    "customProperties": {},
    "id": 2,
    "name": "Create Invoices",
    "projectId": 1,
    "type": {
      "name": "BusinessProcess",
      "projectId": 1,
      "category": {
        "name": "Business Taxonomies",
        "projectId": 1,
        "id": 4
      }
    },
    "taxonomyReferenceCount": 0
  },
  {
    "uid": "#124:196",
    "customProperties": {},
    "id": 3,
    "name": "ARB100",
    "projectId": 1,
    "type": {
      "name": "BusinessSubsystem",
      "projectId": 1,
      "category": {
        "name": "Business Taxonomies",
        "projectId": 1,
        "id": 4
      }
    },
    "taxonomyReferenceCount": 1
  }
];

  //  [{ "recordId": "#226:181", "customProperties": { 'Property1': [{ "name": "customTaxonomyProperty", "value": "A value for the custom Taxonomy property", "dataType": "STRING" }] }, "id": 1, "name": "Employee domain", "projectId": 1, "type": { "recordId": "#186:180", "customProperties": {}, "name": "DataDomain", "projectId": 1, "category": { "projectId": 1, "name": "Business Taxonomies" } }, "taxonomyReferenceCount": 2 }, { "recordId": "#227:181", "customProperties": { 'Property1': [{ "name": "customTaxonomyProperty", "value": null, "dataType": "STRING" }] }, "id": 2, "name": "Create Invoices", "projectId": 1, "type": { "recordId": "#187:180", "customProperties": {}, "name": "BusinessProcess", "projectId": 1, "category": { "projectId": 1, "name": "Business Taxonomies" } }, "taxonomyReferenceCount": 0 }, { "recordId": "#228:181", "customProperties": { 'Property1': [{ "name": "customTaxonomyProperty", "value": null, "dataType": "STRING" }] }, "id": 3, "name": "ARB100", "projectId": 1, "type": { "recordId": "#188:180", "customProperties": {}, "name": "BusinessSubsystem", "projectId": 1, "category": { "projectId": 1, "name": "Business Taxonomies" } }, "taxonomyReferenceCount": 1 }];


  
  
  const clientProjectRelationshipServiceStub: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj<ClientProjectRelationshipService>
    ('ClientProjectRelationshipService', ['getClientProjectObservable']);

  const metricsFilterServiceStub: jasmine.SpyObj<MetricsFilterService> = jasmine.createSpyObj<MetricsFilterService>
    ('MetricsFilterService', ['getMetricsTaxonomyFilter', 'setMetricsTaxonomyFilter'])

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [TaxonomyFilterComponent],
      imports: [
        HttpClientTestingModule,
        TranslateModule.forRoot({})
      ],
      providers: [
        TaxonomyControllerService,
        {
          provide: ClientProjectRelationshipService,
          useValue: clientProjectRelationshipServiceStub
        },
        {
          provide: MetricsFilterService,
          useValue: metricsFilterServiceStub
        }
      ]
    }).compileComponents();
    taxonomyControllerServiceSpy = TestBed.inject(TaxonomyControllerService);
    metricsFilterServiceStub.getMetricsTaxonomyFilter.and.returnValue(of(taxonomyDetails) as any);
    clientProjectRelationshipServiceStub.getClientProjectObservable.and.returnValue(of(new ClientProjectRelationship(1, 'client', 1, 'project')));
    spyOn(taxonomyControllerServiceSpy, 'findAllTaxonomies').and.returnValue(of(taxonomy as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TaxonomyFilterComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('can load instance', () => {
    expect(component).toBeTruthy();
  });

  it('displayTrigger', () => {
    const data = {
      parentNode: {
        title: 'Employee domain'
      },
      title: 'DataDomain'
    };
    spyOn(component, 'displayTrigger');
    component.displayTrigger(data as any);
    expect(component.displayTrigger).toHaveBeenCalled();
  });

  it('should create 3 level tree structure', () => {
    spyOn(component as any, 'getTaxonomyTreeData').and.callThrough();
    (component as any).getTaxonomyTreeData(taxonomy);
    expect((component as any).getTaxonomyTreeData).toHaveBeenCalled();
  });

  it('should update the app', waitForAsync(() => {
    component.useMetricsFilterService = false;
    component.ngOnInit();
    expect(component).toBeTruthy();
    component.useMetricsFilterService = false;
    expect(component.selectedValue.length).toBe(0);
  }));

  it('should test onTaxonomySelection', () => {
    component.onTaxonomySelection(['test_1']);
  });

  it('should test displayTrigger', () => {
    component.displayTrigger({level: 1, key: 'key_test'} as any);
  
  });
  
  

});
