import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpService } from '@app/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { assert } from 'console';
import { of } from 'rxjs/internal/observable/of';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';

import { ImsComponent } from './ims.component';
import { AggregationResultRelationshipFieldName, ReferenceControllerService } from '@innowake/mining-api-angular-client';

describe('ImsComponent', () => {
  let component: ImsComponent;
  let fixture: ComponentFixture<ImsComponent>;
  const clientProjectRelationshipServiceStub: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService', ['getClientProjectObservable']);
  const referenceControllerServiceStub: jasmine.SpyObj<ReferenceControllerService> = jasmine.createSpyObj('ReferenceControllerService', ['getAggregatedValues2', 'getAggregatedValues1']);
  const chartGlobalStylesServiceStub: jasmine.SpyObj<ChartGlobalStyles> = jasmine.createSpyObj('ChartGlobalStyles', ['getPieConfig']);
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
    'GraphQlControllerService',
    ['graphQl']
  );
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(0, 'TestClient', 0, 'TestProject');
  const referenceAggregationResponce: AggregationResultRelationshipFieldName[] = [
    {
      group: {
        PROPERTY_DB_ACCESS_TYPE: 'READ' as any
      },
      fields: {
        ID: 17 as any
      }
    },
    {
      group: {
        PROPERTY_DB_ACCESS_TYPE: 'OTHER'
      },
      fields: {
        ID: 8
      }
    },
    {
      group: {
        PROPERTY_DB_ACCESS_TYPE: 'STORE'
      },
      fields: {
        ID: 4
      }
    },
    {
      group: {
        PROPERTY_DB_ACCESS_TYPE: 'DELETE'
      },
      fields: {
        ID: 5
      }
    },
    {
      group: {
        PROPERTY_DB_ACCESS_TYPE: 'UPDATE'
      },
      fields: {
        ID: 2
      }
    }
  ] 
  const graphQl = {
    data: {
      modules: {
        content: [{
          'dependencyCount': 0,
          id: 5425,
          inCodebase: false,
          metricsDate: "2021-09-24T10:55:46.758Z",
          name: "MMRS00C.A.LOADLIB",
          objectTypeLink: { typeLink: "FILE", technologyLink: "RESOURCE" },
          requiresReview: false
        }],
      }
    }
  }

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ImsComponent ],
      imports: [
        TranslateModule.forRoot({}),
        HttpClientTestingModule
      ],
      providers: [
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        {
          provide: ClientProjectRelationshipService,
          useValue: clientProjectRelationshipServiceStub,
        },
        {
          provide: HttpClient,
          useClass: HttpService
        },
        {
          provide: ReferenceControllerService,
          useValue: referenceControllerServiceStub,
        },
        {
          provide: ChartGlobalStyles,
          useValue: chartGlobalStylesServiceStub,
        }
      ]
    })
    .compileComponents();
    clientProjectRelationshipServiceStub.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    referenceControllerServiceStub.getAggregatedValues1.and.returnValue(of(referenceAggregationResponce as any));
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ImsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create with charts', () => {
    expect(component).toBeTruthy();
    expect(component.metricCardList.length).toEqual(4);
  });

  it('should test generateFilterQuery method when title is All IMS Calls', () => {
    const chartValue = {key : 'READ'}
    const title = 'metrics.ims.allCallsChartTitle';
    component.generateFilterQuery(chartValue, title);
    const filterString = component.generateFilterQuery
    expect(filterString).toBeDefined();
  });

  it('should test generateFilterQuery method when title is Get All Charts', () => {
    const chartValue = {key : 'READ'}
    const title = 'metrics.ims.getCallsChartTitle';
    component.generateFilterQuery(chartValue, title);
    const filterString = component.generateFilterQuery
    expect(filterString).toBeDefined();
  });

  it('should test generateFilterQuery method when title Insert Delete Replace CallsChart', () => {
    const chartValue = {key : 'READ'}
    const title = 'metrics.ims.insertDeleteReplaceCallsChart';
    component.generateFilterQuery(chartValue, title);
    const filterString = component.generateFilterQuery
    expect(filterString).toBeDefined();
  });
});
