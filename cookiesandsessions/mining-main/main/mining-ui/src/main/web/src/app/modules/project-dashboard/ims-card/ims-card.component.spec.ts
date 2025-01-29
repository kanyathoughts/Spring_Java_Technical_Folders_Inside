import { ComponentFixture, TestBed } from '@angular/core/testing';
import { TranslateModule } from '@ngx-translate/core';
import { of } from 'rxjs/internal/observable/of';
import { IMSCardComponent } from './ims-card.component';
import { AggregationResultRelationshipFieldName, ReferenceControllerService } from '@innowake/mining-api-angular-client';

describe('ImsCardComponent', () => {
  let component: IMSCardComponent;
  let fixture: ComponentFixture<IMSCardComponent>;
  const referenceControllerServiceStub: jasmine.SpyObj<ReferenceControllerService> = jasmine.createSpyObj('ReferenceControllerService', ['getAggregatedValues2', 'getAggregatedValues1']);
  const referenceAggregationResponce: AggregationResultRelationshipFieldName[] = [
    {
        "group": {
          "IN_NAME": "INDB1" as any
        },
        "fields": {
          "OUT_ID": 1 as any
        }
      }
  ] 

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ IMSCardComponent ],
      imports: [
        TranslateModule.forRoot({})
      ],
      providers: [
        {
          provide: ReferenceControllerService,
          useValue: referenceControllerServiceStub,
        },
      ]
    })
    .compileComponents();
    referenceControllerServiceStub.getAggregatedValues1.and.returnValue(of(referenceAggregationResponce as any));
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(IMSCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create with charts', () => {
    expect(component).toBeTruthy();
  });

  it('should navigate to Module details page', () => {
    component.projectId = 1;
    spyOn(component,'navigateToDetails').and.callThrough();
    component.navigateToDetails({ id: 1 });
    expect(component.navigateToDetails).toHaveBeenCalled();
  });

});
