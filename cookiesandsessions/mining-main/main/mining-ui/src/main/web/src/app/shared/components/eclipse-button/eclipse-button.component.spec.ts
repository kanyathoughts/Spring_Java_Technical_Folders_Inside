import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { EclipseButtonComponent } from './eclipse-button.component';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor, HttpService } from '@app/core';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { TranslateModule } from '@ngx-translate/core';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { of } from 'rxjs/internal/observable/of';
import { FeatureControllerService } from '@innowake/mining-api-angular-client';

describe('EclipseButtonComponent', () => {
  let component: EclipseButtonComponent;
  let fixture: ComponentFixture<EclipseButtonComponent>;
  const deeplinkServiceSpy = jasmine.createSpyObj<DeepLinkService>('DeepLinkService', ['featureIsActive', 'heartbeat']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ EclipseButtonComponent ],
      imports: [
        HttpClientTestingModule,
        RouterTestingModule,
        TranslateModule.forRoot({})
      ],
      providers: [
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        FeatureToggleService,
        FeatureControllerService,
        {
          provide: HttpClient,
          useClass: HttpService
        },
        { provide: DeepLinkService, useValue: deeplinkServiceSpy }
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    deeplinkServiceSpy.featureIsActive.and.returnValue(of(true));
    deeplinkServiceSpy.heartbeat.and.returnValue(of(true).toPromise());
    fixture = TestBed.createComponent(EclipseButtonComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
    expect(deeplinkServiceSpy.heartbeat).toHaveBeenCalled();
  });

  it('should check for the feature guard', () => {
    deeplinkServiceSpy.featureIsActive.and.returnValue(of(false));
    spyOn((component as any),'checkReachable');
    component.ngOnInit();
    expect((component as any).checkReachable).not.toHaveBeenCalled();
  });
  
  it('Should get the tooltip text', () => {
    component.reachable = true;
    const tooltipText = component.getTooltipText();
    expect(tooltipText).toEqual('eclipseDeeplink.tooltipOn');
  });
  
  it('should display online icon for fromSharedModule', () => {
    component.reachable = false;
    expect(component.getIconType()).toEqual('eclipse-icons:eclipse-offline');
  });

  it('should display offline icon', () => {
    component.reachable = true;
    expect(component.getIconType()).toEqual('eclipse-icons:eclipse-online');
  });

  it('should disable the eclipse button when server is unreachable', () => {
    deeplinkServiceSpy.heartbeat.and.returnValue(of(false).toPromise());
    component.ngOnInit();
    expect(component.reachable).toBeFalsy();
    const eclipseButton = fixture.nativeElement.querySelector('button');
    expect(eclipseButton.disabled).toBeTruthy();
  });

  it('should emit when button is clicked', () => {
    spyOn(component.clicked, 'emit');
    component.onClick();
    expect(component.clicked.emit).toHaveBeenCalled();
  });

  it('should destroy', () => {
    component.ngOnDestroy();
    expect(component.isDestroyed).toEqual(true);
  });
});
