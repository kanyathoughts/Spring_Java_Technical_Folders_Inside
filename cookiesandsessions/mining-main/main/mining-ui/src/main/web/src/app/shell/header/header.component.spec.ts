import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { TranslateModule } from '@ngx-translate/core';
import { RouterTestingModule } from '@angular/router/testing';

import { I18nService, AuthenticationService } from '@app/core';
import { HeaderComponent } from './header.component';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { StateMaintainenceService } from '@app/core/services/state-maintenance/state-maintainence.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Router } from '@angular/router';
import { NzMenuModule } from 'ng-zorro-antd/menu';
import { FormsModule } from '@angular/forms';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { Injectable } from '@angular/core';
import { Observable, of, Subject } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { HttpClient, HttpClientModule } from '@angular/common/http';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { KeycloakService } from '@app/core/authentication/keycloak.service';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { FeatureControllerService } from '@innowake/mining-api-angular-client';

@Injectable()
class TestFeatureToggleService extends FeatureToggleService {
  isActive(featureId: string): Observable<boolean> {
    if (featureId === 'eclipseDeepLink') {
      return of(true);
    } else {
      return super.isActive(featureId);
    }
  }
}

xdescribe('HeaderComponent', () => {
  let component: HeaderComponent;
  let fixture: ComponentFixture<HeaderComponent>;
  const keycloakServiceSpy = jasmine.createSpyObj<KeycloakService>('KeycloakService', ['getUserRoles', 'getUserInitials']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose']);
  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>(
    'ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship', 'setProject']
  );
  const IAMServiceSpy: jasmine.SpyObj<IdentityAccessManagementService> = jasmine.createSpyObj('IAMServiceSpy', ['getUsername','logout']);
  const scrollEventServiceSpy = jasmine.createSpyObj('ScrollEventService', ['getScrollObservable']);
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'skipErrorHandler', 'get']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        TranslateModule.forRoot(),
        NzMenuModule,
        FormsModule,
        HttpClientTestingModule,
        HttpClientModule,
        NzDropDownModule
      ],
      declarations: [HeaderComponent],
      providers: [
        I18nService,
        AuthenticationService,
        OauthtokenService,
        StateMaintainenceService,
        DeepLinkService,
        { 
          provide: FeatureToggleService,
          useClass: TestFeatureToggleService
        },
        FeatureControllerService,
        {
          provide: HttpClient,
          useValue: httpServiceSpy
        },
        KeycloakAuthorizationService,
        {
          provide: KeycloakService,
          useValue: keycloakServiceSpy
        },
        { 
          provide: NzModalService,
          useValue: modalServiceSpy },
        {
          provide: ClientProjectRelationshipService,
          useValue: clientProjectRelationshipServiceSpy
        },
        { provide: IdentityAccessManagementService, useValue: IAMServiceSpy },
        { provide: ScrollEventService, useValue: scrollEventServiceSpy},
      ]
    }).compileComponents();
    IAMServiceSpy.getUsername.and.returnValue('userTest');
    httpServiceSpy.disableApiPrefix.and.returnValue(httpServiceSpy);
    httpServiceSpy.get.and.returnValue(of('99.9.99-TRUNK-MINING-SNAPSHOT'));
    keycloakServiceSpy.getUserRoles.and.returnValue([]);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(new ClientProjectRelationship(1, 'client1', 1, 'project1')));
    scrollEventServiceSpy.getScrollObservable.and.returnValue(of('down'));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(HeaderComponent);
    component = fixture.componentInstance;

    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
  });

  it('should create', () => {
    fixture.detectChanges();
    expect(component).toBeTruthy();
  });

  it('should navigate to project selection screen', () => {
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');
    component.ngOnInit();
  });

  it('should navigate to client selection screen', () => {
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');
  });

  it('should get user initials for authorized user', () => {
    keycloakServiceSpy.getUserInitials.and.returnValue('TU');
    expect(component.getUserInitials()).toEqual('TU');
  });

  it('should get user initials for non authorized user', () => {
    keycloakServiceSpy.getUserInitials.and.returnValue('');
    component.username = "user1";
    expect(component.getUserInitials()).toEqual('U');
  });

  it('should call logout', () => {
    component.logout();
    expect(IAMServiceSpy.logout).toHaveBeenCalled();
  });

  it('should call settings', () => {
    component.settings();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should test opening about mining modal', () => {
   component.aboutMiningModalflag = true;
   component.handleMiningModal(true);
   expect(component.aboutMiningModalflag).toBeTruthy();
  });

  it('should test closing about mining modal', () => {
    component.aboutMiningModalflag = false;
    component.handleMiningModal(false);
    expect(component.aboutMiningModalflag).toBeFalsy();
   });
});
