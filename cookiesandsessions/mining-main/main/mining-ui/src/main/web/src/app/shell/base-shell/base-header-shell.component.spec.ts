import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TranslateModule } from '@ngx-translate/core';
import { CoreModule, HttpService } from '@app/core';
import { BaseHeaderShellComponent } from './base-header-shell.component';
import { HeaderComponent } from '../header/header.component';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { NzMenuModule } from 'ng-zorro-antd/menu';
import { FormsModule } from '@angular/forms';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { KeycloakService } from '@app/core/authentication/keycloak.service';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { NzModalService } from 'ng-zorro-antd/modal';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { of } from 'rxjs';
import { HttpClient, HttpClientModule } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ClientControllerService, FeatureControllerService, ProjectControllerService } from '@innowake/mining-api-angular-client';

describe('BaseHeaderShellComponent', () => {
  let component: BaseHeaderShellComponent;
  let fixture: ComponentFixture<BaseHeaderShellComponent>;
  let keycloakServiceSpy: KeycloakService;

  const identityAccessManagementServiceSpy = jasmine.createSpyObj('identityAccessManagementService', ['getUsername', 'isAccessTokenFromUrl']);
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService', ['getClientProjectObservable']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        TranslateModule.forRoot(),
        CoreModule,
        NzMenuModule,
        HttpClientModule,
        HttpClientTestingModule,
        FormsModule,
        NzDropDownModule
      ],
      providers: [
        ClientControllerService, 
        ProjectControllerService,
        OauthtokenService,
        DeepLinkService,
        FeatureToggleService,
        FeatureControllerService,
        { provide: IdentityAccessManagementService, useValue: identityAccessManagementServiceSpy },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        {
          provide: HttpClient,
          useClass: HttpService
      },
        NzModalService
      ],
      declarations: [HeaderComponent, BaseHeaderShellComponent]
    }).compileComponents();
    keycloakServiceSpy = TestBed.inject( KeycloakService);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(new ClientProjectRelationship(1, 'client', 1, 'project')));  

    spyOn(keycloakServiceSpy, 'getUserRoles').and.returnValue([]);
  }));

  beforeEach(() => {
    identityAccessManagementServiceSpy.getUsername.and.returnValue('userTest');
    fixture = TestBed.createComponent(BaseHeaderShellComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should set fullscreen', () => {
    component.setFullScreen(false);
    expect(component.header).toBeFalsy();
  });
});
