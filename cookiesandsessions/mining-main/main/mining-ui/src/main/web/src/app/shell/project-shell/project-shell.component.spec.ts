import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { ActivatedRoute, Router } from '@angular/router';
import { ProjectShellComponent } from './project-shell.component';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { of } from 'rxjs';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { ModuleBadgeUpdateService } from '@app/core/services/module-details/module-badge-update.service';
import { BadgeCountUpdateOperation } from '@app/modules/module-details/models/module-complexity-details';
import { ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { FeatureControllerService, MiningUiExtensionsControllerService, ModuleControllerService } from '@innowake/mining-api-angular-client';

describe('ProjectShellComponent', () => {

  let component: ProjectShellComponent;
  let fixture: ComponentFixture<ProjectShellComponent>;
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(0, 'TestClient', 0, 'TestProject');
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const routerSpy = jasmine.createSpyObj('Router', ['navigate']);
  const identityAccessManagementServiceSpy = jasmine.createSpyObj('identityAccessManagementService', ['isAccessTokenFromUrl']);
  const extensionServiceSpy = jasmine.createSpyObj('MiningUiExtensionsControllerService', ['getWebUiExtensions', 'getTableExtensions']);
  const moduleBadgeUpdateServiceSpy: jasmine.SpyObj<ModuleBadgeUpdateService> = jasmine.createSpyObj(
    'ModuleBadgeUpdateService',
    ['updateAnnotationDataDictionary', 'getAnnotationDataDictionary', 'updateModuleToBeReviewed', 'getModuleToBeReviewed']
  );
  const fakeActivatedRoute = {
    snapshot: { data: { subMenu: "metricsSubMenu" } }
  };
  const moduleControllerServiceSpy = jasmine.createSpyObj('moduleControllerServiceSpy', ['countRequiresReview']);
  const i18nServiceSpy = { language: 'en-US' };
  const scrollEventServiceSpy = jasmine.createSpyObj('ScrollEventService', ['getScrollObservable']);
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [TranslateModule.forRoot({}), HttpClientTestingModule],
      declarations: [ProjectShellComponent],
      providers: [
        FeatureControllerService,
        FeatureToggleService,
        TranslateService,
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: Router, useValue: routerSpy },
        { provide: IdentityAccessManagementService, useValue: identityAccessManagementServiceSpy },
        { provide: ModuleBadgeUpdateService, useValue: moduleBadgeUpdateServiceSpy },
        { provide: MiningUiExtensionsControllerService, useValue: extensionServiceSpy },
        { provide: ActivatedRoute, useValue: fakeActivatedRoute },
        NumberFormatter,
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: ScrollEventService, useValue: scrollEventServiceSpy}
      ]
    }).compileComponents();
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    moduleBadgeUpdateServiceSpy.getAnnotationDataDictionary.and.returnValue(of(BadgeCountUpdateOperation.ANNOTATION_DELETED) as any);
    moduleBadgeUpdateServiceSpy.getModuleToBeReviewed.and.returnValue(of(1) as any);
    routerSpy.url = '/project-1/modules';
  }));

  beforeEach(() => {
    identityAccessManagementServiceSpy.isAccessTokenFromUrl.and.returnValues(false, true);
    extensionServiceSpy.getWebUiExtensions.and.returnValue(of([] as any));
    extensionServiceSpy.getTableExtensions.and.returnValue(of([] as any));
    moduleControllerServiceSpy.countRequiresReview.and.returnValue(of(0));
    scrollEventServiceSpy.getScrollObservable.and.returnValue(of('down'));
    fixture = TestBed.createComponent(ProjectShellComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
  it('should check when response module is greater than 0', () => {
    component.ngOnInit();
    expect(component.subMenuEnable).toBeDefined();
  });
  it('should check when response getAnnotationDataDictionary and isShowLastScanBadge is true', () => {
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(null as any));
    moduleControllerServiceSpy.countRequiresReview.and.returnValue(of(1));
    component.ngOnInit();
    expect(component.isShowLastScanBadge).toBeTruthy();
  });

  it('should show hide left nav first item', () => {
    component.ngOnInit();
    expect(component.showHide).toBe('project-shell__nav-sider__content-down');
  });

  it('should check when response module is equal to  0', () => {
    moduleControllerServiceSpy.countRequiresReview.and.returnValue(of(0));
    component.ngOnInit();
    expect(component.isShowLastScanBadge).toBeFalsy();
  });

  it('should check when response module is equal to  1', () => {
    moduleControllerServiceSpy.countRequiresReview.and.returnValue(of(1));
    component.ngOnInit();
    expect(component.isShowLastScanBadge).toBeTruthy();
  });

  it('should set current client', () => {
    expect(component.currentClient).toBeTruthy();
    expect((component.currentClient as any).clientId).toEqual(0);
    expect((component.currentClient as any).projectId).toEqual(0);
  });

  it('Should set the sidebarState in localStorage', () => {
    localStorage.setItem('isSidebarCollapsed', 'false');
    component.isCollapsed = false;

    component.changeSidebarState();
    expect(localStorage.getItem('isSidebarCollapsed')).toBe('true');

    component.changeSidebarState();
    expect(localStorage.getItem('isSidebarCollapsed')).toBe('false');
  });

  it('Should set the handleSubMenuOpen and return true', () => {
    component.subMenuEnable = 'metricsSubMenu'
    let result = component.handleSubMenuOpen('metricsSubMenu');
    expect(result).toEqual(true);
  });

  it('Should unset the submenu and return false', () => {
    component.subMenuEnable = 'Submenu1'
    let result = component.handleSubMenuOpen('Submenu2');
    expect(result).toEqual(false);
  });
});
