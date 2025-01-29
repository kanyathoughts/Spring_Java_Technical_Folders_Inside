import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalService } from 'ng-zorro-antd/modal';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { of } from 'rxjs';
import { UntypedFormBuilder } from '@angular/forms';
import { FieldTypeEnum, Column } from '../../mining-table/mining-table-config.interface';
import { SavedSearchComponent } from './saved-search.component';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { HttpClient, HttpHandler, HttpXsrfTokenExtractor } from '@angular/common/http';
import { MiningTableComponent } from '../../mining-table/mining-table.component';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor, HttpService } from '@app/core';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { ActivatedRoute } from '@angular/router';
import { ModuleControllerService, SavedSearchControllerService } from '@innowake/mining-api-angular-client';
import { ReachabilityService } from '@app/modules/reachability-ui-product-vision/utils/reachability.service';

const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>('NzNotificationService', ['create', 'warning', 'error']);

const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
  'success',
  'loading',
  'remove',
  'error'
]);
const IAMServiceSpy: jasmine.SpyObj<IdentityAccessManagementService> = jasmine.createSpyObj('IAMServiceSpy', ['getUsername', 'getUserId', 'logout']);
const savedSearchControllerServiceSpy: jasmine.SpyObj<SavedSearchControllerService> = jasmine.createSpyObj<SavedSearchControllerService>
  ('SavedSearchControllerService', ['findByUsage']);
const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('CustomizableTableColumnService',
  ['getSelectedDataPoints', 'resetTableColumnAndDataPoints', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange', 'handleFilters', 'getGraphQlParam', 'onPageLoad', 'getQueryParams', 'resetTable', 'setColumnIdsSelection', 'getDefaultSortBy']);
const moduleServiceSpy = jasmine.createSpyObj<ModuleControllerService>('ModuleControllerService', ['findModuleById']);
const translateServiceSpy = jasmine.createSpyObj<TranslateService>('TranslateService', ['instant']);
const authServiceSpy = jasmine.createSpyObj<KeycloakAuthorizationService>('KeycloakAuthorizationService', ['isClientAdmin']);
const updatedColumns: { [key: string]: Column; } = {
  "Module Name": {
    "field": "inHasAnnotation.out.name",
    "header": "moduleName",
    "warningMessage": "This module was modified by the last scan. Please review its meta data (annotations, data dictionary, module description, taxonomies).",
    "fieldType": FieldTypeEnum.STRING
  },
  "Annotation Type": {
    "field": "typeLink",
    "header": "Annotation Type",
  },
  "Category": {
    "field": "categoryLink.name",
    "header": "Category",
    "fieldType": FieldTypeEnum.STRING
  },
  "Source Code": {
    "field": "inHasAnnotation.out.sourceAttachmentLink.content",
    "header": "Source Code",
    "fieldType": FieldTypeEnum.STRING
  },
  "State": {
    "field": "stateLink",
    "header": "State",
  },
  "Annotation Description": {
    "field": "name",
    "header": "Annotation Description",
    "fieldType": FieldTypeEnum.STRING
  },
  "Modified By": {
    "field": "updatedByUserId",
    "header": "Modified By",
    "fieldType": FieldTypeEnum.STRING
  }
};

const savedSearch = [{
  id: 47,
  customProperties: {},
  name: "Business Rule Candidates",
  projectId: 0,
  savedSearch: "columns=Annotation.typeLink&columns=AnnotationCategory.name&columns=Module.name&columns=Annotation.sourceAttachment&columns=Annotation.stateLink&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Annotation.name&page=1&sort=in_HasAnnotation.out.name;ASC&filter=[{\"key\":\"typeLink\",\"value\":[\"RULE\"]},{\"key\":\"categoryLink.name\",\"value\":[\"Business Rule\"]},{\"key\":\"stateLink\",\"value\":[\"CANDIDATE\"]}]",
  usage: "miningUi.annotationsTable"
}];

const selectedColumns: NzTreeNodeOptions[] = [
  { 'title': 'Module Name', 'name': 'name', 'key': 'name', 'checked': true, 'disableCheckbox': true, 'path': 'content.name' },
  { 'title': 'Metrics Date', 'name': 'metricsDate', 'key': 'metricsDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.metricsDate' },
  { 'title': 'Technology', 'name': 'technology', 'key': 'technology', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.technologyLink' },
  { 'title': 'Type', 'name': 'type', 'key': 'type', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.typeLink' },
  { 'title': 'Modified Date', 'name': 'modifiedDate', 'key': 'modifiedDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.modifiedDate' }
]
const childComponent: jasmine.SpyObj<MiningTableComponent> = jasmine.createSpyObj('MiningTableComponent', [
  'setCurrentTableFilters',
]);
const oAuthToken: any = {
  access_token: 'ffb3eff8-53a7-4153-bff1',
  token_type: 'bearer',
  refresh_token: 'a30a484a-b1a1-4580-a437',
  expires_in: 315575999,
  scope: 'read write trust',
  username: 'test-admin'
};
describe('SavedSearchComponent', () => {
  let component: SavedSearchComponent;
  let fixture: ComponentFixture<SavedSearchComponent>;
  let oauthServiceSpy: OauthtokenService;
  let savedSearchStatusSpy: jasmine.Spy;
  let reachabilityServiceSpy: jasmine.SpyObj<ReachabilityService>;
  

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [SavedSearchComponent, MiningTableComponent],
      imports: [NzDropDownModule,
      TranslateModule.forRoot({}),
      RouterTestingModule.withRoutes([])],
      providers: [
      UntypedFormBuilder,
      ApiPrefixInterceptor,
      ErrorHandlerInterceptor,
      HttpXsrfTokenExtractor,
      { provide: ReachabilityService, useValue: reachabilityServiceSpy },
      { provide: SavedSearchControllerService, useValue: savedSearchControllerServiceSpy },
      TranslateService,
      { provide: NzNotificationService, useValue: notificationSpy },
      { provide: NzMessageService, useValue: messageServiceSpy },
      { provide: ModuleControllerService, useValue: moduleServiceSpy },
      { provide: NzModalService, useValue: modalServiceSpy },
      { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
      { provide: HttpClient, useClass: HttpService },
      { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
      { provide: IdentityAccessManagementService, useValue: IAMServiceSpy },
      {
        provide: ActivatedRoute,
        useValue: {
        snapshot: {
          params: {
          id: '123'
          }
        }
        }
      },
      HttpHandler
      ]
    })
      .compileComponents();
    userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedColumns as any));
    userCustomizableTableServiceSpy.updateTableConfig.and.returnValue(updatedColumns as any);
    userCustomizableTableServiceSpy.getDefaultSortBy.and.returnValue('{content_name: ASC}');
    translateServiceSpy.instant.and.returnValue({});
    modalServiceSpy.create.and.returnValue(null);
    savedSearchControllerServiceSpy.findByUsage.and.returnValue(of(savedSearch as any));
    authServiceSpy.isClientAdmin.and.returnValue(true);
    oauthServiceSpy = TestBed.inject(OauthtokenService);
    spyOn(oauthServiceSpy, 'getUsername').and.returnValue(oAuthToken.username);
    IAMServiceSpy.getUsername.and.returnValue('userTest');
    IAMServiceSpy.getUserId.and.returnValue('1');
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SavedSearchComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    const activatedRoute = TestBed.get(ActivatedRoute);
    component.savedSearchDetails = {savedSearchName: 'Missing Source Files' , initialTableConfig: Object({}), route: activatedRoute }
    savedSearchStatusSpy = spyOn(component.savedSearchStatus, 'emit');
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should clear saved search', () => {
    component.savedSearchList = [{
      id: 3,
      name: 'Missing Source Files',
      savedSearch: 'columns=Module.name&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Module.identificationLink&page=1&sort=name.toLowerCase();ASC&filter=[{\"key\":\"identificationLink\",\"value\":[\"MISSING\"]}]',
      scope: 'INDIVIDUAL'
    }];
    spyOn((component as any), 'setSavedSearchParameter').and.callThrough();
    component.selectedSavedSearchName = 'Missing Source Files';
    component.setSavedSearchParameter(component.savedSearchList[0]);
    expect((component as any).setSavedSearchParameter).toHaveBeenCalled();
  });

  it('should test onSavedSearchSelection, emit saved search status on selecting saved search', () => {
    component.onSavedSearchSelection();
    component.savedSearchList = [{
      id: 3,
      name: 'Missing Source Files',
      savedSearch: 'columns=Module.name&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Module.identificationLink&page=1&sort=name.toLowerCase();ASC&filter=[{\"key\":\"identificationLink\",\"value\":[\"MISSING\"]}]',
      scope: 'INDIVIDUAL'
    }];
    component.selectedSavedSearch =  {
      id: 3,
      name: 'Missing Source Files',
      savedSearch: 'columns=Module.name&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Module.identificationLink&page=1&sort=name.toLowerCase();ASC&filter=[{\"key\":\"identificationLink\",\"value\":[\"MISSING\"]}]',
      scope: 'INDIVIDUAL'
    }
    expect(component.savedSearchList).toContain(component.selectedSavedSearch);
    expect(savedSearchStatusSpy).toHaveBeenCalledOnceWith(component.savedSearchDetails);
  });

  it('should test if we have to show duplicate button or the save option', () => {
    component.selectedSavedSearch = {
      name: 'Test search',
      savedSearch: "columns=Annotation.typeLink&columns=AnnotationCategory.name&columns=Module.name&columns=Annotation.sourceAttachment&columns=Annotation.stateLink&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Annotation.name&page=1&sort=in_HasAnnotation.out.name;ASC&filter=[{\"key\":\"typeLink\",\"value\":[\"RULE\"]},{\"key\":\"categoryLink.name\",\"value\":[\"Business Rule\"]},{\"key\":\"stateLink\",\"value\":[\"CANDIDATE\"]}]",
      createdByUserId: '2',
      createdByUserName: 'test'
    };
    const resp = component.showDuplicateButton();
    expect(resp).toBeTrue();

    component.selectedSavedSearch = {
      name: 'Test search',
      createdByUserId: '1',
      createdByUserName: 'test'
    };
    const resp1 = component.showDuplicateButton();
    expect(resp1).toBeFalse();
  });
});
