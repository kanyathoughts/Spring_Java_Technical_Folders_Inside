import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { SharedModule } from '@app/shared';
import { CoreModule, I18nService } from '@app/core';
import { ProjectDashboardComponent } from './project-dashboard.component';
import { CUSTOM_ELEMENTS_SCHEMA, NO_ERRORS_SCHEMA } from '@angular/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { of } from 'rxjs';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HotSpotCardComponent } from './hotspot-card/hotspot-card.component';
import { RouterTestingModule } from '@angular/router/testing';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { MetricsSharedModule } from '../metrics/shared/metrics-shared.module';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { IMSCardComponent } from './ims-card/ims-card.component';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { WindowToken } from '@app/core/utils/window';
import { AggregationResultRelationshipFieldName, FeatureControllerService, HotSpotControllerService, ModuleControllerService, ReferenceControllerService, SavedSearchControllerService, SavedSearchCountResponse, TaxonomyControllerService } from '@innowake/mining-api-angular-client';

describe('ProjectDashboardComponent', () => {
  let component: ProjectDashboardComponent;
  let fixture: ComponentFixture<ProjectDashboardComponent>;
  let clientProjectRelationship: ClientProjectRelationship;
  let mockWindow: any;
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
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const featureToggleSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureControllerService', ['isActive']);
  const referenceControllerServiceStub: jasmine.SpyObj<ReferenceControllerService> = jasmine.createSpyObj('ReferenceControllerService', ['getAggregatedValues2', 'getAggregatedValues1']);
  const taxonomyControllerServiceSpy: jasmine.SpyObj<TaxonomyControllerService> = jasmine.createSpyObj('TaxonomyControllerService', ['findAllTaxonomies']);
  const savedSearchControllerServiceSpy: jasmine.SpyObj<SavedSearchControllerService> = jasmine.createSpyObj('SavedSearchControllerService', ['getDashboardSavedSearchCounts']);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
  ['getSelectedDataPoints', 'resetTableColumnAndDataPoints', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange', 'handleFilters', 'getGraphQlParam']);
  const moduleControllerServiceSpyResp = [
    { 'group': { 'TECHNOLOGY': 'JCL' }, 'fields': { 'ID': 1, 'LINES_OF_CODE': 1 } },
    { 'group': { 'TECHNOLOGY': 'COBOL' }, 'fields': { 'ID': 4, 'LINES_OF_CODE': 5 } }
  ];
  const selectedColumns: NzTreeNodeOptions[] = [
    { 'title': 'Module Name', 'name': 'name', 'key': 'name', 'checked': true, 'disableCheckbox': true, 'path': 'content.name' },
    { 'title': 'Metrics Date', 'name': 'metricsDate', 'key': 'metricsDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.metricsDate' },
    { 'title': 'Technology', 'name': 'technology', 'key': 'technology', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.technologyLink' },
    { 'title': 'Type', 'name': 'type', 'key': 'type', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.typeLink' },
    { 'title': 'Modified Date', 'name': 'modifiedDate', 'key': 'modifiedDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.modifiedDate' }
  ];
  const savedSearchCount: SavedSearchCountResponse[]= [
    {
      "savedSearch": {
        "id": 8,
        "name": "Missing Source Files",
        "usage": "miningUi.modulesTable",
        "savedSearch": "columns=Annotation.typeLink&columns=AnnotationCategory.name&columns=Module.name&columns=Annotation.sourceAttachment&columns=Annotation.stateLink&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Annotation.name&page=1&sort=in_HasAnnotation.out.name;ASC&filter=[{\"key\":\"typeLink\",\"value\":[\"RULE\"]},{\"key\":\"categoryLink.name\",\"value\":[\"Business Rule\"]},{\"key\":\"stateLink\",\"value\":[\"CANDIDATE\"]}]",
        "projectId": 1,
      },
      count: 169
    },
    {
      "savedSearch": {
        "id": 10,
        "name": "No Taxonomies Assigned",
        "usage": "miningUi.modulesTable",
        "savedSearch": "columns=Annotation.typeLink&columns=AnnotationCategory.name&columns=Module.name&columns=Annotation.sourceAttachment&columns=Annotation.stateLink&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Annotation.name&page=1&sort=in_HasAnnotation.out.name;ASC&filter=[{\"key\":\"typeLink\",\"value\":[\"RULE\"]},{\"key\":\"categoryLink.name\",\"value\":[\"Business Rule\"]},{\"key\":\"stateLink\",\"value\":[\"CANDIDATE\"]}]",
        "projectId": 1,
      },
      "count": 0
    },
  ]
  const moduleControllerServiceSpy = jasmine.createSpyObj('moduleControllerServiceSpy', ['getAggregatedValues']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
    const i18nServiceSpy = { language: 'en-US' };
    beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CoreModule,
        HttpClientTestingModule,
        SharedModule,
        MetricsSharedModule,
        TranslateModule.forRoot({}),
        BrowserAnimationsModule
      ],
      declarations: [ProjectDashboardComponent, HotSpotCardComponent, IMSCardComponent],
      providers: [
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: FeatureToggleService, useValue: featureToggleSpy },
        {
          provide: ReferenceControllerService,
          useValue: referenceControllerServiceStub,
        },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        {provide : TaxonomyControllerService, useValue : taxonomyControllerServiceSpy},
        { provide: WindowToken, useValue: mockWindow },
        TranslateService,
        FeatureToggleService,
        FeatureControllerService,
        HotSpotControllerService,
        TaxonomyControllerService,
        NumberFormatter,
        SavedSearchControllerService,
        { provide: I18nService, useValue: i18nServiceSpy }
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA, NO_ERRORS_SCHEMA]
    }).compileComponents();
    referenceControllerServiceStub.getAggregatedValues1.and.returnValue(of(referenceAggregationResponce as any));
    clientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject','27/11/2021' as any);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedColumns as any));
    labelMappingServiceSpy.mapLabel.and.returnValue('COBOL' as any);
    taxonomyControllerServiceSpy.findAllTaxonomies.and.returnValue(of([{taxonomyReferenceCount : 10}, {taxonomyReferenceCount : 11}]) as any);
    savedSearchControllerServiceSpy.getDashboardSavedSearchCounts.and.returnValue(of(savedSearchCount as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectDashboardComponent);
    component = fixture.componentInstance;
    clientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');
    component.clientProjectRelationship = clientProjectRelationship;
    clientProjectRelationshipServiceSpy.setClientProjectRelationship(component.clientProjectRelationship);
    moduleControllerServiceSpy.getAggregatedValues.and.returnValue(of(moduleControllerServiceSpyResp));
    featureToggleSpy.isActive.and.returnValue(of(false));
    taxonomyControllerServiceSpy.findAllTaxonomies.and.returnValue(of([{taxonomyReferenceCount : 10}, {taxonomyReferenceCount : 11}]) as any);
    savedSearchControllerServiceSpy.getDashboardSavedSearchCounts.and.returnValue(of(savedSearchCount as any));
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should navigate to module on click of module name ', () => {
    let moduleId = 82455;
    component.hotspotConfigs.forEach(hotspotConfig => {
      let columnActionParent;
      if (hotspotConfig.type === 'DATA_SETS') {
        columnActionParent = 'dataSetName';
      } else if (hotspotConfig.type === 'DATABASE_TABLES') {
        columnActionParent = 'databaseName';
      } else {
        columnActionParent = 'moduleName';
      }
      const path = hotspotConfig.tableConfig.columnMap[columnActionParent].columnAction.resolveURL({id : moduleId});
      expect(path).toEqual(RouteBuilder.buildModuleRoute(clientProjectRelationship.getProjectId(), moduleId, 'details/overview'));
      moduleId++;
    });
  });

  it('hide IMS card', () => {
    component.hideImsCard(true);
    expect(component.showIMSCard).toBeTruthy();
  });

  it('should test fetchTaxonomiesData', () => {
  component['fetchTaxonomiesData'](1);
  expect(component.taxonomies).toBeDefined();
  });

  it('should test fetchSavedSearchData', () => {
    component['fetchSavedSearchdata'](1);
    expect(component.savedSearchs).toBeDefined();
    });
});
