import { ComponentFixture, TestBed } from '@angular/core/testing';
import { I18nService } from '@app/core';
import { CustomPropertiesService, CustomPropertyMetadataWithClass } from '@app/core/services/custom-properties/custom-properties.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { ScrollDirection, ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { WindowToken } from '@app/core/utils/window';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { of } from 'rxjs';
import { SharedModuleDetailsComponent } from './shared-module-details.component';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ModuleDetailsDPSidePanelService } from '@app/core/services/module-details-and-dp-panel-state.service';
import { AnnotationPojo, ModuleControllerService, ModulePojo, TaxonomyAssignmentsGetResponse, TaxonomyControllerService } from '@innowake/mining-api-angular-client';

describe('SharedModuleDetailsComponent', () => {
  const taxonomy: TaxonomyAssignmentsGetResponse = {
    moduleCount: 1,
    taxonomies: [{
      taxonomy: {
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
        }
      },
      "state": "ALL"
  },
  {
    taxonomy: {
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
    }
  },
  {
    taxonomy: {
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
  }
]};
  let component: SharedModuleDetailsComponent;
  let fixture: ComponentFixture<SharedModuleDetailsComponent>;
  const scrollEventServiceSpy = jasmine.createSpyObj<ScrollEventService>('ScrollEventService', ['getScrollObservable']);
  const moduleServiceSpy = jasmine.createSpyObj<ModuleControllerService>('ModuleControllerService', ['findModuleById', 'findAnnotationsForModule']);
  const taxonomyServiceSpy = jasmine.createSpyObj<TaxonomyControllerService>('TaxonomyControllerService', ['getAssignedTaxonomyByModule']);
  const i18nServiceSpy = { language: 'en-US' };
  const graphQlControllerServiceStub = jasmine.createSpyObj<GraphQlControllerService>
    ('GraphQlControllerService', ['graphQl']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'getContentComponent', 'destroy']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);
  const customPropertyService = jasmine.createSpyObj<CustomPropertiesService>('CustomPropertiesService', ['getCustomPropertiesMetadataForClass']);
  const deepLinkServiceSpy: jasmine.SpyObj<DeepLinkService> = jasmine.createSpyObj('DeepLinkService', ['showModuleInEclipse','featureIsActive']);
  const ModuleDetailsDPSidePanelServiceSpy: jasmine.SpyObj<ModuleDetailsDPSidePanelService> = jasmine.createSpyObj('ModuleDetailsDPSidePanelService', ['editModuleDescription','setPanelState' ,'descriptionModal' , 'getPanelState']);
  const testModule: ModulePojo = {
    id: 2,
    name: 'test Module',
    projectId: 2,
    customProperties: {
      "ModuleCustomProperties": [
        {
          "name": "customMetaInfo2",
          "value": "another test value",
          "dataType": "STRING"
        },
        {
          "name": "customMetaInfo1",
          "value": "some test value",
          "dataType": "STRING"
        }
      ]
    }
  }
  let mockWindow: any;
  let openedUrl = '';
  const graphQlData: any = {
    "data": {
      "project": { "customPropertyClasses": { Annotation: ["AnnotationCustomProperties"], Client: ["ClientCustomProperties"], Module: ["ModuleCustomProperties"], Project: ["ProjectCustomProperties"], Taxonomy: ["TaxonomyCustomProperties"] } }
    }
  };

  const customPropertyMetadata: CustomPropertyMetadataWithClass[] = [{
    "name": "customMetaInfo1",
    "label": "Some custom meta information 1",
    "dataType": "STRING",
    "dataSource": "Custom datasource URL 1",
    "description": "This is some more custom meta information 1",
    "fieldType": "DEFAULT",
    "pluginVisible": false,
    "mandatory": false,
    "min": 5 as any,
    "max": 26 as any,
    "readOnly": true,
    "showWhen": {},
    "customViewNames": [],
    "customViewIndex": 0,
    "validationRegex": null,
    "validationErrorMessage": null,
    "autoCompletionKey": null,
    "customPropertyClassName": "ModuleCustomProperties"
  }]

  const codeAnnotationsValue: AnnotationPojo[] = [
    {
      uid: '#172:234',
      customProperties: {
        'Property1': [{
          name: 'customAnnotationProperty',
          value: 'A value for the custom Annotation property',
          dataType: 'STRING'
        }]
      },
      id: 1,
      name: 'Annotation 1',
      projectId: 1,
      state: 'CANDIDATE',
      type: 'RULE',
      categoryId: 1,
      categoryName: 'Annotation Category A',
      createdByUserId: 'admin',
      updatedByUserId: null,
      sourceAttachment: 'abcd',
      moduleName: 'PRG1',
      location: {
        offset: 100,
        length: 4
      }
    }
  ];

  beforeEach(async () => {
    mockWindow = {
      get location() {
        return {
          href: 'http://localhost:8081/#/browse-modules/1/1/1/explore',
          hash: '#/browse-modules/1/1/1/explore'
        };
      },
      open: (sUrl: any) => {
        openedUrl = sUrl;
      }
    };
    mockWindow.open.bind(mockWindow);
    await TestBed.configureTestingModule({
      declarations: [SharedModuleDetailsComponent],
      imports: [TranslateModule.forRoot({}),],
      providers: [
        NumberFormatter,
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: ScrollEventService, useValue: scrollEventServiceSpy },
        { provide: TaxonomyControllerService, useValue: taxonomyServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
        { provide: CustomPropertiesService, useValue: customPropertyService },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
        TranslateService,
        { provide: NzMessageService, useValue: {} },
        {
          provide: DeepLinkService,
          useValue: deepLinkServiceSpy
        },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceStub },
        { provide: ModuleControllerService, useValue: moduleServiceSpy } ,
        { provide: ModuleDetailsDPSidePanelService, useValue: ModuleDetailsDPSidePanelServiceSpy }]
    })
      .compileComponents();
  });

  beforeEach(() => {
    const complex = {
      "sourceMetrics": {
        "recordId": "#713:425",
        "customProperties": {},
        "id": 6816,
        "codeLines": 107,
        "commentLines": 46,
        "complexityMcCabe": 3,
        "deadCodeLines": 15
      }
    }
    scrollEventServiceSpy.getScrollObservable.and.returnValue(of('down' as any));
    moduleServiceSpy.findModuleById.and.returnValue(of(testModule as any));
    taxonomyServiceSpy.getAssignedTaxonomyByModule.and.returnValue(of(taxonomy as any));
    moduleServiceSpy.findAnnotationsForModule.and.returnValue(of(codeAnnotationsValue as any));
    graphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData as any));
    customPropertyService.getCustomPropertiesMetadataForClass.and.returnValue(of(customPropertyMetadata as any));
    deepLinkServiceSpy.featureIsActive.and.returnValue(of(true));
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    fixture = TestBed.createComponent(SharedModuleDetailsComponent);
    component = fixture.componentInstance;
    component.moduleDetails = {
      id: 1,
      name: "Graph",
      technology: 'SQL'
    }
    fixture.detectChanges();
    component.scrollPosition = ScrollDirection.DOWN
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test getDataForSharedModule', () => {
    component.scrollPosition = ScrollDirection.DOWN;
    component.selectedItem = undefined;
    expect(component.getDataForSharedModule);
    fixture.detectChanges();

  });

  it('should test getDataForSharedModule with scrollposition to move up wards', () => {
    component.scrollPosition = ScrollDirection.UP;
    component.selectedItem = "selectedItem";
    component.moduleComplexity = {color: 'red' ,
    description: 'description'}
    expect(component.getDataForSharedModule);
    fixture.detectChanges();

  });


  it('should test the getDataForSharedModule', () => {
    component.getDataForSharedModule();
    expect(component.taxonomyTotalNumber).toBeGreaterThanOrEqual(0);
  });

  it('should toggle the open side bar', () => {
    component.isEclipseLinkAvailable=of(true);
    component.showDropDown = {
      characteristics: false,
      metrics: false,
      description: false,
      taxonomy: false,
      annotations: false,
      customProperty: false
    };
    component.toggelDropDown('taxonomy');
    expect(component.showDropDown.taxonomy).toBeFalsy();
  });

  it('should open module in new browser tab', () => {
    component.moduleDetails = { id: 1, name: "Graph" , storage: ModulePojo.StorageEnum.FILE_SECTION }
    component.openInNewBrowserTab('code-viewer');
    expect(component.moduleDetails.storage).toEqual(ModulePojo.StorageEnum.FILE_SECTION);
  });
  it('should open module in new browser tab', () => {
    component.moduleDetails = { id: 1, name: "Graph" , storage: undefined }
    component.openInNewBrowserTab('code-viewer');
    expect(component.moduleDetails.storage).toBeUndefined();
  });

  it('should test closeEditDetails', () => {
    let value = {
      "ModuleCustomProperties": [
        {
          "name": "customMetaInfo2",
          "value": "another test value",
          "dataType": "STRING"
        },
        {
          "name": "customMetaInfo1",
          "value": "some test value",
          "dataType": "STRING"
        }
      ]
    }
    component.customProperties = value as any;
    spyOn(component.hideShowTab, 'emit');
    component.closeEditDetails();
    expect(component.hideShowTab.emit).toHaveBeenCalledWith(false);
  });

  it('should test setCustomPropertiesDetails', () => {
    let value = {
      ModuleCustomProperties: {
        customMetaInfo2: {
          value: "another test value",
          dataType: "STRING"
        }
      }
    }

    component.moduleCustomProperties = [
        {
            "name": "customMetaInfo2",
            "label": "Some custom meta information 2",
            "dataType": "STRING",
            "dataSource": "Custom datasource URL 2",
            "description": "This is some more custom meta information 2",
            "fieldType": "DEFAULT",
            "pluginVisible": false,
            "mandatory": false,
            "min": null,
            "max": null,
            "readOnly": false,
            "showWhen": {},
            "customViewNames": [],
            "customViewIndex": 0,
            "validationRegex": null,
            "validationErrorMessage": null,
            "autoCompletionKey": null,
            "customPropertyClassName": "ModuleCustomProperties"
        }
    ]
    component.customProperties = value as any;
    (component as any).setCustomPropertiesDetails();
    expect(component.customPropertiesDetails.length).toBeGreaterThan(0);
  });


  it('should get taxonomy tree data', () => {
    component.moduleDetails = {
      id: 1,
      name: "Graph"
    }
    spyOn(component, 'getAssignedTaxonomyByModule').and.callThrough();
    component.getAssignedTaxonomyByModule();
    component.moduleComplexity={color:'brown',description:'notAvailable'}
    expect(component.moduleComplexity.description).toBeDefined();
  });

  it('should open in eclipse', () => {
    component.openInEclipse();
    expect(deepLinkServiceSpy.showModuleInEclipse).toHaveBeenCalledWith(
       {projectId: component.projectId, path: component.modulePath});
  });

  it('should open DescriptionModal', () => {
    let event = jasmine.createSpyObj('event', ['preventDefault', 'stopPropagation']);
    component.openDescriptionModal(event);
    expect(ModuleDetailsDPSidePanelServiceSpy.editModuleDescription).toHaveBeenCalled();
  });
  it('should test editTaxonomyDetails ', () => {
    component.moduleDetails = { id: 1, name: "Graph" , storage: ModulePojo.StorageEnum.FILE_SECTION };
    component.editTaxonomyDetails();
    expect( typeof(component as any).editTaxonomyDetails()).toBeDefined();
  });
  
});
