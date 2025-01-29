import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule } from '@ngx-translate/core';
import { UntypedFormBuilder } from '@angular/forms';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { of } from 'rxjs/internal/observable/of';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { CustomPropertyFieldListComponent } from './custom-property-field-list.component';
import { CustomPropertiesService } from '@app/core/services/custom-properties/custom-properties.service';
import { CustomPropertyMetadata, MetamodelControllerService } from '@innowake/mining-api-angular-client';

const customPropertyMetadata: any = [
      {
          "name": "test1",
          "label": "test1",
          "dataType": "EMBEDDEDLIST",
          "dataSource": null,
          "description": null,
          "fieldType": "TAG",
          "pluginVisible": false,
          "mandatory": false,
          "min": null,
          "max": null,
          "readOnly": false,
          "showWhen": {},
          "customViewNames": [],
          "customViewIndex": 12,
          "validationRegex": null,
          "validationErrorMessage": null,
          "autoCompletionKey": "test1AutoCompletionKey",
          "customPropertyClassName": 'test'
      }
  ];
const graphQlData: any =  { "annotationTags": ['Tag 2', 'Tag 1'], "otherTags": ['Tag O2', 'Tag O2', 'Tag O1'] };

describe('CustomPropertyFieldListComponent', () => {
  let component: CustomPropertyFieldListComponent;
  let fixture: ComponentFixture<CustomPropertyFieldListComponent>;
  const currentClient: ClientProjectRelationship = new ClientProjectRelationship(1, 'test', 1, 'test');
  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('ClientProjectRelationshipService',
    ['getClientProjectObservable']);
  const customPropertyService = jasmine.createSpyObj<CustomPropertiesService>('CustomPropertiesService', ['getCustomPropertiesMetadataForClass', 'getAutoCompletionValues']);
  const formBuilder: UntypedFormBuilder = new UntypedFormBuilder();
  const formBuilderStub = {};
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [CustomPropertyFieldListComponent],
      imports: [
        HttpClientTestingModule,
        TranslateModule.forRoot({})
      ],
      providers: [
        { provide: UntypedFormBuilder, useValue: formBuilderStub },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService()},
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy},
        MetamodelControllerService,
        { provide: CustomPropertiesService, useValue: customPropertyService }
      ]
    }).compileComponents();
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(currentClient as any));
    customPropertyService.getAutoCompletionValues.and.returnValue(of(graphQlData));
    customPropertyService.getCustomPropertiesMetadataForClass.and.returnValue(of(customPropertyMetadata as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CustomPropertyFieldListComponent);
    component = fixture.componentInstance;
    let fb: UntypedFormBuilder = new UntypedFormBuilder();
    component['fb'] = fb;
    component.propertiesMetamodels = [
      {
          "name": "AnnotationCustomProperties.customMetaInfo",
          "customPropertyClassName": "Annotations",
          "label": "Some custom meta information",
          "dataType": "STRING",
          "dataSource": "Custom datasource URL",
          "description": "This is some more custom meta information",
          "fieldType": "DEFAULT",
          "pluginVisible": false,
          "mandatory": false,
          "min": 5 as any,
          "max": 30 as any,
          "readOnly": true,
          "showWhen": {
              "annotationCategoryId": "42"  as any
          },
          "customViewNames": [
              "First Custom View",
              "Second Custom View"
          ],
          "customViewIndex": 21,
          "validationRegex": "[a-z]*",
          "validationErrorMessage": "Incorrect Format",
          "autoCompletionKey": null,
          "inputName": "AnnotationCustomProperties_customMetaInfo"
      },
      {
          "name": "AnnotationCustomProperties.annotationTags",
          "customPropertyClassName": "Annotations",
          "label": "Annotation Tags",
          "dataType": "EMBEDDEDLIST",
          "dataSource": null,
          "description": null,
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
          "autoCompletionKey": "annotationTags",
          "inputName": "AnnotationCustomProperties_annotationTags",
          "optionList": []
      },
      {
          "name": "AnnotationCustomProperties.colorTags",
          "customPropertyClassName": "Annotations",
          "label": "Annotation Colors",
          "dataType": "EMBEDDEDLIST",
          "dataSource": null,
          "description": null,
          "fieldType": "TAG",
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
          "autoCompletionKey": "colorTags",
          "inputName": "AnnotationCustomProperties_colorTags",
          "optionList": []
      },
      {
          "name": "AnnotationCustomProperties.ruleTags",
          "customPropertyClassName": "Annotations",
          "label": "Annotation Rules",
          "dataType": "EMBEDDEDLIST",
          "dataSource": null,
          "description": null,
          "fieldType": "TAG",
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
          "autoCompletionKey": "ruleTags",
          "inputName": "AnnotationCustomProperties_ruleTags",
          "optionList": []
      },
      {
          "name": "AnnotationCustomProperties.customAnnotationProperty",
          "customPropertyClassName": "Annotations",
          "label": "Custom Annotation Property",
          "dataType": "STRING",
          "dataSource": null,
          "description": "A custom property for the Annotation class",
          "fieldType": "DEFAULT",
          "pluginVisible": true,
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
          "inputName": "AnnotationCustomProperties_customAnnotationProperty"
      },
      {
        "name": "AnnotationCustomProperties.someUrl",
        "customPropertyClassName": "Annotations",
        "label": "Annotation URL",
        "dataType": "STRING",
        "dataSource": null,
        "description": "A custom URL property for the Annotation class",
        "fieldType": "URL",
        "pluginVisible": true,
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
        "inputName": "AnnotationCustomProperties_customAnnotationProperty"
    }
  ];
  component.parentForm = formBuilder.group({
    type: "RULE",
    name: "test",
    state: "INVALID",
    categoryId: 2,
    englishTranslation: 'translation',
    category: 'test',
    TAG: '["hi","hello"]',
    customProperties: [{ 'name': 'customAnnotationProperty','value': 'hello', 'mandatory': false }, { 'name': 'ruleTags', 'value': '["hello","hi"]', 'mandatory': true }]
  });
  component.customProperties = {
    "AnnotationCustomProperties": [
        {
            "name": "ruleTags",
            "value": null,
            "dataType": "EMBEDDEDLIST"
        },
        {
            "name": "customAnnotationProperty",
            "value": null,
            "dataType": "STRING"
        },
        {
            "name": "annotationTags",
            "value": null,
            "dataType": "EMBEDDEDLIST"
        },
        {
            "name": "customMetaInfo",
            "value": null,
            "dataType": "STRING"
        },
        {
            "name": "dependentAnnotations",
            "value": null,
            "dataType": "LINKLIST"
        },
        {
            "name": "referencedTables",
            "value": null,
            "dataType": "LINKLIST"
        },
        {
            "name": "colorTags",
            "value": null,
            "dataType": "EMBEDDEDLIST"
        }
    ]
  };
  });

  it('can load instance', () => {
    expect(component).toBeTruthy();
  });

  it('ngOnInit', () => {
    customPropertyService.getCustomPropertiesMetadataForClass.and.returnValue(of(customPropertyMetadata as any));
    component.ngOnInit();
    expect(component.propertiesMetamodels).toBeDefined();
  });

  it('test ngOnInit when getCustomPropertiesMetadataForClass returns no data', () => {
    customPropertyService.getCustomPropertiesMetadataForClass.and.returnValue(of(undefined));
    component.ngOnInit();
    expect(component.propertiesMetamodels).toBeDefined();
  });

  it('test updateAutoCompletionValues', () => {
    (component as any).updateAutoCompletionValues();
    expect(component.autoCompletions).toBeDefined()
   });

   it('should test getSubmitValue when no custom properties', () => {
    component.customProperties = {"NoClassName": []};
    component.getSubmitValue();
    expect(component.customProperties).toBeDefined();
  });

  it('should test getSubmitValue when custom properties has some random data', () => {
    component.customProperties = {"AnnotationCustomProperties": [{"name": "customMetaInfo"}]};
    component.getSubmitValue();
    expect(component.customProperties).toBeDefined();
  });

  it('should test getAutoCompletionList', () => {
    component.getAutoCompletionList({});
    expect(component.propertiesMetamodels).toBeDefined();
  });

  it('should test getAutoCompletionList', () => {
    const simpleChangesMock: any = {
      selectedCustomPropertyCategory: {
        currentValue: 'new-value',
        previousValue: 'old-value',
        isFirstChange: () => false, 
      }
    }
    component.ngOnChanges(simpleChangesMock);
    expect(component.autoCompletions).toBeDefined()
  });

  it ('should test getAutoCompletionList for TAG', () => {
    component.autoCompletions["someAutoCompletionKey"] = ["red", "green", "blue"];
    const value = component.getAutoCompletionList({
      fieldType: 'TAG',
      autoCompletionKey: "someAutoCompletionKey"
    });
    expect(component.propertiesMetamodels).toBeDefined();
    expect(value).toEqual(["red", "green", "blue"]);
  });

});
