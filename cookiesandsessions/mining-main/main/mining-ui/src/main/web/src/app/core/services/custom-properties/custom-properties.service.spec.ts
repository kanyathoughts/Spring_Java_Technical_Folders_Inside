import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { TranslateService } from '@ngx-translate/core';
import { of } from 'rxjs';
import { HttpService } from '@app/core';
import { HttpClient } from '@angular/common/http';
import { CustomPropertiesService } from './custom-properties.service';
import { GraphQlControllerService } from '../graphql.service';
import { MetamodelControllerService } from '@innowake/mining-api-angular-client';

describe('CustomPropertiesService', () => {
  let customPropertiesService: CustomPropertiesService;
  let http: HttpClient;
  let httpMock: HttpTestingController;
  const graphQlData: any =  {"data": { "project": {"customPropertyClasses": { Annotation: ["AnnotationCustomProperties"], Client: ["ClientCustomProperties"], Module: ["ModuleCustomProperties"], Project: ["ProjectCustomProperties"], Taxonomy: ["TaxonomyCustomProperties"] } }
  }};
  const metaModelProperty: any = [
    {
      customViewIndex: 0,
      customViewNames: [],
      dataSource: null,
      dataType: "STRING",
      description: "A custom property for the Annotation class",
      label: "Custom Annotation Property",
      mandatory: false,
      max: null,
      min: null,
      name: "customAnnotationProperty",
      pluginVisible: true,
      readOnly: false,
      showWhen: {
        annotationCategoryName: 'test'
      },
      validationErrorMessage: null,
      validationRegex: null
    }
  ];

  const moduleCP: any = [
    {
      customViewIndex: 3,
      customViewNames: [],
      customPropertyClassName: 'ModuleCustomProperties',
      dataSource: null,
      dataType: "STRING",
      description: "A custom property for the Module class",
      label: "New Custom property",
      mandatory: false,
      max: null,
      min: null,
      name: "newCustomproperty",
      pluginVisible: true,
      readOnly: false,
      validationErrorMessage: null,
      validationRegex: null
    },
    {
      customViewIndex: 2,
      customViewNames: [],
      customPropertyClassName: 'ModuleCustomProperties',
      dataSource: null,
      dataType: "STRING",
      description: "A custom property for the Module class",
      label: "Test Custom property",
      mandatory: false,
      max: null,
      min: null,
      name: "testCustomproperty",
      pluginVisible: true,
      readOnly: false,
      validationErrorMessage: null,
      validationRegex: null
    },
    {
      customViewIndex: 1,
      customViewNames: [],
      customPropertyClassName: 'ModuleCustomProperties',
      dataSource: null,
      dataType: "STRING",
      description: "A custom property for the Module class",
      label: "Custom Module property",
      mandatory: false,
      max: null,
      min: null,
      name: "customModuleProperty",
      pluginVisible: true,
      readOnly: false,
      validationErrorMessage: null,
      validationRegex: null
    },
  ]
  
  const autocompletionMap = { "annotationTags": ['Tag 2', 'Tag 1'], "otherTags": ['Tag O2', 'Tag O2', 'Tag O1'] }
  const autocompletionRequestData =  {"data": { "project": {"autoCompletionMap":  autocompletionMap }}};

  const translateServiceSpy = jasmine.createSpyObj<TranslateService>('TranslateService', ['instant']);
  const graphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>
  ('GraphQlControllerService', ['graphQl']);
  const metamodelControllerServiceStub: jasmine.SpyObj<MetamodelControllerService> = jasmine.createSpyObj<MetamodelControllerService>
    ('MetamodelControllerService', ['findMetaModel']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        CustomPropertiesService,
        { provide: TranslateService, useValue: translateServiceSpy },
        { provide: HttpClientTestingModule, useValue: HttpService },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceStub },
        {
          provide: MetamodelControllerService,
          useValue: metamodelControllerServiceStub
        },
      ],
    });
    customPropertiesService = TestBed.inject(CustomPropertiesService);
    http = TestBed.inject(HttpClient);
    httpMock = TestBed.inject(HttpTestingController);

    translateServiceSpy.instant.and.returnValue({});
    graphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData as any));
    metamodelControllerServiceStub.findMetaModel.and.returnValue(of(metaModelProperty as any));
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should create instance of CustomPropertiesService', () => {
    expect(CustomPropertiesService).toBeTruthy();
  });

  it('should test downloadUrl method with incorrect end point', () => {
    customPropertiesService
    .getCustomPropertiesMetadataForClass('Annotation', 1)
    .subscribe(result => expect(result).toBeTruthy());
    graphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData as any));
    expect(metamodelControllerServiceStub.findMetaModel).toHaveBeenCalled();
  });

  it('should test sorting of customproperties', () => {
    graphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData as any));
    metamodelControllerServiceStub.findMetaModel.and.returnValue(of(moduleCP as any));
    const sortdCP = moduleCP.sort((prop1: any, prop2: any) => +prop1.customViewIndex - +prop2.customViewIndex)
    customPropertiesService.getCustomPropertiesMetadataForClass('Module', 1).subscribe(response => {
      spyOn(Array.prototype, 'sort');
      expect(response).toEqual(sortdCP)
    });
  });

  it('should return the annotation map', () => {
    graphQlControllerServiceStub.graphQl.and.returnValue(of(autocompletionRequestData as any));
    customPropertiesService.getAutoCompletionValues(1).subscribe(response => {
      expect(response).toBe(autocompletionMap);
    })
  });
});
