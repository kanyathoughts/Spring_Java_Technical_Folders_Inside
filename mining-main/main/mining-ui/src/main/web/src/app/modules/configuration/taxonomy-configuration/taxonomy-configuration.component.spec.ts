import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { SharedModule } from '@app/shared';
import { NzTableModule } from 'ng-zorro-antd/table';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpTestingController, HttpClientTestingModule } from '@angular/common/http/testing';
import { Subject, of, throwError } from 'rxjs';
import { NzSelectModule } from 'ng-zorro-antd/select';
import { ActivatedRoute } from '@angular/router';
import { NzCardModule } from 'ng-zorro-antd/card';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { TaxonomyConfigurationComponent } from './taxonomy-configuration.component';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { AllowedTableActions } from '@app/shared/components/mining-table/mining-table-action.interface';
import { HttpClient, HttpResponse } from '@angular/common/http';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { AggregationResultTaxonomyFieldName, AnnotationControllerService, DataDictionaryControllerService, DataPointControllerService, FeatureControllerService, JobControllerService, JobInformation, MiningDataPointDefinitionWithPath, ModuleControllerService, ProjectControllerService, TaxonomyControllerService, TaxonomyPojo, TaxonomyTypeControllerService } from '@innowake/mining-api-angular-client';
import { TaxonomyModalService } from './taxonomy-modal.service';
import { EventEmitter } from '@angular/core';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { NzModalRef } from 'ng-zorro-antd/modal';

describe('TaxonomyConfigurationComponent', () => {
  let component: TaxonomyConfigurationComponent;
  let fixture: ComponentFixture<TaxonomyConfigurationComponent>;
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix']);
  const mockModalRef = {
    updateConfig: jasmine.createSpy('updateConfig'),
  };
  const dataPoints: MiningDataPointDefinitionWithPath[] = [
    {
      "name": "createdByUserId",
      "parentTypeName": "Annotation",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Modifications"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationV2"]),
      "displayName": "Created By",
      "description": "Name of the user who created the Annotaion",
      "path": "content.createdByUserId",
      "array": false,
      "id": "Annotation.createdByUserId",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "content",
      "parentTypeName": "SourceAttachment",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Base Data"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.SourceAttachmentV2"]),
      "displayName": "Source Code",
      "description": "Source Code attached to the Annotation",
      "path": "content.sourceAttachmentLink.content",
      "array": false,
      "id": "SourceAttachment.content",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "stateLink",
      "parentTypeName": "Annotation",
      "scalarType": null,
      "referenceTypeName": "AnnotationState",
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Base Data"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationV2"]),
      "displayName": "State",
      "description": "State of the Annotation",
      "path": "content.stateLink",
      "array": false,
      "id": "Annotation.stateLink",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "path",
      "parentTypeName": "Module",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable",
        "miningUi.moduleDetails",
        "miningUi.modulesTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Base Data"
        },
        "miningUi.modulesTable": {
          "category": "Base Data"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
      "displayName": "Path",
      "description": "Path of the file containing the Module",
      "path": "content.inHasAnnotation.out.path",
      "array": false,
      "id": "Module.path",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "name",
      "parentTypeName": "Module",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set([
        "general.viewMode",
        "miningUi.annotationsTable",
        "general.sortBy",
        "miningUi.modulesTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "general.viewMode": {
          "displayAs": "link",
          "linkTemplate": "#/project-${$projectId}/module-${id}/details/overview",
          "togetherWith": "id"
        },
        "miningUi.annotationsTable": {
          "rsqlFragment": "$in.HasAnnotation.name=='${query}*'",
          "category": "Base Data"
        },
        "general.searchFilter": {
          "filterMode": "text"
        },
        "miningUi.modulesTable": {
          "rsqlFragment": "name=='${query}*'",
          "sortByFieldName": "name",
          "category": "Base Data",
          "defaultColumnIndex": "0"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
      "displayName": "Module Name",
      "description": "The name of the Module",
      "path": "content.inHasAnnotation.out.name",
      "array": false,
      "id": "Module.name",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "typeLink",
      "parentTypeName": "ObjectType",
      "scalarType": null,
      "referenceTypeName": "Type",
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable",
        "general.sortBy",
        "miningUi.modulesTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "rsqlFragment": "$in.HasAnnotation.objectTypeLink.typeLink.name=in=(${query})'",
          "multiSelectValueRetrievalFieldName": "MODULE_TYPE",
          "multiSelectValueRetrievalMode": "annotationControllerAggregatedValues",
          "category": "Base Data"
        },
        "general.searchFilter": {
          "filterMode": "multiSelect"
        },
        "miningUi.modulesTable": {
          "rsqlFragment": "objectTypeLink.typeLink.name=in=(${query})'",
          "multiSelectValueRetrievalFieldName": "TYPE",
          "sortByFieldName": "objectTypeLink.typeLink.name",
          "multiSelectValueRetrievalMode": "moduleControllerDistinctFieldValues",
          "category": "Base Data",
          "defaultColumnIndex": "2"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.ObjectTypeV2"]),
      "displayName": "Type",
      "description": "Type of the Module",
      "path": "content.inHasAnnotation.out.objectTypeLink.typeLink",
      "array": false,
      "id": "ObjectType.typeLink",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "technologyLink",
      "parentTypeName": "ObjectType",
      "scalarType": null,
      "referenceTypeName": "Technology",
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable",
        "general.sortBy",
        "miningUi.modulesTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "rsqlFragment": "$in.HasAnnotation.objectTypeLink.technologyLink.name=in=(${query})'",
          "multiSelectValueRetrievalFieldName": "MODULE_TECHNOLOGY",
          "multiSelectValueRetrievalMode": "annotationControllerAggregatedValues",
          "category": "Base Data"
        },
        "general.searchFilter": {
          "filterMode": "multiSelect"
        },
        "miningUi.modulesTable": {
          "rsqlFragment": "objectTypeLink.technologyLink.name=in=(${query})'",
          "multiSelectValueRetrievalFieldName": "TECHNOLOGY",
          "sortByFieldName": "objectTypeLink.technologyLink.name",
          "multiSelectValueRetrievalMode": "moduleControllerDistinctFieldValues",
          "category": "Base Data",
          "defaultColumnIndex": "1"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.ObjectTypeV2"]),
      "displayName": "Technology",
      "description": "Technology of the Module",
      "path": "content.inHasAnnotation.out.objectTypeLink.technologyLink",
      "array": false,
      "id": "ObjectType.technologyLink",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "content",
      "parentTypeName": "SourceObject",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Base Data"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.SourceObjectV2"]),
      "displayName": "Annotation Source Code",
      "description": "Source Code attached to the Annotation",
      "path": "content.inHasAnnotation.out.sourceAttachmentLink.content",
      "array": false,
      "id": "SourceObject.content",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "name",
      "parentTypeName": "Annotation",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Description"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationV2"]),
      "displayName": "Annotation Description",
      "description": "The Description of the Annotation",
      "path": "content.name",
      "array": false,
      "id": "Annotation.name",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "typeLink",
      "parentTypeName": "Annotation",
      "scalarType": null,
      "referenceTypeName": "AnnotationType",
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Base Data"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationV2"]),
      "displayName": "Annotation Type",
      "description": "Type of the Annotation",
      "path": "content.typeLink",
      "array": false,
      "id": "Annotation.typeLink",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "updatedByUserId",
      "parentTypeName": "Annotation",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Modifications"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationV2"]),
      "displayName": "Modified By",
      "description": "Name of the user who last modified the Annotation",
      "path": "content.updatedByUserId",
      "array": false,
      "id": "Annotation.updatedByUserId",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "name",
      "parentTypeName": "AnnotationCategory",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set([
        "miningUi.annotationsTable"
      ]),
      "usageAttributes": {
        "miningUi.annotationsTable": {
          "category": "Base Data"
        }
      },
      "providedBy": new Set(["innowake.mining.data.model.springdata.AnnotationCategoryV2"]),
      "displayName": "Category",
      "description": "Category of the Annotation",
      "path": "content.categoryLink.name",
      "array": false,
      "id": "AnnotationCategory.name",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    }];
  const taxonomyAggregatedResponse: AggregationResultTaxonomyFieldName[] = [
    {
      "group": {
        "TYPE_NAME": "File Access" as any,
        "CATEGORY_NAME": "Technical Taxonomies" as any,
        "NAME": "Read" as any
      },
      "fields": {
        "MODULE_ID": 7 as any
      }
    },
    {
      "group": {
        "TYPE_NAME": "testTaxonomy",
        "CATEGORY_NAME": "Business Taxonomies",
        "NAME": "value 1"
      },
      "fields": {
        "MODULE_ID": 0
      }
    },
    {
      "group": {
        "TYPE_NAME": "test2",
        "CATEGORY_NAME": "Business Taxonomies",
        "NAME": "hello"
      },
      "fields": {
        "MODULE_ID": 1
      }
    },
    {
      "group": {
        "TYPE_NAME": "Program Type",
        "CATEGORY_NAME": "Technical Taxonomies",
        "NAME": "UI"
      },
      "fields": {
        "MODULE_ID": 24
      }
    },
    {
      "group": {
        "TYPE_NAME": "DB Access",
        "CATEGORY_NAME": "Technical Taxonomies",
        "NAME": "Delete"
      },
      "fields": {
        "MODULE_ID": 5
      }
    },
    {
      "group": {
        "TYPE_NAME": "Program Type",
        "CATEGORY_NAME": "Technical Taxonomies",
        "NAME": "Library"
      },
      "fields": {
        "MODULE_ID": 52
      }
    },
    {
      "group": {
        "TYPE_NAME": "DB Access",
        "CATEGORY_NAME": "Technical Taxonomies",
        "NAME": "Update"
      },
      "fields": {
        "MODULE_ID": 5
      }
    },
    {
      "group": {
        "TYPE_NAME": "DB Access",
        "CATEGORY_NAME": "Technical Taxonomies",
        "NAME": "Store"
      },
      "fields": {
        "MODULE_ID": 4
      }
    },
    {
      "group": {
        "TYPE_NAME": "Program Type",
        "CATEGORY_NAME": "Technical Taxonomies",
        "NAME": "Batch"
      },
      "fields": {
        "MODULE_ID": 7
      }
    },
    {
      "group": {
        "TYPE_NAME": "DB Access",
        "CATEGORY_NAME": "Technical Taxonomies",
        "NAME": "Read"
      },
      "fields": {
        "MODULE_ID": 9
      }
    },
    {
      "group": {
        "TYPE_NAME": "File Access",
        "CATEGORY_NAME": "Technical Taxonomies",
        "NAME": "Write"
      },
      "fields": {
        "MODULE_ID": 6
      }
    }
  ];
  const taxonomy = [
    {
      "recordId": "#122:196",
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
        "recordId": "#110:192",
        "customProperties": {},
        "name": "DataDomain",
        "projectId": 1,
        "category": {
          "name": "Business Taxonomies",
          "projectId": 1,
          "recordId": "#495:520",
          "customProperties": {},
          "id": 4
        }
      },
      "taxonomyReferenceCount": 2
    },
    {
      "recordId": "#123:196",
      "customProperties": {},
      "id": 2,
      "name": "Create Invoices",
      "projectId": 1,
      "type": {
        "recordId": "#111:192",
        "customProperties": {},
        "name": "BusinessProcess",
        "projectId": 1,
        "category": {
          "name": "Business Taxonomies",
          "projectId": 1,
          "recordId": "#495:520",
          "customProperties": {},
          "id": 4
        }
      },
      "taxonomyReferenceCount": 0
    },
    {
      "recordId": "#124:196",
      "customProperties": {},
      "id": 3,
      "name": "ARB100",
      "projectId": 1,
      "type": {
        "recordId": "#112:192",
        "customProperties": {},
        "name": "BusinessSubsystem",
        "projectId": 1,
        "category": {
          "name": "Business Taxonomies",
          "projectId": 1,
          "recordId": "#495:520",
          "customProperties": {},
          "id": 4
        }
      },
      "taxonomyReferenceCount": 1
    }
  ];

  const graphQlResponse = {
    "data": {
      "taxonomyCategories": {
        "categories": [
          {
            "id": 4,
            "name": "Business Taxonomies",
            "types": [
              {
                "name": "BusinessSubsystem",
                "terms": [
                  {
                    "id": 3,
                    "name": "ARB100",
                    "assignments": 0,
                    "uid": "124:196"
                  }
                ]
              },
              {
                "name": "DataDomain",
                "terms": [
                  {
                    "id": 1,
                    "name": "Employee domain",
                    "assignments": 2,
                    "uid": "122:196"
                  }
                ]
              }
            ]
          },
          {
            "id": 3,
            "name": "Technical Taxonomies",
            "types": [
              {
                "name": "DB Access",
                "terms": [
                  {
                    "id": 2,
                    "name": "Read",
                    "assignments": 5,
                    "uid": "223:524"
                  }
                ]
              },
              {
                "name": "File Access",
                "terms": [
                  {
                    "id": 1,
                    "name": "Write",
                    "assignments": 0,
                    "uid": "126:186"
                  }
                ]
              }
            ]
          }
        ]
      }
    }
  }
  const jobInfoSuccess: JobInformation = { status: JobInformation.StatusEnum.SUCCESS };
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', ['getJobInformation', 'getJobResult']);
  const taxonomyModalServiceSpy: jasmine.SpyObj<TaxonomyModalService> = jasmine.createSpyObj('TaxonomyModalService', ['setProjectId']);
  const taxonomyTypeControllerServiceSpy: jasmine.SpyObj<TaxonomyTypeControllerService> = jasmine.createSpyObj('TaxonomyTypeControllerService',
    ['findAllTaxonomyTypes', 'createTaxonomyType', 'updateTaxonomyType', 'deleteTaxonomyType']);
  const taxonomyControllerServiceSpy: jasmine.SpyObj<TaxonomyControllerService> = jasmine.createSpyObj('TaxonomyControllerService',
    ['getAggregatedValues', 'findAllTaxonomies', 'createTaxonomy', 'updateTaxonomy', 'deleteTaxonomy']);
  const graphqlServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService',
    ['graphQl']);
  const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj('DataPointControllerService',
    ['getDataPointsForType']);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
    ['getSelectedDataPoints', 'resetTableColumnAndDataPoints', 'checkSelectedColumns', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange', 'handleFilters', 'getGraphQlParam', 'onPageLoad', 'getQueryParams', 'queryParamsDetails']);
  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);
  let messageService: NzMessageService;
  let translateService: TranslateService;
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [TaxonomyConfigurationComponent],
      imports: [
        SharedModule,
        NzSelectModule,
        NzTableModule,
        FormsModule,
        RouterTestingModule,
        HttpClientTestingModule,
        NzCardModule,
        TranslateModule.forRoot({}),
        BrowserAnimationsModule
      ],
      providers: [
        HttpTestingController,
        TranslateService,
        NumberFormatter,
        ModuleControllerService,
        AnnotationControllerService,
        ProjectControllerService,
        CustomizableTableColumnService,
        DataDictionaryControllerService,
        I18nService,
        { provide: NzMessageService },
        { provide: TaxonomyControllerService, useValue: taxonomyControllerServiceSpy },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: TaxonomyTypeControllerService, useValue: taxonomyTypeControllerServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        { provide: GraphQlControllerService, useValue: graphqlServiceSpy },
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: DataPointControllerService, useValue: dataPointControllerServiceSpy },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              project: { id: 1 }
            })
          }
        },
        {
          provide: KeycloakAuthorizationService,
          useValue: new NoAuthorizationService()
        },
        FeatureControllerService
      ]
    }).compileComponents();
    jobControllerServiceSpy.getJobInformation.and.returnValues(of(jobInfoSuccess as any));

    taxonomyControllerServiceSpy.getAggregatedValues.and.returnValue(of(taxonomyAggregatedResponse as any));
    taxonomyControllerServiceSpy.findAllTaxonomies.and.returnValue(of(taxonomy as any));
    taxonomyControllerServiceSpy.createTaxonomy.and.returnValue(of({} as any));
    taxonomyControllerServiceSpy.updateTaxonomy.and.returnValue(of({} as any));
    taxonomyControllerServiceSpy.deleteTaxonomy.and.returnValue(of({} as any));

    taxonomyTypeControllerServiceSpy.findAllTaxonomyTypes.and.returnValue(of([{ name: 'parent', id: '1' }, { name: 'test', id: '2' }, { name: 'New Taxonomy Type', id: '3'}] as any));
    taxonomyTypeControllerServiceSpy.createTaxonomyType.and.returnValue(of({} as any));
    taxonomyTypeControllerServiceSpy.updateTaxonomyType.and.returnValue(of({} as any));
    taxonomyTypeControllerServiceSpy.deleteTaxonomyType.and.returnValue(of({} as any));
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    graphqlServiceSpy.graphQl.and.returnValue(of(graphQlResponse as any));
    userCustomizableTableServiceSpy.checkSelectedColumns.and.returnValue((true as any));
    taxonomyModalServiceSpy.taxonomyImportLink = 'http://appmod-documentation.deloitte.com/innowake-documentation/trunk/mining/mining-manual/working-with-mining/#taxonomy-batch-import';
    taxonomyModalServiceSpy.taxonomyModalEventEmitter = new EventEmitter<string>();
    taxonomyModalServiceSpy.cancelWarningSubject = new Subject<string>();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TaxonomyConfigurationComponent);
    component = fixture.componentInstance;
    component.clientProjectData = new ClientProjectRelationship(1, 'client', 1, 'project');
    component.confirmModal = mockModalRef as any;
    component.dataPointsList = [{
      "name": "name", "parentTypeName": "FieldInfo", "referenceTypeName": null, "path": "content.fieldInfos.name",
      providedBy: new Set(["innowake.mining.shared.model.FieldInfo"]), "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, "parameters": [], "usages": new Set(["miningUi.tableColumnsTable", "general.searchFilter", "miningUi.modulesTable"]), "usageAttributes": { "general.searchFilter": { "filterMode": "text" }, "miningUi.modulesTable": { "category": "Data Schema" }, "miningUi.tableColumnsTable": { "defaultColumnIndex": "1" } }, "displayName": "Field/Column Name", "description": "Name of the data field or table column", "array": false, "id": "FieldInfo.name", "aliasFor": null, "nullable": true, "alias": false
    }, { "name": "newTaxonomyType1", "parentTypeName": "Taxonomies", "referenceTypeName": null, "path": "content.taxonomy.newTaxonomyType1", "providedBy": new Set(["innowake.mining.server.graphql.config.TaxonomyDataPointSource"]), "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, "parameters": [], "usages": new Set(["graphql.query.annotations", "miningUi.annotationsTable", "graphql.query.modules", "savedSearch.allTaxonomies", "miningUi.modulesTable"]), "usageAttributes": { "graphql.query.annotations": { "sqlFragmentEq": "in('HasAnnotation').out('HasTaxonomy') CONTAINS (name = ? AND typeLink.name=\"New Taxonomy Type1\")" }, "general.viewMode": { "displayAs": "tag" }, "miningUi.annotationsTable": { "filterMode": "multiSelect", "rsqlFragment": "$in.HasAnnotation.$out.HasTaxonomy.id=in=(${$query})", "multiSelectValueRetrievalFilter": "typeLink.name=='New Taxonomy Type1'", "multiSelectValueRetrievalFieldName": "NAME", "multiSelectValueRetrievalMode": "taxonomyControllerAggregatedValues", "category": "Business Taxonomies" }, "miningUi.modulesTable": { "filterMode": "multiSelect", "rsqlFragment": "$out.HasTaxonomy.id=in=(${$query})", "multiSelectValueRetrievalFilter": "typeLink.name=='New Taxonomy Type1'", "multiSelectValueRetrievalFieldName": "NAME", "multiSelectValueRetrievalMode": "taxonomyControllerAggregatedValues", "category": "Business Taxonomies" }, "graphql.query.modules": { "sqlFragmentEq": "out('HasTaxonomy') CONTAINS (name = ? AND typeLink.name=\"New Taxonomy Type1\")" } }, "displayName": "New Taxonomy Type1", "description": "", "array": true, "id": "Taxonomies.newTaxonomyType1", "aliasFor": null, "nullable": true, "alias": false }, {
      "name": "programType", "parentTypeName": "Taxonomies", "referenceTypeName": null, "path": "content.taxonomy.programType", "providedBy": new Set(["innowake.mining.server.graphql.config.TaxonomyDataPointSource"]), "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, "parameters": [],
      "usages": new Set(["graphql.query.annotations", "miningUi.annotationsTable", "graphql.query.modules",
        "savedSearch.allTaxonomies", "miningUi.modulesTable"]), "usageAttributes": { "graphql.query.annotations": { "sqlFragmentEq": "in('HasAnnotation').out('HasTaxonomy') CONTAINS (name = ? AND typeLink.name=\"Program Type\")" }, "general.viewMode": { "displayAs": "tag" }, "miningUi.annotationsTable": { "filterMode": "multiSelect", "rsqlFragment": "$in.HasAnnotation.$out.HasTaxonomy.id=in=(${$query})", "multiSelectValueRetrievalFilter": "typeLink.name=='Program Type'", "multiSelectValueRetrievalFieldName": "NAME", "multiSelectValueRetrievalMode": "taxonomyControllerAggregatedValues", "category": "Technical Taxonomies" }, "miningUi.modulesTable": { "filterMode": "multiSelect", "rsqlFragment": "$out.HasTaxonomy.id=in=(${$query})", "multiSelectValueRetrievalFilter": "typeLink.name=='Program Type'", "multiSelectValueRetrievalFieldName": "NAME", "multiSelectValueRetrievalMode": "taxonomyControllerAggregatedValues", "category": "Technical Taxonomies" }, "graphql.query.modules": { "sqlFragmentEq": "out('HasTaxonomy') CONTAINS (name = ? AND typeLink.name=\"Program Type\")" } }, "displayName": "Program Type", "description": "", "array": true, "id": "Taxonomies.programType", "aliasFor": null, "nullable": true, "alias": false
    }];
    component.defaultColumns = [{
      "name": "name", "parentTypeName": "Module", "referenceTypeName": null,
      "path": "content.name", "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]), "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String, "parameters": [], "usages": new Set(["graphql.query.annotations", "general.viewMode", "miningUi.annotationsTable", "graphql.query.modules", "miningUi.dataDictionaryTable", "general.sortBy", "graphql.query.dataDictionaries", "miningUi.modulesTable", "general.searchFilter", "miningUi.graphMlExport"]), "usageAttributes": { "graphql.query.annotations": { "sqlFragmentEq": "in('HasAnnotation') CONTAINS (SEARCH_INDEX(\"Module_name_ft\", ?) = true)", "sqlFragmentEqFlags": "toLowercaseescapeLucenebeginsWith" }, "general.viewMode": { "linkTemplate": "/project-${$projectId}/module-${id}/details/overview", "displayAs": "link", "togetherWith": "id" }, "miningUi.annotationsTable": { "sortByFieldName": "in_HasAnnotation.out.name.toLowerCase()", "rsqlFragment": "$in.HasAnnotation.name=='${$query}*'", "category": "Base Data", "defaultColumnIndex": "0" }, "graphql.query.modules": { "sqlFragmentEq": "SEARCH_INDEX(\"Module_name_ft\", ?) = true", "sqlFragmentEqFlags": "toLowercaseescapeLucenebeginsWith" }, "miningUi.dataDictionaryTable": { "sortByFieldName": "in_HasDataDictionaryEntry.out.name.toLowerCase()", "rsqlFragment": "$in.HasDataDictionaryEntry.name=='${$query}*'", "category": "Base Data", "defaultColumnIndex": "0" }, "graphql.query.dataDictionaries": { "sqlFragmentEq": "in('HasDataDictionaryEntry') CONTAINS (name LIKE ?)", "sqlFragmentEqFlags": "beginsWith" }, "miningUi.modulesTable": { "sortByFieldName": "name.toLowerCase()", "rsqlFragment": "name=='${$query}*'", "category": "Base Data", "defaultColumnIndex": "0" }, "general.searchFilter": { "filterMode": "text" }, "miningUi.graphMlExport": { "sqlFragment": "name" } }, "displayName": "Module Name", "description": "The name of the Module", "array": false, "id": "Module.name", "aliasFor": null, "nullable": true, "alias": false
    }, { "name": "typeLink", "parentTypeName": "ObjectType", "referenceTypeName": "Type", "path": "content.objectTypeLink.typeLink", "providedBy": new Set(["innowake.mining.data.model.springdata.ObjectTypeV2"]), "scalarType": null, "parameters": [], "usages": new Set(["graphql.query.annotations", "general.viewMode", "miningUi.annotationsTable", "graphql.query.modules", "general.sortBy", "miningUi.modulesTable", "general.searchFilter", "miningUi.graphMlExport"]), "usageAttributes": { "graphql.query.annotations": { "sqlFragmentEq": "first(in('HasAnnotation')).objectTypeLink.typeLink.name = ?" }, "general.viewMode": { "labelMapping": "TYPE_LABELS" }, "miningUi.annotationsTable": { "multiSelectValueRetrievalMode": "annotationControllerAggregatedValues", "rsqlFragment": "$in.HasAnnotation.objectTypeLink.typeLink.name=in=($'{$query})", "category": "Base Data", "multiSelectValueRetrievalFieldName": "MODULE_TYPE" }, "graphql.query.modules": { "sqlFragmentEq": "objectTypeLink.typeLink.name = ?" }, "miningUi.modulesTable": { "rsqlFragment": "objectTypeLink.typeLink.name=in=($'{$query})", "multiSelectValueRetrievalFieldName": "TYPE", "sortByFieldName": "objectTypeLink.typeLink.name", "multiSelectValueRetrievalMode": "moduleControllerDistinctFieldValues", "category": "Base Data", "defaultColumnIndex": "2" }, "general.searchFilter": { "filterMode": "multiSelect" }, "miningUi.graphMlExport": { "sqlFragment": "objectTypeLink.typeLink.name as type" } }, "displayName": "Type", "description": "Type of the Module", "array": false, "id": "ObjectType.typeLink", "aliasFor": null, "nullable": true, "alias": false }, { "name": "technologyLink", "parentTypeName": "ObjectType", "referenceTypeName": "Technology", "path": "content.objectTypeLink.technologyLink", "providedBy": new Set(["innowake.mining.data.model.springdata.ObjectTypeV2"]), "scalarType": null, "parameters": [], "usages": new Set(["graphql.query.annotations", "general.viewMode", "miningUi.annotationsTable", "graphql.query.modules", "general.sortBy", "miningUi.modulesTable", "general.searchFilter", "miningUi.graphMlExport"]), "usageAttributes": { "graphql.query.annotations": { "sqlFragmentEq": "first(in('HasAnnotation')).objectTypeLink.technologyLink.name = ?" }, "general.viewMode": { "labelMapping": "TECHNOLOGY_LABELS" }, "miningUi.annotationsTable": { "multiSelectValueRetrievalMode": "annotationControllerAggregatedValues", "rsqlFragment": "$in.HasAnnotation.objectTypeLink.technologyLink.name=in=($'{$query})", "category": "Base Data", "multiSelectValueRetrievalFieldName": "MODULE_TECHNOLOGY" }, "graphql.query.modules": { "sqlFragmentEq": "objectTypeLink.technologyLink.name = ?" }, "miningUi.modulesTable": { "rsqlFragment": "objectTypeLink.technologyLink.name=in=($'{$query})", "multiSelectValueRetrievalFieldName": "TECHNOLOGY", "sortByFieldName": "objectTypeLink.technologyLink.name", "multiSelectValueRetrievalMode": "moduleControllerDistinctFieldValues", "category": "Base Data", "defaultColumnIndex": "1" }, "general.searchFilter": { "filterMode": "multiSelect" }, "miningUi.graphMlExport": { "sqlFragment": "objectTypeLink.technologyLink.name as technology" } }, "displayName": "Technology", "description": "Technology of the Module", "array": false, "id": "ObjectType.technologyLink", "aliasFor": null, "nullable": true, "alias": false }, { "name": "metricsDate", "parentTypeName": "Module", "referenceTypeName": null, "path": "content.metricsDate", "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]), "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.DateTime, "parameters": [], "usages": new Set(["general.viewMode", "general.sortBy", "miningUi.modulesTable", "miningUi.graphMlExport"]), "usageAttributes": { "general.viewMode": { "displayAs": "date" }, "miningUi.modulesTable": { "sortByFieldName": "metricsDate", "category": "Modifications", "defaultColumnIndex": "3" }, "miningUi.graphMlExport": { "sqlFragment": "metricsDate" } }, "displayName": "Last Scan", "description": "The date and time when the code metrics and dependencies for this Module were last updated", "array": false, "id": "Module.metricsDate", "aliasFor": null, "nullable": true, "alias": false }];
    fixture.detectChanges();
    messageService = TestBed.inject(NzMessageService);
    translateService = TestBed.inject(TranslateService);
  });

  it('should test onOpenImportAssignmentsModal', () => {
    component.onOpenImportAssignmentsModal();
    expect(component.createModal).toBeDefined();
  });

  it('should test set table data', () => {
    spyOn(component as any, 'setTableData').and.callThrough();
    (component as any).setTableData();
    expect(component.taxonomyData).toBeDefined();
  });

  it('should test optionSelected', () => {
    spyOn(component as any, 'setTableData');
    component.optionSelected({ optionValue: 'reloadData', data: {} });
    expect((component as any).setTableData).toHaveBeenCalled();

    spyOn(component.dataChangeEvent, 'next');
    const newTaxonomyType = { optionValue: 'addTaxonomyType', data: { children: [{ name: 'testing Type' }] } };
    component.optionSelected(newTaxonomyType);
    expect(newTaxonomyType.data.children[0].name).toBe('New Taxonomy Type');
    expect(component.dataChangeEvent.next).toHaveBeenCalled();

    const newTaxonomyTerm = { optionValue: 'addTaxonomyTerm', data: { children: [{ name: 'testing Term' }] } };
    component.optionSelected(newTaxonomyTerm);
    expect(newTaxonomyTerm.data.children[0].name).toBe('New Term');
    expect(component.dataChangeEvent.next).toHaveBeenCalled();
  });



  xit('should test delete Taxonomy functionality for term with level 0', () => {
    const newTaxonomyTerm = { optionValue: 'deleteTaxonomy', data: { children: [{ name: 'type' }], type: 'taxonomyType', level: 1, name: 'Employee domain', moduleCount: 1 } };
    component.optionSelected(newTaxonomyTerm);
    component.deleteTaxonomy(newTaxonomyTerm);
    expect(component.confirmModal).toBeDefined();
  });

  xit('should test delete Taxonomy functionality for term with level 1', () => {
    const newTaxonomyTerm = { optionValue: 'deleteTaxonomy', data: { children: [{ name: 'type' }], type: 'term', level: 0, name: 'Employee domain', moduleCount: 0 } };
    component.optionSelected(newTaxonomyTerm);
    component.deleteTaxonomy(newTaxonomyTerm);
    expect(component.confirmModal).toBeDefined();
  });

  xit('should test delete Taxonomy functionality for taxonomyTerm', () => {
    const newTaxonomyTerm = { optionValue: 'deleteTaxonomy', data: { children: [{ name: 'type' }], type: 'taxonomyTerm', level: 0 } };
    component.optionSelected(newTaxonomyTerm);
    component.deleteTaxonomy(newTaxonomyTerm);
    expect(component.confirmModal).toBeDefined();
  });

  it('should create taxonomy type/term', () => {
    /* Create new Taxonomy type */
    const typeParent = {
      "name": "Business Taxonomies",
      "id": "Business Taxonomies",
      "moduleCount": 0,
      "expand": true,
      "isEditable": false,
      "showCountAsText": true,
      "removeActions": [] as any,
      "children": [
          {
              "name": "New Taxonomy Type",
              "id": "newTaxonomyType",
              "moduleCount": 0,
              "isNewRecord": true,
              "showCountAsText": true,
              "type": "taxonomyType",
              "children": [] as any
          }
      ],
      "level": 0,
      "key": "parent-Business Taxonomies"
  };
    let data = { isNewRecord: true, type: 'taxonomyType', newValue: 'test', parent: typeParent };
    component.updateTaxonomy(data);
    expect((component as any).taxonomyTypeControllerService.createTaxonomyType).toHaveBeenCalled();

    /* Create new Taxonomy term */
    const termParent = {
      "name": "New Taxonomy Type",
      "id": "Business Taxonomies__New Taxonomy Type",
      "moduleCount": 0,
      "type": "taxonomyType",
      "showCountAsText": true,
      "isEditable": true,
      "expand": true,
      "children": [
          {
              "name": "New Term",
              "id": "newTaxonomyTerm",
              "moduleCount": 0,
              "isNewRecord": true,
              "showCountAsText": true,
              "type": "taxonomyTerm"
          }
      ],
      "level": 1,
      "parent": {
          "name": "Business Taxonomies",
          "id": "Business Taxonomies",
          "moduleCount": 0,
          "children": [
              {
                  "name": "New Taxonomy Type",
                  "id": "Business Taxonomies__New Taxonomy Type",
                  "moduleCount": 0,
                  "type": "taxonomyType",
                  "showCountAsText": true,
                  "isEditable": true,
                  "expand": true,
                  "children": [
                      {
                          "name": "New Term",
                          "id": "newTaxonomyTerm",
                          "moduleCount": 0,
                          "isNewRecord": true,
                          "showCountAsText": true,
                          "type": "taxonomyTerm"
                      }
                  ]
              }
          ],
          "expand": true,
          "isEditable": false,
          "showCountAsText": true,
          "level": 0,
          "key": "parent-Business Taxonomies"
      },
      "key": "child-Business Taxonomies__New Taxonomy Type"
  };
    data = { isNewRecord: true, type: 'taxonomyTerm', newValue: 'test', parent: termParent as any};
    component.updateTaxonomy(data);
    expect((component as any).taxonomyTypeControllerService.findAllTaxonomyTypes).toHaveBeenCalled();
    expect((component as any).taxonomyControllerService.createTaxonomy).toHaveBeenCalled();
  });

  it('should updateTaxonomy taxonomy for array case', () => {
    const typeParent = {
      "name": "Business Taxonomies",
      "id": "Business Taxonomies",
      "moduleCount": 0,
      "expand": true,
      "isEditable": false,
      "showCountAsText": true,
      "removeActions": [] as any,
      "children": [
          {
              "name": "New Taxonomy Type",
              "id": "newTaxonomyType",
              "moduleCount": 0,
              "isNewRecord": true,
              "showCountAsText": true,
              "type": "taxonomyType",
              "children": [] as any
          }
      ],
      "level": 0,
      "key": "parent-Business Taxonomies"
  };
    let data = { isNewRecord: true, type: 'taxonomyType', newValue: ["New Taxonomy Type", "ad"], actualValue: 'New Taxonomy Type', expand: false, parent: typeParent };
    component.updateTaxonomy(data);
    expect((component as any).taxonomyControllerService.createTaxonomy).toHaveBeenCalled();
  });

  xit('should update taxonomy type/term', () => {
    /* Update Taxonomy type */
    let data: Record<any, any> = { isNewRecord: false, type: 'taxonomyType', newValue: 'test new', actualValue: 'test', expand: false};
    component.updateTaxonomy(data);
    expect((component as any).taxonomyTypeControllerService.findAllTaxonomyTypes).toHaveBeenCalled();
    expect((component as any).taxonomyTypeControllerService.updateTaxonomyType).toHaveBeenCalled();

    /* Update Taxonomy term */
    data = { isNewRecord: false, type: 'taxonomyTerm', newValue: 'test new', actualValue: 'Employee domain', expand: true, parent: {id: 1} };
    component.updateTaxonomy(data);
    expect((component as any).taxonomyControllerService.findAllTaxonomies).toHaveBeenCalled();
    expect((component as any).taxonomyControllerService.updateTaxonomy).toHaveBeenCalled();
  });

  xit('should download taxonomy', () => {
    spyOn((component as any).exportDownloadService, 'downloadUrl');
    component.downloadTaxonomiesList();
    expect((component as any).exportDownloadService.downloadUrl).toHaveBeenCalled();
  });

  it('should emit event when editing starts', () => {
    const data = {
      "name": "value 1234",
      "id": "Business Taxonomies__testTaxonomy123__value 1234",
      "moduleCount": 0,
      "type": "taxonomyTerm",
      "isEditable": true,
      "level": 2,
      "expand": false,
      "parent": {
        "name": "testTaxonomy123",
        "id": "Business Taxonomies__testTaxonomy123",
        "moduleCount": 0,
        "children": [
          {
            "name": "value 1234",
            "id": "Business Taxonomies__testTaxonomy123__test1234",
            "moduleCount": 0,
            "type": "taxonomyTerm",
            "isEditable": true
          }
        ],
        "key": "child-Business Taxonomies__testTaxonomy123"
      },
      "key": "child-Business Taxonomies__testTaxonomy123__value 1234"
    }
    spyOn(component.dataChangeEvent, 'next');
    component.editingStarted(data);
    expect(component.dataChangeEvent.next).toHaveBeenCalledWith({ action: AllowedTableActions.RESTRICT_EDITING, data: data });
  });

  it('should cancel form editing', () => {
    spyOn(component, 'optionSelected');
    spyOn(component.dataChangeEvent, 'next');
    component.cancelTaxonomyForm({ isNewRecord: true });
    expect(component.dataChangeEvent.next).toHaveBeenCalledWith({ action: AllowedTableActions.TOGGLE_ACTIONS, data: false });
    expect(component.optionSelected).toHaveBeenCalled();
  });

  it('should Show error message', () => {
    spyOn((component as any).notification, 'error');

    (component as any).showErrorMsg('error');
    expect((component as any).notification.error).toHaveBeenCalled();

    spyOn((component as any).messageService, 'remove');
    (component as any).showErrorMsg('error', 1);
    expect((component as any).messageService.remove).toHaveBeenCalledWith(1);
    expect((component as any).notification.error).toHaveBeenCalled();
  });

  it('Showing the error in case save fails', () => {
    spyOn((component as any).notification, 'error');
    const mockCall = throwError({});

    (component as any).handleSavingTaxonomy(mockCall, 1, []);
    expect((component as any).notification.error).toHaveBeenCalled();
  });

  xit('Should test camel case conversion', () => {
    const str1 = (component as any).toCamelCase('Business SubSystem');
    expect(str1).toBe('businessSubsystem');
    const str2 = (component as any).toCamelCase('BusinessSub System');
    expect(str2).toBe('businesssubSystem');
    const str3 = (component as any).toCamelCase('BusinessSubSystem');
    expect(str3).toBe('businessSubSystem');
  });

  it('should delete taxonomy type when type is not taxonomyTerm and level is 0', () => {
    const data: MiningTableOptionSelected = {
      optionValue: '',
      data: {
        type: 'notTaxonomyTerm',
        level: 0,
        children: [{ name: 'child1' }, { name: 'child2' }],
      },
    };
    taxonomyTypeControllerServiceSpy.deleteTaxonomyType.and.returnValue(  of(new HttpResponse({ body: ['deleteTaxonomyType'] })));
    component.deleteTaxonomy(data);
    expect(taxonomyTypeControllerServiceSpy.deleteTaxonomyType).toHaveBeenCalledTimes(2);
  });

  it('should delete taxonomy type when type is not taxonomyTerm and level is not 0', () => {
    const data: MiningTableOptionSelected = {
      optionValue: '',
      data: {
        type: 'notTaxonomyTerm',
        level: 1,
        name: 'name',
      },
    };
    taxonomyTypeControllerServiceSpy.deleteTaxonomyType.and.returnValue( of(new HttpResponse({ body: [] })));
    component.deleteTaxonomy(data);
    expect(taxonomyTypeControllerServiceSpy.deleteTaxonomyType).toHaveBeenCalled();
  });

  it('should delete taxonomy when type is taxonomyTerm', () => {
    const data: MiningTableOptionSelected = {
      optionValue: '',
      data: {
        type: 'taxonomyTerm',
        name: 'name',
      },
    };
    const resp = [{ name: 'name', id: 1 }];
    taxonomyTypeControllerServiceSpy.findAllTaxonomyTypes.and.returnValue(of(resp as any));
    taxonomyTypeControllerServiceSpy.deleteTaxonomyType.and.returnValue(of(new HttpResponse({ body: ['response'] })));
    component.deleteTaxonomy(data);
    expect(taxonomyTypeControllerServiceSpy.deleteTaxonomyType).toHaveBeenCalled();
  });

  it('should call error with savingTaxonomyError when data type is taxonomyType and newValue is empty', () => {
    const data = { type: 'taxonomyType', newValue: [''] };
    const spy = spyOn(messageService, 'error');
    component.updateTaxonomy(data);
    expect(spy).toHaveBeenCalledWith(translateService.instant('taxonomyReportingComponent.savingTaxonomyError'));
  });

  it('should call error with savingTaxonomyTermError when data type is not taxonomyType and newValue is empty', () => {
    const data = { type: 'notTaxonomyType', newValue: [''] };
    const spy = spyOn(messageService, 'error');
    component.updateTaxonomy(data);
    expect(spy).toHaveBeenCalledWith(translateService.instant('taxonomyReportingComponent.savingTaxonomyTermError'));
  });
  
  it('should be [] if category is editable', () => {
    const isEditable =(component as any).isEditable('Technical Taxonomies11');
    const removeActions = isEditable ? [] : ['addTaxonomyTerm', 'deleteTaxonomy'];
    expect(removeActions).toEqual([]);
  });

  it('should be ["addTaxonomyTerm", "deleteTaxonomy"] if category is not editable', () => {
    const isEditable = (component as any).isEditable('Technical Taxonomies');
    const removeActions = isEditable ? [] : ['addTaxonomyTerm', 'deleteTaxonomy'];
    expect(removeActions).toEqual(['addTaxonomyTerm', 'deleteTaxonomy']);
  });

});
