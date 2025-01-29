import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { of, Subject, throwError } from 'rxjs';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { HttpService } from '@app/core/http/http.service';
import { ApiPrefixInterceptor } from '@app/core/http/api-prefix.interceptor';
import { ErrorHandlerInterceptor } from '@app/core/http/error-handler.interceptor';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { AnnotationReportingComponent } from './annotation-reporting.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { Column, FieldTypeEnum } from '@app/shared/components/mining-table/mining-table-config.interface';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { ActivatedRoute } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { NzDrawerModule, NzDrawerService } from 'ng-zorro-antd/drawer';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { WindowToken } from '@app/core/utils/window';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { CancelRequestOnNavigationInterceptor } from '@app/core/http/cancel-request-on-navigation.interceptor';
import { AnnotationControllerService, AnnotationPojo, AnnotationReport, DataDictionaryControllerService, DataPointControllerService, FeatureControllerService, JobControllerService, JobInformation, MiningDataPointDefinitionWithPath, ModuleControllerService, ModulePojo, SavedSearchControllerService } from '@innowake/mining-api-angular-client';
import { SetSerializationInterceptor } from '@app/core/http/set-serialization.interceptor';
import { HttpClient } from '@angular/common/http';
import { BehaviorSubject, Observable } from 'rxjs';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { AllowedTableActions, LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';

describe('CustomizableAnnotationTableViewComponent', () => {
  let component: AnnotationReportingComponent;
  let fixture: ComponentFixture<AnnotationReportingComponent>;
  let route: ActivatedRoute;
  let oauthServiceSpy: OauthtokenService;
  let mockWindow: any;
  let openedUrl = '';
  const jobControllerServiceSpy = jasmine.createSpyObj<JobControllerService>('JobControllerService', ['getJobInformation', 'getJobResult', 'cancelJob', 'getJobInformations', 'getJobLog', 'getJobResult', 'submitJobExtension', 'submitJobExtensionV2']);
  const drawerServiceSpy = jasmine.createSpyObj('NzDrawerService', ['create']);
  const annotationReport: any = {
    recordId: '#89:1280',
    customProperties: {},
    id: 4,
    name: 'Annotation 4',
    moduleName: 'PRG1',
    annotationType: AnnotationReport.AnnotationTypeEnum.DATABASE,
    categoryName: 'Annotation Category A',
    annotationState: AnnotationReport.AnnotationStateEnum.CANDIDATE,
    sourceCode: '5678',
    taxonomy: '[ARB100, Employee domain]',
    updatedByUserId: 'system',
    createdByUserId: 'system',
    inHasAnnotation: {
      out: {
        id: 1
      }
    }
  };

  const oAuthToken: any = {
    access_token: 'ffb3eff8-53a7-4153-bff1',
    token_type: 'bearer',
    refresh_token: 'a30a484a-b1a1-4580-a437',
    expires_in: 315575999,
    scope: 'read write trust',
    username: 'test-admin'
  };
  const codeAnnotationsValue: AnnotationPojo[] = [{
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
    updatedByUserId: 'admin',
    sourceAttachment: 'abcd',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {
      'Property1': [{
        name: 'customAnnotationProperty',
        value: 'A value for the custom Annotation property',
        dataType: 'STRING'
      }]
    },
    id: 1,
    name: 'Annotation 2',
    projectId: 1,
    state: 'FOR_REVIEW',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  },
  {
    uid: '#555:555',
    customProperties: {
      'Property1': [{
        name: 'customAnnotationProperty',
        value: 'A value for the custom Annotation property',
        dataType: 'STRING'
      }]
    },
    id: 1,
    name: 'Annotation 2',
    projectId: 1,
    state: 'REJECTED',
    type: 'RULE',
    categoryId: 1,
    categoryName: 'Annotation Category B',
    createdByUserId: 'admin',
    updatedByUserId: null,
    sourceAttachment: 'efgh',
    moduleName: 'PRG1',
    location: {
      offset: 100,
      length: 4
    }
  }];

  const moduleValue: ModulePojo = {
    uid: '#136:600',
    customProperties: {
      'Property1': [{
        name: 'customMetaInfo2',
        value: null,
        dataType: 'STRING'
      }, {
        name: 'customMetaInfo1',
        value: null,
        dataType: 'STRING'
      }]
    },
    id: 2007,
    name: 'CC1',
    projectId: 1,
    path: 'src/cobol/programs/CC1.cpy',
    technology: 'COBOL',
    type: 'COPYBOOK',
    storage: 'FILE',
    identification: 'IDENTIFIED',
    origin: 'CUSTOM',
    info: null,
    description: 'A test copy',
    sourceMetrics:{
      codeLines: null,
      commentLines: null,
      complexityMcCabe: null,
    },
    content: null,
    sourceCodeAvailable: false
  };
  const dataPoints: MiningDataPointDefinitionWithPath[] = [
    {
      "name": "createdByUserId",
      "parentTypeName": "Annotation",
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "referenceTypeName": null,
      "parameters": [],
      "usages": new Set(["miningUi.annotationsTable"]),
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
      "usages": new Set(["miningUi.annotationsTable"]),
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
      "usages": new Set(["miningUi.annotationsTable"]),
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
  const graphQl = {
    "data": {
      "annotations": {
        "content": [
          {
            "typeLink": "DEAD_CODE",
            "categoryLink": {
              "name": "Annotation Category A"
            },
            "inHasAnnotation": {
              "out": {
                "name": "QBGPSLP1MMRS710A.STEP01.MMRS7102",
                "id": 2001
              }
            },
            "stateLink": "IN_ANALYSIS",
            "name": "Annotation 6",
            "id": 5
          }
        ],
        "totalElements": 9
      }
    }
  }
  const selectedColumns: NzTreeNodeOptions[] = [
    { 'title': 'Module Name', 'name': 'name', 'key': 'name', 'checked': true, 'disableCheckbox': true, 'path': 'content.inHasAnnotation.out.name' },
    { 'title': 'State', 'name': 'stateLink', 'key': 'stateLink', 'checked': true, 'disableCheckbox': false, 'path': 'content.stateLink' },
    { 'title': 'Category', 'name': 'categoryLink', 'key': 'categoryLink', 'checked': true, 'disableCheckbox': false, 'path': 'content.categoryLink.name'},
    { 'title': 'Annotation Type', 'name': 'typeLink', 'key': 'typeLink', 'checked': true, 'disableCheckbox': false, 'path': 'content.typeLink' },
    { 'title': 'Annotation Description', 'name': 'name', 'key': 'name', 'checked': true, 'disableCheckbox': false, 'path': 'content.name' }

  ];
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
  const queryParameters = {
    page: 1,
    filter: 'name=="PRG*"',
    sort: ['name:DESC']
  }

  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(0, 'TestClient', 0, 'TestProject');
  const i18nServiceSpy = { language: 'en-US' };
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'close', 'getContentComponent']);
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable']);
  const annotationControllerServiceSpy: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj<AnnotationControllerService>
    ('AnnotationControllerService',
      ['updateAnnotation', 'findAnnotationById', 'deleteAnnotation']);
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
    ['findModuleById']);
  const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj('DataPointControllerService',
    ['getDataPointsForType']);
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService',
    ['graphQl']);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
    ['getSelectedDataPoints', 'resetTableColumnAndDataPoints', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange', 'handleFilters', 'getGraphQlParam', 'onPageLoad', 'getQueryParams', 'bulkDelete']);
  const authServiceSpy = jasmine.createSpyObj<KeycloakAuthorizationService>('KeycloakAuthorizationService',
    ['hasAccessToClientProjects', 'hasUserRole', 'isAdmin', 'isClientAdmin']);
  const savedSearchControllerServiceSpy: jasmine.SpyObj<SavedSearchControllerService> = jasmine.createSpyObj<SavedSearchControllerService>
  ('SavedSearchControllerService', ['findByUsage']);
  const dataDictionaryControllerServiceSpy: jasmine.SpyObj<DataDictionaryControllerService> = jasmine.createSpyObj('DataDictionaryControllerService', [
    'updateDataDictionaryEntry', 'deleteDataDictionaryEntry', 'findDataDictionaryEntryByRecordId', 'findLinkedBusinessVariables'
  ]);
  const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);
  const jobManagerServiceSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['register']);
  const jobInfoSuccess: JobInformation = { status: JobInformation.StatusEnum.SUCCESS };
  let response = {
    object: {
      className: "innowake.mining.shared.model.AnnotationImportJobResult",
      overallResult: "NONE",
      markers: [{ 'test': 'testing' }]
    }
  };
  const blob = new Blob([JSON.stringify(response)] as any, { type: "application/json" });
  const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>('NzNotificationService', ['create', 'warning']);
  const savedSearch = [{
    customProperties: {},
    name: "Business Rule Candidates",
    projectId: 0,
    savedSearch: "columns=Annotation.typeLink&columns=AnnotationCategory.name&columns=Module.name&columns=Annotation.sourceAttachment&columns=Annotation.stateLink&columns=ObjectType.technologyLink&columns=ObjectType.typeLink&columns=Annotation.name&page=1&sort=in_HasAnnotation.out.name;ASC&filter=[{\"key\":\"typeLink\",\"value\":[\"RULE\"]},{\"key\":\"categoryLink.name\",\"value\":[\"Business Rule\"]},{\"key\":\"stateLink\",\"value\":[\"CANDIDATE\"]}]",
    usage: "miningUi.annotationsTable"
  }];
  const dataDictionaryEntry: any = {
    "recordId": "#220:628",
    "customProperties": {},
    "id": 3695,
    "dataElementName": "MYFXOUT-RECORD",
    "projectId": 6,
    "description": "MYFXOUT-RECORD 1",
    "format": "Group",
    "scopes": [
        "FILE"
    ],
    "length": 0,
    "createdByUserId": "system_user",
    "updatedByUserId": "admin",
    "createdByUserName": "system_user",
    "updatedByUserName": "admin",
    "otherScope": null,
    "otherScopeSource": null,
    "scopeAttributes": {},
    "picClause": null,
    "definedLocation": "Program",
    "state": "APPROVED",
    "isBusiness": true,
    "fieldTransformation": null,
    "sourceInput": "sip1",
    "targetOutput": "tip 12",
    "isReferenced": true,
    "usage": "DISPLAY",
    "reference": {
        "recordId": "#284:628",
        "customProperties": {},
        "id": 12787,
        "relationship": "HAS_DATA_DICTIONARY_ENTRY",
        "fromId": "#148:1228",
        "fromModuleId": 6047,
        "toId": "#220:628",
        "toModuleId": 3695,
        "fromName": "MMRS7111",
        "toName": null,
        "fromModuleLocation": {
            "offset": 4089,
            "length": 14
        },
        "toModuleLocation": {
            "offset": 0,
            "length": 0
        },
        "binding": null,
        "properties": {},
        "conditionalDependency": {
            "ifReachedFromModules": []
        }
    },
    "hasBusinessRules": [
        {
            "recordId": "#867:9",
            "customProperties": {},
            "id": 12854,
            "relationship": "HAS_BUSINESS_RULE",
            "fromId": "#220:628",
            "fromModuleId": 3695,
            "toId": "#189:301",
            "toModuleId": 1470,
            "fromName": null,
            "toName": "Business Rule Candidate [System identified]",
            "fromModuleLocation": {
                "offset": 0,
                "length": 0
            },
            "toModuleLocation": {
                "offset": 0,
                "length": 0
            },
            "binding": null,
            "properties": {},
            "conditionalDependency": {
                "ifReachedFromModules": []
            }
        }
    ],
    "isCandidate": true,
    "fieldLevel": 1,
    "parentGroup": null,
    "groupPath": "MYFXOUT-RECORD",
    "indentation": 0
}
  beforeEach(waitForAsync(() => {
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
    TestBed.configureTestingModule({
      declarations: [AnnotationReportingComponent],
      imports: [
        FormsModule,
        BrowserAnimationsModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({}),
        NzDropDownModule,
        NzDrawerModule],
      providers: [
        NzMessageService,
        TranslateService,
        SetSerializationInterceptor,
        DeepLinkService,
        FeatureControllerService,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        CancelRequestOnNavigationInterceptor,
        NumberFormatter,
        JobControllerService,
        { provide: NzNotificationService, useValue: notificationSpy },
        { provide: SavedSearchControllerService, useValue: savedSearchControllerServiceSpy},
        { provide: AnnotationControllerService, useValue: annotationControllerServiceSpy },
        { provide: DataPointControllerService, useValue: dataPointControllerServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: FeatureToggleService, useValue: featureToggleServiceSpy },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: NzModalRef, useValue: { destroy: () => true, close: () => true } },
        {
          provide: HttpClient,
          useClass: HttpService
        },
        { provide: KeycloakAuthorizationService, useValue: authServiceSpy },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        {provide: DataDictionaryControllerService, useValue: dataDictionaryControllerServiceSpy},
        { provide: NzDrawerService, useValue: drawerServiceSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              project: { id: 1 }
            }),
            snapshot: {queryParams: of({
              filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
              page: 1,
              sort:'name;ASC'
            })}
          }
        },
        { provide: WindowToken, useValue: mockWindow },
      ]
    }).compileComponents();
    let status = {
      "status$": of({
        "_isScalar": false,
        "closed": false,
        "isStopped": true,
        "hasError": false,
        "_value": "SUCCESS"
      })
    };
    const percentage = new BehaviorSubject<number>(10);
    annotationControllerServiceSpy.updateAnnotation.and.returnValues(of(codeAnnotationsValue[0] as any), of(codeAnnotationsValue[1] as any),
      of(codeAnnotationsValue[2] as any), of(codeAnnotationsValue[3] as any),
      of(codeAnnotationsValue[4] as any), of(codeAnnotationsValue[5] as any),
      throwError(new Error('Test Error')));
    annotationControllerServiceSpy.findAnnotationById.and.returnValues(of(codeAnnotationsValue[0] as any), throwError('TEST_ERROR'));
    annotationControllerServiceSpy.deleteAnnotation.and.returnValues(of(null as any), throwError(new Error('Deletion Error')));
    moduleControllerServiceSpy.findModuleById.and.returnValue(of(moduleValue as any));
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
    userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedColumns as any));
    userCustomizableTableServiceSpy.updateTableConfig.and.returnValue(updatedColumns as any);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    jobManagerServiceSpy.register.and.returnValue(status as any);
    jobManagerServiceSpy.percent = percentage;
    jobControllerServiceSpy.getJobResult.and.returnValues(of(blob) as any);
    jobControllerServiceSpy.getJobInformation.and.returnValues(of(jobInfoSuccess as any));
    savedSearchControllerServiceSpy.findByUsage.and.returnValue(of(savedSearch as any));
    dataDictionaryControllerServiceSpy.findDataDictionaryEntryByRecordId.and.returnValue(of(dataDictionaryEntry as any));
    dataDictionaryControllerServiceSpy.findLinkedBusinessVariables.and.returnValue(of([dataDictionaryEntry] as any));
    oauthServiceSpy = TestBed.inject(OauthtokenService);
    spyOn(oauthServiceSpy, 'getUsername').and.returnValue(oAuthToken.username);
  }));

  beforeEach(() => {
    route = TestBed.get(ActivatedRoute);
    fixture = TestBed.createComponent(AnnotationReportingComponent);
    component = fixture.componentInstance;
    nzModalRefSpy.afterClose = new Subject;
    nzModalRefSpy.afterClose.next('done');
    nzModalRefSpy.getContentComponent.and.returnValue({});
    drawerServiceSpy.create.and.returnValue({
      afterClose: of({result: FormResult.Saved}),
      afterOpen: undefined,
      close: undefined,
      open: undefined,
      getContentComponent: undefined,
      getContentComponentRef: undefined
    });
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    spyOn(component, 'initialize').and.returnValue(of(clientProjectRelationship));
    component.rowActions = [[{ label: 'btnLabel.edit', value: 'edit' },
      {
        type: LinkType.DROPDOWN,
        icon: 'more',
        options: [
          { label: 'codeviewer', value: 'codeviewer', disableItem: (): boolean => false },
          { label: 'copyAnnotationDetails', value: 'copyAnnotationDetails', disableItem: (): boolean => false},
        ]
      }]];
    component.tableConfig.importAction = component.importAnnotationAction();
    route.snapshot.queryParams = {
      filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
      page: 1,
      sort:'name;ASC'
    }
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('if case of hasUserRole', () => {
    authServiceSpy.hasUserRole.and.returnValue(true);
    component.parametersService.reloadTableData.next(true);
    featureToggleServiceSpy.isActive.and.returnValue(of(true));
    component.ngOnInit();
    expect(component.rowActions.length).toBe(1);
  });

  it('else case of hasUserRole', () => {
    authServiceSpy.hasUserRole.and.returnValue(false);
    component.ngOnInit();
    expect(component.rowActions.length).toBe(0);
  });

  it('should open annotation details with advanced option', () => {
    spyOn((component as any).messageService, 'error');
    component.ngOnInit();
    let drawer = drawerServiceSpy.create({});
    drawerServiceSpy.create.and.returnValue(drawer);
    fixture.detectChanges();
    component.handleSelectedOption({ data: annotationReport, optionValue: 'edit' });
    expect(drawerServiceSpy.create).toHaveBeenCalled();
    component.handleSelectedOption({ data: annotationReport, optionValue: 'codeviewer' });
    const rowData = {module:{ path: 'path', name: 'test'}, selectedDDId: '1'};
    const openDDSpy = spyOn((component as any), 'openSharedDataDictionaryEditorModal');
    component.handleSelectedOption({ data: rowData, optionValue: 'linkedBusinessVariables' });
    expect(openDDSpy).toHaveBeenCalled();
    const clipboardServiceSpy = spyOn((component as any).clipBoardService, 'copyToClipboard');
    component.handleSelectedOption({ data: rowData, optionValue: 'copyAnnotationDetails' });
    expect(clipboardServiceSpy).toHaveBeenCalled();
  });

  it('should update annotation', () => {
    component.annotationToBeUpdated = annotationReport;
    component.afterCloseAnnotationEditor(annotationReport, AllowedTableActions.UPDATE);
    expect(component.refreshCoreTable).toBeTruthy();
  });

  it('should not update annotation on cancel', () => {
    component.afterCloseAnnotationEditor(annotationReport, undefined);
    expect(component.refreshCoreTable).toBeFalsy();
  });

  it('test empty graphQL response', () => {
    let emptyGraphQL = {};
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(emptyGraphQL as any));
    expect(component.tableConfig.loading).toBeFalsy();
  });

  it('should submit generate-annotation-descriptions job extension', () => {
      component.projectId = 1;
      component.explainAnnotationsOverwrite = false;
      let ids = new Set<number>([1, 23, 4])
      component.identifyWebBasedAnalyses(ids, 'explainAnnotations');
      jobControllerServiceSpy.submitJobExtensionV2.and.returnValue(of('some-job-id') as Observable<any>);
      component.handleAnnotationsModalOk();
      expect(jobControllerServiceSpy.submitJobExtensionV2).toHaveBeenCalledWith(1, 'generate-annotation-descriptions', { 'ids': [1, 23, 4], 'overwrite': false });
    });

    it('should  import annotations', () => {
      component.importAnnotationAction();
      expect(modalServiceSpy.create).toHaveBeenCalled();
    });

    it('should  register job', () => {
      (component as any).registerJob('1', false);
      expect(modalServiceSpy.create).toHaveBeenCalled();
    });

    it('should test openSharedDataDictionaryEditorModal', () => {
      (component as any).openSharedDataDictionaryEditorModal(1, 'path', 'test', '1');
    })
});
