import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpService, I18nService } from '@app/core';
import { ModuleSchemaInfoComponent } from './module-schema-info.component';
import { of } from 'rxjs';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { ModuleDetailsModule } from '../module-details.module';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { AnnotationControllerService, DataDictionaryControllerService, DataPointControllerService, DataSchemaControllerService, FeatureControllerService, JobControllerService, MetamodelControllerService, MiningDataPointDefinitionWithPath, ModuleControllerService, ProjectControllerService, ReferenceControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { WindowToken } from '@app/core/utils/window';

describe('ModuleSchemaInfoComponent', () => {
  let component: ModuleSchemaInfoComponent;
  let fixture: ComponentFixture<ModuleSchemaInfoComponent>;
  let drawerServiceSpy: NzDrawerService;
  const mockWindow = jasmine.createSpyObj('WindowToken', ['open']);
  const graphQlData: any = {data: {fieldInfos: [
    {
      "ordinal": 1,
      "name": "xserver_name",
      "dataType": "varchar",
      "reference": 'test',
      "primaryKey": '',
      "autoIncrement": false,
      "comment": 'test'
    }
  ]}};
  const highlightedNodes: any[] = [
    {
      "type": "DATA_INTERFACE",
      "name": "ACCOUNT_NAME",
      "id": "module-2001-container-DATABASE_TABLE-interface-2",
      "incomings": [
        "module-2000-container-DATABASE_ACCESS-798-interface-0"
      ],
      "outgoings": [
        "module-2000-container-DATABASE_ACCESS-434-interface-0"
      ],
      "children": [],
      "group": "module-2001-Data-Interfaces",
      "parentModule": "module-2001"
    },
    {
      "type": "DATA_INTERFACE",
      "name": "ACCOUNT_BALANCE",
      "id": "module-2001-container-DATABASE_TABLE-interface-3",
      "incomings": [
        "module-2000-container-DATABASE_ACCESS-798-interface-1"
      ],
      "outgoings": [
        "module-2000-container-DATABASE_ACCESS-434-interface-1"
      ],
      "children": [],
      "group": "module-2001-Data-Interfaces",
      "parentModule": "module-2001"
    }
  ];
  const dataPoints: MiningDataPointDefinitionWithPath[] = [
    {
      "name": "reference",
      "parentTypeName": "FieldInfo",
      "referenceTypeName": null,
      "path": "reference",
      "providedBy": new Set(["innowake.mining.shared.model.FieldInfo"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "parameters": [],
      "usages": new Set([
        "miningUi.tableColumnsTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "miningUi.tableColumnsTable": {
          "defaultColumnIndex": "3"
        },
        "general.searchFilter": {
          "filterMode": "text"
        }
      },
      "displayName": "Reference",
      "description": "Fields or columns referenced by this column",
      "array": false,
      "id": "FieldInfo.reference",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "autoIncrement",
      "parentTypeName": "FieldInfo",
      "referenceTypeName": null,
      "path": "autoIncrement",
      "providedBy": new Set(["innowake.mining.server.graphql.controller.DataSchemaGraphQlController"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.Boolean,
      "parameters": [],
      "usages": new Set([
        "miningUi.tableColumnsTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "miningUi.tableColumnsTable": {
          "defaultColumnIndex": "5"
        },
        "general.searchFilter": {
          "filterMode": "multiSelect"
        }
      },
      "displayName": "Auto Increment",
      "description": "Whether the value for this column is generated via auto increment",
      "array": false,
      "id": "FieldInfo.autoIncrement",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "dataType",
      "parentTypeName": "FieldInfo",
      "referenceTypeName": null,
      "path": "dataType",
      "providedBy": new Set(["innowake.mining.server.graphql.controller.DataSchemaGraphQlController"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "parameters": [],
      "usages": new Set([
        "miningUi.tableColumnsTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "miningUi.tableColumnsTable": {
          "defaultColumnIndex": "2"
        },
        "general.searchFilter": {
          "filterMode": "multiSelect"
        }
      },
      "displayName": "Data Type",
      "description": "Data Type of the Field or Column",
      "array": false,
      "id": "FieldInfo.dataType",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "name",
      "parentTypeName": "FieldInfo",
      "referenceTypeName": null,
      "path": "name",
      "providedBy": new Set(["innowake.mining.shared.model.FieldInfo"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "parameters": [],
      "usages": new Set([
        "miningUi.tableColumnsTable",
        "general.searchFilter",
        "miningUi.modulesTable"
      ]),
      "usageAttributes": {
        "miningUi.modulesTable": {
          "category": "Data Schema"
        },
        "general.searchFilter": {
          "filterMode": "text"
        },
        "miningUi.tableColumnsTable": {
          "defaultColumnIndex": "1"
        }
      },
      "displayName": "Field/Column Name",
      "description": "Name of the data field or table column",
      "array": false,
      "id": "FieldInfo.name",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "comment",
      "parentTypeName": "FieldInfo",
      "referenceTypeName": null,
      "path": "comment",
      "providedBy": new Set(["innowake.mining.shared.model.FieldInfo"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "parameters": [],
      "usages": new Set([
        "general.editMode",
        "miningUi.tableColumnsTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "general.searchFilter": {
          "filterMode": "text"
        },
        "miningUi.tableColumnsTable": {
          "defaultColumnIndex": "6"
        },
        "general.editMode": {
          "editAs": "textArea",
          "editEndpoint": "/api/v1/projects/${$projectId}/modules/${$moduleId}/fields/${ordinal}",
          "editEndpointFieldName": "comment",
          "togetherWith": "ordinal"
        }
      },
      "displayName": "Comment",
      "description": "Comment describing the field or column",
      "array": false,
      "id": "FieldInfo.comment",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "primaryKey",
      "parentTypeName": "FieldInfo",
      "referenceTypeName": null,
      "path": "primaryKey",
      "providedBy": new Set(["innowake.mining.server.graphql.controller.DataSchemaGraphQlController"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
      "parameters": [],
      "usages": new Set([
        "miningUi.tableColumnsTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "miningUi.tableColumnsTable": {
          "defaultColumnIndex": "4"
        },
        "general.searchFilter": {
          "filterMode": "multiSelect"
        }
      },
      "displayName": "Primary Key",
      "description": "The index of the column in the table's primary key",
      "array": false,
      "id": "FieldInfo.primaryKey",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    },
    {
      "name": "ordinal",
      "parentTypeName": "FieldInfo",
      "referenceTypeName": null,
      "path": "ordinal",
      "providedBy": new Set(["innowake.mining.shared.model.FieldInfo"]),
      "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.Int,
      "parameters": [],
      "usages": new Set([
        "miningUi.tableColumnsTable",
        "general.searchFilter"
      ]),
      "usageAttributes": {
        "miningUi.tableColumnsTable": {
          "defaultColumnIndex": "0"
        },
        "general.searchFilter": {
          "filterMode": "number"
        }
      },
      "displayName": "Ordinal",
      "description": "Index of the data field or table column",
      "array": false,
      "id": "FieldInfo.ordinal",
      "nullable": true,
      "alias": false,
      "aliasFor": null
    }
  ];
  const dataPointControllerServiceSpy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj('DataPointControllerService',
    ['getDataPointsForType']);
  const graphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>
    ('GraphQlControllerService', ['graphQl']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const authServiceSpy = new NoAuthorizationService();
  const i18nServiceSpy = { language: 'en-US' };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ModuleSchemaInfoComponent],
      imports: [
        ModuleDetailsModule,
        HttpClientTestingModule,
        RouterTestingModule,
        BrowserAnimationsModule,
        TranslateModule.forRoot()
      ],
      providers: [
        DataSchemaControllerService,
        TranslateService,
        NumberFormatter,
        FeatureControllerService,
        ModuleControllerService,
        AnnotationControllerService,
        ProjectControllerService,
        TaxonomyControllerService,
        DataDictionaryControllerService,
        MetamodelControllerService,
        ReferenceControllerService,
        JobControllerService,
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: authServiceSpy },
        { provide: HttpClientTestingModule, useValue: HttpService },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceStub },
        { provide: DataPointControllerService, useValue: dataPointControllerServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        { provide: WindowToken, useValue: mockWindow }
      ]
    }).compileComponents();
    drawerServiceSpy = TestBed.inject(NzDrawerService);
    dataPointControllerServiceSpy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    graphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleSchemaInfoComponent);
    component = fixture.componentInstance;
    component.moduleId = 1;
    component.projectId = 1;
    component.highlightedNodes = highlightedNodes;
    fixture.detectChanges();
  });

  it('should create an app', () => {
    component.moduleId = 1;
    component.projectId = 1;
    component.ngOnInit();
    expect(component).toBeTruthy();

    // check when highlighted nodes are empty
    component.highlightedNodes = [];
    component.ngOnInit();
    expect(component.isSqlFromDL).toBeFalsy();
    expect(component.loadState).toBe(LoaderState.success);
  });

  it('should check if content is available', () => {
    let isContentAvailable: boolean;
    component.loadState = LoaderState.nocontent;
    isContentAvailable = component.isContentAvailable();
    expect(isContentAvailable).toBeFalsy();
    component.loadState = LoaderState.success;
    isContentAvailable = component.isContentAvailable();
    expect(isContentAvailable).toBeTruthy();
  });

  it('should test onOptionSelected', () => {
    let drawer = drawerServiceSpy.create({});
    const spy = spyOn(drawerServiceSpy, 'create');
    spy.and.returnValue(drawer);
    fixture.detectChanges();
    component.onOptionSelected({optionValue: 'edit', data: {id: 2022}});
    expect(spy).toHaveBeenCalled();
  });
});
