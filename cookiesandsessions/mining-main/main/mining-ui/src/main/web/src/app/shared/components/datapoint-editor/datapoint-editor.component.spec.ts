import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { DataPointEditorComponent } from './datapoint-editor.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { of } from 'rxjs';
import { SharedModule } from '@app/shared/shared.module';
import { DataPointControllerService, MiningDataPointDefinitionWithPath } from '@innowake/mining-api-angular-client';

describe('DataPointEditorComponent', () => {
    let component: DataPointEditorComponent;
    let fixture: ComponentFixture<DataPointEditorComponent>;

    const dataPointControllerSpy = jasmine.createSpyObj<DataPointControllerService>('DataPointControllerService', ['getDataPointsForType']);
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
    beforeEach((() => {
        TestBed.configureTestingModule({
            declarations: [ DataPointEditorComponent ],
            imports: [
                SharedModule,
                HttpClientTestingModule,
                RouterTestingModule,
                BrowserAnimationsModule,
                TranslateModule.forRoot()
            ],
            providers: [
                NzMessageService,
                TranslateService,
                NzNotificationService,
                { provide: DataPointControllerService, useValue: dataPointControllerSpy }
            ]
        })
        .compileComponents();
        dataPointControllerSpy.getDataPointsForType.and.returnValue(of(dataPoints as any))
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(DataPointEditorComponent);
        component = fixture.componentInstance;
        component.dataTypeName = '';
        component.context = { projectId: 1, moduleId: 2001 }
        component.data = [];
        fixture.detectChanges();
      });

    it('should create', () => {
        component.ngOnInit();
        expect(component).toBeTruthy();
    });

    it('should test onSubmit', () => {
      component.onSubmit();
      spyOn(component, 'onCancel').and.callThrough();
      expect(component.isSaving).toBe(true);
    });
});