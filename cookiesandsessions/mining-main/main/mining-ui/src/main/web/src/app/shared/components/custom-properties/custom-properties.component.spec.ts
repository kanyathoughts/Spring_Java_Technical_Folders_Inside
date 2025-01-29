import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpTestingController, HttpClientTestingModule } from '@angular/common/http/testing';
import { of } from 'rxjs';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { CustomPropertiesComponent } from './custom-properties.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { MiningDataPointDefinitionWithPath } from '@innowake/mining-api-angular-client/model/miningDataPointDefinitionWithPath';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { DataPointControllerService, FeatureControllerService, MetamodelControllerService } from '@innowake/mining-api-angular-client';

describe('CustomPropertiesComponent', () => {
    let component: CustomPropertiesComponent;
    let fixture: ComponentFixture<CustomPropertiesComponent>;

    const metaData: any = [{
        dataType: 'STRING',
        fieldType: "DEFAULT",
        label: "Some custom meta information",
        name: "customMetaInfo",
        autoCompletionKey: null,
        disabledActions: [],
        disabledToolTip: null
    }, {
        dataType: "STRING",
        fieldType: "DEFAULT",
        label: "Custom Annotation Property",
        name: "customAnnotationProperty",
        autoCompletionKey: null,
        disabledActions: [],
        disabledToolTip: null
    }, {
        dataType: "EMBEDDEDLIST",
        fieldType: "TAG",
        label: "Annotation Colors",
        autoCompletionKey: "colorTags",
        disabledActions: []
    }];

    const dataPoints: MiningDataPointDefinitionWithPath[] = [
        {
            "name": "modifiedDate",
            "parentTypeName": "Module",
            "referenceTypeName": null,
            "path": "content.modifiedDate",
            "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
            "parameters": [],
            "usageAttributes": {
                "general.viewMode": {
                    "displayAs": "date"
                },
                "miningUi.modulesTable": {
                    "sortByFieldName": "modifiedDate",
                    "category": "Modifications"
                },
            },
            "displayName": "Last modified",
            "description": "The date when this Module was last modified (either through code scanning or manually)",
            "array": false,
            "id": "Module.modifiedDate",
            "nullable": true,
            "aliasFor": null,
            "alias": false
        },
        {
            "name": "stateLink",
            "parentTypeName": "Annotation",
            "referenceTypeName": "WorkingState",
            "usageAttributes": {
                "graphql.query.annotations": { "sqlFragmentEq": "stateLink.name = ?", "sqlFragmentOrderBy": "stateLink.name" },
                "miningUi.annotationsTable": {
                    "rsqlFragment": "stateLink.name=in=($'{$query})", "multiSelectValueRetrievalFieldName": "STATE",
                    "sortByFieldName": "stateLink.name", "multiSelectValueRetrievalMode": "annotationControllerAggregatedValues", "category": "Base Data",
                    "defaultColumnIndex": "3"
                }, "general.searchFilter": { "filterMode": "multiSelect" }
            }, "displayName": "State"
        }
        ,
        {
            "name": "name",
            "parentTypeName": "Module",
            "referenceTypeName": null,
            "path": "content.name",
            "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
            "parameters": [],
            "usageAttributes": {
                "general.viewMode": {
                    "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
                    "displayAs": "link",
                    "togetherWith": "linkHash"
                },
                "miningUi.modulesTable": {
                    "sortByFieldName": "name.toLowerCase()",
                    "rsqlFragment": "name=='${$query}*'",
                    "category": "Base Data",
                    "defaultColumnIndex": "0"
                },
                "general.searchFilter": {
                    "filterMode": "text"
                }
            },
            "displayName": "Module Name",
            "description": "The name of the Module",
            "array": false,
            "id": "Module.name",
            "nullable": true,
            "aliasFor": null,
            "alias": false
        }
    ];

    const graphQlData: any = {
        "data": {
            "project": { "customPropertyClasses": { Annotation: ["AnnotationCustomProperties"], Client: ["ClientCustomProperties"], Module: ["ModuleCustomProperties"], Project: ["ProjectCustomProperties"], Taxonomy: ["TaxonomyCustomProperties"] } }
        }
    };

    const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
        'GraphQlControllerService',
        ['graphQl']
    );

    const currentClient: ClientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');

    const metamodelControllerServiceSpy: jasmine.SpyObj<MetamodelControllerService> = jasmine.createSpyObj<MetamodelControllerService>
        ('MetamodelControllerService', ['findMetaModel']);
    const dataPointControllerServicespy: jasmine.SpyObj<DataPointControllerService> = jasmine.createSpyObj<DataPointControllerService>
        ('DataPointControllerService', ['getDataPointsForType']);
    const authServiceSpy = new NoAuthorizationService();

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            schemas: [NO_ERRORS_SCHEMA],
            declarations: [CustomPropertiesComponent],
            imports: [
                RouterTestingModule,
                HttpClientTestingModule,
                TranslateModule.forRoot({}),
                BrowserAnimationsModule
            ],
            providers: [
                TranslateService,
                NumberFormatter,
                I18nService,
                HttpTestingController,
                HttpClient,
                HttpHandler,
                FeatureControllerService,
                { provide: DataPointControllerService, useValue: dataPointControllerServicespy },
                { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
                { provide: MetamodelControllerService, useValue: metamodelControllerServiceSpy },
                { provide: KeycloakAuthorizationService, useValue: authServiceSpy },
            ]
        }).compileComponents();
        graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQlData as any));
        metamodelControllerServiceSpy.findMetaModel.and.returnValue(of(metaData as any));
        dataPointControllerServicespy.getDataPointsForType.and.returnValue(of(dataPoints as any));
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(CustomPropertiesComponent);
        component = fixture.componentInstance;
        component.customPropertyClass = 'Annotation';
        component.projectId = 1;
        component.currentClient = currentClient;
        fixture.detectChanges();
    });

    afterEach(() => {
        fixture.destroy();
    });

    it('should create', () => {
        component.ngOnInit();
        fixture.detectChanges();
        expect(component).toBeTruthy();
    });

    it('test openCustomPropertyEditor method', () => {
        component.openCustomPropertyEditor();
        expect(component.newRecord).toBeTruthy();
    });

    it('test hideshowTab method', () => {
        component.setDrawerVisibilty(false);
        expect(component.newRecord).toBeFalsy();
    });

    it('test checkDataType method', () => {
        let fieldType = "DEFAULT";
        let dataType = "EMBEDDEDLIST";
        const naturallySorted = (component as any).checkDataType(fieldType, dataType);
        expect(naturallySorted).toEqual("customPropertyDataTypes.stringRepeater");
    });

    it('test default condition of  checkDataType method', () => {
        let fieldType = "DEFAULT";
        let dataType = "EMBEDDEDLISTasa";
        const naturallySorted = (component as any).checkDataType(fieldType, dataType);
        expect(naturallySorted).toEqual(dataType);
    });

    it('test updateTable method', () => {
        component.updateTable({ result: FormResult.Saved });
        expect(component.tableConfig.loading).toBeFalsy();
    });

    it('it Should test custom category list for different useges and types', () => {
        component.customCategoryList = ['other'];
        component.customPropertyClass = 'Module';
        expect(component.customCategoryList.length).toEqual(1);
        component.ngOnInit();
        expect(component.customCategoryList.length).toEqual(1);
        component.customPropertyClass = 'Annotation';
        component.customCategoryList = ['other'];
        expect(component.customCategoryList.length).toEqual(1);
        component.ngOnInit();
        expect(component.customCategoryList.length).toEqual(1);
    });
});