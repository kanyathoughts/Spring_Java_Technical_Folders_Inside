import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { SharedModule } from '@app/shared';
import { AllowedTableActions } from '@app/shared/components/mining-table/mining-table-action.interface';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NzTableModule } from 'ng-zorro-antd/table';
import { of, Subject, throwError } from 'rxjs';
import { ModuleDataDictionaryComponent } from './module-data-dictionary.component';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { WindowToken } from '@app/core/utils/window';
import { AnnotationControllerService, DataDictionaryControllerService, DataDictionaryPojo, FeatureControllerService, ModuleControllerService } from '@innowake/mining-api-angular-client';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';

xdescribe('ModuleDataDictionaryComponent', () => {
  let drawerServiceSpy: NzDrawerService;
  let openedUrl = '';
  const annotation: any = {
    "recordId": "#222:628",
    "customProperties": {},
    "id": 3697,
    "dataElementName": "MYFBOUT-RECORD",
    "projectId": 6,
    "description": "MYFBOUT-RECORD",
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
    "state": "CANDIDATE",
    "isBusiness": true,
    "fieldTransformation": null,
    "sourceInput": null,
    "targetOutput": "tip 1",
    "isReferenced": true,
    "usage": "DISPLAY",
    "reference": {
        "recordId": "#286:628",
        "customProperties": {},
        "id": 12789,
        "relationship": "HAS_DATA_DICTIONARY_ENTRY",
        "fromId": "#148:1228",
        "fromModuleId": 6047,
        "toId": "#222:628",
        "toModuleId": 3697,
        "fromName": "MMRS7111",
        "toName": null,
        "fromModuleLocation": {
            "offset": 4514,
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
            "recordId": "#868:9",
            "customProperties": {},
            "id": 12856,
            "relationship": "HAS_BUSINESS_RULE",
            "fromId": "#222:628",
            "fromModuleId": 3697,
            "toId": "#190:300",
            "toModuleId": 1471,
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
    "groupPath": "MYFBOUT-RECORD",
    "indentation": 0
}

const dde: DataDictionaryPojo = {
  uid: '#203:117',
  id: 2,
  dataElementName: 'MY-PROGRAM-NAME',
  description: 'This is an english description of the data element name MY-PROGRAM-NAME',
  format: 'PICX',
  scopes: {'scope1': {'name': 'SQLDATABASE'}, 'scope2': {'name': 'CICSUI'}},
  length: 15,
  createdByUserId: 'admin',
  name: 'MMRS7101',
  location: {
    offset: 1005,
    length: 15
  }
  };
  let dataDictionaryValue: DataDictionaryPojo[] = [{
    uid: '#203:117',
    id: 2,
    dataElementName: 'MY-PROGRAM-NAME',
    description: 'This is an english description of the data element name MY-PROGRAM-NAME',
    format: 'PICX',
    scopes: {'scope1': {'name': 'SQLDATABASE'}, 'scope2': {'name': 'CICSUI'}},
    length: 15,
    createdByUserId: 'admin',
    name: 'MMRS7101',
    location: {
      offset: 1005,
      length: 15
    }
    },
    {
      uid: '#203:117',
      id: 1,
      dataElementName: 'MY-PROGRAM-NAME',
      description: 'This is an english description of the data element name MY-PROGRAM-NAME',
      format: 'PICX',
      scopes: {'scope1': {'name': 'SQLDATABASE'}, 'scope2': {'name': 'CICSUI'}},
      length: 15,
      createdByUserId: 'admin',
      name: 'MMRS7101',
      location: {
        offset: 1005,
        length: 15
      }
  }];
  let component: ModuleDataDictionaryComponent;
  let fixture: ComponentFixture<ModuleDataDictionaryComponent>;
  const modalHeader = 'Edit Data Dictionary Description';
  const dataDictionaryControllerServiceSpy: jasmine.SpyObj<DataDictionaryControllerService> =
      jasmine.createSpyObj('DataDictionaryControllerService', ['findAllDataDictionaryEntries', 'updateDataDictionaryEntry', 'deleteDataDictionaryEntry', 'findDataDictionaryEntryByRecordId', 'findLinkedBusinessRulesById']);
  const annotationControllerServiceSpy: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj<AnnotationControllerService>
  ('AnnotationControllerService',
    ['updateAnnotation', 'findAnnotationById', 'deleteAnnotation']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose','close']);
  const translateServiceSpy = jasmine.createSpyObj<TranslateService>('TranslateService', ['instant']);
  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
    ['bulkDelete']);
  let mockWindow: any;

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
      declarations: [],
      providers: [
        {provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService()},
        {provide: TranslateService, useValue: translateServiceSpy},
        {provide: DataDictionaryControllerService, useValue: dataDictionaryControllerServiceSpy},
        { provide: NzModalRef, useValue: nzModalRefSpy},
        { provide: WindowToken, useValue: mockWindow },
        { provide: AnnotationControllerService, useValue: annotationControllerServiceSpy },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        HttpTestingController,
        ModuleControllerService,
        NzMessageService,
        FeatureControllerService
      ],
      imports: [
        SharedModule,
        NzTableModule,
        FormsModule,
        RouterTestingModule,
        TranslateModule.forRoot({}),
        HttpClientTestingModule,
        BrowserAnimationsModule
      ]
    }).compileComponents();
    drawerServiceSpy = TestBed.inject(NzDrawerService);
    dataDictionaryControllerServiceSpy.updateDataDictionaryEntry.and.returnValue(of(dataDictionaryValue[0] as any));
    annotationControllerServiceSpy.findAnnotationById.and.returnValue(of(annotation as any));
    dataDictionaryControllerServiceSpy.deleteDataDictionaryEntry.and.returnValues(of(null as any), throwError(new Error('Deletion Error')));
    dataDictionaryControllerServiceSpy.findLinkedBusinessVariables.and.returnValue(of([] as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleDataDictionaryComponent);
    component = fixture.componentInstance;
    component.module = {name: "MMRS501" , id : 566};
    component.projectId = 1;
    nzModalRefSpy.afterClose = new Subject;
    nzModalRefSpy.afterClose.next(null);
    fixture.detectChanges();
  });

  it('should edit value and update', () => {
    dataDictionaryValue[0].description = 'new description';
    dataDictionaryControllerServiceSpy.findDataDictionaryEntryByRecordId.and.returnValue(of(dde as any));
    component.editDataDictionary(dataDictionaryValue[0]);
    expect(dataDictionaryValue[0].description).toBe('new description');
    component.editDataDictionary(dataDictionaryValue[0]);
    let drawer = drawerServiceSpy.create({});
    const spy = spyOn(drawerServiceSpy, 'create');
    spy.and.returnValue(drawer);
    drawer.close(AllowedTableActions.UPDATE);
  });

  it('should delete value', () => {
    dataDictionaryControllerServiceSpy.findDataDictionaryEntryByRecordId.and.returnValue(of(dde as any));
    component.editDataDictionary(dataDictionaryValue[1]);
    spyOn(component.dataChangeEvent, 'next');
    // expect(component.dataChangeEvent.next).toHaveBeenCalled();
  });

  it('should close Data Dictionary Editor', () => {
    spyOn(component.dataChangeEvent, 'next');
    component.handleFormResult(null);
    expect(component.dataChangeEvent.next).not.toHaveBeenCalled();
    component.handleFormResult({action: AllowedTableActions.DELETE, data: dataDictionaryValue[0]});
    expect(component.dataChangeEvent.next).toHaveBeenCalled();
    component.handleFormResult({action: AllowedTableActions.UPDATE, data: dataDictionaryValue[0]});
    expect(component.dataChangeEvent.next).toHaveBeenCalled();
  });

  it('should check handleSelectedOption cases', () => {
    let value: MiningTableOptionSelected = { optionValue: "edit", data: dataDictionaryValue[0] }
    dataDictionaryControllerServiceSpy.findDataDictionaryEntryByRecordId.and.returnValue(of(dataDictionaryValue[0] as any));
    component.handleSelectedOption(value);
    spyOn(component, 'editDataDictionary').and.callThrough();
    (component as any).editDataDictionary('xyz', dataDictionaryValue[0])
    expect(component.editDataDictionary).toHaveBeenCalled();

    const testValue: MiningTableOptionSelected = {
      optionValue: "codeviewer",
      data: {
          recordId: "#222:79",
          customProperties: {},
          id: 637,
          dataElementName: "MMRS71BI",
          description: "MMRS71BI",
          format: "GROUP",
          "scopes": [
              "OTHER"
          ],
          "length": null,
          "createdByUserId": "system_user",
          "updatedByUserId": "system_user",
          "createdByUserName": "system_user",
          "updatedByUserName": "system_user",
          "otherScope": null,
          "otherScopeSource": null,
          "scopeAttributes": {},
          "picClause": null,
          "definedLocation": null,
          "state": null,
          "isBusiness": false,
          "fieldTransformation": null,
          "sourceInput": null,
          "targetOutput": null,
          "isReferenced": false,
          "usage": null,
          "inHasDataDictionaryEntry": {
            "fromModuleLocation": {
              "offset": 10,
              "length": 8
          },
          },
          "reference": {
              "recordId": "#285:80",
              "customProperties": {},
              "id": 3795,
              "relationship": "HAS_DATA_DICTIONARY_ENTRY",
              "fromId": "#147:49",
              "fromModuleId": 566,
              "toId": "#222:79",
              "toModuleId": 637,
              "fromName": "MMRS71B",
              "toName": null,
              "fromModuleLocation": {
                  "offset": 10,
                  "length": 8
              },
              "toModuleLocation": {
                  "offset": 0,
                  "length": 0
              },
              "binding": null,
              "properties": {}
          },
          "isCandidate": true,
          "fieldLevel": 0,
          "parentGroup": null,
          "groupPath": null,
          "indentation": 0,
          "fieldType": "GROUP",
          "level": 0,
          "expand": false,
          "key": "parent-637"
      }
  };
    component.handleSelectedOption(testValue);
    expect(openedUrl).toContain('module-566');
  });
});
