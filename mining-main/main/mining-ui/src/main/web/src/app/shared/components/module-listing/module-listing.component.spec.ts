import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ModuleListingComponent } from './module-listing.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { TranslateModule } from '@ngx-translate/core';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import { UntypedFormBuilder } from '@angular/forms';
import { of } from 'rxjs';
import { ModuleTableItem, Modules } from './module-listing.interface';
import { ModuleListingService } from './module-listing.service';
import { Apollo } from 'apollo-angular';
import { ModulesGQL } from '@app/graphql/generated/generated';
const modules = {
  data: {
    modules:
    {
      content: [{ id: 2002, name: 'PRG1', path: '/test', objectTypeLink: { technologyLink: 'Cobol', objectTypeLink: 'test' } },
      {
        id: 2002, name: 'PRG1', path: '/test', objectTypeLink: { technologyLink: 'Cobol', objectTypeLink: 'test' }
      }, {
        id: 2003, name: 'PRA1', path: '/test', objectTypeLink: { technologyLink: 'JCL', objectTypeLink: 'test' }
      }]
    }
  }
};

const moduleSearchList = {
  "data": {
      "modules": {
          "content": [
              {
                  "uid": "1d6d2b7a-7973-5b8b-bfd5-977cfe489b7e",
                  "id": 1593,
                  "customProperties": {},
                  "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                  "name": "M1_DSN8ED2",
                  "technology": "SQL",
                  "type": "STORED_PROCEDURE",
                  "storage": "FILE_SECTION",
                  "origin": "CUSTOM",
                  "creator": "DISCOVERY",
                  "identification": "MISSING",
                  "info": {
                      "__typename": "MAP_STRING_java_lang_Object"
                  },
                  "inCodebase": false,
                  "linkHash": "0700F2FF81BF2B94F6D060A2674C01DB",
                  "requiresReview": false,
                  "modifiedDate": "2023-04-05T16:59:22.246Z",
                  "metricsDate": "2023-04-05T16:59:22.246Z",
                  "complexityLevel": "UNKNOWN",
                  "errors": 0,
                  "statements": 0,
                  "sqlStatements": 0,
                  "sourceCodeAvailable": false,
                  "projectId": 1,
                  "__typename": "Module"
              },
              {
                  "uid": "39692159-c80c-5a9e-a954-d9d89262067e",
                  "id": 1595,
                  "customProperties": {},
                  "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                  "name": "M2_DSN8ED2",
                  "technology": "SQL",
                  "type": "STORED_PROCEDURE",
                  "storage": "FILE_SECTION",
                  "origin": "CUSTOM",
                  "creator": "DISCOVERY",
                  "identification": "MISSING",
                  "info": {
                      "__typename": "MAP_STRING_java_lang_Object"
                  },
                  "inCodebase": false,
                  "linkHash": "4861463A500B712C5109ACCB2F707D2C",
                  "requiresReview": false,
                  "modifiedDate": "2023-04-05T16:59:22.246Z",
                  "metricsDate": "2023-04-05T16:59:22.246Z",
                  "complexityLevel": "UNKNOWN",
                  "errors": 0,
                  "statements": 0,
                  "sqlStatements": 0,
                  "sourceCodeAvailable": false,
                  "projectId": 1,
                  "__typename": "Module"
              },
              {
                  "uid": "aa67a181-3b7b-58b6-9e3a-d0b376df4f2e",
                  "id": 514,
                  "customProperties": {},
                  "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                  "name": "M4030A",
                  "path": "src/jcl/A/jobs/jobs/M4030A.job",
                  "technology": "JCL",
                  "type": "JOB",
                  "storage": "FILE",
                  "origin": "CUSTOM",
                  "creator": "DISCOVERY",
                  "identification": "IDENTIFIED",
                  "info": {
                      "__typename": "MAP_STRING_java_lang_Object"
                  },
                  "inCodebase": true,
                  "source": "2882d864-c1d0-5508-b931-4c9fbca3b49d",
                  "contentHash": {
                      "empty": false,
                      "__typename": "innowake_mining_shared_access_BinaryValue"
                  },
                  "linkHash": "2Q8yRYwf3aJZ5tDhBnMamY",
                  "representation": "PHYSICAL",
                  "requiresReview": false,
                  "modifiedDate": "2023-04-05T16:59:22.246Z",
                  "metricsDate": "2023-04-05T16:59:22.246Z",
                  "sourceMetrics": {
                      "codeLines": 9,
                      "physicalLines": 16,
                      "commentLines": 7,
                      "complexityMcCabe": 1,
                      "deadCodeLines": -1,
                      "__typename": "SourceMetrics"
                  },
                  "complexityLevel": "LOW",
                  "content": "//M4018A JOB (20,FB3),'I',\r\n// CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),\r\n// NOTIFY=&SYSUID,\r\n// REGION=0M\r\n//*\r\n//*\r\n//DFSE10 JCLLIB ORDER=(VENDOR.PROCLIB,DFSE10.PROCLIB)\r\n//*\r\n//        SET PSB=PSB4030A\r\n//        SET PGM=P4030A\r\n//*\r\n//*\r\n//*\r\n//EXECPGM1 EXEC PGM=DFSRRC00,\r\n//             PARM='DLI,&PGM.,&PSB.,,,,,,,,,,N,N'\r\n//*",
                  "errors": 0,
                  "statements": 1,
                  "sqlStatements": 0,
                  "sourceCodeAvailable": true,
                  "projectId": 1,
                  "__typename": "Module"
              }
          ],
          "totalElements": 260,
          "size": 10,
          "__typename": "PAGED_Module"
      }
  }
};

const tableState: ModuleTableItem[] = [{
    "id": 2002,
    "module": "PRG1",
    "path": "/test",
    "technology": "Cobol",
    "type": undefined,
    'validationResult': {iconTheme: 'outline', iconType: 'check'},
},{
    "id": 2002,
    "module": "PRG1",
    "path": "/test",
    "technology": "Cobol",
    "type": undefined,
    'validationResult': {iconTheme: 'outline', iconType: 'check'},
},{
    "id": 2003,
    "module": "PRA1",
    "path": "/test",
    "technology": "JCL",
    "type": undefined,
    'validationResult': {iconTheme: 'outline', iconType: 'check'},
}];
describe('ModuleListingComponent', () => {   
    let component: ModuleListingComponent;
    let fixture: ComponentFixture<ModuleListingComponent>;
    const graphqlServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService', 
    ['graphQl']);
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
    const moduleStateSpy = jasmine.createSpyObj<ModuleListingService>('ModuleListingService', ['setModuleTableState', 'getModuleTableState']);
    const modulesGQLSpy = jasmine.createSpyObj<ModulesGQL>('ModulesGQL', ['fetch']);
    
    const listSearchModuleExpected = { content: [
        { value: 'M1_DSN8ED2', label: 'M1_DSN8ED2' },
        { value: 'M2_DSN8ED2', label: 'M2_DSN8ED2' },
        { value: 'M4030A', label: 'M4030A' }
    ]} as any;

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
          declarations: [ModuleListingComponent],
          providers :[
            { provide: NzMessageService },
            Apollo,
            { provide: GraphQlControllerService, useValue: graphqlServiceSpy },
            { provide: NzModalRef, useValue: nzModalRefSpy },
            { provide: ModulesGQL, useValue: modulesGQLSpy},
            UntypedFormBuilder
          ],
          imports: [
            NzModalModule,
            NzMessageModule,
            TranslateModule.forRoot({}),
            BrowserAnimationsModule
        ]
        }).compileComponents();
        graphqlServiceSpy.graphQl.and.returnValue(of(modules as any));
        modulesGQLSpy.fetch.and.returnValue(of(moduleSearchList as any));
    }));
    
    beforeEach(() => {
        fixture = TestBed.createComponent(ModuleListingComponent);
        component = fixture.componentInstance;
        component.moduleIds = ['2002'];
        (component as ModuleListingComponent).getInvalidDuplicateModule = jasmine.createSpy('getInvalidRepeatedModules ').and.returnValue({duplicateModules : ['2002']});
        component.moduleIdsFromReportingPage = '"2001,2002"';
        fixture.detectChanges();
    });

    it('should load instance', () => {
        expect(component).toBeTruthy();
    });

    it('should create new form control if moduleIds are empty', () => {
        component.moduleIds = undefined;
        expect(component).toBeTruthy();
        expect(component.moduleIds).toBeUndefined();
    });

    it('should contain the default selected module', () => {
        component.ngOnInit();
        expect(component.moduleIds).toEqual(['2002']);
        expect(component.moduleListForm.value.moduleList).toEqual(['2002']);
    });

    it('should test getNewValues', () => {
        component.tagFieldInput = {activatedValue : '2002'} as any
        component.getNewValues();
        expect(component.combinedValues).toBeDefined()
    });

    it('should test onValidation', () => {
        component.combinedValues =  ['a' , 'b' , 'c'];
        component.onValidation();
        expect(component.combinedValues.length).toBeGreaterThan(0);
    });

    it('should test onValidation with an empty array of combinedValues', () => {
        component.tagFieldInput = null;
        component.onValidation();
        expect(component.combinedValues.length).toEqual(1);
    });

    it('should test removeRecordFromTable', () => {
        component.removeRecordFromTable({data : {id : '2002'}} as any);
        expect(component.tableData ).toBeDefined();
        expect(component.totalRecords).toEqual(component.tableData.length);
    });

    it('should test if listSearchModule is sorted naturally', () => {
        component.onNameSearch('test');
        expect(component.listSearchModule).toEqual(listSearchModuleExpected.content);
    });

    it('should test onEnterConvertToTag', () => {
        component.tagFieldInput = {activatedValue : '2002'} as any;
        component.onEnterConvertToTag(['2002']);
        expect(component.invalidModules ).toBeDefined()
    });

    it('should test onEnterConvertToTag with no value', () => {
        component.tagFieldInput = {activatedValue : '2002'} as any;
        component.onEnterConvertToTag(undefined);
        expect(component.invalidModules ).toBeDefined()
    });

    it('should trigger backend when there is atleast 1 character', () => {
        component.onNameSearch('test');
        expect(component.moduleSearchLoading).toBeFalse();
        expect(modulesGQLSpy.fetch).toHaveBeenCalled();
    });

    it('should set empty listSearchModule and not call backend when search input is empty', () => {
        component.onNameSearch('');
        expect(component.listSearchModule).toEqual([]);
    });

    it('should test convertToTag when list is string', () => {
        component.tagFieldInput = {activatedValue : '2002'} as any;
        component.onEnterConvertToTag('"2002"');
        expect(component.invalidModules ).toBeDefined()
    });

    it('should test convertToTag when list is array', () => {
        component.tagFieldInput = {activatedValue : '2002'} as any;
        component.onEnterConvertToTag(['"2002"']);
        expect(component.invalidModules ).toBeDefined()
    });

    it('should test getInvalidRepeatedModules', () => {
        component.getInvalidDuplicateModule(['2002'], ['2002']);
        expect(component.invalidModules).toBeDefined()
    });

    it('should test sendDataToParentComponent', () => {
        component.sendDataToParentComponent();
        expect(component.modal.close).toHaveBeenCalled();
    });

    it('should test count for repeated modules and single module', () => {
        component.tableData = component['createTable'](modules.data.modules as Modules);
        component.sendDataToParentComponent();
        expect(component.modal.close).toHaveBeenCalledWith({
            modulesId: [2002, 2002, 2003], modulesName: ['PRG1', 'PRG1', 'PRA1'],
             selectedOption: 'name', repeatedModuleIds: [[2002,2002],[2003]]
        });
    });

    it('should test closeModal', () => {
        component.closeModal();
        expect(component.modal.close).toHaveBeenCalled();
    });

    it('should test getInvalidDuplicateModule', () => {
        const element = document.createElement('div');
        const child = document.createElement('span');
        const subChild = document.createElement('span');
        subChild.textContent = '2002'
        child.appendChild(subChild);
        element.appendChild(child)
        document.getElementById = jasmine.createSpy('tags').and.returnValue(element);
        component.getInvalidDuplicateModule(['2002', '2002'], ['2002']);
        expect(component.invalidModules).toBeDefined();
    });
})