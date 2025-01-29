import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { ListFunctionalGroupComponent } from './list-functional-group.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { of } from 'rxjs';
import { TranslateModule } from '@ngx-translate/core';
import { AnnotationToFunctionalBlockControllerService, FunctionalBlockControllerService, FunctionalBlockPojo } from '@innowake/mining-api-angular-client';
import { AnnotationsFunctionalGroups } from '@app/modules/mining-code-viewer/mining-code-viewer';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { ModulesGQL } from '@app/graphql/generated/generated';
import { NgZone } from '@angular/core';
import { HttpResponse } from '@angular/common/http';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';

describe('ListFunctionalGroupComponent', () => {
  let component: ListFunctionalGroupComponent;
  let fixture: ComponentFixture<ListFunctionalGroupComponent>;
  let ngZone: NgZone
  const functionalTree = [{
    "title": "TEST EMPTY 1",
    "key": "f874c698-5180-4e27-b614-b7eedbc91df8",
    "uid": "f874c698-5180-4e27-b614-b7eedbc91df8",
    "isCheckBox": false,
    "type": "FUNCTIONAL_GROUP",
    "isDescriptionProvided": true,
    "children": [] as any,
    "annotations": [] as any,
    "expanded": true
}, {
  "title": "TEST EMPTY 1",
  "key": "testUid",
  "uid": "testUid",
  "isCheckBox": false,
  "type": "FUNCTIONAL_GROUP",
  "isDescriptionProvided": true,
  "children": [] as any,
  "annotations": [] as any,
  "expanded": true
}]
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
  const fbgData = [
    {
      uid: '39f1d9e8-29fc-4bf9-a310-78bb9f3397cd',
      name: 'A!',
      description: '',
      type: ['FUNCTIONAL_GROUP'],
      generatedFrom: {},
      children: {
        content: [
          {
            uid: 'c36ccba6-8b13-4245-a9e4-0980af9c9715',
            name: 'Data Validation Rule Candidate [System identified]',
            description: 'Data Validation Rule Candidate [System identified]',
            type: ['FUNCTIONAL_UNIT'],
            generatedFrom: {
              annotationId: 98,
            },
          },
          {
            uid: 'b7ce540e-1662-4ce4-aede-53bda420ef75',
            name: 'Business Rule Candidate [System identified]',
            description: 'Business Rule Candidate [System identified]',
            type: ['FUNCTIONAL_UNIT'],
            generatedFrom: {
              annotationId: 76,
            },
          },
        ],
      },
    },
  ];
  const mockedDataFunctionGroupTableData: AnnotationsFunctionalGroups[] = [
    {
      uid: 'uid1',
      name: 'Group 1',
      description: 'Description 1',
      generatedFrom: {
        annotationId: 123,
      },
      children: {
        content: [
          {
            uid: 'uid1',
            name: 'Child 1',
            description: 'Child Description 1',
            generatedFrom: {
              annotationId: 456,
            },
            children: {
              content: [],
            },
          },
        ],
      },
    },
    {
      uid: 'uid2',
      name: 'Group 1',
      description: 'Description 1',
      generatedFrom: {
        annotationId: 123,
      },
      children: {
        content: [
          {
            uid: 'uid2',
            name: 'Child 1',
            description: 'Child Description 1',
            generatedFrom: {
              annotationId: 456,
            },
            children: {
              content: [],
            },
          },
        ],
      },
    },
  ];
  const graphQl = {
    data: {
      functionalBlocks: {
        content: [
          {
            uid: '717bbe47-60e2-47ef-b693-4ea296d5b200',
            parents: {
              content: [
                {
                  uid: '28556850-f9c0-4802-ab86-090005d044ae',
                  name: 'Test Group 1',
                },
              ],
            },
            children: {
              content: [
                {
                  uid: '28556850-f9c0-4802-ab86-090005d044ae',
                  name: 'Test Group 1',
                  generatedFrom: {
                    annotationId: 5,
                  },
                },
              ],
            },
          },
          {
            uid: '032f7917-f1bb-4979-b767-f91ee8957ce5',
            parents: {
              content: [
                {
                  uid: 'fc869a6c-b65c-4467-91cd-d87a8fed2eec',
                  name: 'Test Group 2',
                  generatedFrom: {
                    annotationId: 5,
                  },
                },
              ],
            },
            children: {
              content: [
                {
                  uid: '28556850-f9c0-4802-ab86-090005d044ae',
                  name: 'Test Group 1',
                  generatedFrom: {
                    annotationId: 7,
                  },
                },
              ],
            },
          },
        ],
      },
    },
  };
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
    'GraphQlControllerService',
    ['graphQl']
  );
  const modulesGQLSpy = jasmine.createSpyObj<ModulesGQL>('ModulesGQL', ['fetch']);

  const functionalBlockController: jasmine.SpyObj<FunctionalBlockControllerService> = jasmine.createSpyObj(
    'FunctionalBlockControllerService',
    ['createFunctionalBlock', 'computeFunctionalBlock', 'updateFunctionalBlock', 'deleteFunctionalBlock']
  );
  const annotationFunctionalController: jasmine.SpyObj<AnnotationToFunctionalBlockControllerService> =
    jasmine.createSpyObj('AnnotationToFunctionalBlockControllerService', ['getFunctionalUnitsForAnnotations']);
  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [ListFunctionalGroupComponent],
      imports: [TranslateModule.forRoot()],
      providers: [
        { provide: NzModalRef, useValue: jasmine.createSpyObj('NzModalRef', ['destroy']) },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: FunctionalBlockControllerService, useValue: functionalBlockController },
        { provide: AnnotationToFunctionalBlockControllerService, useValue: annotationFunctionalController },
        {provide: ModulesGQL, useValue: modulesGQLSpy}
      ],
    });

    fixture = TestBed.createComponent(ListFunctionalGroupComponent);
    component = fixture.componentInstance;
    component.annotation = mockedDataFunctionGroupTableData;
    ngZone = new NgZone({});
    component.nzExpandedKeys = [];
  });

  graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQl as any));
  modulesGQLSpy.fetch.and.returnValue(of(moduleSearchList as any));
  functionalBlockController.computeFunctionalBlock.and.returnValue(of('43df9bcc-af0b-473b-a659-c0278bb7e0df' as any));

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('test graphQL response', () => {
    component.projectId = 3;
    component.annotationIds = [5, 7];
    const graphQlResponse = {
      data: {
        functionalBlocks: {
          content: [
            {
              uid: 'functionalGroupId1',
              name: 'Functional Group 1',
              description: 'Description 1',
              children: {
                content: [
                  {
                    uid: 'childId1',
                    name: 'Child 1',
                    generatedFrom: {
                      annotationId: 5,
                    },
                  },
                ],
              },
            },
            {
              uid: 'functionalGroupId2',
              name: 'Functional Group 2',
              description: 'Description 2',
              children: {
                content: [
                  {
                    uid: 'childId2',
                    name: 'Child 2',
                    generatedFrom: {
                      annotationId: 7,
                    },
                  },
                ],
              },
            },
          ],
        },
      },
    };
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQlResponse as any));
  });

  xit('onSave should call modalRef.destroy', () => {
    component.onSave();
    const modalRef = TestBed.inject(NzModalRef);
    expect(modalRef.destroy).toHaveBeenCalledWith({ save: true, createNew: false });
  });

  it('handleCancel should call modalRef.destroy', () => {
    component.handleCancel();
    const modalRef = TestBed.inject(NzModalRef);
    expect(modalRef.destroy).toHaveBeenCalled();
  });

  it('should check or uncheck an individual item in the functional group table', () => {
    const uid = 'example_uid';
    const checked = true;
    spyOn(component, 'updateCheckedSet');
    spyOn(component, 'refreshCheckedStatus');
    component.onItemChecked(uid, checked, false);
    expect(component.updateCheckedSet).toHaveBeenCalledWith(uid, checked);
    expect(component.refreshCheckedStatus).toHaveBeenCalled();
  });

  it('should destroy the modal with specific parameters for creating a new item', () => {
    const modalRef = TestBed.inject(NzModalRef);
    component.createNew();
    expect(modalRef.destroy).toHaveBeenCalledWith({ save: false, createNew: true });
  });


  it('should call getFunctionalUnitsForAnnotations if annotationIds is not empty', () => {
    component.annotationIds = [123];
    const mockFunctionalUnits = { 'testKey': 'testUid' };
    annotationFunctionalController.getFunctionalUnitsForAnnotations.and.returnValue(of(mockFunctionalUnits) as any);
    component.ngOnInit();
    expect(annotationFunctionalController.getFunctionalUnitsForAnnotations).toHaveBeenCalledWith(component.projectId, [component.annotationIds[0]]);
    expect(component.annotation['uid']).toEqual('testUid');
  });

  it('should update checkStatus of functionalGroup if it is found', () => {
    const uid = 'testUid';
    const checked = true;
    component.functionalAnalysisTree = functionalTree;
    component.updateCheckedSet(uid, checked);
    expect(component.findFunctionalGroupByUid(uid)['checkStatus']).toEqual('checked');
  });

  it('should add uid to setOfCheckedId if checked is true', () => {
    const uid = 'testUid';
    const checked = true;
    component.updateCheckedSet(uid, checked);
    expect(component.setOfCheckedId.has(uid)).toBeTrue();
  });

  it('should remove uid from setOfCheckedId if checked is false', () => {
    const uid = 'testUid';
    const checked = false;
    component.setOfCheckedId.add(uid);
    component.updateCheckedSet(uid, checked);
    expect(component.setOfCheckedId.has(uid)).toBeFalse();
  });

  it('should find parent UIDs of the updated functional group', () => {
    const flattenFunctionalTree = [
      {
        uid: 'parentUid',
        children: [
          {
            uid: 'childUid',
            children: [] as any
          }
        ]
      },
      {
        uid: 'anotherParentUid',
        children: [
          {
            uid: 'anotherChildUid',
            children: []
          }
        ]
      }
    ];
    (component as any).findParentUidOfUpdatedFG('childUid', flattenFunctionalTree);
    expect(component['parentUUIDs']).toContain('parentUid');
    expect(component['parentUUIDs']).not.toContain('anotherParentUid');
  });

  it('should call ngZone.run when updating nzExpandedKeys', () => {
    const mockFunction = jasmine.createSpy('mockFunction');
    const tree = [
      {
        uid: 'uid',
        children: [],
        expanded: false,
        isChecked: false
      },
      {
        uid: 'anotherUid',
        children: [
          {
            uid: 'childUid',
            children: [] as any,
            expanded: false,
            isChecked: false
          }
        ],
        expanded: false,
        isChecked: false
      }
    ];
    ngZone.run(mockFunction);
    (component as any).expandNodesByUid('childUid', tree);
    expect(mockFunction).toHaveBeenCalled();
  });

  it('should include description in every request body for updateFunctionalBlock', () => {
    component.onSave();
    const flattenedTree = component.flattenTree(component.functionalAnalysisTree);
    flattenedTree.forEach((args: NzTreeNodeOptions) => {
      const requestBody = args[2];
      expect(requestBody).toBeDefined();
      expect(requestBody.description).toEqual('test description');
    });
  });

});
