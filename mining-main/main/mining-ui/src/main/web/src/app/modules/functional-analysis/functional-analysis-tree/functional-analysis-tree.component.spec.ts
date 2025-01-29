import { ComponentFixture, TestBed } from '@angular/core/testing';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { ActivatedRoute } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { of } from 'rxjs/internal/observable/of';
import { FunctionalAnalysisTreeComponent } from './functional-analysis-tree.component';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { ControlFlowGraph, FunctionalBlockControllerService, AnnotationToFunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { Subject } from 'rxjs';
import { NzFormatBeforeDropEvent, NzFormatEmitEvent, NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { MonacoEditorMockComponent } from '@app/core/mocks/monaco-editor.mock';
import { WindowToken } from '@app/core/utils/window';
import { CodeAnnotationEditorComponent } from '@app/modules/mining-code-viewer/code-annotation/code-annotation-editor.component';
import { CodeAnnotationEditorMock } from '@app/core/mocks/code-annotation-editor.mock';
import { ControlFlowUtility } from '../utils/functional-analysis-utility';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { FunctionalAnalysisGraphInfo } from '../models/functional-analysis-graph-info';
import { fakeAsync, tick } from '@angular/core/testing';
import { JobControllerService } from '@innowake/mining-api-angular-client';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { Apollo } from 'apollo-angular';
import { FaChildrenDeepGQL, FaTreeGQL } from '@app/graphql/generated/generated';
import { RESULT_DELETED } from '@app/shared/components/create-edit-functional-group/create-edit-functional-group.component';

class ControlFlowUtilityMock extends ControlFlowUtility {
  getGraphInfo(graph: any, moduleName: string): FunctionalAnalysisGraphInfo {
    return null;
  }
}

describe('FunctionalAnalysiTreeComponent', () => {
  let component: FunctionalAnalysisTreeComponent;
  let fixture: ComponentFixture<FunctionalAnalysisTreeComponent>;
  let mockWindow: any;
  let openedUrl = '';
  const i18nServiceSpy = { language: 'en-US' };

  const graphQlData = {
    "data": {
      "functionalBlocks": {
        "description": 'Test Description',
        "content": []
      }
    }
  } as any;

  const functionalBlockResponse = {
    "data": {
      "functionalBlock": {
        "description": "dummy description",
        "childrenDeep": {
          "totalElements": 0,
        }
      }
    }
  } as any;

  const functionalUnitDetails = {
    data: {
      functionalBlock: {
        resolvedModuleParts: [
          {
            module: {
              name: "'BABKREU'",
              id: 3065,
              source: '7fca1ecd-e6d0-40bc-976f-bfbb3523c099',
              taxonomies: [] as any[],
            },
          },
        ],
        description: 'Test Description',
        childrenDeep: {
          totalElements: 2,
          content: [
            {
              name: 'Data Validation Rule Candidate [System identified]',
              uid: '4d9d09cc-d9e3-4c1f-ab33-a29f30441bba',
              type: ['FUNCTIONAL_UNIT'],
              generatedFrom: {
                annotation: {
                  location: {
                    offset: 2881,
                    length: 580,
                  },
                  module: {
                    id: 3065,
                    name: "'BABKREU'",
                  },
                  name: 'Data Validation Rule Candidate [System identified]',
                  id: 151,
                  sourceAttachment: 'Test code',
                  categoryName: 'Validation Rule',
                  type: 'RULE',
                  state: 'CANDIDATE',
                  createdByUserName: 'system_user',
                  dataDictionaryEntries: ['UID1'],
                },
              },
            },
            {
              name: 'Data Validation Rule Candidate [System identified]',
              uid: 'b97b234f-c6dc-48ec-9a6e-8bf5ec4d2ffd',
              type: ['FUNCTIONAL_UNIT'],
              generatedFrom: {
                annotation: {
                  location: {
                    offset: 7555,
                    length: 110,
                  },
                  module: {
                    id: 3065,
                    name: "'BABKREU'",
                  },
                  name: 'Data Validation Rule Candidate [System identified]',
                  id: 154,
                  sourceAttachment: 'test code 1',
                  categoryName: 'Validation Rule',
                  type: 'RULE',
                  state: 'CANDIDATE',
                  createdByUserName: 'system_user',
                  dataDictionaryEntries: ['UID2'],
                },
              },
            }
          ],
        },
      },
    },
  }
  const controlFlowGraph: ControlFlowGraph = {
    "nodes": [
      {
        "id": "c8a347d7-b540-49d5-acef-d1d73f94c44d",
        "type": null as any,
        "entity": null as any,
        "parent": null as any,
        "label": "REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD",
        "properties": {},
        "offset": 18194,
        "length": 44,
        "superTypes": new Set([
          "Statement"
        ])
      },
      {
        "id": "79ccf01d-1554-40a4-ba21-10ab5e745dde",
        "type": null,
        "entity": null,
        "parent": null,
        "label": "PERFORM 1050-UPDATE-ACCOUNT ELSE MOVE 'N' TO WS-FIRST-TIME END-IF MOVE 0 TO WS-TOTAL-INT MOVE TRANCAT-ACCT-ID TO WS-LAST-ACCT-NUM MOVE TRANCAT-ACCT-ID TO FD-ACCT-ID PERFORM 1100-GET-ACCT-DATA MOVE TRANCAT-ACCT-ID TO FD-XREF-ACCT-ID PERFORM 1110-GET-XREF-DATA END-IF",
        "properties": {},
        "offset": 12440,
        "length": 496,
        "superTypes": new Set([
          "Statement"
        ])
      }
    ],
    edges: [
      {
        "fromId": "c8a347d7-b540-49d5-acef-d1d73f94c44d",
        "toId": "79ccf01d-1554-40a4-ba21-10ab5e745dde",
        "label": "FALSE"
      }
    ]
  }

  const annotationDetailsData = {
    data: {
      functionalBlock: {
        description: 'Test description',
        resolvedModuleParts: null,
        generatedFrom: {
          annotation: {
            location: {
              offset: 852,
              length: 30,
            },
            module: {
              id: 232,
              name: 'MMRS7101',
              taxonomies: [
                {
                  name: 'Read',
                  type: {
                    name: 'DB Access',
                  },
                },
              ],
            },
            sourceAttachment: 'SECTION',
            updatedByUserName: 'admin',
            name: 'jwedew',
            state: 'IN_ANALYSIS',
            type: 'RULE',
            categoryName: null,
            dataDictionaryEntries: [],
            id: 1,
          },
        },
        childrenDeep: {
          totalElements: 0,
          content: [],
        },
      },
    },
  } as any;
  

  const faTreeNodeData = {
    "data": {
      "functionalBlocks": {
        "totalElements": 2,
        "content": [
          {
            "uid": "b6943fb5-23b9-4bd0-9139-4f1485941bf0",
            "name": "Level 1",
            "type": [
              "FUNCTIONAL_GROUP"
            ],
            "flags": {
              "TYPE": [
                "FUNCTIONAL_GROUP"
              ],
              "COMPUTED_AT": 1721210653701
            },
            "children": {
              "totalElements": 1,
              "content": [
                {
                  "uid": "7d807292-38d9-43f1-a087-c33134bbbc36",
                  "name": "Level 2",
                  "type": [
                    "FUNCTIONAL_GROUP"
                  ],
                  "flags": {
                    "TYPE": [
                      "FUNCTIONAL_GROUP"
                    ],
                    "COMPUTED_AT": 1721152790479
                  },
                  "parents": {
                    "content": [
                      {
                        "uid": "b6943fb5-23b9-4bd0-9139-4f1485941bf0",
                        "__typename": "FunctionalBlock"
                      }
                    ]
                  },
                  "children": {
                    "totalElements": 1
                  }
                }
              ]
            }
          }
        ],
      }
    }
  } as any;

  const faChildrenDeepData = {
    "data": {
      "functionalBlock": {
        "childrenDeep": {
          "totalElements": 2,
          "content": [
            {
              uid:1,
              parents: {
                content: [
                  { uid: 'p1', name: 'Parent 1' },
                  { uid: 'p2', name: 'Parent 2' }
                ]
              },
              generatedFrom: {
                annotation: {
                  id: 1,
                  sourceAttachment: 'Test code 1',
                  location: {
                    offset: 100,
                    length: 50
                  },
                  module: {
                    name: 'Module 1'
                  }
                }
              }
            },
            {
              uid: 2,
              parents: {
                content: [
                  { uid: 'p1', name: 'Parent 1' },
                  { uid: 'p2', name: 'Parent 2' }
                ]
              },
              generatedFrom: {
                annotation: {
                  id: 2,
                  sourceAttachment: 'Test code 2',
                  location: {
                    offset: 200,
                    length: 60
                  },
                  module: {
                    name: 'Module 2'
                  }
                }
              }
            }
          ]
        }
      }
    }
  } as any;

  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['error', 'success', 'info']);
  const graphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>('GraphQlControllerService', ['graphQl']);
  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', ['getJobInformation', 'getJobResult']);

  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', [
    'create',
    'confirm'
  ]);

  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', [
    'init', 'mapLabel'
  ]);

  const functionalBlockController: jasmine.SpyObj<FunctionalBlockControllerService> = jasmine.createSpyObj('FunctionalBlockControllerService', [
    'createFunctionalBlock',
    'getFunctionalBlockAsControlFlowGraph',
    'updateFunctionalBlock',
    'deleteFunctionalBlock',
    'computeFunctionalBlock',
    'deleteFbOnUnGroup'
  ]);

  const annotationToFunctionalBlockControllerService: jasmine.SpyObj<any> = jasmine.createSpyObj('AnnotationToFunctionalBlockControllerService', [
    'deleteEmptyAutoGeneratedFunctionalUnits'
  ]);

  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('ClientProjectRelationshipService', [
    'getClientProjectObservable',
    'setClientProjectRelationship'
  ]);

  const languageServiceSpy: jasmine.SpyObj<any> = jasmine.createSpyObj('LanguageProviderService', [
    'injectCSS',
    'loadMonacoWASM'
  ]);

  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', [
    'afterClose',
    'close',
    'getContentComponent'
  ]);

  const faTreeGQLSpy = jasmine.createSpyObj<FaTreeGQL>('FaTreeGQL', ['fetch']);
  const faChildrenDeepGQLSpy = jasmine.createSpyObj<FaChildrenDeepGQL>('FaChildrenDeepGQL', ['fetch']);

  beforeEach((() => {
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
      declarations: [FunctionalAnalysisTreeComponent, MonacoEditorMockComponent],
      imports: [TranslateModule.forRoot({}), NzDropDownModule],
      providers: [
        TranslateService,
        NumberFormatter,
        Apollo,
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceStub },
        { provide: LanguageProviderService, useValue: languageServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: FunctionalBlockControllerService, useValue: functionalBlockController },
        { provide: AnnotationToFunctionalBlockControllerService , useValue: annotationToFunctionalBlockControllerService},
        { provide: LabelMappingService, useValue: labelMappingServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            queryParamMap: of({
              has: (key: string) => key === 'token',
              get: (key: string) => key === 'filterApplied'
            })
          }
        },
        { provide: CodeAnnotationEditorComponent, useClass: CodeAnnotationEditorMock },
        { provide: FaTreeGQL, useValue: faTreeGQLSpy },
        { provide: FaChildrenDeepGQL, useValue: faChildrenDeepGQLSpy }
      ]
    }).compileComponents();
    nzModalRefSpy.afterClose = new Subject;
    nzModalRefSpy.afterClose.next(null);
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of({ getprojectId: 1 } as any));
    graphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData as any));
    functionalBlockController.updateFunctionalBlock.and.returnValue(of({} as any));
    functionalBlockController.computeFunctionalBlock.and.returnValue(of({} as any));
    functionalBlockController.getFunctionalBlockAsControlFlowGraph.and.returnValue(of({} as any));
    faTreeGQLSpy.fetch.and.returnValue(of(faTreeNodeData));
    faChildrenDeepGQLSpy.fetch.and.returnValue(of(faChildrenDeepData));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(FunctionalAnalysisTreeComponent);
    component = fixture.componentInstance;
    component.projectId = 1
    component.selectedBlock = {
      title: '', uid: '', isLeaf: false, selectedType: '', annotationState: '', updatedBy: '',
      currentNode: {parentNode: {origin: {title: 'title1'}}}, immediateParent: 'parentUid1'
    };
    component.functionalAnalysisTree = [];
    spyOn(component, 'getTreeDetailsBasedOnFilter').and.callThrough();
    fixture.detectChanges();
  });

  it('should not set a functional block uid to selectedBlock.uid ', () => {
    const mockEvent: NzFormatEmitEvent = {
      eventName: 'expand',
      node: { origin: { title: 'demo', uid: '', isLeaf: true, selectedType: '', annotationState: '' }, addChildren: () => {},
        children: {totalElements: 0 }} as any,
      event: null, dragNode: null, selectedKeys: null, checkedKeys: null, matchedKeys: null, nodes: null, keys: null
    };
    component.nzEvent(mockEvent, 'expand');
    expect(component.selectedBlock.uid).toEqual(''); // Ensure selectedTree.uid is empty
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should toggleDropDown test', () => {
    const eventMock = jasmine.createSpyObj<Event>('Event', ['stopPropagation']);
    component.panelState['tets'] = true;
    component.toggleDropDown(eventMock, 'test');
    expect(component.panelState['test']).toBeTruthy();
  });

  it('should call getTreeDetailsBasedOnFilter with correct arguments', () => {
    const current = 2;
    const expectedPageIndex = current - 1;
    component.onPaginationChange(current);
    expect(component.selectedPageIndex).toBe(expectedPageIndex);
    expect(component.getTreeDetailsBasedOnFilter).toHaveBeenCalledWith(component.filterDetails, expectedPageIndex, undefined, component.sortDetails);
  });

  it('should build route for module Id', () => {
    const route = component.buildRouteForModule(2002) as any;
    expect(route).toBe(RouteBuilder.buildModuleRoute(component.projectId, 2002, '/details/overview'));
  });

  it('should test the onMonacoEditorInit method', () => {
    component.onMonacoInit({
      dispose: () => { },
      changeViewZones: () => { },
    } as any, { name: 'test', uid: 'testUid' } as any);
    expect(component.currentEditorInstances.length).toBe(1);

    // Reset the editor
    component.monacoEditor = null;
    component.currentEditorInstances.length = 0;
  });

  it('should toggle isChecked property and update groupedFunctionalBlockDetails', () => {
    const node = { origin: { uid: 'test-uid', type: 'FUNCTIONAL_BLOCK' }, isChecked: false };
    component.groupedFunctionalBlockDetails = [];
    component.onGrouping(node);
    expect(component.groupedFunctionalBlockDetails).toContain({ uid: 'test-uid', type: 'FUNCTIONAL_BLOCK', parent: undefined });
    component.onGrouping(node);
    expect(component.groupedFunctionalBlockDetails).not.toContain({ uid: 'test-uid', type: 'FUNCTIONAL_BLOCK', parent: undefined });
  });

  it('should update if selectedBlock.uid is present', () => {
    graphQlControllerServiceStub.graphQl.and.returnValue(of(functionalBlockResponse));
    component.selectedBlock = {
      uid: '123', title: 'string',
      isLeaf: true,
      selectedType: 'string',
      annotationState: 'string',
      updatedBy: '',
      currentNode: {parentNode: {origin: {title: 'title1'}}},
      immediateParent: 'parentUid1'
    };
    component.updateFunctionalBlock();
    expect(modalServiceSpy.create).toHaveBeenCalled(); // Ensure modal is created
  });

  it('should perform delete actions if result.state is RESULT_DELETED', (done) => {
    const showFunctionalDetailsSpy = spyOn(component.showFunctionalDetails, 'emit');

    component.selectedBlock = {
      uid: '123', title: 'string',
      isLeaf: true,
      selectedType: 'string',
      annotationState: 'string',
      updatedBy: '',
      currentNode: { parentNode: { origin: { title: 'title1' } } },
      immediateParent: 'parentUid1'
    };

    component.updateFunctionalBlock();

    nzModalRefSpy.afterClose.next({ state: RESULT_DELETED });

    setTimeout(() => {
      expect(component.functionalKeys.length).toBe(0);
      expect(component.selectedBlock).toEqual({ title: '', uid: '', isLeaf: false, selectedType: '', annotationState: '', updatedBy: '', currentNode: null, immediateParent: '' });
      expect(showFunctionalDetailsSpy).toHaveBeenCalledWith({ buttonStatus: true, msg: '' });
      expect(component.getTreeDetailsBasedOnFilter).toHaveBeenCalled();
      done();
    });
  });

  it('should handle event for click operation', () => {
    graphQlControllerServiceStub.graphQl.and.returnValue(of(functionalBlockResponse));
    component.currentEditorInstances = [{ dispose: () => { } }] as any;
    const event = {
      node: {
        origin: {
          uid: '1',
          type: 'FUNCTIONAL_GROUP',
          title: 'Functional Group 1'
        },
        level: 1
      }
    };

    component.nzEvent(event as any, 'click');
    expect(component.selectedBlock.selectedType).toBe('FUNCTIONAL_GROUP');

    event.node.origin.type = 'FUNCTIONAL_BLOCK';
    event.node.origin.uid = '2';
    component.nzEvent(event as any, 'click');
    expect(component.selectedBlock.selectedType).toBe('FUNCTIONAL_BLOCK');

    graphQlControllerServiceStub.graphQl.and.returnValue(of(annotationDetailsData));
    event.node.origin.type = 'FUNCTIONAL_UNIT';
    event.node.origin.uid = '3';
    component.nzEvent(event as any, 'click');
    expect(component.selectedBlock.selectedType).toBe('FUNCTIONAL_UNIT');
    expect(component.selectedBlock.isLeaf).toBe(true);

    // Test when same block is selected again.
    const result = component.nzEvent(event as any, 'click');
    expect(result).toBeUndefined();

  });

  it('should ungroup functional blocks', fakeAsync(() => {
    component.confirmModal = nzModalRefSpy;
    functionalBlockController.deleteFbOnUnGroup.and.returnValue(of(null));
    functionalBlockController.deleteFunctionalBlock.and.returnValue(of(null));

    component.selectedBlock = {
      title: 'Test Title',
      uid: 'test-uid',
      isLeaf: false,
      selectedType: 'Test Type',
      annotationState: 'Test State',
      updatedBy: 'Test User',
      currentNode: {},
      immediateParent: 'parentUid1'
    };
    component.deletedUID = '';
    component.projectId = 1;
    component.filterDetails = {};
    component.selectedPageIndex = 0;
    component.sortDetails = {};

    component.unGroupedFG();

    tick();

    expect(functionalBlockController.deleteFbOnUnGroup).toHaveBeenCalledWith(component.projectId, component.selectedBlock.immediateParent,
      component.selectedBlock.uid);
    expect(nzModalRefSpy.close).toHaveBeenCalled();
  }));

  xit('should test last modified by', () => {
    graphQlControllerServiceStub.graphQl.and.returnValue(of(annotationDetailsData as any));
    const event = { node: { origin: { type: 'FUNCTIONAL_UNIT', title: 'test', uid: '123' }, parentNode: { title: 123 } } };
    component.nzEvent(event as any, 'click');
    expect(component.selectedBlock.updatedBy).toBe('admin');
    component.getGroupLevelDetails('testUid', 1);
    graphQlControllerServiceStub.graphQl.and.returnValue(of(functionalUnitDetails as any));
    expect(graphQlControllerServiceStub.graphQl).toHaveBeenCalled();
  });

  xit('should not prepare breadCrumb for level 0', () => {
    const level = 0;
    const node = {
      parentNode: { origin: { title: 'Parent Node' } },
      origin: { title: 'Current Node' }
    };
    component['prepareBreadCrumb'](level, node);
    expect(component.breadCrumb).toEqual([]);
  });

  xit('should prepare breadCrumb for level greater than 1 without annotationNode', () => {
    const level = 2;
    const node = {
      parentNode: { origin: { title: 'Parent Node' } },
      origin: { title: 'Current Node' }
    };
    component['prepareBreadCrumb'](level, node);
    expect(component.breadCrumb).toEqual(['..', 'Parent Node', 'Current Node']);
  });

  it('should get functional block as CFG', () => {
    functionalBlockController.getFunctionalBlockAsControlFlowGraph.and.returnValue(of(controlFlowGraph) as any);
    component.controlFlowUtility = new ControlFlowUtilityMock();
    component.getFunctionalBlockAsCFG('testUid', 'moduleName');
    expect(functionalBlockController.getFunctionalBlockAsControlFlowGraph).toHaveBeenCalledWith(1, 'testUid')
  });

  it('Should test the createChildHierarchy Method', () => {
    const blocks = [
      {
        "uid": "1",
        "name": "child",
        "type": ["FUNCTIONAL_UNIT"],
        "parents": {
          "content": [
            {
              "uid": "2",
              "name": "Parent"
            }
          ]
        },
        "children": {
          "totalElements": 0,
        }
      }, {
        "uid": "2",
        "name": "parent",
        "type": ["FUNCTIONAL_GROUP"],
        "parents": null,
        "children": {
          "totalElements": 1,
        }
      } 
    ];
    const returnValue = (component as any).createChildHierarchy(blocks, '2', false);
    expect(returnValue).toEqual([
      {
        "title": "child",
        "key": "1",
        "uid": "1",
        "isCheckBox": false,
        "isDescriptionProvided": true,
        "type": "FUNCTIONAL_UNIT",
        "isLeaf": true,
        "isLoaded": true,
        "children": null,
        "generatedBy": undefined
      }]);
  });

  it('Should test the treeSearch Method', () => {
    const node = {
      "title": "child",
      "key": "1"
    };
    component.filterDetails = {
      'blockNameSearch': ['child']
    };
    const returnValue = component.treeSearch(node);
    expect(returnValue).toEqual(true);
  });

  it('should set properties and call methods when operation is click and childrenDeep has length', () => {
    const response = {
      data: {
        functionalBlock: {
          childrenDeep: {
            content: [
              {
                generatedFrom: {
                  annotation: {
                    sourceAttachment: 'test sourceAttachment',
                    location: {
                      offset: 1,
                    },
                    module: 'test module',
                    id: 1809,
                  },
                },
              },
            ],
          },
          description: 'test description',
          resolvedModuleParts: [
            {
              module: {
                id: 1993,
              },
            },
          ],
        },
      },
    };
    const functionBlockId = 'test functionBlockId';
    component['setFunctionalUnitsDetails'](response, functionBlockId);
    component['resetMappedDataKeys'];
    component['groupModulesForTaxonomies'](response);
    expect(component.functionalPanelData['Description']).toEqual(response.data.functionalBlock.description);
    expect(component.moduleId).toEqual(response.data.functionalBlock.resolvedModuleParts[0]?.module.id);
  });

  it('should set noTaxModules to false when childrenDeep is null', () => {
    const response = {
      data: {
        functionalBlock: {
          childrenDeep: null as null,
          description: 'test description',
          resolvedModuleParts: [
            {
              module: {
                id: 'test id',
              },
            },
          ],
        },
      },
    };
    component['setFunctionalUnitsDetails'](response, 'test');
    expect(component.noTaxModules).toBe(false);
  });

  it('should include description in every functionalAnalysisItem for prepareTreeData', () => {
    const data = [
      {
        uid: 'test uid',
        name: 'test name',
        description: 'test description',
        children: {
          content: [
            {
              flags: {
                TYPE: ['FUNCTIONAL_UNIT'],
              },
            },
          ],
        },
        flags: {
          GENERATED_BY: 'test generatedBy',
        },
      },
    ];
    const childrenMap = new Map<string, any[]>();
    childrenMap.set('test uid', data[0].children.content);
    component.functionalAnalysisTree = [];
    component['prepareTreeData'](data, 'test uid', childrenMap);
    component.functionalAnalysisTree.forEach((item) => {
      expect(item.description).toBeDefined();
      expect(item.description).toEqual('test description');
    });
  });

  it('should open unGrouped annotation links in new tab', () => {
    component.projectId = 1;
    const currentPath = 'annotations?savedSearch=Ungrouped Annotations';
    component.navigateToUnGroupedAnnotationsSavedSearch();
    const expectedUrl = location.pathname + '#/project-1/' + currentPath;
    expect(openedUrl).toBe(expectedUrl);
  });

  it('should prevent default event when node type is not FUNCTIONAL_UNIT or FUNCTIONAL_CONDITION', () => {
    const event = {
      dragNode: { origin: { uid: 'uid1' }, parentNode: { origin: { uid: 'uid', description: 'description', children: [] } } },
      node: { origin: { type: 'OTHER_TYPE' }, parentNode: { origin: { type: 'FUNCTIONAL_BLOCK' } } },
      event: { preventDefault: jasmine.createSpy('preventDefault') }
    } as unknown as NzFormatEmitEvent;

    component.onDragStart(event);

    expect(event.event.preventDefault).toHaveBeenCalled();
  });

  it('should not prevent default event when node type is FUNCTIONAL_UNIT', () => {
    const event = {
      dragNode: { origin: { uid: 'uid1' }, parentNode: { origin: { uid: 'uid', description: 'description', children: [] } } },
      node: { origin: { type: 'FUNCTIONAL_UNIT' }, dragNode: { parentNode: { origin: { uid: 'uid', description: 'description', children: [] } } }, parentNode: { origin: { type: 'FUNCTIONAL_CONDITION' } } },
      event: { preventDefault: jasmine.createSpy('preventDefault') }
    } as unknown as NzFormatEmitEvent;
    component.onDragStart(event);
    expect(event.event.preventDefault).not.toHaveBeenCalled();
  });

  it('should not prevent default event when node type is FUNCTIONAL_CONDITION', () => {
    const event = {
      dragNode: { origin: { uid: 'uid1' }, parentNode: { origin: { uid: 'uid', description: 'description', children: [] } } },
      node: { origin: { type: 'FUNCTIONAL_CONDITION' }, dragNode: { parentNode: { origin: { uid: 'uid', description: 'description', children: [] } } }, parentNode: { origin: { type: 'FUNCTIONAL_CONDITION' } } },
      event: { preventDefault: jasmine.createSpy('preventDefault') }
    } as unknown as NzFormatEmitEvent;
    component.onDragStart(event);
    expect(event.event.preventDefault).not.toHaveBeenCalled();
  });

  it('should correctly update children after onDrop', () => {
    const event = {
      dragNode: { origin: { uid: 'uid1' }, parentNode: { origin: { uid: 'uid', description: 'description', children: [] } } },
      node: { origin: { type: 'FUNCTIONAL_CONDITION' }, dragNode: { parentNode: { origin: { uid: 'uid', description: 'description', children: [] } } }, parentNode: { origin: { type: 'FUNCTIONAL_CONDITION', children: [{ type: 'FUNCTIONAL_CONDITION', ui: 'uid', name: 'name' }] } } },
      event: { preventDefault: jasmine.createSpy('preventDefault') }
    } as unknown as NzFormatEmitEvent;
    component['draggedNodeParents'] = event?.dragNode?.parentNode?.origin;
    component.onDrop(event);
    expect(event.node.parentNode.origin.children.length).toBeGreaterThan(0);
    expect(event.dragNode.parentNode.origin.children.length).toEqual(0);
  });

  it('should add node to selected nodes when checked', () => {
    const node = {
      parentNode: {
        origin: {
          "title": "fb5",
          "key": "13988a75-4204-45e7-90ae-28a0b5602a6f",
          "uid": "13988a75-4204-45e7-90ae-28a0b5602a6f",
          "isCheckBox": false,
          "type": "FUNCTIONAL_BLOCK",
          "isDescriptionProvided": true,
          "children": [
            {
              "title": "System identified Database Query",
              "key": "13988a75-4204-45e7-90ae-28a0b5602a6f-0",
              "uid": "81c192a1-6cd6-4493-9b27-037706e03e84",
              "isLeaf": true,
              "type": "FUNCTIONAL_UNIT",
              "isDescriptionProvided": true
            }
          ],
          "expanded": true,
          "selected": false
        }
      }, 
      origin: { uid: 'uid' }, title: '', key: ''
    } as NzTreeNodeOptions;
    
    component.onAnnotationCheck(node, true);
    expect(component.selectedAnnotations.has('uid')).toBeTrue();

    component.onAnnotationCheck(node, false);
    expect(component.selectedAnnotations.has('uid')).toBeFalse();
  });

  it('should set annotations as per tree node', () => {
    (component as any).setAnnotationsForSelectedNode(0);
    expect(component.selectedBlockAnnotations).toEqual([
      {
        content: 'Test code 1',
        EditorOption: { ...component.EDITOR_OPTIONS, lineNumbers: jasmine.any(Function) },
        fgName: 'Parent 1',
        moduleDetails: { name: 'Module 1' },
        parents: [{ uid: 'p1', name: 'Parent 1' }, { uid: 'p2', name: 'Parent 2' }],
        annotationId: 1,
        codeAnnotation: {
          id: 1,
          sourceAttachment: 'Test code 1',
          location: {
            offset: 100,
            length: 50
          },
          module: {
            name: 'Module 1'
          }
        },
        uid: 1
      },
      {
        content: 'Test code 2',
        EditorOption: { ...component.EDITOR_OPTIONS, lineNumbers: jasmine.any(Function) },
        fgName: '',
        moduleDetails: { name: 'Module 2' },
        parents: [{ uid: 'p1', name: 'Parent 1' }, { uid: 'p2', name: 'Parent 2' }],
        annotationId: 2,
        codeAnnotation: {
          id: 2,
          sourceAttachment: 'Test code 2',
          location: {
            offset: 200,
            length: 60
          },
          module: {
            name: 'Module 2'
          }
        },
        uid: 2
      }
    ]);
  });

  it('should set loader details to loading and call getFunctionalBlockAsCFG', () => {
    component.loader = { details: null };
    component.selectedBlock = {
      title: 'Test Title',
      uid: 'test-uid',
      isLeaf: false,
      selectedType: 'Test Type',
      annotationState: 'Test State',
      updatedBy: 'Test User',
      currentNode: {},
      immediateParent: 'parentUid1'
    };
    component.moduleName = 'test-module';
    spyOn(component, 'getFunctionalBlockAsCFG');
    component.handleReCreateGraph();
    expect(component.loader.details).toBe(LoaderState.loading);
    expect(component.getFunctionalBlockAsCFG).toHaveBeenCalledWith('test-uid', 'test-module');
  });

  it('should set enableRestoreBranch to true when handleRestore is called', () => {
    component.loader = { details: null };
    component.selectedBlock = {
      title: 'Test Title',
      uid: 'test-uid',
      isLeaf: false,
      selectedType: 'Test Type',
      annotationState: 'Test State',
      updatedBy: 'Test User',
      currentNode: {},
      immediateParent: 'parentUid1'
    };
    component.moduleName = 'test-module';
    spyOn(component, 'getFunctionalBlockAsCFG');
    functionalBlockController.getFunctionalBlockAsControlFlowGraph.and.returnValue(of(controlFlowGraph) as any);
    component.controlFlowUtility = new ControlFlowUtilityMock();
    component.handleRestore();
    expect(component.getFunctionalBlockAsCFG).toHaveBeenCalledWith('test-uid', 'test-module');
    expect(component.enableRestoreBranch).toBeTrue();
  });

  it('should handle the drop event', () => {
    const event = {
      dragNode: {
        origin: { uid: 'uid1' }, parentNode: {
          origin: {
            uid: 'uid1', description: 'description',
            children: [{ type: 'FUNCTIONAL_UNIT', ui: 'uid1', name: 'name' }]
          }
        }
      },
      node: {
        key: "uid1", origin: { type: 'FUNCTIONAL_UNIT' }, dragNode: {
          parentNode: {
            origin: {
              uid: 'uid1', description: 'description',
              children: [{ type: 'FUNCTIONAL_UNIT', ui: 'uid1', name: 'name' }]
            }
          }
        },
        parentNode: { origin: { type: 'FUNCTIONAL_UNIT', children: [{ type: 'FUNCTIONAL_UNIT', ui: 'uid1', name: 'name' }] } }
      },
      event: { preventDefault: jasmine.createSpy('preventDefault') }
    } as unknown as NzFormatEmitEvent;
    component['draggedNodeParents'] = event?.dragNode?.parentNode?.origin;
    const updateNodeSpy = spyOn(component as any, 'updateNode').and.returnValue(of(null));
    const onDropSpy = spyOn(component, 'onDrop').and.callThrough();
    component.onDrop(event);
    (component as any).updateNode({ uid: 'uid1', description: 'description', name: undefined, children: [] }, 'drag-drop');
    expect(onDropSpy).toHaveBeenCalled();
    expect(updateNodeSpy).toHaveBeenCalledTimes(1);
    expect(updateNodeSpy).toHaveBeenCalledWith(
      { uid: 'uid1', description: 'description', name: undefined, children: [] }, 'drag-drop'
    );
  });

  it('should return true if there are multiple types of functional blocks', () => {
    component.groupedFunctionalBlockDetails = [
      { uid: '1', type: 'FUNCTIONAL_UNIT', parent: { uid: '1', type: 'FUNCTIONAL_UNIT', title: '', key: '' } },
      { uid: '2', type: 'FUNCTIONAL_BLOCK', parent: { uid: '2', type: 'FUNCTIONAL_BLOCK', title: '', key: '' } },
    ];
    expect(component.checkCreateGroupStatus(true)).toBe(true);
  });

  it('should return true if there are multiple parents and multipleParentSupport is false', () => {
    component.groupedFunctionalBlockDetails = [
      { uid: '1', parent: { uid: '1', type: 'FUNCTIONAL_BLOCK', title: '', key: '1' }, type: 'FUNCTIONAL_UNIT' },
      { uid: '2', parent: { uid: '2', type: 'FUNCTIONAL_BLOCK', title: '', key: '2' }, type: 'FUNCTIONAL_UNIT' },
    ];
    expect(component.checkCreateGroupStatus(false)).toBe(true);
  });

  it('should return false if there is only one type of functional block', () => {
    component.groupedFunctionalBlockDetails = [
      { uid: '1', type: 'FUNCTIONAL_BLOCK', parent: { uid: '1', type: 'FUNCTIONAL_BLOCK', title: '', key: '' } },
      { uid: '2', type: 'FUNCTIONAL_BLOCK', parent: { uid: '2', type: 'FUNCTIONAL_BLOCK', title: '', key: '' } },
    ];
    expect(component.checkCreateGroupStatus(true)).toBe(false);
  });

  it('should return false if there is only one parent and multipleParentSupport is false', () => {
    component.groupedFunctionalBlockDetails = [
      { uid: '1', parent: { uid: '1', type: 'FUNCTIONAL_BLOCK', title: '', key: '1' }, type: 'FUNCTIONAL_BLOCK' },
      { uid: '2', parent: { uid: '1', type: 'FUNCTIONAL_BLOCK', title: '', key: '1' }, type: 'FUNCTIONAL_BLOCK' },
    ];
    expect(component.checkCreateGroupStatus(false)).toBe(false);
  });

  it('should return true if there are no functional block details', () => {
    component.groupedFunctionalBlockDetails = [];
    expect(component.checkCreateGroupStatus(true)).toBe(true);
  });

  it('should return false and show error message if the user tries to drop a node on the first root node', () => {
    const event = {
      node: {
        origin: {
          title: 'Root Node',
        },
        title: 'Root Node'
      },
      pos: -1,
      dragNode: {
        origin: {
          uid: 'dragged-node',
        },
        parentNode: {
          origin: {
            uid: 'parent-node',
          },
        },
      },
    } as any;

    component.functionalAnalysisTree = [
      {
        title: 'Root Node',
      },
      {
        title: 'Child Node',
      },
    ] as any;

    component.beforeDrop(event);
    expect(component['messageService'].error).toHaveBeenCalledWith('functionalAnalysis.dropNotAllowed');
  });

  it('should return false and show error message if the user tries to drop a node on a group node which is not yet loaded', () => {
    const event: NzFormatBeforeDropEvent = {
      node: {
        origin: {
          isLoaded: false,
          type: 'FUNCTIONAL_GROUP',
        },
      },
      pos: 0,
      dragNode: {
        origin: {
          uid: 'dragged-node',
        },
        parentNode: {
          origin: {
            uid: 'parent-node',
          },
        },
      },
    } as any;

    spyOn((component as  any), 'loadTreeNodeChildren');
    component.beforeDrop(event);

    expect(component['loadTreeNodeChildren']).toHaveBeenCalledWith(event.node);
    expect(component['messageService'].error).toHaveBeenCalledWith('functionalAnalysis.dropFailed');
  
  });

  it('should update the target node and source node, and return true if the drop is successful', () => {
    const event: NzFormatBeforeDropEvent = {
      node: {
        origin: {
          isLeaf: true,
          uid: 'target-node',
          isLoaded: true,
          type: 'FUNCTIONAL_GROUP',
        },
        parentNode: {
          origin: {
            uid: 'parent-node',
          },
          addChildren: jasmine.createSpy('addChildren'),
        },
        isLeaf: true
      },
      pos: 0,
      dragNode: {
        origin: {
          uid: 'dragged-node',
          parents: []
        },
        parentNode: {
          origin: {
            uid: 'parent-node',
            children: []
          },
        },
        remove: jasmine.createSpy('remove'),
      },
    } as any;

    const sourceReqBody = {
      uid: 'dragged-node',
    };

    const targetReqBody = {
      uid: 'target-node',
      children: ['child-node'],
    };

    spyOn((component as any), 'createSourceReqBody').and.returnValue(sourceReqBody);
    spyOn((component as any), 'createReqBodyFromNode').and.returnValue(targetReqBody);
    spyOn((component as any), 'updateNode').and.returnValue(of(null));
    spyOn((component as any), 'flattenTree').and.returnValue([
      {
        key: 'target-node',
        children: [],
      },
      {
        uid: 'dragged-node',
        children: [],
      },
    ]);

    component.beforeDrop(event);

    expect(component['createSourceReqBody']).toHaveBeenCalledWith(event.dragNode.parentNode.origin, 'dragged-node');
    expect(component['createReqBodyFromNode']).toHaveBeenCalledWith(event.node.parentNode.origin);
    expect(component['updateNode']).toHaveBeenCalledTimes(2);
    expect(event.node.parentNode.addChildren).toHaveBeenCalledWith([event.dragNode.origin], 0);
    expect(event.dragNode.remove).toHaveBeenCalled();    
  });

  it('should return the uid as the key', () => {
    const index = 0;
    const code = { uid: '123456789' };
    const result = component.trackByKey(index, code);
    expect(result).toEqual('123456789');
  });

  it('should clear selected blocks', () => {
    component.selectedAnnotations.set('uid1', { uid: 'uid1' } as unknown as  NzTreeNodeOptions);
    component.groupedFunctionalBlockDetails = [
      { uid: 'uid1', type: 'type1', parent: { uid: 'parent1' } as unknown as  NzTreeNodeOptions },
    ];

    component.clearSelectedBlocks();
    expect(component.selectedAnnotations.size).toBe(0);
    expect(component.groupedFunctionalBlockDetails.length).toBe(0);
  });
});
