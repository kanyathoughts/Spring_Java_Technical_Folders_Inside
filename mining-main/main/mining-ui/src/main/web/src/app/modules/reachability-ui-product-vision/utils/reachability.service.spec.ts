import { ReachabilityService } from './reachability.service';
import { TestBed } from '@angular/core/testing';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { FunctionalBlockControllerService, ModuleControllerService } from '@innowake/mining-api-angular-client';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalService } from 'ng-zorro-antd/modal';
import { I18nService } from '@app/core/services/i18n/i18n.service';
import { of, throwError } from 'rxjs';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { MergeBlockComponent } from '../block-view/merge-block/merge-block.component';
import { MergeBlockData } from './reachability-interface';
import { WindowToken } from '@app/core/utils/window';
import { model } from 'model-test-data';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { HttpErrorResponse } from '@angular/common/http';
import { FbTotalElementsCountGQL } from '@app/graphql/generated/generated';


describe('ReachabilityService', () => {
  let service: ReachabilityService;

  const graphQlSpy = jasmine.createSpyObj<GraphQlControllerService>('GraphQlControllerService', ['graphQl']);
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService', ['getAggregatedValues2']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  const functionalBlockControllerSpy = jasmine.createSpyObj<FunctionalBlockControllerService>('FunctionalBlockControllerService', ['mergeFunctionalBlock', 'updateBlockStatus']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['success', 'error']);
  const LabelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['mapLabel']);
  const fbCountGqlSpy = jasmine.createSpyObj<FbTotalElementsCountGQL>('FbTotalElementsCountGQL', ['fetch']);

    const reachabilityBlocksGql = {
        "data": {
            "reachabilityBlocks": {
                "content": [
                    {
                        "uid": "000b38b0-d745-49df-9125-0bdf5bd90da7",
                        "name": "READCARD Reachability",
                        "description": "",
                        "type": [
                            "RA_TOP_DOWN",
                            "REACHABILITY"
                        ],
                        "resolvedModuleParts": [
                            {
                                "module": {
                                    "id": 79
                                },
                                "referencedTaxonomies": [
                                    {
                                        "name": "Batch"
                                    },
                                    {
                                        "name": "Read"
                                    }
                                ]
                            },
                            {
                                "module": {
                                    "id": 121
                                },
                                "referencedTaxonomies": [
                                    {
                                        "name": "Taxonomy2"
                                    }
                                ]
                            },
                            {
                                "module": {
                                    "id": 266
                                },
                                "referencedTaxonomies": null
                            },
                            {
                                "module": {
                                    "id": 120
                                },
                                "referencedTaxonomies": null
                            }
                        ],
                        "peers": {
                            "totalElements": 0
                        },
                        "upperBound": {
                            "content": [
                                {
                                    "children": {
                                        "content": [
                                            {
                                                "generatedFrom": {
                                                    "module": {
                                                        "name": "READCARD",
                                                        "linkHash": "2nr9XCNNobwafBOFkeRyLV",
                                                        "objectTypeLink": {
                                                            "technologyLink": "JCL",
                                                            "typeLink": "JOB"
                                                        }
                                                    }
                                                }
                                            }
                                        ]
                                    }
                                }
                            ]
                        },
                        "lowerBound": {
                            "aggregations": [
                                {
                                    "groupBy": {
                                        "REFERENCED_MODULE_TYPE": "VSAM_FILE",
                                        "REFERENCED_MODULE_TECHNOLOGY": "RESOURCE"
                                    },
                                    "fields": {
                                        "UID": {
                                            "COUNT": 1
                                        }
                                    }
                                },
                                {
                                    "groupBy": {
                                        "REFERENCED_MODULE_TYPE": "LIB",
                                        "REFERENCED_MODULE_TECHNOLOGY": "RESOURCE"
                                    },
                                    "fields": {
                                        "UID": {
                                            "COUNT": 1
                                        }
                                    }
                                }
                            ]
                        }
                    },
                    {
                        "uid": "3fa85f64-5717-4562-b3fc-2c963f66afa7",
                        "name": "functionalBlock1",
                        "description": "No description",
                        "type": [
                            "RA_TOP_DOWN",
                            "REACHABILITY"
                        ],
                        "resolvedModuleParts": [
                            {
                                "module": {
                                    "id": 259
                                },
                                "referencedTaxonomies": [
                                    {
                                        "name": "taxonomyA1"
                                    }
                                ]
                            }
                        ],
                        "peers": {
                            "totalElements": null
                        },
                        "upperBound": {
                            "content": []
                        },
                        "lowerBound": {
                            "aggregations": []
                        }
                    },
                    {
                        "uid": "3fa85f75-5798-4565-b3fd-2c963f66afa9",
                        "name": "functionalBlock4",
                        "description": "No description",
                        "type": [
                            "RA_TOP_DOWN",
                            "REACHABILITY"
                        ],
                        "resolvedModuleParts": [
                            {
                                "module": {
                                    "id": 295
                                },
                                "referencedTaxonomies": [
                                    {
                                        "name": "TaxonomyA3"
                                    }
                                ]
                            }
                        ],
                        "peers": {
                            "totalElements": null
                        },
                        "upperBound": {
                            "content": [
                                {
                                    "children": {
                                        "content": [
                                            {
                                                "generatedFrom": {
                                                    "module": {
                                                        "name": "CLOSEFIL",
                                                        "linkHash": "3KucTZE7eNG4uf9s1PoiOq",
                                                        "objectTypeLink": {
                                                            "technologyLink": "JCL",
                                                            "typeLink": "JOB"
                                                        }
                                                    }
                                                }
                                            }
                                        ]
                                    }
                                }
                            ]
                        },
                        "lowerBound": {
                            "aggregations": []
                        }
                    },
                    {
                        "uid": "4f8784b2-cfc3-4fe7-a52a-c3cae4f70b82",
                        "name": "READACCT Reachability",
                        "description": "",
                        "type": [
                            "RA_TOP_DOWN",
                            "REACHABILITY"
                        ],
                        "resolvedModuleParts": [
                            {
                                "module": {
                                    "id": 187
                                },
                                "referencedTaxonomies": null
                            },
                            {
                                "module": {
                                    "id": 151
                                },
                                "referencedTaxonomies": [
                                    {
                                        "name": "Taxonomy1"
                                    }
                                ]
                            },
                            {
                                "module": {
                                    "id": 120
                                },
                                "referencedTaxonomies": null
                            },
                            {
                                "module": {
                                    "id": 90
                                },
                                "referencedTaxonomies": [
                                    {
                                        "name": "Batch"
                                    },
                                    {
                                        "name": "Read"
                                    }
                                ]
                            }
                        ],
                        "peers": {
                            "totalElements": null
                        },
                        "upperBound": {
                            "content": [
                                {
                                    "children": {
                                        "content": [
                                            {
                                                "generatedFrom": {
                                                    "module": {
                                                        "name": "READACCT",
                                                        "linkHash": "6sBrhtWYwUqBsXyM9OamQi",
                                                        "objectTypeLink": {
                                                            "technologyLink": "JCL",
                                                            "typeLink": "JOB"
                                                        }
                                                    }
                                                }
                                            }
                                        ]
                                    }
                                }
                            ]
                        },
                        "lowerBound": {
                            "aggregations": [
                                {
                                    "groupBy": {
                                        "REFERENCED_MODULE_TYPE": "VSAM_FILE",
                                        "REFERENCED_MODULE_TECHNOLOGY": "RESOURCE"
                                    },
                                    "fields": {
                                        "UID": {
                                            "COUNT": 1
                                        }
                                    }
                                },
                                {
                                    "groupBy": {
                                        "REFERENCED_MODULE_TYPE": "LIB",
                                        "REFERENCED_MODULE_TECHNOLOGY": "RESOURCE"
                                    },
                                    "fields": {
                                        "UID": {
                                            "COUNT": 1
                                        }
                                    }
                                }
                            ]
                        }
                    },
                    {
                        "uid": "93cc2df8-b73a-4db2-8909-64ec6fe7990b",
                        "name": "Merged block 2",
                        "description": "",
                        "type": [
                            "MERGE_PARENT",
                            "RA_TOP_DOWN",
                            "REACHABILITY"
                        ],
                        "resolvedModuleParts": null,
                        "peers": {
                            "totalElements": null
                        },
                        "upperBound": {
                            "content": []
                        },
                        "lowerBound": {
                            "aggregations": []
                        }
                    }
                ],
                "totalElements": 5
            }
        }
    };
    let mockWindow: any;
    let openedUrl = '';
    const aggregationResult = model.aggregationResult;

  beforeEach(() => {
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
      providers: [
        NumberFormatter,
        TranslateService,
        I18nService,
        { provide: GraphQlControllerService, useValue: graphQlSpy },
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: FunctionalBlockControllerService, useValue: functionalBlockControllerSpy },
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: LabelMappingService, useValue:  LabelMappingServiceSpy},
        { provide: FbTotalElementsCountGQL, useValue: fbCountGqlSpy },
        { provide: WindowToken, useValue: mockWindow },
      ],
      imports: [
        BrowserAnimationsModule,
        NzMessageModule,
        TranslateModule.forRoot({}),
      ]
    });
    service = TestBed.inject(ReachabilityService);
    moduleControllerServiceSpy.getAggregatedValues2.and.returnValues(of(aggregationResult as any), of([] as any));
    graphQlSpy.graphQl.and.returnValue(of(reachabilityBlocksGql) as any);
    LabelMappingServiceSpy.mapLabel.and.returnValue(of(  "label") as any);
  });

  it('should create', () => {
    expect(service).toBeTruthy();
  });

  it('should create upper bound details when blocks has valid data', () => {
    const blocks = {
      upperBound: {
        content: [
          {
            children: {
              content: [
                {
                  generatedFrom: {
                    module: {
                      name: 'Module1',
                      id: 1,
                      linkHash: 'abc123',
                      technology: 'Technology1',
                      type: 'Type1'
                    }
                  }
                }
              ]
            }
          }
        ]
      },
      resolvedModuleParts: [
        {
          module: {
            id: 1
          }
        },
        {
          module: {
            id: 2
          },
          referencedTaxonomies: [] as any
        }
      ],
      upperTaxonomy:[] as any
     };
    const result = service.createUpperBound(blocks);
    expect(result.upperBound).toEqual([
      {
        "name": "Module1",
        "id": 1,
        "linkHash": "abc123",
        "technology": "Technology1",
        "type": "Type1",
        "taxonomy": [
            {
                "moduleId": 1,
                "referencedTaxonomies": []
            },
            {
                "moduleId": 2,
                "referencedTaxonomies": []
            }
        ],
        "upperTaxonomies": []
    }
    ]);
  })

  it('should create upper bound when blocks is null', () => {
    const blocks: any = null;
    const result = service.createUpperBound(blocks);
    expect(result.upperBound).toEqual([]);
  });

  it('should create upper bound correctly when resolvedModuleParts is null', () => {
    const blocks: any = {resolvedModuleParts: null, upperBound: {
        content: [
          {
            children: {
              content: [
                {
                  generatedFrom: {
                    module: {
                      name: 'Module1',
                      id: 1,
                      linkHash: 'abc123',
                      technology: 'Technology1',
                      type: 'Type1'
                    }
                  }
                }
              ]
            }
          }
        ]
      }};
    const result = service.createUpperBound(blocks);
    expect(result.upperBound).toEqual([{
      "name": "Module1",
      "id": 1,
      "linkHash": "abc123",
      "technology": "Technology1",
      "type": "Type1",
      "taxonomy": [],
      "upperTaxonomies": []
  }]);
  });

  it('should create upper bound correctly when upperBound and resolvedModuleParts is null', () => {
    const blocks: any = {resolvedModuleParts: [], upperBound: null};
    const result = service.createUpperBound(blocks);
    expect(result.upperBound).toEqual([]);
  });

  it('should create lower bound as empty array when blocks is null', () => {
    const blocks: any = null;
    const result = service.createLowerBoundAggregation(blocks);
    expect(result).toEqual([]);
  });

  it('should create lower bound as empty array when lowerBound is null', () => {
    const blocks: any = { lowerBound: null };
    const result = service.createLowerBoundAggregation(blocks);
    expect(result).toEqual([]);
  });

  it('should create lower bound as empty array when lowerBound.aggregation is null', () => {
    const blocks: any = { lowerBound: { aggregations: null } };
    const result = service.createLowerBoundAggregation(blocks);
    expect(result).toEqual([]);
  });

  it('should create lower bound aggregation correctly', () => {
    const blocks: { [key: string]: any } = {
      lowerBound: {
        aggregations: [
          {
            groupBy: {
              REFERENCED_MODULE_TYPE: 'Module1',
              REFERENCED_MODULE_TECHNOLOGY: 'Tech1'
            },
            fields: {
              UID: {
                COUNT: 5
              }
            }
          },
          {
            groupBy: {
              REFERENCED_MODULE_TYPE: 'Module2',
              REFERENCED_MODULE_TECHNOLOGY: 'Tech2'
            },
            fields: {
              UID: {
                COUNT: 3
              }
            }
          },
          {
            groupBy: {
              REFERENCED_MODULE_TYPE: 'Module1',
              REFERENCED_MODULE_TECHNOLOGY: 'Tech1'
            },
            fields: {
              UID: {
                COUNT: 2
              }
            }
          }
        ]
      }
    };
    const result = service.createLowerBoundAggregation(blocks);
    expect(result).toEqual([
      {
        groupBy: {
          REFERENCED_MODULE_TYPE: 'Module1',
          REFERENCED_MODULE_TECHNOLOGY: 'Tech1'
        },
        fields: {
          UID: {
            COUNT: 7
          }
        }
      },
      {
        groupBy: {
          REFERENCED_MODULE_TYPE: 'Module2',
          REFERENCED_MODULE_TECHNOLOGY: 'Tech2'
        },
        fields: {
          UID: {
            COUNT: 3
          }
        }
      }
    ]);
  });

  it('should open modal to merge reachability blocks', () => {
    const modalRef = service.openModalToMergeReachabilityBlocks();
    expect(modalServiceSpy.create).toHaveBeenCalledWith({
      nzTitle: 'reachability.mergeBlockTitle',
      nzClosable: true,
      nzMaskClosable: false,
      nzWrapClassName: 'vertical-center-modal',
      nzContent: MergeBlockComponent
    });
  });

  it('should merge the reachability blocks', () => {
    const data: MergeBlockData = {
      blockName: 'Test Block',
      blockDescription: 'Test Description'
    };
    const projectId = 123;
    const commonParent = 'commonParent';
    const selectedBlockIds = ['block1', 'block2'];
    functionalBlockControllerSpy.mergeFunctionalBlock.and.returnValue(of({} as any));
    service.mergeReachabilityBlocks(data, projectId, commonParent, selectedBlockIds);
    expect(functionalBlockControllerSpy.mergeFunctionalBlock).toHaveBeenCalledWith(projectId, {
      commonParent,
      mergeParentPrototype: {
        name: data.blockName,
        description: data.blockDescription,
        project: projectId.toString(),
        flags: { TYPE: ['REACHABILITY', 'RA_TOP_DOWN'] }
      },
      mergeChildren: selectedBlockIds,
      removeEmptyBlocks: true
    });
  });

  it('should create a ReachabilityBlocks object', () => {
    const blocks: { [key: string]: any } = {
      uid: '123',
      name: 'Test Block',
      description: 'This is a test block',
      type: ['Test Type'],
      isSelected: false,
      status: 'Test Status',
      outdatedModule: false,
      deletedModule: false,
      blockState: { errorCount: 0, warningsCount: 0 },
      upperBound: [],
      lowerBound: [],
      totalCount: 0
    };

    const result = service.createReachabilityBlock(blocks);

    expect(result).toEqual({
      uid: '123',
      name: 'Test Block',
      description: 'This is a test block',
      type: ['Test Type'],
      isSelected: false,
      status: 'Test Status',
      outdatedModule: false || undefined,
      deletedModule: false || undefined,
      blockState: { errorCount: 0, warningsCount: 0 },
      upperBound: [],
      lowerBound: [],
      totalCount: 0
    });
  });

  it('should store reachability details', () => {
    const selectedNode = { name: 'Test Node', uid: '123', info: { OUTDATED: true } } as any;
    const projectId = 1;
    const mergeParent = false;
    const selectedBlocks = [] as any;
    service.storeReachabilityDetails(selectedNode, projectId, mergeParent, selectedBlocks);
    const storedDetails = JSON.parse(localStorage.getItem(`${projectId}-reachabilityDetails`));
    expect(storedDetails.pageTitle).toBe(selectedNode.name);
    expect(storedDetails.blockId).toBe(selectedNode.uid);
    expect(storedDetails.mergeParent).toBe(mergeParent);
    expect(storedDetails.outdated).toBe(selectedNode.info.OUTDATED);
    expect(storedDetails.deleted).toBeFalsy();
  });

  it('should store reachability details for multiple selected blocks', () => {
    const selectedBlocks: Array<{ [key: string]: any }> = [
      { uid: 'block1', info: { OUTDATED: true } },
      { uid: 'block2', info: { Type: ['MERGE_PARENT'] } },
      { uid: 'block3', info: { OUTDATED: false } }
    ];
    const projectId = 123;
    service.storeReachabilityDetails({}, projectId, false, selectedBlocks);
    const storedDetails = JSON.parse(localStorage.getItem(`${projectId}-reachabilityDetails`));
    expect(storedDetails.outdated).toBe(true);
    expect(storedDetails.deleted).toBe(true);
  });

  it('should return an array of merged block ids', () => {
    const projectId = 1;
    const blockId = 'blockId';
    const response: any = {
      data: {
        functionalBlocks: {
          content: [
            {
              reachabilityData: {
                content: [
                  { uid: 'mergedBlock1' },
                  { uid: 'mergedBlock2' }
                ]
              }
            }
          ]
        }
      }
    };
    graphQlSpy.graphQl.and.returnValue(of(response));

    let result: string[] = [];
    service.getMergedBlocksId(projectId, blockId).subscribe(blocks => {
      result = blocks;
    });

    expect(result).toEqual(['mergedBlock1', 'mergedBlock2']);
  });

  it('should return an empty array when the response is null', () => {
    const projectId = 1;
    const blockId = 'blockId';
    const response: any = null;
    graphQlSpy.graphQl.and.returnValue(of(response));

    let result: string[] = [];
    service.getMergedBlocksId(projectId, blockId).subscribe(blocks => {
      result = blocks;
    });

    expect(result).toEqual([]);
  });

  it('should return an empty array when there is an error', () => {
    const projectId = 1;
    const blockId = 'blockId';
    const error = new Error('Internal Server Error');
    graphQlSpy.graphQl.and.returnValue(throwError(error));

    let result: string[] = [];
    service.getMergedBlocksId(projectId, blockId).subscribe(blocks => {
      result = blocks;
    });

    expect(result).toEqual([]);
  });

  it('should return an empty array if reachabilityResult is null', () => {
    const reachabilityResult: any = null;
    graphQlSpy.graphQl.and.returnValue(of(reachabilityResult));

    service.getReachabilityBlocks(1, 10, 1).subscribe((result) => {
      expect(result).toEqual([]);
    });
    expect(graphQlSpy.graphQl).toHaveBeenCalled();
  });

  it('should return an empty array if reachabilityBlocks content is null', () => {
    const reachabilityResult: any = {
      data: {
        reachabilityBlocks: {
          content: null
        }
      }
    };
    graphQlSpy.graphQl.and.returnValue(of(reachabilityResult));

    service.getReachabilityBlocks(1, 10, 1).subscribe((result) => {
      expect(result).toEqual([]);
    });
    expect(graphQlSpy.graphQl).toHaveBeenCalled();
  });

  it('should return an empty array if graphQl throws an error', () => {
    graphQlSpy.graphQl.and.returnValue(throwError('Error'));
    service.getReachabilityBlocks(1, 10, 1).subscribe((result) => {
      expect(result).toEqual([]);
    });
    expect(graphQlSpy.graphQl).toHaveBeenCalled();
  });

  it('should return an empty object if there is an error', () => {
    const projectId = 1;
    const blockId = '123';

    graphQlSpy.graphQl.and.returnValue(throwError('Error'));

    service.getReachabilityBlockDetails(projectId, blockId).subscribe((result) => {
      expect(result).toEqual({});
    });
  });

  it('should open module in new browser tab', () => {
    let tag = "errors";
    service.openModulesTable(tag, 1);
    expect(openedUrl).toContain('modules?preFilter=');
  });

  it('should update block status', () => {
    const projectId = 123;
    const status = 'ACTIVE';
    const ids = ['block1', 'block2'];
    const inactiveSuccessDescription = 'Block deactivated';
    let successMessage = '';
    let errorMessage = '';
    const onSuccess = () => {
      successMessage = 'Block status updated successfully';
    };
    const onError = () => {
      errorMessage = 'Failed to update block status';
    };
    functionalBlockControllerSpy.updateBlockStatus.and.returnValue(of({} as any));
    service.updateBlockStatus(projectId, status, ids, inactiveSuccessDescription, onSuccess, onError);
    expect(functionalBlockControllerSpy.updateBlockStatus).toHaveBeenCalledWith(projectId, status, ids);
    expect(messageServiceSpy.success).toHaveBeenCalledWith('reachability.activeSuccess');
    expect(successMessage).toBe('Block status updated successfully');
  });

  it('should call onError when updateBlockStatus fails', () => {
    const projectId = 123;
    const status = 'ACTIVE';
    const ids = ['block1', 'block2'];
    const inactiveSuccessDescription = 'Block deactivated';
    let errorMessage = '';
    const onError = () => {
      errorMessage = 'Failed to update block status';
    };

    functionalBlockControllerSpy.updateBlockStatus.and.returnValue(throwError(new HttpErrorResponse({ error: 'Error' })));

    service.updateBlockStatus(projectId, status, ids, inactiveSuccessDescription, () => {}, onError);

    expect(functionalBlockControllerSpy.updateBlockStatus).toHaveBeenCalledWith(projectId, status, ids);
    expect(messageServiceSpy.error).toHaveBeenCalledWith('reachability.statusErrorMessage');
    expect(errorMessage).toBe('Failed to update block status');
  });
});
