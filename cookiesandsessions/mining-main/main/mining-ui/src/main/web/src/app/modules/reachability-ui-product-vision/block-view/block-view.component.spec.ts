import { ComponentFixture, TestBed } from '@angular/core/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BlockViewComponent } from './block-view.component';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { Router } from '@angular/router';
import { BehaviorSubject, of, throwError } from 'rxjs';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzMessageService } from 'ng-zorro-antd/message';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { FunctionalBlockControllerService, ModuleControllerService } from '@innowake/mining-api-angular-client';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { JobControllerService, JobInformation } from '@innowake/mining-api-angular-client';
import { ModulesGQL } from '@app/graphql/generated/generated';
import { ReachabilityService } from '../utils/reachability.service';
import { TaxonomyFilterComponent } from '@app/shared/components/taxonomy-filter/taxonomy-filter.component';
import { NzButtonModule } from 'ng-zorro-antd/button';
import { NzLayoutModule } from 'ng-zorro-antd/layout';
import { NzGridModule } from 'ng-zorro-antd/grid';
import { NzListModule } from 'ng-zorro-antd/list';
import { NzPageHeaderModule } from 'ng-zorro-antd/page-header';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { BlockViewFilter, ReachabilityBlocks, ReachabilityFilter } from '../utils/reachability-interface';
import { model } from 'model-test-data';

describe('BlockViewComponent', () => {
  let component: BlockViewComponent;
  let fixture: ComponentFixture<BlockViewComponent>;

  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj<ClientProjectRelationshipService>
    ('ClientProjectRelationshipService', ['getClientProjectObservable']);
  const graphQlSpy = jasmine.createSpyObj<GraphQlControllerService>('GraphQlControllerService', ['graphQl']);
  const routerSpy = jasmine.createSpyObj<Router>('Router', ['navigate', 'parseUrl']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['mapLabel']);
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService', ['getAggregatedValues', 'getAggregatedValues2']);

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
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(0, 'TestClient', 0, 'TestProject');
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'warning']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['success', 'error']);
  const functionalBlockControllerSpy = jasmine.createSpyObj<FunctionalBlockControllerService>('FunctionalBlockControllerService',
    ['recalculateOutDatedFunctionalBlocks', 'removeFunctionalBlocksWithoutUpperBoundModule', 'updateBlockStatus']);
  const featureToggleServiceSpy = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);
  const jobManagerServiceSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['register']);
  const jobControllerServiceSpy = jasmine.createSpyObj<JobControllerService>('JobControllerService', ['submitJobExtensionV2']);
  const modulesGQLSpy = jasmine.createSpyObj<ModulesGQL>('ModulesGQL', ['fetch']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['close', 'triggerOk']);
  const aggregationResult = [{
    "group": {
        "TECHNOLOGY": "ASSEMBLER",
        "TYPE": "UNKNOWN"
    },
    "fields": {
        "ID": 77
    }
}, {
    "group": {
        "TECHNOLOGY": "BASIC",
        "TYPE": "OBJECT"
    },
    "fields": {
        "ID": 17
    }
}, {
    "group": {
        "TECHNOLOGY": "BASIC",
        "TYPE": "PROGRAM"
    },
    "fields": {
        "ID": 2
    }
}, {
    "group": {
        "TECHNOLOGY": "BASIC",
        "TYPE": "SUBROUTINE"
    },
    "fields": {
        "ID": 2
    }
}, {
    "group": {
        "TECHNOLOGY": "C",
        "TYPE": "FUNCTION"
    },
    "fields": {
        "ID": 44
    }
  }];

  const reachabilityBlocks: ReachabilityBlocks[] = [{
    name: "READACCT Reachability",
    description: "",
    uid: "dhwdhwihiwjdowjd",
    isSelected: false,
    upperBound: [{
      name: "READACCT",
      id: 2002,
      linkHash: "cnskckscjsjc",
      technology: "JCL",
      type: "JOB"
    }]
  }];

  const reachabilityServiceSpy = jasmine.createSpyObj('ReachabilityService',
    ['getReachabilityBlocks', 'getReachabilityBlocksCount', 'getReachabilityBlocksId', 'setSwitchState', 'getSwitchState', 'checkToEnableMerge', 'getUpdateOutdatedState', 'buildReachabilityFilter', 'getUpdateGraph',
    'openModalToMergeReachabilityBlocks', 'mergeReachabilityBlocks', 'getMergeStatus', 'updateBlockStatus']);

  const childComponent: jasmine.SpyObj<TaxonomyFilterComponent> = jasmine.createSpyObj('TaxonomyFilterComponent', ['selectedValue'])
  const filter = '{"taxonomies":[],"functionalBlocks":[],"modulesIds":[],"type":[],"technology":[],"referenceType":[],"inActiveSwitch":false,"dependency":"resource"}';

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [BlockViewComponent],
      providers: [
        NumberFormatter,
        TranslateService,
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlSpy },
        { provide: Router, useValue: routerSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: FunctionalBlockControllerService, useValue: functionalBlockControllerSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService()},
        { provide: FeatureToggleService, useValue: featureToggleServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: ModulesGQL, useValue: modulesGQLSpy },
        { provide: ReachabilityService, useValue: reachabilityServiceSpy}
      ],
      imports: [
        HttpClientTestingModule,
        BrowserAnimationsModule,
        NzButtonModule,
        NzGridModule,
        NzListModule,
        NzLayoutModule,
        NzPageHeaderModule,
        TranslateModule.forRoot({}),
      ],
    }).compileComponents();
    graphQlSpy.graphQl.and.returnValue(of(reachabilityBlocksGql as any));
    moduleControllerServiceSpy.getAggregatedValues2.and.returnValues(of(aggregationResult as any), of([] as any));
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
    featureToggleServiceSpy.isActive.and.returnValue(of(false));
    jobControllerServiceSpy.submitJobExtensionV2.and.returnValue(of('job-id' as any));
    jobManagerServiceSpy.register.and.returnValue({ jobId: 'job-id', label: 'abc', foreground: true, cancellable: true, autoDownloadResult: false, status$: new BehaviorSubject(JobInformation.StatusEnum.SUCCESS)});
    reachabilityServiceSpy.checkToEnableMerge.and.returnValue(of('parent-uid'));
    reachabilityServiceSpy.getReachabilityBlocks.and.returnValue(of(reachabilityBlocks as any));
    reachabilityServiceSpy.getReachabilityBlocksCount.and.returnValue(of(reachabilityBlocks.length));
    reachabilityServiceSpy.getReachabilityBlocksId.and.returnValue(of([]));
    reachabilityServiceSpy.getSwitchState.and.returnValue(true);
    reachabilityServiceSpy.getUpdateOutdatedState.and.returnValue(of(false));
	reachabilityServiceSpy.getUpdateGraph.and.returnValue(of(true));
    functionalBlockControllerSpy.recalculateOutDatedFunctionalBlocks.and.returnValue(of([] as any));
    functionalBlockControllerSpy.removeFunctionalBlocksWithoutUpperBoundModule.and.returnValue(of([] as any));
    functionalBlockControllerSpy.updateBlockStatus.and.returnValue(of([] as any));
    routerSpy.parseUrl.and.returnValue({queryParams: {filter}} as any);
    routerSpy.navigate.and.returnValue(true as any);
    reachabilityServiceSpy.openModalToMergeReachabilityBlocks.and.returnValue({afterClose: of(model.mergeBlockData)});
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockViewComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.projectId = 1;
    component.allReachabilityIds = ['1', '2', '3'];
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should test when all blocks are unchecked', () => {
    component.allBlocksChecked = false;
    component.updateAllBlocksChecked();
    expect(component.allReachabilityIds.length).toEqual(0);
  });

  it('should not show generate-button when feature-toggle is inactive', () => {
    expect(fixture.nativeElement.querySelector('#generate-button')).toBeFalsy();
  });

  xit('should show generate-button when feature-toggle is active', () => {
    featureToggleServiceSpy.isActive.and.returnValue(of(false));
    component.ngOnInit();
    fixture.detectChanges();
    expect(fixture.nativeElement.querySelector('#generate-button')).toBeTruthy();
  });

  it('should submit job on modal confirmation', () => {
    component.handleConfirmModalOk();
    expect(jobControllerServiceSpy.submitJobExtensionV2).toHaveBeenCalled();
    expect(jobManagerServiceSpy.register).toHaveBeenCalled();
  });

  it('should test apply filter', () => {
    spyOn((component as any), 'makeFilterActive').and.callThrough();
    component.selectedTaxonomyIds = [21, 22];
    component.selectedFunctionalBlock = ['fb1'];
    component.selectedModuleIds = ['READCARD'];
    component.onApplyFilter();
    expect(component['makeFilterActive']).toHaveBeenCalled();
    expect(component.isFiltersActive).toBeTruthy();
  });

  it('should test construct filter details', () => {
    component.showInactiveBlocks = false;
    component.selectedTaxonomyIds = [21, 22];
    component.selectedFunctionalBlock = ['fb1'];
    component.selectedModuleIds = ['READCARD'];
    component['constructFilterDetails']();
    const filter: BlockViewFilter = {
      reachabilityFilter: {
        taxonomies: [21, 22],
        functionalBlocks: ['fb1'],
        modulesIds: ['READCARD'],
        type: [],
        technology: [],
        referenceType: [],
        excludeParentTypes: ['MERGE_PARENT']
      } as ReachabilityFilter,
      inActiveSwitch: false
    };
    expect(component.blockViewFilter).toEqual(filter);
  });

  it('should test reset filter', () => {
    component.taxonomyFilterComponent = childComponent;
    component.onResetFilter();
    expect(component.isFiltersActive).toBeFalsy();
  });

  it('should test reachability name search', () => {
    component.showInactiveBlocks = false;
    spyOn((component as any), 'getReachabilityBlocks').and.callThrough();
    component.onReachabilityNameSearch();
    expect(component['getReachabilityBlocks']).toHaveBeenCalled();
  });

  it('should test getAllblockIds', () => {
    spyOn((component as any), 'getAllBlockIds').and.callThrough();
    component.allBlocksChecked = true;
    component.updateAllBlocksChecked();
    expect(component['getAllBlockIds']).toHaveBeenCalled();
    expect(component.allReachabilityIds).toBeDefined();
  });

  it('should retain selected blocks on name search', () => {
    component.blockNameSearch = 'READCARD';
    spyOn((component as any), 'getReachabilityBlocks').and.callThrough();
    spyOn((component as any), 'maintainBlockSelectionOnPaginateAndFilter').and.callThrough();
    component.onReachabilityNameSearch();
    expect(component.pageIndex).toEqual(0);
    expect(component['getReachabilityBlocks']).toHaveBeenCalled();
    expect(component['maintainBlockSelectionOnPaginateAndFilter']).toHaveBeenCalled();
    expect(component.allReachabilityIds).toEqual(['1', '2', '3']);
  });

  it('should Open Modal on clicking updateBlockStatus', () => {
    const status = 'ACTIVE';
    const warningSpy = modalServiceSpy.warning.and.callThrough();
    modalServiceSpy.warning.and.returnValue(nzModalRefSpy);

    component.bulkUpdateFunctionalBlocksStatus(status);

    expect(warningSpy).toHaveBeenCalledWith({
      nzTitle: jasmine.any(String),
      nzOkText: jasmine.any(String),
      nzIconType: 'warning',
      nzOnOk: jasmine.any(Function),
      nzCancelText: jasmine.any(String)
    });
  });

  it('should reset the pageIndex when filterDetails has data', () => {
    component.pageIndex = 2;
    component.selectedTaxonomyIds = [21, 22];
    component.selectedFunctionalBlock = ['fb1'];
    component.selectTypeTechnology = ['COBOL EXEC'];
    spyOn((component as any), 'getReachabilityBlocks').and.callThrough();
    component.onApplyFilter();
    expect(component.pageIndex).toEqual(0);
    expect(component.switchStateChange).toBeFalsy();
    expect(component['getReachabilityBlocks']).toHaveBeenCalled();
  });

  it('should set filters from URL', () => {
    routerSpy.parseUrl.and.returnValue({queryParams: {
      filter: JSON.stringify({
        modulesIds: ['module1', 'module2'],
        functionalBlocks: ['block1', 'block2'],
        type: ['type1', 'type2'],
        technology: ['technology1', 'technology2'],
        referenceType: ['read', 'write'],
        dependency: 'resource',
        nameSearch: 'search',
        taxonomies: [1, 2],
        inActiveSwitch: true
      })
    }} as any);
    (component as any).setFiltersFromUrl();
    expect(component.selectedModuleIds).toEqual(['module1', 'module2']);
    expect(component.selectedFunctionalBlock).toEqual(['block1', 'block2']);
    expect(component.selectTypeTechnology).toEqual(['technology1,type1', 'technology2,type2']);
    expect(component.selectedReferenceType).toEqual(['read', 'write']);
    expect(component.dependencyFilterValue).toEqual('resource');
    expect(component.blockNameSearch).toEqual('search');
    expect(component.selectedTaxonomyIds).toEqual([1, 2]);
    expect(component.showInactiveBlocks).toEqual(true);
  });

  it('should open modal to merge reachability blocks', () => {
    reachabilityServiceSpy.getMergeStatus.and.returnValue(of('Success'));
    component.commonParent = 'parent-uid';
    component.openModalToMerge();
    expect(reachabilityServiceSpy.openModalToMergeReachabilityBlocks).toHaveBeenCalled();
    expect(reachabilityServiceSpy.mergeReachabilityBlocks).toHaveBeenCalled();
    expect(reachabilityServiceSpy.getMergeStatus).toHaveBeenCalled();
    expect(component.pageIndex).toEqual(0);
    expect(component.allBlocksChecked).toBeFalsy();
  });

  it('should set isFiltersActive to true in "graph" mode when relevant filter values are present', () => {
    // only link type is both
    component.dependencyFilterValue = 'both';
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('graph');
    expect(component.isFiltersActive).toBe(true);

    // only functional blocks is selected
    component.dependencyFilterValue = 'resource';
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: ['fb1'],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('graph');
    expect(component.isFiltersActive).toBe(true);

    // only modules selected
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: ['1', '2'],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('graph');
    expect(component.isFiltersActive).toBe(true);

    // only taxonomies is selected
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: ['1', '2'],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('graph');
    expect(component.isFiltersActive).toBe(true);

    // only tech and type is selected
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: ['COBOL'],
          type: ['PROGRAM'],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('graph');
    expect(component.isFiltersActive).toBe(true);

    // only inactive switch is on
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: true,
      };
    component.checkFilterValue('graph');
    expect(component.isFiltersActive).toBe(true);
  });

  it('should set isFiltersActive to false, in "graph" mode when relevant filter values are not present', () => {
    component.dependencyFilterValue = 'resource';

    // empty filter
    component.switchBlockGraph = 'graph';
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('graph');

    // referenceType is ignored for graphs
    expect(component.isFiltersActive).toBe(false);
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: ['READ'],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('graph');
    expect(component.isFiltersActive).toBe(false);
  });

  it('should set isFiltersActive to true in "block" mode when relevant filter values are present', () => {
    component.dependencyFilterValue = 'resource';

    // all values selected
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: ['block1', 'block2'],
          modulesIds: ['module1', 'module2'],
          taxonomies: ['taxonomy1', 'taxonomy2'],
          technology: ['tech1', 'tech2'],
          type: ['type1', 'type2'],
          referenceType: ['read', 'write'],
        },
      inActiveSwitch: true,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(true);

    // only reference type is selected
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: ['READ'],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(true);

    // only functional blocks is selected
    component.dependencyFilterValue = 'resource';
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: ['fb1'],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(true);

    // only modules selected
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: ['1', '2'],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(true);

    // only taxonomies is selected
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: ['1', '2'],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(true);

    // only tech and type is selected
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: ['COBOL'],
          type: ['PROGRAM'],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(true);

    // only inactive switch is on
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: true,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(true);
  });

  it('should set isFiltersActive to false in "block" mode when relevant filter values are not present', () => {
    component.dependencyFilterValue = 'resource';

    // empty filter
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(false);

    // linktype is ignored for graphs
    component.dependencyFilterValue = 'both';
    component.blockViewFilter = {
      reachabilityFilter: {
          functionalBlocks: [],
          modulesIds: [],
          taxonomies: [],
          technology: [],
          type: [],
          referenceType: [],
        },
      inActiveSwitch: false,
      };
    component.checkFilterValue('block');
    expect(component.isFiltersActive).toBe(false);
  });
});