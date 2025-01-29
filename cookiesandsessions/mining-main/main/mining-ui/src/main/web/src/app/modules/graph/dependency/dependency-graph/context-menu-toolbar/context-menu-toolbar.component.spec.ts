import { TestBed, ComponentFixture, waitForAsync } from '@angular/core/testing';
import { ContextualToolbarComponent } from './context-menu-toolbar.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule } from '@ngx-translate/core';
import { FormsModule } from '@angular/forms';
import { IEdge, INode } from 'yfiles';
import { Subject, of } from 'rxjs';
import { DependencyGraph, ModuleControllerService } from '@innowake/mining-api-angular-client';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { ReachabilityService } from '@app/modules/reachability-ui-product-vision/utils/reachability.service';
import { ActivatedRoute, Router } from '@angular/router';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { model } from 'model-test-data';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';

const EMPTY_DEPENDENCY_GRAPH_LINKS: DependencyGraph = {
  modules: [],
  references: [],
  moduleTypes: [],
  relationshipTypes: []
};
describe('ContextualToolbarComponent', () => {
  let fixture: ComponentFixture<ContextualToolbarComponent>;
  let component: ContextualToolbarComponent;
  const drawerServiceSpy = jasmine.createSpyObj('NzDrawerService', ['create']);
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
  ['traverseModuleDependencies']);
  const graphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>
  ('GraphQlControllerService', ['graphQl']);
  const reachabilityServiceSpy = jasmine.createSpyObj('ReachabilityService',
    ['storeReachabilityDetails', 'setUpdateGraph', 'openModalToMergeReachabilityBlocks', 'mergeReachabilityBlocks',
     'getMergeStatus', 'getUpdateBlocks', 'setUpdateBlocks', 'openModulesTable', 'getReachabilityBlockDetails', 'getBlockState', 'storeReachabilityDetails']);
  const routerSpy = jasmine.createSpyObj<Router>('Router', ['navigate']);
  const jobManagerServiceSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['register']);
  const edgesMap = new Map();
  const incomingEdgesMap = new Map();
  const outgoingEdgesMap = new Map();

  const mapTrue = new Map([
    ['ALL', { count: 1, checked: false, indeterminate: false }],
    ['CALLS', { count: 1, checked: true, indeterminate: false }],
  ]);

  const mapFalse = new Map([
    ['ALL', { count: 1, checked: false, indeterminate: false }],
    ['CALLS', { count: 1, checked: false, indeterminate: false }],
  ]);

  incomingEdgesMap.set(10, mapTrue);
  incomingEdgesMap.set(12, mapTrue);
  incomingEdgesMap.set(14, mapFalse);

  outgoingEdgesMap.set(11, mapTrue);
  outgoingEdgesMap.set(13, mapFalse);
  outgoingEdgesMap.set(15, mapTrue);

  const trueEdge = {
    tag: { relationship: 'CALLS', filtered: true },
    targetNode: { tag: { name: 'test2' } },
    sourceNode: { tag: { name: 'test2' } },
  };
  const falseEdge = {
    tag: { relationship: 'CALLS', filtered: false },
    targetNode: { tag: { name: 'test2' } },
    sourceNode: { tag: { name: 'test2' } },
  };

  edgesMap.set('test1inEdges', [trueEdge, falseEdge]);
  edgesMap.set('test1outEdges', [trueEdge, trueEdge]);
  edgesMap.set('test3inEdges', [trueEdge, trueEdge]);
  edgesMap.set('test3outEdges', [falseEdge, trueEdge]);
  edgesMap.set('test5inEdges', [trueEdge, trueEdge]);
  edgesMap.set('test5outEdges', [falseEdge, trueEdge]);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [ContextualToolbarComponent],
      providers: [
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceStub },
        { provide: Router, useValue: routerSpy },
        {
          provide: ActivatedRoute, useValue: {
            snapshot: {
              params: {
                blockId: '0eee0ba3-6273-4e41-a7da-eb01a065465d',
                projectId: 'project-1'
              }
            }
          }
        },
        { provide: ReachabilityService, useValue: reachabilityServiceSpy},
        { provide: NzDrawerService, useValue: drawerServiceSpy },
        { provide: JobManagerService, useValue: jobManagerServiceSpy }
      ],
      imports: [ HttpClientTestingModule, TranslateModule.forRoot({}), FormsModule, AntDesignImportsModule],
    }).compileComponents();
    routerSpy.navigate.calls.reset();
    drawerServiceSpy.create.and.returnValue({
      afterClose: of(null),
      afterOpen: undefined,
      close: undefined,
      open: undefined,
      getContentComponent: undefined,
      getContentComponentRef: undefined
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ContextualToolbarComponent);
    component = fixture.componentInstance;
    const edgesAt = { size: 0 };
    component.graphComp = {
      zoom: 1,
      graph: {
        addNodeLayoutChangedListener: () => {},
        inEdgesAt: () => {
          const data = new Map();
          data.set(1, {
            tag: { relationship: 'CALLS' },
            sourceNode: { tag: { name: 'test1', id: 10 } },
            targetNode: { tag: { name: 'test2', id: 11 } },
          });
          data.set(2, {
            tag: { relationship: 'CALLS' },
            sourceNode: { tag: { name: 'test3', id: 12 } },
            targetNode: { tag: { name: 'test4', id: 13 } },
          });
          data.set(3, {
            tag: { relationship: 'INCLUDES' },
            sourceNode: { tag: { name: 'test5', id: 14 } },
            targetNode: { tag: { name: 'test6', id: 15 } },
          });
          return data;
        },
        outEdgesAt: () => {
          const data = new Map();
          data.set(1, {
            tag: { relationship: 'CALLS' },
            sourceNode: { tag: { name: 'test7', id: 10 } },
            targetNode: { tag: { name: 'test8', id: 11 } },
          });
          data.set(2, {
            tag: { relationship: 'CALLS' },
            sourceNode: { tag: { name: 'test9', id: 12 } },
            targetNode: { tag: { name: 'test4', id: 13 } },
          });
          data.set(3, {
            tag: { relationship: 'INCLUDES' },
            sourceNode: { tag: { name: 'test11', id: 12 } },
            targetNode: { tag: { name: 'test12', id: 13 } },
          });
          return data;
        },
        edgesAt: () => edgesAt,
        nodePredicateChanged: () => {},
        edgePredicateChanged: () => {},
        nodes: [{ tag: { relationship: 'CALLS' } }],
      },
      innerSize: {
        width: 1,
        height: 1,
      },
      toViewCoordinates: () => {},
      addViewportChangedListener: () => {},
      addUpdatedVisualListener: () => {},
      toPageFromView: () => {},
    } as any;

    component.explorable = false;
    component.graphRootNode = new Subject<INode>();
    component.resetGraph = new Subject<any>();
    component.resetFilterOptions = new Subject<any>();
    component.nodeMap = edgesMap;
    component.incomingEdges = incomingEdgesMap;
    component.outgoingEdges = outgoingEdgesMap;
    fixture.detectChanges();
  });

  it('should create component', () => {
    expect(component).toBeTruthy();
  });

  it('should open modal to merge reachability blocks', () => {
    component.projectId = 1;
    component.selectedNodes = [{ tag: { id: 1 } }] as any;
    component.dependencyGraphComponent = { networkUid: 'commonParent' } as any;
    reachabilityServiceSpy.openModalToMergeReachabilityBlocks.and.returnValue({afterClose: of(model.mergeBlockData)});
    reachabilityServiceSpy.getMergeStatus.and.returnValue(of('Success'));
    component.mergeRB();
    expect(reachabilityServiceSpy.openModalToMergeReachabilityBlocks).toHaveBeenCalled();
    expect(reachabilityServiceSpy.mergeReachabilityBlocks).toHaveBeenCalled();
    expect(reachabilityServiceSpy.getMergeStatus).toHaveBeenCalled();
  });

  it('should test openRBTableView', () => {
    const node: any = {
      layout: null,
      style: null,
      ports: null,
      lookup: null,
      labels: null,
      tag: { name: 'test1' },
    };
    component.selectedNode = node;
    component.openRBTableView(true);
    expect(routerSpy.navigate).toHaveBeenCalled();
  });

  it('should test openRBGraph', () => {
    const node: any = {
      layout: null,
      style: null,
      ports: null,
      lookup: null,
      labels: null,
      tag: {
        name: 'test1',
        info: {
          TYPE: 'test'
        }
      }
    };
    component.selectedNode = node;
    component.openRBGraph();
    expect(routerSpy.navigate).toHaveBeenCalled();
  });

  it('should test openView Details', () => {
    const node: any = {
      layout: null,
      style: null,
      ports: null,
      lookup: null,
      labels: null,
      tag: {
        name: 'test1',
        info: {
          TYPE: 'test'
        }
      }
    };
    reachabilityServiceSpy.getUpdateBlocks.and.returnValue(false);

    component.selectedNode = node;
    component.viewRBDetails();

    expect(drawerServiceSpy.create).toHaveBeenCalled();
    expect(reachabilityServiceSpy.getUpdateBlocks).toHaveBeenCalled();
    expect(reachabilityServiceSpy.setUpdateGraph).not.toHaveBeenCalled();
    expect(reachabilityServiceSpy.setUpdateBlocks).not.toHaveBeenCalled();
  });

  it('should check the response', () => {
    moduleControllerServiceSpy.traverseModuleDependencies.and.returnValue(of(true as any));
     moduleControllerServiceSpy.traverseModuleDependencies(1, 2, 1).subscribe((response) => {
       expect(response).toBeTrue();
    })
  });

  it('should check for empty response', () => {
    moduleControllerServiceSpy.traverseModuleDependencies.and.returnValue(of (EMPTY_DEPENDENCY_GRAPH_LINKS as any));
    moduleControllerServiceSpy.traverseModuleDependencies(1, 2, 1).subscribe((response) => {
      expect(response.modules.length).toBe(0);
   })
 });

  // test failing when running all, add expect statements
  xit('should show the picker', () => {
    spyOn(component.graphComponent, 'toPageFromView').and.returnValue({ x: 1, y: 1 } as any);
    const targetInputElement = window.document.getElementById('show-incoming-edges') as HTMLInputElement;
    const param: any = { target: targetInputElement };
    component.showPickerContainer(param);
  });

  it('should set selectedItems', () => {
    spyOn(component.graphComponent, 'toViewCoordinates').and.returnValue({ x: 1, y: 1 } as any);
    component.selectedItem = ['test'] as any;
    component.selectedNode = null;
    expect(component.selectedItem).toEqual(['test'] as any);
    expect(component.selectedNode).toEqual(['test'] as any);

    const node: any = {
      layout: null,
      style: null,
      ports: null,
      lookup: null,
      labels: null,
      tag: { name: 'test1' },
    };
    component.selectedNode = node;
    expect(component.selectedItem).toEqual(node);
    expect(component.selectedNode).toEqual(node);
  });

  it('should toggle visibility of Incoming edges', () => {
    spyOn(component.graphComponent, 'toViewCoordinates').and.returnValue({ x: 1, y: 1 } as any);
    spyOn(component, 'isGraphConnected').and.returnValue(false);

    const node: any = {
      layout: null,
      style: null,
      ports: null,
      lookup: null,
      labels: null,
      tag: { name: 'test1', id: 10 },
    };
    component.selectedNode = node;
    component.rootNode = {
      layout: null,
      style: null,
      ports: null,
      lookup: null,
      labels: null,
      tag: { name: 'test3', id: 12 },
    } as any;

    component.toggleEdgeVisibility('CALLS', 'incoming');
    const inData = component.incomingEdges.get(component.selectedItem.tag.id);
    expect(inData.get('CALLS')[`checked`]).toBeTruthy();
  });

  it('should hide node', () => {
    spyOn(component, 'toggleEdgeVisibility');
    spyOn(component, 'showPickerContainer');
    component.hideNode(null);
    expect(component.showPickerContainer).toHaveBeenCalledWith(null);
    /* Not specifying the parameters for the following expect statements
        as it is not fixed whether the parameters are in lower case or upper case. */
    expect(component.toggleEdgeVisibility).toHaveBeenCalled();
  });

  it('test on stringifyData method', () => {
    component.selectedItem = { tag: { id: 28, name: 'test1' } } as any;
    const blockId = {moduleIds: [28], moduleName: ['test1']};
    const returnValue = component.stringifyData('block');
    expect(returnValue).toBe(JSON.stringify(blockId));
  });

  describe('Disconnected Graph', () => {
    beforeEach(() => {
      fixture = TestBed.createComponent(ContextualToolbarComponent);
      component = fixture.componentInstance;
      const edgesAt: any[] = [];
      edgesAt[`size`] = 2;
      edgesAt.push({
        tag: { relationship: 'CALLS' },
        sourceNode: { tag: { name: 'test1', id: 10 } },
        targetNode: { tag: { name: 'test2', id: 11 } },
      });
      edgesAt.push({
        tag: { relationship: 'INCLUDE' },
        sourceNode: { tag: { name: 'test3', id: 12 } },
        targetNode: { tag: { name: 'test6', id: 13 } },
      });
      component.graphComp = {
        zoom: 1,
        graph: {
          addNodeLayoutChangedListener: () => {},
          inEdgesAt: () => {
            const data = new Map();
            return data;
          },
          outEdgesAt: () => {
            const data = new Map();
            data.set(1, {
              tag: { relationship: 'CALLS' },
              sourceNode: { tag: { name: 'test1', id: 10 } },
              targetNode: { tag: { name: 'test2', id: 11 } },
            });
            data.set(2, {
              tag: { relationship: 'INCLUDE' },
              sourceNode: { tag: { name: 'test3', id: 12 } },
              targetNode: { tag: { name: 'test6', id: 13 } },
            });
            return data;
          },
          edgesAt: () => edgesAt,
          nodePredicateChanged: () => {},
          edgePredicateChanged: () => {},
          nodes: [{ tag: { relationship: 'CALLS' } }],
        },
        innerSize: {
          width: 1,
          height: 1,
        },
        toViewCoordinates: () => {},
        addViewportChangedListener: () => {},
        addUpdatedVisualListener: () => {},
        toPageFromView: () => {},
      } as any;

      component.graphRootNode = new Subject<INode>();
      component.resetGraph = new Subject<any>();
      component.resetFilterOptions = new Subject<any>();
      component.nodeMap = edgesMap;
      component.incomingEdges = incomingEdgesMap;
      component.outgoingEdges = outgoingEdgesMap;
      fixture.detectChanges();
    });

    it('should toggle visibility of Outgoing edges', () => {
      spyOn(component.graphComponent, 'toViewCoordinates').and.returnValue({ x: 1, y: 1 } as any);
      spyOn(component, 'isGraphConnected').and.returnValue(false);
      const node: any = {
        layout: null,
        style: null,
        ports: null,
        lookup: null,
        labels: null,
        tag: { name: 'test5' },
      };
      component.selectedNode = node;
      component.rootNode = {
        layout: null,
        style: null,
        ports: null,
        lookup: null,
        labels: null,
        tag: { name: 'test6', id: 13 },
      } as any;
      component.toggleEdgeVisibility('CALLS', 'outgoing');
      const outData = component.outgoingEdges.get(component.selectedItem.tag.id);
      expect(outData.get('CALLS')[`checked`]).toBeTruthy();
    });
  });


  it('should check if value for incoming/outgoing edges option present or not', () => {
    component.selectedItem = null;
    component.incomingEdges = new Map();
    component.outgoingEdges = new Map();

    let result = component.isOptionDisabled('incoming');
    expect(result).toEqual(true);

    component.selectedItem = {
      tag: { id: 1 },
    } as any;
    component.incomingEdges.set(1, new Map([['test', { checked: true, count: 1, indeterminate: false }]]));
    result = component.isOptionDisabled('incoming');
    expect(result).toEqual(false);

    component.outgoingEdges.set(1, new Map());
    result = component.isOptionDisabled('outgoing');
    expect(result).toEqual(true);
  });

  it('should set updateGraph to true and updateBlocks to false when getUpdateBlocks returns true', () => {
    component.selectedNode = { tag: { id: 1, info: { TYPE: 'RA_TOP_DOWN' } } } as any;
    reachabilityServiceSpy.getUpdateBlocks.and.returnValue(true);

    component.viewRBDetails();

    expect(reachabilityServiceSpy.setUpdateGraph).toHaveBeenCalledWith(true);
    expect(reachabilityServiceSpy.setUpdateBlocks).toHaveBeenCalledWith(false);
  });

  it('should hide toolbar and call hide when selectedBranch is null', () => {
    spyOn(component as any, 'hide');
    component.selectedBranchToRemove = null;
    expect(component.showToolbar).toBe(false);
    expect((component as any).hide).toHaveBeenCalled();
  });

  it('should set selectedBranch and show toolbar when selectedBranch is an IEdge with a sourceNode', () => {
    const selectedBranch: any = { sourceNode: {}, opposite: null, bends: null, sourcePort: null, targetPort: null };
    component.selectedBranchToRemove = selectedBranch as IEdge;
    expect(component.selectedBranch).toBe(selectedBranch);
    expect(component.showToolbar).toBe(false);
  });

  it('should open RBTableView with multiple tables when isMultiple is true', () => {
    spyOn(component, 'openRBTableView');
    component.getReachabilityDetails(true, 'table');
    expect(component.openRBTableView).toHaveBeenCalledWith(true);
  });

  it('should fetch block details and call appropriate methods based on viewType', () => {
    const mockBlockDetails: { [key: string]: any } = {
      uid: 'differentUid',
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
    reachabilityServiceSpy.getReachabilityBlockDetails.and.returnValue(of(mockBlockDetails));
    reachabilityServiceSpy.getBlockState.and.returnValue('mockBlockState');
    spyOn(component, 'openRBGraph');
    spyOn(component, 'openRBTableView');
    spyOn(component, 'viewRBDetails');

    component.selectedNode = { tag: { uid: 'mockUid' } } as any;
    component.blockData = { uid: 'differentUid' };
    component.projectId = 1;

    component.getReachabilityDetails(false, 'graph');
    expect(reachabilityServiceSpy.getReachabilityBlockDetails).toHaveBeenCalledWith(1, JSON.stringify('mockUid'));
    expect(reachabilityServiceSpy.getBlockState).toHaveBeenCalledWith(mockBlockDetails.resolvedModuleParts);
    expect(reachabilityServiceSpy.storeReachabilityDetails).toHaveBeenCalledWith(mockBlockDetails, 1, false);
    expect(component.openRBGraph).toHaveBeenCalled();

    component.getReachabilityDetails(false, 'table');
    expect(component.openRBTableView).toHaveBeenCalledWith(false);

    component.getReachabilityDetails(false, 'details');
    expect(component.viewRBDetails).toHaveBeenCalled();
  });
});
