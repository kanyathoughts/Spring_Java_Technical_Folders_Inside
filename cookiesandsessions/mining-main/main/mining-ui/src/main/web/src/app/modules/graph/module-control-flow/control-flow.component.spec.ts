import { ControlFlowComponent } from './control-flow.component';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import {
  Injector,
  ApplicationRef
} from '@angular/core';
import { ICommand } from 'yfiles';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import {
  of,
  Subject,
  throwError
} from 'rxjs';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import {
  ControlFlowNodeDetails,
  CfgNodeType,
  CfgNodeSuperType
} from './models/control-flow-node-details';
import { ControlFlowGraphInfo } from './models/control-flow-graph-info';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { WindowToken } from '@app/core/utils/window';
import { SharedModule } from '@app/shared';
import { ControlFlowGraphComponent } from './control-flow-graph/control-flow-graph.component';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { Router, Navigation, ActivatedRoute, UrlTree } from '@angular/router';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { GraphOverviewPanelComponent } from '../yfiles-graph-overview-panel/graph-overview-panel.component';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { ControlFlowUtility } from './utils/control-flow-utility';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { ErrorMarker } from '@innowake/mining-api-angular-client/model/errorMarker';
import { AnnotationControllerService, ControlFlowControllerService, ControlFlowEdge, ControlFlowGraph, ControlFlowNode, FeatureControllerService, JobControllerService, JobInformation, ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';
import { CfgSupportedTypeService } from '@app/core/services/cfg-supported-type.service';
import { EclipseService } from '@app/core/authentication/eclipse.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { RemoteJob } from '@app/core/services/job-manager/job-manager-service.interface';
import { CFG_SUPPORTED_TYPES } from '@app/core/services/cfg-supported-types.service.spec';

describe('ControlFlowComponent', () => {
  let component: ControlFlowComponent;
  let fixture: ComponentFixture<ControlFlowComponent>;
  let router: Router;
  const mockWindow: any = {};

  const validResponse: ControlFlowGraph = {
    nodes: [
      { id: '#177:1', type: 'ENTRY', superTypes: null, entity: ControlFlowNode.EntityEnum.TERMINAL },
      {
        id: '#143:32', type: 'CobolMoveStmt', superTypes: new Set<string>(['statement']), entity: ControlFlowNode.EntityEnum.AST_NODE,
        label: 'test_description that is actually more than seventy two characters to check ellipsis'
      },
      { id: '#142:33', type: 'CobolMoveStmt', superTypes: new Set<string>(['statement']), entity: ControlFlowNode.EntityEnum.AST_NODE }
    ],
    edges: [
      { fromId: '#177:1', toId: '#143:32', label: 'Move data' },
      { fromId: '#143:32', toId: '#142:33' }
    ]
  };

  const errorResponse: ErrorMarker[] = [
    {
      "severity": "ERROR",
      "key": "UNDISCOVERED_DEPENDENCY",
      "cause": "Unable to resolve file MACEXT to actual data set."
    },
    {
      "severity": "WARNING",
      "key": "UNDISCOVERED_DEPENDENCY",
      "cause": "Unable to resolve file CURMTHND to actual data set."
    }
  ];

  const controlFlowGraphWithGroups: ControlFlowGraph = {
    nodes: [
      { id: '#177:1', type: 'ENTRY', superTypes: null, entity: ControlFlowNode.EntityEnum.TERMINAL },
      { id: '#1107:311', type: 'CobolIfStmt', superTypes: null, entity: ControlFlowNode.EntityEnum.AST_NODE },
      { id: '#1105:311', type: 'CobolElseBlock', superTypes: new Set<string>(["DefaultBranch", "Branch", "CfgCollapsibleNode"]), entity: ControlFlowNode.EntityEnum.AST_NODE, parent: '#1107:311' },
      { id: '#1109:311', type: 'CobolDisplayStmt', superTypes: new Set<string>(["Statement"]), entity: ControlFlowNode.EntityEnum.AST_NODE, parent: '#1105:311' },
      { id: '#1109:312', type: 'CobolLabelStmt', superTypes: new Set<string>(["Invocable", "Statement", "CfgCollapsibleNode"]), entity: ControlFlowNode.EntityEnum.AST_NODE },
      { id: '#1109:313', type: 'CobolDisplayStmt', superTypes: new Set<string>(["Statement"]), entity: ControlFlowNode.EntityEnum.AST_NODE, parent: '#1109:312' },
      { id: '#1109:314', type: 'CobolIfStmt', superTypes: new Set<string>(["Statement", "BranchStatement"]), entity: ControlFlowNode.EntityEnum.AST_NODE, parent: '#1109:312' },
      { id: '#1109:315', type: 'CobolThenBlock', superTypes: new Set<string>(["Branch", "CfgCollapsibleNode"]), entity: ControlFlowNode.EntityEnum.AST_NODE, parent: '#1109:314' },
      { id: '#1109:316', type: 'CobolDisplayStmt', superTypes: new Set<string>(["Statement"]), entity: ControlFlowNode.EntityEnum.AST_NODE, parent: '#1109:315' }
    ],
    edges: [
      { fromId: '#177:1', toId: '#1107:311' },
      { fromId: '#1107:311', toId: '#1105:311', label: 'FALSE' },
      { fromId: '#1105:311', toId: '#1109:311' },
      { fromId: '#1109:311', toId: '#1109:312' },
      { fromId: '#1109:312', toId: '#1109:313' },
      { fromId: '#1109:313', toId: '#1109:314' },
      { fromId: '#1109:314', toId: '#1109:315' },
      { fromId: '#1109:315', toId: '#1109:316' }
    ]
  }

  const ifNode: ControlFlowNode = {
    id: '#281:1',
    type: 'CobolIfStmt',
    superTypes: new Set<string>([CfgNodeSuperType.BRANCH_STATEMENT]),
    entity: ControlFlowNode.EntityEnum.AST_NODE,
    parent: '#282:5',
    properties: {},
    offset: 300
  };
  const thenNode: ControlFlowNode = {
    id: '#285:3',
    type: 'CobolThenBlock',
    superTypes: null,
    entity: ControlFlowNode.EntityEnum.AST_NODE, 
    parent: '#281:1',
    properties: {},
    offset: 300
  };
  const otherNode: ControlFlowNode = {
    id: '#286:0',
    type: 'CobolPerformStmt',
    superTypes: null,
    entity: ControlFlowNode.EntityEnum.AST_NODE,
    parent: '#282:5',
    properties: {},
    offset: 300
  };
  const prevNavigation: Navigation = {
    id: 2,
    initialUrl: new UrlTree(),
    extractedUrl: null,
    finalUrl: null,
    trigger: 'imperative',
    extras: null,
    previousNavigation: null
  };
  const navigation: Navigation = {
    id: 1,
    initialUrl: new UrlTree(),
    extractedUrl: null,
    finalUrl: null,
    trigger: 'imperative',
    extras: null,
    previousNavigation: prevNavigation
  };
  const controlFlowControllerServiceSpy: jasmine.SpyObj<ControlFlowControllerService> = jasmine.createSpyObj<ControlFlowControllerService>
    ('ControlFlowControllerService', ['getControlFlow', 'calculateControlFlowForModule', 'getSupportedModuleTypes']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose']);
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
    ['updateModule', 'getAggregatedValues', 'getDataLineageAvailableForModule', 'findErrorMarkers']);
  const jobControllerServiceSpy = jasmine.createSpyObj<JobControllerService>('JobControllerService', ['getJobInformation']);
  const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);
  const cfgSupportedTypeServiceSpy = jasmine.createSpyObj<CfgSupportedTypeService>('CfgSupportedTypeService', ['init', 'checkIfSupported']);
  const eclipseServiceSpy = jasmine.createSpyObj<EclipseService>('EclipseService', ['getAccessToken']);
  const moduleVal: ModulePojo = {
    name: 'Test_Module_Name',
    projectId: 1,
    id: 1
  };

  const remoteJob: RemoteJob = {
    jobId: 'test-job',
    label:'test',
    foreground: false,
    cancellable: false,
    autoDownloadResult: false,
    status$: of(JobInformation.StatusEnum.SCHEDULED) as any
  }

  const languageServiceSpy: jasmine.SpyObj<any> = jasmine.createSpyObj('LanguageProviderService', [
    'injectCSS', 'init'
  ]);
  const jobManagerSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['register'])

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        ControlFlowComponent,
        ControlFlowGraphComponent,
        GraphOverviewPanelComponent
      ],
      providers: [
        Injector,
        ApplicationRef,
        NzMessageService,
        OauthtokenService,
        TranslateService,
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        FeatureControllerService,
        AnnotationControllerService,
        { provide: LanguageProviderService, useValue: languageServiceSpy },
        { provide: FeatureToggleService, usevalue: featureToggleServiceSpy },
        { provide: JobControllerService, useValue: jobControllerServiceSpy },
        { provide: ControlFlowControllerService, useValue: controlFlowControllerServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: NzModalRef, useValue: { afterClose: () => true } },
        { provide: EclipseService, useValue: eclipseServiceSpy },
        {
          provide: FeatureToggleService,
          useValue: featureToggleServiceSpy
        },
        {
          provide: CfgSupportedTypeService,
          useValue: cfgSupportedTypeServiceSpy
        },
        { provide: JobManagerService, useValue: jobManagerSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              module: moduleVal
            }),
            snapshot: {
              queryParams: of({
                annotationId: '#661:0'
              })
            }
          }
        },

      ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        SharedModule,
        BrowserAnimationsModule,
        TranslateModule.forRoot({}),
      ],
    }).compileComponents();
    TestBed.compileComponents();
    router = TestBed.inject(Router);
    spyOn<any>(router, 'lastSuccessfulNavigation').and.returnValue(navigation);
    controlFlowControllerServiceSpy.getControlFlow.and.returnValue(of(validResponse as any));
    controlFlowControllerServiceSpy.calculateControlFlowForModule.and.returnValue(of(['1'] as any));
    controlFlowControllerServiceSpy.getSupportedModuleTypes.and.returnValue(of[CFG_SUPPORTED_TYPES as any]);
    cfgSupportedTypeServiceSpy.checkIfSupported.and.returnValue(true);
    jobManagerSpy.register.and.returnValue(remoteJob);
    featureToggleServiceSpy.isActive.and.returnValue(of(false));
    moduleControllerServiceSpy.findErrorMarkers.and.returnValue(of(errorResponse as any));
    languageServiceSpy.injectCSS.and.returnValue(null);
    languageServiceSpy.init.and.returnValue(of(true));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ControlFlowComponent);
    component = fixture.componentInstance;
    nzModalRefSpy.afterClose = new Subject;
    nzModalRefSpy.afterClose.next(null);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    fixture.detectChanges();
  });

  it('should call java__callback__clicked', () => {
    component.moduleId = 1;
    // tslint:disable-next-line: variable-name
    const java__callback__clicked_spy = jasmine.createSpyObj('javacallback', ['spyMethod']);
    // tslint:disable-next-line: no-string-literal
    window['java__callback__clicked'] = (moduleId: number, offset: number) => {
      java__callback__clicked_spy.spyMethod(moduleId, offset);
    };
    component.eclipseView = true;
    (component as any).cfgComponent.eclipseView = true;
    const clickItem = {
      item: {
        tag: new ControlFlowNodeDetails(ifNode, null, null)
      }
    };
    (component as any).cfgComponent.getNodeSingleClickListener()(null, (clickItem as any));
    expect(java__callback__clicked_spy.spyMethod).toHaveBeenCalledWith(1, 300);
    component.eclipseView = false;
    (component as any).cfgComponent.eclipseView = false;
    window['java__callback__clicked'] = null;
  });

  it('should create component', () => {
    expect(component).toBeTruthy();
    expect(component.loadState).toBe(LoaderState.success);

    
    controlFlowControllerServiceSpy.getControlFlow.and.returnValue(of(null));
    component.loadState = LoaderState.loading;
    component.ngOnInit();
    expect(component.loadState).toBe(LoaderState.error);

    controlFlowControllerServiceSpy.getControlFlow.and.returnValue(throwError(new Error('Test Error')));
    component.loadState = LoaderState.loading;
    component.ngOnInit();
    expect(component.loadState).toBe(LoaderState.error);
    expect(component.errorMessage).toBe('unableToRenderGraph');

    component.module.sourceCodeAvailable = true;
    spyOn(component, 'calculateCFG').and.callThrough();
    component.ngOnInit();
    expect(component.calculateCFG).toHaveBeenCalled();
  });

  it('should call zoom methods', () => {
    spyOn(ICommand.INCREASE_ZOOM, 'execute');
    spyOn(ICommand.ZOOM, 'execute');
    spyOn(ICommand.DECREASE_ZOOM, 'execute');
    spyOn(ICommand.FIT_GRAPH_BOUNDS, 'execute');

    component.fitContent();
    expect(ICommand.FIT_GRAPH_BOUNDS.execute).toHaveBeenCalled();

    component.zoomIn();
    expect(ICommand.INCREASE_ZOOM.execute).toHaveBeenCalled();

    component.zoomOriginal();
    expect(ICommand.ZOOM.execute).toHaveBeenCalled();

    component.zoomOut();
    expect(ICommand.DECREASE_ZOOM.execute).toHaveBeenCalled();
  });

  it('should have proper labels for edges', () => {
    const graphInfo = component.graphInfo;
    let moveLabelExists = false;
    let labelCount = 0;
    graphInfo.graphEdges.forEach(edge => {
      if (edge.label) {
        labelCount++;
      }
      if (edge.label === 'Move data') {
        moveLabelExists = true;
      }
    });
    expect(labelCount).toBe(1);
    expect(moveLabelExists).toBeTruthy();
  });

  it('should set and get values for CfgNode', () => {
    const cfgNode = new ControlFlowNodeDetails(validResponse.nodes[1], null, null);
    expect(cfgNode.nodeRecordId).toBe('#143:32');
    //expect(cfgNode.nodeCustomProperties).toBeDefined();
    expect(cfgNode.nodeType).toBe(CfgNodeType.MOVE_STMT);
    expect(cfgNode.entity).toBe(ControlFlowNode.EntityEnum.AST_NODE);
    expect(cfgNode.isNodeBranch).toBeFalsy();
    expect(cfgNode.isNodeEntryExit).toBeFalsy();
    expect(cfgNode.nodeDescription).toBe('test_description that is actually more than'
      + ' seventy two characters to ch...');
    expect(cfgNode.shapeType).toBe('process');

    cfgNode.nodeRecordId = 'new_record_id';
    cfgNode.nodeType = CfgNodeType.DISPLAY_STMT;
    cfgNode.entity = ControlFlowNode.EntityEnum.AST_NODE;
    cfgNode.parentId = '#test_id';
    cfgNode.childrenIds = ['#test_child_id'];
    const testPropMap: { [key: string]: object } = {};
    testPropMap[`test_key`] = 'test_value' as any;
    cfgNode.propertyMap = testPropMap;
    cfgNode.nodeOffset = 2;

    expect(cfgNode.nodeRecordId).toBe('new_record_id');
    expect(cfgNode.nodeType).toBe(CfgNodeType.DISPLAY_STMT);
    expect(cfgNode.entity).toBe(ControlFlowNode.EntityEnum.AST_NODE);
    expect(cfgNode.parentId).toBe('#test_id');
    expect(cfgNode.childrenIds).toEqual(['#test_child_id']);
    expect(cfgNode.propertyMap[`test_key`]).toBe('test_value' as any);
    expect(cfgNode.nodeOffset).toBe(2);
  });

  it('should set graph info', () => {
    const cfgIfNode = new ControlFlowNodeDetails(ifNode, null, null);
    const cfgThenNode = new ControlFlowNodeDetails(thenNode, null, null);
    const cfgOtherNode = new ControlFlowNodeDetails(otherNode, null, null);
    const graphInfo = new ControlFlowGraphInfo();

    graphInfo.graphNodes = [cfgIfNode, cfgThenNode, cfgOtherNode];

    expect(graphInfo.graphNodes).toEqual([cfgIfNode, cfgThenNode, cfgOtherNode]);

    const cfgEdge: ControlFlowEdge = {
      fromId: cfgIfNode.nodeRecordId,
      toId: cfgThenNode.nodeRecordId,
      label: 'TRUE'
    };
    graphInfo.graphEdges = [cfgEdge];

    expect(graphInfo.graphEdges).toEqual([cfgEdge]);
  });

  it('should remove the extra space from the CfgNode', () => {
    const nodeData = {
      recordId: '#143:32',
      type: 'CobolMoveStmt',
      superTypes: null as any,
      entity: ControlFlowNode.EntityEnum.AST_NODE,
      label: 'testing     the remove space "should not    remove space" from here'
    };
    const cfgNode = new ControlFlowNodeDetails(nodeData, null, null);
    expect(cfgNode.nodeDescription).toBe('testing the remove space "should not    remove space" from here');
  });

  it('should have node shape as decision where superType is LoopStatement', () => {
    const nodeData = {
      recordId: '#143:32',
      type: 'CobolMoveStmt',
      superTypes: new Set<string>([CfgNodeSuperType.LOOP_STATEMENT]),
      entity: ControlFlowNode.EntityEnum.AST_NODE,
    };
    const cfgNode = new ControlFlowNodeDetails(nodeData, null, null);
    expect(cfgNode.shapeType).toBe('decision');
  });

  it('should call calculateGraph', () => {
    component.loadState = LoaderState.nocontent;
    spyOn<any>(component, 'createGraph');
    component.reCreateGraph();
    expect(component.loadState).toBe(LoaderState.loading);
    expect(component['createGraph']).toHaveBeenCalled();
  });

  it('should call createGraph', () => {
    component.loadState = LoaderState.nocontent;
    controlFlowControllerServiceSpy.getControlFlow.and.returnValue(of(validResponse as any));
    spyOn(component.controlFlowUtility, 'getGraphInfo').and.returnValue(null);
    component['createGraph']();
    expect(component.controlFlowUtility.getGraphInfo).toHaveBeenCalled();
    expect(component.graphInfo).toBe(null);
    expect(component.loadState).toBe(LoaderState.error);
  });

  it('should call calculateCFG and return error', () => {
    controlFlowControllerServiceSpy.calculateControlFlowForModule.and.returnValue(throwError({ message: 'Test error' }));
    component.calculateCFG();
    expect(component.errorMessage).toBe('Test error');
  });

  it('should call getJobStatus and return error', () => {
    const errorJob = {...remoteJob, status$: throwError({ message: 'Test error' }) as any };
    jobManagerSpy.register.and.returnValue(errorJob);
    (component as any).getJobStatus('1');
    expect(component.errorMessage).toBe('Test error');
  });

  it('should call getJobStatus with success', () => {
    spyOn(component, 'reCreateGraph');
    const successJob = {...remoteJob, status$: of(JobInformation.StatusEnum.SUCCESS) as any};
    jobManagerSpy.register.and.returnValue(successJob);
    component['getJobStatus']('1');
    expect(component.reCreateGraph).toHaveBeenCalled();
  });

  it('should call getJobStatus with unknown', () => {
    spyOn((component as any ), 'createNavigationUrl');
    const unknowJob = {...remoteJob, status$: of(JobInformation.StatusEnum.UNKNOWN) as any};
    jobManagerSpy.register.and.returnValue(unknowJob);
    component['getJobStatus']('1');
    expect((component as any ).createNavigationUrl).toHaveBeenCalled();
  });

  it('should check recalculation', () => {
    cfgSupportedTypeServiceSpy.checkIfSupported.and.returnValue(true);
    component.module.sourceCodeAvailable = false;
    const disableRecalculation = component.needsRecalculation();
    expect(disableRecalculation).toBeTrue();

    cfgSupportedTypeServiceSpy.checkIfSupported.and.returnValue(false);
    component.module.sourceCodeAvailable = true;
    const enableRecalculation = component.needsRecalculation();
    expect(enableRecalculation).toBeFalse();
  });

  it('should test collapseAllGroups', () => {
    spyOn(component.cfgComponent, 'collapseAllGroups');
    component.collapseAllGroups();
    expect(component.cfgComponent.collapseAllGroups).toHaveBeenCalled();
  });

  it('should test expandAllGroups', () => {
    spyOn(component.cfgComponent, 'expandAllGroups');
    component.expandAllGroups();
    expect(component.cfgComponent.expandAllGroups).toHaveBeenCalled();
  });

  it('should create groups', () => {
    const controlFlowUtility = new ControlFlowUtility();
    const graphInfo = controlFlowUtility.getGraphInfo(controlFlowGraphWithGroups, 'TEST');
    const elseGroup = graphInfo.graphGroups.find(group => group.recordId === '#1105:311');
    const displayInElse = graphInfo.graphNodes.find(node => node.recordId === '#1109:311');
    expect(displayInElse.group).toBe(elseGroup.recordId);

    const paragraphGroup = graphInfo.graphGroups.find(group => group.recordId === '#1109:312');
    const displayInParagraph = graphInfo.graphNodes.find(node => node.recordId === '#1109:313');
    expect(displayInParagraph.group).toBe(paragraphGroup.recordId);

    const ifInParagraph = graphInfo.graphNodes.find(node => node.recordId === '#1109:314');
    const thenInParagraph = graphInfo.graphGroups.find(group => group.recordId === '#1109:315');
    const displayInThen = graphInfo.graphNodes.find(node => node.recordId === '#1109:316');
    expect(ifInParagraph.group).toBe(paragraphGroup.recordId);
    expect(displayInThen.group).toBe(thenInParagraph.recordId);
  })

  it('should omit redundand nodes', () => {
    const controlFlowUtility = new ControlFlowUtility();
    const graphInfo = controlFlowUtility.getGraphInfo(controlFlowGraphWithGroups, 'TEST');
    const group = graphInfo.graphGroups.find(group => group.recordId === '#1105:311');
    const node = graphInfo.graphNodes.find(node => node.recordId === '#1105:311');
    /* There should be a group but no node with the recordId #1105:311 */
    expect(group).toBeTruthy();
    expect(node).toBeUndefined();
    expect(graphInfo.graphEdges.length).toBe(5);
    const redirectedEdge = graphInfo.graphEdges.find(edge => edge.fromId === '#1107:311' && edge.toId === '#1109:311');
    expect(redirectedEdge).toBeTruthy();
  });
});
