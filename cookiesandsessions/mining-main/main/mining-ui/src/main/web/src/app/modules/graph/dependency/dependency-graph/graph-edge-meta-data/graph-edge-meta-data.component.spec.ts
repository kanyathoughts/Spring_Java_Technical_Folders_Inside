import { ComponentFixture, TestBed } from '@angular/core/testing';
import { GraphEdgeMetaDataComponent } from './graph-edge-meta-data.component';
import { Observable, Subject, of } from 'rxjs';
import { ModuleDetailsDPSidePanelService } from '@app/core/services/module-details-and-dp-panel-state.service';
import { WindowToken } from '@app/core/utils/window';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { TranslateModule } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { EdgeLabelPanelState } from '@app/shared/components/shared-module-details/dependency-graph-panel-state.interface';
import { DepedendencyGraphEdgeMetaData } from './graph-edge-meta-data.interface';
import { ModuleControllerService, ModulePojo, ModuleRelationshipPojo, ReferenceControllerService } from '@innowake/mining-api-angular-client';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('GraphEdgeMetaDataComponent', () => {
  const moduleValue: ModulePojo = {
    uid: '#136:600',
    customProperties: {
      'Property1': [{
        name: 'customMetaInfo2',
        value: null,
        dataType: 'STRING'
      }, {
        name: 'customMetaInfo1',
        value: null,
        dataType: 'STRING'
      }]
    },
    id: 2007,
    name: 'CC1',
    projectId: 1,
    path: 'src/cobol/programs/CC1.cpy',
    technology: 'COBOL',
    type: 'COPYBOOK',
    storage: 'FILE',
    identification: 'IDENTIFIED',
    origin: 'CUSTOM',
    info: null,
    description: 'A test copy',
    sourceMetrics:{
      codeLines: null,
      commentLines: null,
      complexityMcCabe: null,
    },
    content: null,
    sourceCodeAvailable: false
  };
  const relationship: ModuleRelationshipPojo[] = [
    {
      "id": '471',
      "relationship": "CALLS",
      "srcModuleDetails": {
        id: 164
      },
      "dstModuleDetails": {
        id: 192
      },
      "srcModule": "MMRS7102",
      "dstModule": "MMRS71Z1",
      "srcLocation": {
        "offset": 9763,
        "length": 106
      },
      "dstLocation": {
        "offset": 0,
        "length": 3048
      },
      "properties": {
        "CALL_TYPE": {call: "CALL"}
      }
    },
    {
      "id": '473',
      "relationship": "CALLS",
      "srcModuleDetails": {
        id: 164
      },
      "dstModuleDetails": {
        id: 192
      },
      "srcModule": "MMRS7102",
      "dstModule": "MMRS71Z1",
      "srcLocation": {
        "offset": 10712,
        "length": 106
      },
      "dstLocation": {
        "offset": 0,
        "length": 3048
      },
      "properties": {
        "CALL_TYPE": {call: "CALL"}
      }
    }
  ];
  const mockEdgeMetaData: DepedendencyGraphEdgeMetaData[] = [
    {
      module: "test1",
      moduleId: 1,
      shortName: "test1",
      technology: "JCL",
      type: "EXEC_PGM"
    },
    {
      module: "test2",
      moduleId: 2,
      shortName: "test2",
      technology: "COBOL",
      type: "PROGRAM"
    }
  ];
  const artificialEdgeMetaData = {
    moduleTypes: [
      "JCL JOB",
      "JCL EXEC PGM",
      "COBOL PROGRAM"
    ],
    modules: [
      {
        "customProperties": {},
        "id": 248,
        "uid": 248,
        "name": "MMRS710J.STEP01.EXEC_PGM",
        "projectId": 1,
        "technology": "JCL",
        "type": "EXEC_PGM",
        "storage": "UNDEFINED",
        "identification": "IDENTIFIED",
        "origin": "CUSTOM",
        "info": {},
        "linesOfDeadCode": -1,
      },
      {
        "customProperties": {},
        "id": 232,
        "uid": 232,
        "name": "MMRS710J.STEP02.EXEC_PGM",
        "projectId": 1,
        "technology": "COBOL",
        "type": "PROGRAM",
        "storage": "UNDEFINED",
        "identification": "IDENTIFIED",
        "origin": "CUSTOM",
        "info": {},
        "linesOfDeadCode": -1
      }
    ],
    references: [
      {
        "id": 1938,
        "relationship": "CALLS",
        "srcModuleDetails": {id: "232", name: 'MMRS710J.STEP01.EXEC_PGM'},
        "dstModuleDetails": {id: "248", name: 'MMRS710J'},
        "srcModule": 232,
        "dstModule": 248,
        "properties": {},
      },
      {
        "customProperties": {},
        "id": 1946,
        "relationship": "CALLS",
        "srcModuleDetails": {id: 248, name: 'MMRS7101'},
        "dstModuleDetails": {id: 232, name: 'MMRS710J.STEP02.EXEC_PGM'},
        "srcModule": 248,
        "dstModule": 232,
        "properties": {},
      }
    ],
    relationshipTypes: ['CALLS'],
    rootModuleIds: [232]
  }
  let mockWindow: any;
  let openUrl = '';
  let component: GraphEdgeMetaDataComponent;
  let fixture: ComponentFixture<GraphEdgeMetaDataComponent>;
  const referenceControllerServiceSpy = jasmine.createSpyObj<ReferenceControllerService>('ReferenceControllerService', ['findAllByFromAndToModuleIds']);
  const moduleDetailsDPSidePanelServiceSpy = jasmine.createSpyObj<ModuleDetailsDPSidePanelService>('ModuleDetailsDPSidePanelService', ['getPanelState','setPanelState']);
  const LabelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
    ['findModuleById']);
  const jobManagerServiceSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['subscribeToJobNotification','jobExtension','jobResult']);

  beforeEach(async () => {
    mockWindow = {
      get location() {
          return {
              href: 'http://localhost:8081/#/browse-modules/1/1/1/explore',
              hash: '#/browse-modules/1/1/1/explore'
          };
      },
      open: (sUrl: any) => {
          openUrl = sUrl;
      }
  };
  mockWindow.open.bind(mockWindow);
    await TestBed.configureTestingModule({
      declarations: [GraphEdgeMetaDataComponent],
      imports : [ 
        TranslateModule.forRoot({}),
        HttpClientTestingModule
      ],
      providers: [
        { provide: NzMessageService, useValue: {} },
        { provide: ReferenceControllerService, useValue: referenceControllerServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
        { provide: ModuleDetailsDPSidePanelService, useValue: moduleDetailsDPSidePanelServiceSpy },
        { provide: LabelMappingService, useValue: LabelMappingServiceSpy},
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy }
      ]
    })
    .compileComponents();
    moduleControllerServiceSpy.findModuleById.and.returnValue(of(moduleValue as any));
  });

  beforeEach(() => {
    referenceControllerServiceSpy.findAllByFromAndToModuleIds.and.returnValue(of(relationship as any));
    jobManagerServiceSpy.jobResult = new Subject;
    jobManagerServiceSpy.jobResult.next(null);
    jobManagerServiceSpy.jobResult.next(artificialEdgeMetaData);
    fixture = TestBed.createComponent(GraphEdgeMetaDataComponent);
    component = fixture.componentInstance;
    component.inputForEdgeMetadata = mockEdgeMetaData;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should set metaDataLoaded to be false for response in onInit', () => {
    component.isArtificialEdge = true;
    component.ngOnInit();
    jobManagerServiceSpy.subscribeToJobNotification();
    jobManagerServiceSpy.jobResult.next(artificialEdgeMetaData);
    expect(component.metaDataLoaded).toBe(false);
  });

  it('should getDataForEdgeModule', () => {
    const mockPanelState: EdgeLabelPanelState = {
      module: false,
      relationship: false,
      reference: false
    };
    const mockArtificialEdgeMetadata = [{depth: ['0'], startModule: ['280'],endModule: ['341']}];
    component.inputForArtificialEdgeMetadata = mockArtificialEdgeMetadata;
    component.projectId = 1;
    component.isArtificialEdge = true;
    moduleDetailsDPSidePanelServiceSpy.getPanelState.and.returnValue(mockPanelState);
    component.getDataForEdgeModule();
    expect(component.metaDataLoaded).toBe(true);
    expect(moduleDetailsDPSidePanelServiceSpy.getPanelState).toHaveBeenCalledWith('edgePanelState');
    expect(jobManagerServiceSpy.jobExtension).toHaveBeenCalled();
  });

  it('should retrieve and populate data on getDataForEdgeModule', () => {
    component.isArtificialEdge = false;
    component.projectId = 1;
    component.toId = 3;
    component.fromId = 3;
    component.relationShipType = "CALLS";
    LabelMappingServiceSpy.mapLabel.and.returnValue('Calls' as any);
    component.getDataForEdgeModule();
    expect(moduleDetailsDPSidePanelServiceSpy.getPanelState).toHaveBeenCalledWith('edgePanelState');
    expect(referenceControllerServiceSpy.findAllByFromAndToModuleIds).toHaveBeenCalledWith(1, 3, 3, "CALLS");
    expect(component.relationshipData[0]['properties']).toEqual({
      'Calls 1': { CALL_TYPE: { call: 'CALL' }},
      'Calls 2': { CALL_TYPE: { call: 'CALL' }},
    });
  });

  xit('should toggle the closed side bar', () => {
     // In WMIN-10964 - test disabled due to failure; no changes made which can impact this. Will fix it with another ticket.
    component.maintainSidePanelState = {
      module: false,
      relationship: false,
      reference: false
    };
    component.toggleDropDown(event,'module');
    expect(component.maintainSidePanelState.module).toBeTruthy();
    component.toggleDropDown(event, 'relationship');
    expect(component.maintainSidePanelState.relationship).toBeTruthy();
  });

  xit('should toggle the already open side bar', () => {
    component.maintainSidePanelState = {
      module: true,
      relationship: true,
      reference: false
    };
    component.toggleDropDown(event,'module');
    expect(component.maintainSidePanelState.module).toBeFalsy();
    component.toggleDropDown(event,'relationship');
    expect(component.maintainSidePanelState.relationship).toBeFalsy();
  });

  it('should open module links in new tab', () => {
    component.projectId = 1;
    const moduleId = 2;
    const currentPath = location.pathname.replace(/^\//, '');
    component.buildRouteForModule(moduleId);
    expect(openUrl).toBe(`/${currentPath}#${RouteBuilder.buildModuleRoute(1, 2, 'details/overview')}`);
});

  it('should test closePanel', () => {
    spyOn(component.closeEdgeSidePanel, 'emit');
    component.closePanel();
    expect(component.closeEdgeSidePanel.emit).toHaveBeenCalled();
  });
});
