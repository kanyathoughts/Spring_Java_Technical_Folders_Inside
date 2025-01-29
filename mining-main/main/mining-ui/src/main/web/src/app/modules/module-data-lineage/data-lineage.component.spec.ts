import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { TranslateModule } from '@ngx-translate/core';
import { BrowserAnimationsModule, NoopAnimationsModule } from '@angular/platform-browser/animations';
import { SharedModule } from '@app/shared';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ModulePojo, ModuleControllerService, JobControllerService, JobInformation } from '@innowake/mining-api-angular-client';
import { DataLineageComponent } from './data-lineage.component';
import { ActivatedRoute } from '@angular/router';
import { BehaviorSubject, of } from 'rxjs';
import { DataFlowUtility } from './utils/data-flow-utility';
import { ICommand } from 'yfiles';
import { DataFlowGraphComponent } from './data-flow-graph/data-flow-graph.component';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { ComponentFactoryResolver } from '@angular/core';
import { WindowToken } from '@app/core/utils/window';

describe('DataLineageComponent', () => {
    let component: DataLineageComponent;
    let fixture: ComponentFixture<DataLineageComponent>;

    const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
    ['getCodeViewerDataFlowGraphJob', 'findModuleById']);
    const jobControllerSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj<JobControllerService>('JobControllerService', ['getJobResult']);
    const jobManagerSpy = jasmine.createSpyObj<JobManagerService>('JobManagerService', ['jobNotification', 'getRemoteJob', 'register', ]);
    let incomings: any[] = [];
    let outgoings: any[] = [];
    let children: any[] = [];
    let parentModule: string = null;
    let sourceLocation: string = null;
    let CFR: ComponentFactoryResolver;
    const jobResult = {
      "object": {
        "className": "innowake.mining.shared.model.datalineage.graph.DataFlowGraph",
          "nodes": [
            {
              "type": "MODULE",
              "id": "module-192",
              "moduleId": 192,
              "name": "MMRS71Z1",
              "location": {
                "offset": 0,
                "length": 0
              },
              "incomings": incomings,
              "outgoings": outgoings,
              "parentModule": parentModule,
              "children": [
                "module-192-field-1447",
                "module-192-field-1491"
              ],
              "sourceLocation": sourceLocation,
              "dataInterfaces": [
                "module-192-container-ENTRY_POINT-1926-interface-0",
                "module-192-container-ENTRY_POINT-1926-interface-1",
                "module-192-container-ENTRY_POINT-1926-interface-2"
              ]
            },
            {
              "type": "DATA_INTERFACE",
              "id": "module-192-container-ENTRY_POINT-1926-interface-0",
              "name": "MY-HEX-CONV-RESULT",
              "location": null,
              "incomings": [
                "module-232-container-CALL-3314-interface-0",
                "module-232-container-CALL-3314-interface-1"
              ],
              "outgoings": [],
              "parentModule": "module-192",
              "children": [],
              "sourceLocation": null
            },
            {
              "type": "DATA_INTERFACE",
              "id": "module-192-container-ENTRY_POINT-1926-interface-1",
              "name": "MY-HEX-ORIGIN",
              "location": null,
              "incomings": [
                "module-232-container-CALL-3314-interface-0",
                "module-232-container-CALL-3314-interface-1"
              ],
              "outgoings": [
                "module-192-field-1491"
              ],
              "parentModule": "module-192",
              "children": [],
              "sourceLocation": null
            },
            {
              "type": "MODULE",
              "id": "module-232",
              "moduleId": 232,
              "name": "MMRS7101",
              "location": {
                "offset": 0,
                "length": 0
              },
              "incomings": [],
              "outgoings": [],
              "parentModule": null,
              "children": [
                "module-232-field-1019",
                "module-232-field-1217"
              ],
              "sourceLocation": null,
              "dataInterfaces": [
                "module-232-container-CALL-3314-interface-0",
                "module-232-container-CALL-3314-interface-1",
                "module-232-container-CALL-3314-interface-2",
                "module-232-container-CALL-3314-interface-3",
                "module-232-container-CALL-3752-interface-0",
                "module-232-container-CALL-3752-interface-1",
                "module-232-container-CALL-3752-interface-2",
                "module-232-container-CALL-3752-interface-3"
              ]
            },
            {
              "type": "DATA_INTERFACE",
              "id": "module-232-container-CALL-3314-interface-0",
              "name": "MY-HEX-CONV-RESULT",
              "location": null,
              "incomings": [
                "module-232-statement-3314"
              ],
              "outgoings": [
                "module-192-container-ENTRY_POINT-1926-interface-0",
                "module-192-container-ENTRY_POINT-1926-interface-1"
              ],
              "parentModule": "module-232",
              "children": [],
              "sourceLocation": null
            },
            {
              "type": "DATA_INTERFACE",
              "id": "module-232-container-CALL-3314-interface-1",
              "name": "MY-HEX-ORIGIN",
              "location": null,
              "incomings": [
                "module-232-statement-3314"
              ],
              "outgoings": [
                "module-192-container-ENTRY_POINT-1926-interface-0",
                "module-192-container-ENTRY_POINT-1926-interface-1"
              ],
              "parentModule": "module-232",
              "children": [],
              "sourceLocation": null
            },
            {
              "type": "FIELD",
              "id": "module-232-field-1019",
              "name": "MY-PROGRAM-NAME",
              "location": {
                "offset": 1019,
                "length": 48
              },
              "incomings": [],
              "outgoings": [
                "module-232-statement-2460",
                "module-232-statement-2703"
              ],
              "parentModule": "module-232",
              "children": [],
              "sourceLocation": {
                "moduleId": 232,
                "moduleName": "MMRS7101",
                "moduleLocation": {
                  "offset": 1019,
                  "length": 48
                }
              }
            },
            {
              "type": "FIELD",
              "id": "module-232-field-1217",
              "name": "TRUNC-TEST-DISP",
              "location": {
                "offset": 1217,
                "length": 42
              },
              "incomings": [
                "module-232-statement-6529",
                "module-232-statement-6833"
              ],
              "outgoings": [
                "module-232-statement-6576",
                "module-232-statement-6880"
              ],
              "parentModule": "module-232",
              "children": children,
              "sourceLocation": {
                "moduleId": 232,
                "moduleName": "MMRS7101",
                "moduleLocation": {
                  "offset": 1217,
                  "length": 42
                }
              }
            },
            {
              "type": "STATEMENT",
              "id": "module-232-statement-2460",
              "name": "Display",
              "location": {
                "offset": 2460,
                "length": 34
              },
              "incomings": [
                "module-232-field-1019"
              ],
              "outgoings": [],
              "parentModule": "module-232",
              "children": [],
              "sourceLocation": {
                "moduleId": 232,
                "moduleName": "MMRS7101",
                "moduleLocation": {
                  "offset": 2460,
                  "length": 34
                }
              }
            }
          ]
        }
    }

    const nodes = [
        {
            "type": "DATA_INTERFACE",
            "name": "MY-HEX-CONV-RESULT",
            "id": "module-192-container-ENTRY_POINT-1926-interface-0",
            "incomings": [
                "module-232-container-CALL-3314-interface-0",
                "module-232-container-CALL-3314-interface-1"
            ],
            "outgoings": outgoings,
            "children": children,
            "group": "module-192-Data-Interfaces",
            "parentModule": "module-192"
        },
        {
            "type": "DATA_INTERFACE",
            "name": "MY-HEX-ORIGIN",
            "id": "module-192-container-ENTRY_POINT-1926-interface-1",
            "incomings": [
                "module-232-container-CALL-3314-interface-0",
                "module-232-container-CALL-3314-interface-1"
            ],
            "outgoings": [
                "module-192-field-1491"
            ],
            "children": children,
            "group": "module-192-Data-Interfaces",
            "parentModule": "module-192"
        },
        {
            "type": "DATA_INTERFACE",
            "name": "MY-HEX-CONV-RESULT",
            "id": "module-232-container-CALL-3314-interface-0",
            "incomings": [
                "module-232-statement-3314"
            ],
            "outgoings": [
                "module-192-container-ENTRY_POINT-1926-interface-0",
                "module-192-container-ENTRY_POINT-1926-interface-1"
            ],
            "children": [],
            "group": "module-232-Data-Interfaces",
            "parentModule": "module-232"
        },
        {
            "type": "DATA_INTERFACE",
            "name": "MY-HEX-ORIGIN",
            "id": "module-232-container-CALL-3314-interface-1",
            "incomings": [
                "module-232-statement-3314"
            ],
            "outgoings": [
                "module-192-container-ENTRY_POINT-1926-interface-0",
                "module-192-container-ENTRY_POINT-1926-interface-1"
            ],
            "children": [],
            "group": "module-232-Data-Interfaces",
            "parentModule": "module-232"
        },
        {
            "type": "FIELD",
            "name": "MY-PROGRAM-NAME",
            "id": "module-232-field-1019",
            "incomings": [],
            "outgoings": [
                "module-232-statement-2460",
                "module-232-statement-2703"
            ],
            "children": [],
            "group": "module-232",
            "parentModule": "module-232"
        },
        {
            "type": "FIELD",
            "name": "TRUNC-TEST-DISP",
            "id": "module-232-field-1217",
            "incomings": [
                "module-232-statement-6529",
                "module-232-statement-6833"
            ],
            "outgoings": [
                "module-232-statement-6576",
                "module-232-statement-6880"
            ],
            "children": [],
            "group": "module-232",
            "parentModule": "module-232"
        },
        {
            "type": "STATEMENT",
            "name": "Display",
            "id": "module-232-statement-2460",
            "incomings": [
                "module-232-field-1019"
            ],
            "outgoings": [],
            "children": [],
            "group": "module-232",
            "parentModule": "module-232"
        }
    ];
    const edges = [
        {
            "fromId": "module-192-container-ENTRY_POINT-1926-interface-1",
            "toId": "module-192-field-1491"
        },
        {
            "fromId": "module-232-container-CALL-3314-interface-0",
            "toId": "module-192-container-ENTRY_POINT-1926-interface-0"
        },
        {
            "fromId": "module-232-container-CALL-3314-interface-0",
            "toId": "module-192-container-ENTRY_POINT-1926-interface-1"
        },
        {
            "fromId": "module-232-container-CALL-3314-interface-1",
            "toId": "module-192-container-ENTRY_POINT-1926-interface-0"
        },
        {
            "fromId": "module-232-container-CALL-3314-interface-1",
            "toId": "module-192-container-ENTRY_POINT-1926-interface-1"
        },
        {
            "fromId": "module-232-field-1019",
            "toId": "module-232-statement-2460"
        },
        {
            "fromId": "module-232-field-1019",
            "toId": "module-232-statement-2703"
        },
        {
            "fromId": "module-232-field-1217",
            "toId": "module-232-statement-6576"
        },
        {
            "fromId": "module-232-field-1217",
            "toId": "module-232-statement-6880"
        }
    ];
    const groupNodes = [
        {
            "id": "module-192",
            "label": "MMRS71Z1"
        },
        {
            "id": "module-192-Data-Interfaces",
            "label": "Data Interfaces",
            "parentGroup": "module-192"
        },
        {
            "id": "module-232",
            "label": "MMRS7101"
        },
        {
            "id": "module-232-Data-Interfaces",
            "label": "Data Interfaces",
            "parentGroup": "module-232"
        }
    ];
    const dataLineageGraphInfo = {
        "nodes": nodes,
        "groupNodes": groupNodes,
        "edges": edges
    };

    const moduleValue: ModulePojo[] = [
      {
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
        content: null
      }];
    let mockWindow: any;
    let openedUrl = '';
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
            declarations: [DataLineageComponent, DataFlowGraphComponent],
            imports: [
                BrowserAnimationsModule,
                NoopAnimationsModule,
                TranslateModule.forRoot({}),
                SharedModule,
                RouterTestingModule,
                HttpClientTestingModule
            ],
            providers: [
                { provide: ModuleControllerService, useValue: moduleControllerServiceSpy},
                { provide: JobControllerService, useValue: jobControllerSpy },
                { provide: WindowToken, useValue: mockWindow },
                { provide: JobManagerService, useValue: jobManagerSpy },
                {
                  provide: ActivatedRoute,
                  useValue: {
                    data: of({
                      module: moduleValue[0]
                    }),
                    queryParams: {
                      _value:
                        { offset: 10 }
                    }
                  },
                },
            ]
        }).compileComponents();
        TestBed.compileComponents();
        moduleControllerServiceSpy.getCodeViewerDataFlowGraphJob.and.returnValue(of('test' as any));
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(DataLineageComponent);
        jobManagerSpy.jobNotification = new BehaviorSubject<any>({});
        jobManagerSpy.jobNotification.next('Sucess' as JobInformation);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    xit('should test graphInfo', () => {
        component.ngOnInit();
        const dataFlowUtility = new DataFlowUtility();
        const graphInfo = dataFlowUtility.getDataLineageGraphInfo(jobResult.object['nodes'] as any);
        component.graphInfo = graphInfo;
        expect(JSON.stringify(graphInfo)).toEqual(JSON.stringify(dataLineageGraphInfo));
    });

    it('should call zoom methods', () => {
      component.dfgComponent = new DataFlowGraphComponent(moduleControllerServiceSpy, CFR);
      component.dfgComponent.graphComponent = {
        graph: {
          zoom: 1
        }
      } as any;
      spyOn(ICommand.INCREASE_ZOOM, 'execute');
      spyOn(ICommand.DECREASE_ZOOM, 'execute');
      spyOn(ICommand.ZOOM, 'execute');
      spyOn(ICommand.FIT_GRAPH_BOUNDS, 'execute');
    
      component.zoomIn();
      expect(ICommand.INCREASE_ZOOM.execute).toHaveBeenCalled();
    
      component.zoomOut();
      expect(ICommand.DECREASE_ZOOM.execute).toHaveBeenCalled();
    
      component.zoomOriginal();
      expect(ICommand.ZOOM.execute).toHaveBeenCalled();
    
      component.fitContent();
      expect(ICommand.FIT_GRAPH_BOUNDS.execute).toHaveBeenCalled();
    });

    it('should test collapseAllGroups', () => {
      component.dfgComponent = new DataFlowGraphComponent(moduleControllerServiceSpy, CFR);
      spyOn(component.dfgComponent, 'collapseAllGroups');
      component.collapseAllGroups();
      expect(component.dfgComponent.collapseAllGroups).toHaveBeenCalled();
    });
  
    it('should test expandAllGroups', () => {
      component.dfgComponent = new DataFlowGraphComponent(moduleControllerServiceSpy, CFR);
      spyOn(component.dfgComponent, 'expandAllGroups');
      component.expandAllGroups();
      expect(component.dfgComponent.expandAllGroups).toHaveBeenCalled();
    });
});