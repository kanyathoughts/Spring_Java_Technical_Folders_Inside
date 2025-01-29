import { ComponentFixture, TestBed } from '@angular/core/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ReachabilityBlockGraphComponent } from './reachability-block-graph-component';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';
import { FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ReachabilityService } from '../utils/reachability.service';
import { ReachabilityCallchainData } from '../utils/reachability-interface';
import { GraphQlControllerService } from '@app/core/services/graphql.service';

describe('Reachability Block Graph compoenent', () => {
    let component: ReachabilityBlockGraphComponent;
    let fixture: ComponentFixture<ReachabilityBlockGraphComponent>;

    const functionalBlockControllerSpy = jasmine.createSpyObj<FunctionalBlockControllerService>('FunctionalBlockControllerService',
        ['getFunctionalBlockAsDependencyGraph', 'getReachabilityNetworkGraph']);
    const grphQlSpy = jasmine.createSpyObj<GraphQlControllerService>('GraphQlControllerService', ['graphQl']);
    const reachabilityServiceSpy: jasmine.SpyObj<ReachabilityService> = jasmine.createSpyObj<ReachabilityService>('ReachabilityService', ['fetchReachabilityCallChainDetails','getUpdateGraph']);
    const callChainData: ReachabilityCallchainData = {
        name: 'Job1',
        callChain: { content: [{uid: '000b38b0-d745-49df-9125-0bdf5bd90da7'}]}
    };
    const graphData = {
        modules: [
            {
                "uid": "3ecf69f5-4ea7-4012-aa0c-c0ed7432afb5",
                "customProperties": {},
                "technology": "JCL",
                "type": "EXEC_PGM",
                "storage": "FILE_SECTION",
                "origin": "CUSTOM",
                "creator": "DISCOVERY",
                "identification": "IDENTIFIED",
                "parentId": 502,
                "parentPath": "src/jcl/procs/MMRS712P.proc",
                "id": 509
            }
        ],
        references: [
            {
                "id": "0c0d8c48-457a-437b-8068-f008537a31cd",
                "srcModule": "029c597b-d2f4-4d18-83fc-7a9cc2a8fa86",
                "dstModule": "3ecf69f5-4ea7-4012-aa0c-c0ed7432afb5",
                "properties": {},
                "dependencyBinding": "LATE",
                "dependencyAttributes": "{}",
                "relationship": "CALLS"
            }
        ],
        rootModuleIds: [
            484
        ],
        moduleTypes: [
            "JCL EXEC PGM"
        ],
        relationshipTypes: [
            "CALLS"
        ]
    };

    const networkGraph = {
        "modules": [
          {
            "uid": "317f3b66-b99d-4c78-8008-bcf6f29d5cd0",
            "customProperties": {},
            "project": "3169632b-51f5-479e-8ec0-104132cdaba9",
            "projectId": 1,
            "name": "MMRS712V",
            "technology": "UNKNOWN",
            "type": "UNKNOWN",
            "info": {
              "TYPE": [
                "REACHABILITY",
                "RA_TOP_DOWN"
              ],
              "DELETED": false,
              "OUTDATED": false,
              "GENERATED_AT": 1716811777117,
              "GENERATED_BY": "ReachabilityTopDownGeneration"
            },
            "description": "",
            "errors": 0,
            "statements": 0,
            "sqlStatements": 0,
            "sourceCodeAvailable": false,
            "id": 34
          }
        ],
        "references": [
          {
            "id": "93287605-b8c6-4af1-adee-44de248c17db",
            "srcModule": "317f3b66-b99d-4c78-8008-bcf6f29d5cd0",
            "dstModule": "daa1d3c8-bf99-46e9-89b6-973b6e62140d",
            "direction": "OUT",
            "properties": {
              "TYPE": [
                "RA_SHARED_RESOURCE",
                "DIRECTED"
              ],
              "GENERATED_BY": "ReachabilityResourceNetwork",
              "RA_SHARED_RESOURCE_ID": [
                "47899eb0-2f1a-4367-960b-170388e7ced3"
              ]
            },
            "relationship": "REFERENCES"
          }
        ],
        "rootModuleIds": [
          34,
          66,
          26
        ],
        "moduleTypes": [
          "UNKNOWN UNKNOWN"
        ],
        "relationshipTypes": [
          "REFERENCES"
        ]
      };

    beforeEach(async () => {
        await TestBed.configureTestingModule({
            declarations: [ReachabilityBlockGraphComponent],
            providers: [
                NumberFormatter,
                TranslateService,
                { provide: FunctionalBlockControllerService, useValue: functionalBlockControllerSpy },
                { provide: GraphQlControllerService, useValue: grphQlSpy },
                {
                    provide: ActivatedRoute, useValue: {
                        snapshot: {
                            params: {
                                blockId: '0eee0ba3-6273-4e41-a7da-eb01a065465d',
                                projectId: 'project-1'
                            }
                        },
                        queryParams: of({
                            type: ['RA_TOPDOWN'], name: 'MMRS711s' 
                        })
                    }
                },
                { provide: ReachabilityService, useValue: reachabilityServiceSpy }
            ],
            imports: [
                RouterTestingModule,
                HttpClientTestingModule,
                BrowserAnimationsModule,
                AntDesignImportsModule,
                TranslateModule.forRoot({}),
              ],
        }).compileComponents();
        grphQlSpy.graphQl.and.returnValue(of(callChainData as any));
        reachabilityServiceSpy.fetchReachabilityCallChainDetails.and.returnValue(of(callChainData as any));
        reachabilityServiceSpy.getUpdateGraph.and.returnValue(of(true as any));
        functionalBlockControllerSpy.getFunctionalBlockAsDependencyGraph.and.returnValue(of(graphData as any));
        functionalBlockControllerSpy.getReachabilityNetworkGraph.and.returnValue(of(networkGraph as any));
    });
    
    beforeEach(() => {
        fixture = TestBed.createComponent(ReachabilityBlockGraphComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should test onInit', () => {
        component.ngOnInit();
        expect(component.blockId).toEqual('"0eee0ba3-6273-4e41-a7da-eb01a065465d"');
    });

    it('should test fetchCallChain', () => {
        spyOn(component as any, 'fetchBlockGraphDetails');
        component.fetchReachabilityBlockDetails();
        expect(reachabilityServiceSpy.fetchReachabilityCallChainDetails).toHaveBeenCalled();
        expect(component['fetchBlockGraphDetails']).toHaveBeenCalled();
    });

    xit('should test fetchNetworkGraphDetails', () => {
        const networkGraphSpy = spyOn(component as any, 'fetchNetworkGraphDetails').and.callThrough();
        component.networkUid = '0eee0ba3-6273-4e41-a7da-eb01a065465d';
        expect(networkGraphSpy).toHaveBeenCalled();
    });
});

