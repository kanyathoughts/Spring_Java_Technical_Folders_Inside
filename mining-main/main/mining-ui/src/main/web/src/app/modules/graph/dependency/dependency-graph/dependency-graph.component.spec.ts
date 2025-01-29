import { DependencyGraphComponent } from './dependency-graph.component';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { DEPENDENCY_GRAPH_LINKS } from '../dependency-explorer/dependency-explorer.component.spec';
import { GraphUtility } from '../utils/dependency-graph-utility';
import {
    License,
    INode,
    ShapeNodeStyle,
    SolidColorFill,
    ILabel,
    IconLabelStyle,
    PolylineEdgeStyle,
    FilteredGraphWrapper
} from 'yfiles';
import {
    FormsModule,
    ReactiveFormsModule
} from '@angular/forms';
import { NodeImages, NodeType } from '../../utils/node-configurations';
import { LayoutType } from '../../utils/graph-layouts';
import { environment } from '@env/environment';
import { TranslateModule } from '@ngx-translate/core';
import { BrowserAnimationsModule, NoopAnimationsModule, provideAnimations } from '@angular/platform-browser/animations';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { NODE_COLORS, NodeColor } from '../../utils/node-colors';
import { FilterParameters } from '@app/modules/graph/models/yfile-graph-info.model';
import { GraphGlobalStyles, DEFAULT_NODE_SHAPE, DEFAULT_STROKE_THICKNESS, SELECTED_NODE_STROKE_THICKNESS } from '../../utils/graph-global-styles';
import { WindowToken } from '@app/core/utils/window';
import { GraphOverviewPanelComponent } from '../../yfiles-graph-overview-panel/graph-overview-panel.component';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { HttpClient } from '@angular/common/http';
import { HttpService } from '@app/core/http/http.service';
import { ApiPrefixInterceptor } from '@app/core/http/api-prefix.interceptor';
import { ErrorHandlerInterceptor } from '@app/core/http/error-handler.interceptor';
import { of } from 'rxjs';
import { ActivatedRoute } from '@angular/router';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { NzSelectModule } from 'ng-zorro-antd/select';
import { MiningSelectOption } from '@app/shared/interfaces/mining-select-option.interface';
import { CancelRequestOnNavigationInterceptor } from '@app/core/http/cancel-request-on-navigation.interceptor';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { CustomPropertiesService } from '@app/core/services/custom-properties/custom-properties.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { DependencyGraph, FeatureControllerService, JobControllerService, MetamodelControllerService, ModuleControllerService, ModulePojo, ModuleRelationshipPojo, ReferenceControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { SetSerializationInterceptor } from '@app/core/http/set-serialization.interceptor';
import { ReachabilityService } from '@app/modules/reachability-ui-product-vision/utils/reachability.service';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { NzModalModule } from 'ng-zorro-antd/modal';
import { MonacoEditorMockComponent } from '@app/core/mocks/monaco-editor.mock';
import { getValuesFromUncheckedOptions } from '../../utils/dependency-reachability-graph.util';
import * as dependencyUtility from '../../utils/dependency-reachability-graph.util';

describe('DependencyGraphComponent', () => {
    License.value = environment.yFilesLicense;
    let component: DependencyGraphComponent;
    let fixture: ComponentFixture<DependencyGraphComponent>;
    let mockWindow: any;
    let openedUrl = '';
    const ModuleControllerServiceSpy = jasmine.createSpyObj<ModuleControllerService>('ModuleControllerService', ['findModuleById']);
    const metamodelControllerServiceStub: jasmine.SpyObj<MetamodelControllerService> = jasmine.createSpyObj<MetamodelControllerService>('MetamodelControllerService', ['findMetaModel']);
    const GraphQlControllerServiceStub = jasmine.createSpyObj<GraphQlControllerService>('GraphQlControllerService', ['graphQl']);
    const referenceControllerServiceSpy = jasmine.createSpyObj<ReferenceControllerService>('ReferenceControllerService', ['findAllByFromAndToModuleIds']);
    const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);

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
            schemas: [NO_ERRORS_SCHEMA],
            declarations: [
                DependencyGraphComponent,
                GraphOverviewPanelComponent,
                MonacoEditorMockComponent
            ],
            imports: [
                BrowserAnimationsModule,
                NoopAnimationsModule,
                FormsModule,
                ReactiveFormsModule,
                TranslateModule.forRoot({}),
                HttpClientTestingModule,
                NzSelectModule,
                NzModalModule
            ],
            providers: [
                SetSerializationInterceptor,
                DeepLinkService,
                FeatureControllerService,
                ApiPrefixInterceptor,
                TaxonomyControllerService,
                NumberFormatter,
                I18nService,
                JobManagerService,
                JobControllerService,
                CustomPropertiesService,
                ReachabilityService,
                { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
                {
                    provide: GraphQlControllerService,
                    useValue: GraphQlControllerServiceStub
                },
                {
                    provide: MetamodelControllerService,
                    useValue: metamodelControllerServiceStub
                },
                ErrorHandlerInterceptor,
                CancelRequestOnNavigationInterceptor,
                {
                    provide: HttpClient,
                    useClass: HttpService
                },
                { provide: WindowToken, useValue: mockWindow },
                { provide: ModuleControllerService, useValue: ModuleControllerServiceSpy },
                { provide: ReferenceControllerService, useValue: referenceControllerServiceSpy },
                { provide: LabelMappingService, useValue:labelMappingServiceSpy },
                {
                    provide: ActivatedRoute,
                    useValue: {
                        data: of({
                            module: {
                                id: 2,
                                projectId: 1,
                                path: 'test'
                            }
                        })
                    }
                }
            ]
        }).compileComponents();
        ModuleControllerServiceSpy.findModuleById.and.returnValue(of({ id: 2, path: 'test' } as any));
    }));
    beforeEach(() => {
        fixture = TestBed.createComponent(DependencyGraphComponent);
        component = fixture.componentInstance;
        const graphInfo = GraphUtility.getYFilesGraphInfo(DEPENDENCY_GRAPH_LINKS);
        graphInfo.graphNodeTypes = DEPENDENCY_GRAPH_LINKS.moduleTypes.map(filter => filter as NodeType);
        graphInfo.nodes[0].isRoot = true;
        component.graphInfo = graphInfo;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
        expect(GraphUtility.isLarge(component.graphComponent.graph)).toBeFalsy();
    });

    xdescribe('graph with multiple roots', () => {
        beforeEach(() => {
            const graphWithMultipleRoots: DependencyGraph = {
                modules: [],
                references: [],
                moduleTypes: ['COBOL PROGRAM'],
                relationshipTypes: ['CALLS']
            };
            for (let index = 1; index <= 9; index++) {
                const module = {
                    uid: index + '',
                    name: 'node-' + `${index}`,
                    technology: 'COBOL' as ModulePojo.TechnologyEnum,
                    type: 'PROGRAM' as ModulePojo.TypeEnum
                };
                graphWithMultipleRoots.modules.push(module);
            }
            const links = [
                { srcModule: '1', dstModule: '4', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '4', dstModule: '5', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '2', dstModule: '6', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '6', dstModule: '7', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '6', dstModule: '8', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '3', dstModule: '9', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
            ];
            links.forEach(link => {
                graphWithMultipleRoots.references.push(link);
            });
            const graphInfo = GraphUtility.getYFilesGraphInfo(graphWithMultipleRoots);
            graphInfo.nodes[0].isRoot = true;
            graphInfo.nodes[1].isRoot = true;
            graphInfo.nodes[2].isRoot = true;
            component.graphInfo = graphInfo;
            fixture.detectChanges();
        });

        it('should create graph with multiple roots', () => {
            expect(component).toBeTruthy();
        });

        it('should have node levels', () => {
            component.ngOnChanges();
            const nodeIdToLevelMap: Map<string, number> = new Map();
            component.graphInfo.nodes.forEach(node => {
                nodeIdToLevelMap.set(node.uid, node.level);
            });
            expect(nodeIdToLevelMap.get('1')).toEqual(0);
            expect(nodeIdToLevelMap.get('2')).toEqual(0);
            expect(nodeIdToLevelMap.get('3')).toEqual(0);
            expect(nodeIdToLevelMap.get('4')).toEqual(1);
            expect(nodeIdToLevelMap.get('5')).toEqual(2);
            expect(nodeIdToLevelMap.get('6')).toEqual(1);
            expect(nodeIdToLevelMap.get('7')).toEqual(2);
            expect(nodeIdToLevelMap.get('8')).toEqual(2);
            expect(nodeIdToLevelMap.get('9')).toEqual(1);
        });

        it('should redirect to module reporting page', () => {
            const mockModules = [{ id: 1 }, { id: 2 }, { id: 3 }];
            spyOn(localStorage, 'setItem');
            spyOn(RouteBuilder, 'buildProjectRoute').and.returnValue('mockedRoute');
            component.dependencyGraph = { modules: mockModules };
            component.projectId = 1;
            component.redirectToModuleReporting();
            expect(localStorage.setItem).toHaveBeenCalled();
            expect(RouteBuilder.buildProjectRoute).toHaveBeenCalled();
        });

        it('nodes should be visible', () => {
            component.ngOnChanges();
            /* The graph is a filtered. If it contains all nodes it means they are visible. */
            expect(component.graphComponent.graph.nodes.size).toEqual(9);
        });
    })

    xdescribe('expandable graph', () => {
        const relationship = [
            {
                "uid": "#857:3",
                "customProperties": {},
                "id": 471,
                "relationship": "CALLS",
                "srcModule": 164,
                "dstModule": 192,
                "fromName": "MMRS7102",
                "toName": "MMRS71Z1",
                "fromModuleLocation": {
                    "offset": 9763,
                    "length": 106
                },
                "toModuleLocation": {
                    "offset": 0,
                    "length": 3048
                },
                "binding": "LATE",
                "properties": {
                    "CALL_TYPE": "CALL"
                }
            },
            {
                "uid": "#858:3",
                "customProperties": {},
                "id": 473,
                "relationship": "CALLS",
                "srcModule": 164,
                "dstModule": 192,
                "fromName": "MMRS7102",
                "toName": "MMRS71Z1",
                "fromModuleLocation": {
                    "offset": 10712,
                    "length": 106
                },
                "toModuleLocation": {
                    "offset": 0,
                    "length": 3048
                },
                "binding": "LATE",
                "properties": {
                    "CALL_TYPE": "CALL"
                }
            }
        ];
        beforeEach(() => {
            referenceControllerServiceSpy.findAllByFromAndToModuleIds.and.returnValue(of(relationship as any));
            const expandableGraph: DependencyGraph = {
                modules: [],
                references: [],
                moduleTypes: ['COBOL PROGRAM'],
                relationshipTypes: ['CALLS']
            };
            for (let index = 1; index <= 48; index++) {
                const module = {
                    id: index,
                    name: 'node-' + `${index}`,
                    technology: 'COBOL' as ModulePojo.TechnologyEnum,
                    type: 'PROGRAM' as ModulePojo.TypeEnum
                };
                expandableGraph.modules.push(module);
            }
            for (let fromIndex = 2; fromIndex <= 5; fromIndex++) {
                for (let toIndex = 6; toIndex <= 25; toIndex++) {
                    const link = { srcModule: `${fromIndex}`, dstModule: `${toIndex}`, relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum };
                    expandableGraph.references.push(link);
                }
            }
            const links = [
                { srcModule: '1', dstModule: '2', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '1', dstModule: '3', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '1', dstModule: '4', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '1', dstModule: '5', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '2', dstModule: '48', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '3', dstModule: '48', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '5', dstModule: '46', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
                { srcModule: '5', dstModule: '47', relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum },
            ];
            links.forEach(link => {
                expandableGraph.references.push(link);
            });
            for (let toIndex = 26; toIndex <= 45; toIndex++) {
                const link = { srcModule: '4', dstModule: `${toIndex}`, relationship: 'CALLS' as ModuleRelationshipPojo.RelationshipEnum };
                expandableGraph.references.push(link);
            }
            const graphInfo = GraphUtility.getYFilesGraphInfo(expandableGraph);
            graphInfo.nodes[0].isRoot = true;
            component.graphInfo = graphInfo;
            fixture.detectChanges();
        });

        it('should create expandable graph', () => {
            expect(component).toBeTruthy();
        });

        it('should test toggleSidePanel when sidePanelIconState is true and nodeEdgeClicked is node', () => {
            component.sidePanelIconState = true;
            component.nodeEdgeClicked = 'node';
            component.toggleSidePanel();
            expect(component.sidePanelIconState).toBe(false);
            expect(component.componentInstances.length).toBe(0);
        });
        
        it('should test toggleSidePanel when sidePanelIconState is false and nodeEdgeClicked is node', () => {
            const testModule: ModulePojo = {
                id: 2,
                name: 'test Module',
                projectId: 2,
                customProperties: {
                    "ModuleCustomProperties": [
                        {
                            "name": "customMetaInfo2",
                            "value": "another test value",
                            "dataType": "STRING"
                        },
                        {
                            "name": "customMetaInfo1",
                            "value": "some test value",
                            "dataType": "STRING"
                        }
                    ]
                }
            }
            component.sidePanelIconState = false;
            component.nodeEdgeClicked = 'node';
            component.moduleDetails = testModule;
            component.toggleSidePanel();
            expect(component.sidePanelIconState).toBe(true);
            expect(component.componentInstances.length).toBe(1);
        });

        it('should test toggleSidePanel when sidePanelIconState is false and nodeEdgeClicked is edge', () => {
            component.sidePanelIconState = false;
            component.nodeEdgeClicked = 'edge';
            component.toggleSidePanel();
            expect(component.sidePanelIconState).toBe(true);
            expect(component.componentInstances.length).toBe(1);
        });
    });

    xdescribe('large graphs', () => {
        beforeEach(() => {
            const largeGraphLinks: DependencyGraph = {
                modules: [],
                references: [],
                moduleTypes: [],
                relationshipTypes: []
            };
            for (let index = 0; index < 900; index++) {
                const module = {
                    id: index,
                    uid: index + '',
                    name: 'index ' + `${index}`,
                    technology: 'CSD' as ModulePojo.TechnologyEnum,
                    type: 'TRANSACTION' as ModulePojo.TypeEnum,
                    description: 'Node ABC'
                };
                const link = { srcModule: `${index}`, dstModule: '200' };
                largeGraphLinks.modules.push(module);
                largeGraphLinks.references.push(link);
                largeGraphLinks.moduleTypes = ['CSD TRANSACTION'];
                largeGraphLinks.relationshipTypes = ['CALLS']
            }

            const levelOneModule = {
                id: 900,
                uid: 900 + '',
                name: 'index 900',
                technology: 'CSD' as ModulePojo.TechnologyEnum,
                type: 'TRANSACTION' as ModulePojo.TypeEnum,
                description: 'Node ABC'
            };
            const levelOneLink = { srcModule: '0', dstModule: '900' };
            largeGraphLinks.modules.push(levelOneModule);
            largeGraphLinks.references.push(levelOneLink);

            for (let index = 0; index < 10; index++) {
                const lazyModule = {
                    id: index + 901,
                    uid: (index + 901) + '',
                    name: 'index ' + `${index + 901}`,
                    technology: 'CSD' as ModulePojo.TechnologyEnum,
                    type: 'TRANSACTION' as ModulePojo.TypeEnum,
                    description: 'Node ABC'
                };
                const lazyLink = { srcModule: `${index + 901}`, dstModule: '900' };
                largeGraphLinks.modules.push(lazyModule);
                largeGraphLinks.references.push(lazyLink);
            }

            const immediateExpandModule = {
                id: 920,
                uid: '920',
                name: 'index 920',
                technology: 'CSD' as ModulePojo.TechnologyEnum,
                type: 'TRANSACTION' as ModulePojo.TypeEnum,
                description: 'Node ABC'
            };
            const immediateExpandLink = { srcModule: '300', dstModule: '920' };
            largeGraphLinks.modules.push(immediateExpandModule);
            largeGraphLinks.references.push(immediateExpandLink);
            component.layout = 'HIERARCHIC_LAYOUT' as LayoutType;
            const graphInfo = GraphUtility.getYFilesGraphInfo(largeGraphLinks);
            graphInfo.nodes[0].isRoot = true;
            component.graphInfo = graphInfo;
            fixture.detectChanges();
            component.ngOnChanges();
        });

        it('should create large graph', () => {
            const filteredGraph = component.graphComponent.graph as FilteredGraphWrapper;
            expect(GraphUtility.isLarge(filteredGraph.wrappedGraph)).toBeTruthy();
        });

        it('should disable layout options', () => {
            const filteredGraph = component.graphComponent.graph as FilteredGraphWrapper;
            (component as any).disableLayoutOptions(filteredGraph.wrappedGraph);
            const layoutOptions = component.layoutItems as MiningSelectOption[];

            layoutOptions.forEach(layoutOption => {
               const value = layoutOption.value;
               fixture.detectChanges();
               switch (value) {
                case 'HIERARCHIC_LAYOUT':
                case 'HIERARCHIC_LAYOUT_LEFT_TO_RIGHT':
                case 'RADIAL_LAYOUT':
                    expect(layoutOption.disabled).withContext(`layout option ${value} should be disabled`).toBeTrue();
                    break;
                default:
                    expect(layoutOption.disabled).withContext(`layout option ${value} should be enabled`).toBeFalse();
               }
            });

            expect(layoutOptions.length).withContext('number of layout options').toBeGreaterThanOrEqual(6);
        });

        it('should expand all the nodes', async () => {
            const filteredGraph = component.graphComponent.graph as FilteredGraphWrapper;
            expect(filteredGraph.nodes.size).toBe(912);
            expect(component.graphComponent.graphModelManager).toBeTruthy();
        });

        it('should open context menu on node click', async () => {
            const filteredGraph = component.graphComponent.graph as FilteredGraphWrapper;
            const nonExpandibleNode: INode = filteredGraph.nodes.get(1);
            const clickItem = {
                item: nonExpandibleNode
            };
            await (component as any).getSingleClickExpandListener()(null, (clickItem as any));
            expect(component.displayModal).toBeFalsy();
        });
    });

    xit('should render nodes with proper shape, fill and stroke', () => {
        const rootNode = component.graphComponent.graph.nodes.toArray().find(
            (node: INode) => node.tag.id === 100
        );
        const childNode = component.graphComponent.graph.nodes.toArray().find(
            (node: INode) => node.tag.id !== 100
        );
        const expectedNodeStyle = GraphGlobalStyles.getShapeNodeStyle(childNode.tag.style.color, DEFAULT_NODE_SHAPE, DEFAULT_STROKE_THICKNESS);
        const realStyle = childNode.style as ShapeNodeStyle;
        expect(realStyle.shape).toBe(expectedNodeStyle.shape);
        expect((realStyle.fill as SolidColorFill).color).toEqual((expectedNodeStyle.fill as SolidColorFill).color, 'fill not same');
        expect((realStyle.stroke.fill as SolidColorFill).color).toEqual((expectedNodeStyle.stroke.fill as SolidColorFill).color, 'stroke color not same');
        expect(realStyle.stroke.thickness).toEqual(expectedNodeStyle.stroke.thickness, 'thickness not same');
        /* check for root node */
        const expectedRootStyle = GraphGlobalStyles.getShapeNodeStyle(childNode.tag.style.color, DEFAULT_NODE_SHAPE, SELECTED_NODE_STROKE_THICKNESS);
        expect((rootNode.style as ShapeNodeStyle).stroke.thickness).toEqual(expectedRootStyle.stroke.thickness, 'root node thickness not right');
    });

    xit('should render nodes with proper icon', () => {
        const rootLabel = component.graphComponent.graph.labels.toArray().find(
            (label: ILabel) => label.owner.tag.id === 100
        );
        expect((rootLabel.style as IconLabelStyle).icon).toEqual('/assets/icons/32x32_csd_exectransaction.png', 'icon is wrong');
    });

    xit('should change depth', () => {
        spyOn(component.depthChange, 'emit');
        component.graphFilter.setValue({ typeControl: [], depthControl: 2, selectedLayout: 'HIERARCHIC_LAYOUT' });
        component.changeDepth();
        expect(component.depthChange.emit).toHaveBeenCalledWith(2);
    });

    xit('should change layout', () => {
        spyOn(component.layoutChange, 'emit');
        component.graphFilter.setValue({ typeControl: [], depthControl: 1, selectedLayout: 'ORGANIC_LAYOUT' });
        component.changeLayout();
        expect(component.layoutChange.emit).toHaveBeenCalledWith('ORGANIC_LAYOUT');
    });

    xit('should filter graph', () => {
        spyOn(component.filterChange, 'emit');
        const expectedFilterParams: FilterParameters = {
            moduleFilter: ['JCL_PROC', 'ASSEMBLER_ASSEMBLER'],
            relationshipFilter: []
        };
        component.graphFilter.controls[`typeControl`].markAsDirty();
        component.graphFilter.setValue({
            typeControl: [component.filterOptions[0].value, component.filterOptions[3].value],
            depthControl: 2,
            selectedLayout: 'HIERARCHIC_LAYOUT'
        });
        component.filterGraph();
        expect(component.filterChange.emit).toHaveBeenCalledWith(expectedFilterParams);
    });

    xit('should provide image url for multiselect', () => {
        expect(component.getImagePath('CALLS')).toBe('');
        expect(component.getImagePath('NATURAL PROGRAM')).toBe(NodeImages['NATURAL PROGRAM']);
        expect(component.getImagePath('UNKNOWN UNKNOWN')).toBe(NodeImages.GENERIC);
    });

    xit('should give image style', () => {
        const color: NodeColor = NODE_COLORS.NATURAL;
        const validStyle = {
            'border': '1px solid ' + color.strokeColor,
            'background': color.fillColor
        };
        expect(component.getStyleForImage('CALLS')).toEqual({});
        expect(component.getStyleForImage('NATURAL PROGRAM')).toEqual(validStyle);
    });

    xit('should get tool-tip for node', () => {
        const hoverItemWithDesc = {
            item: component.graphComponent.graph.nodes.first(),
            handled: false,
            toolTip: ''
        };
        hoverItemWithDesc.item.tag['uid'] = 100;
        (component as any).getNodeToolTipListener()(null, (hoverItemWithDesc as any));
        expect(hoverItemWithDesc.handled).toBeTruthy();
        expect(hoverItemWithDesc.toolTip).toBe('Node ABC');

        const hoverItemWithoutDesc = {
            item: component.graphComponent.graph.nodes.get(1),
            handled: false,
            toolTip: ''
        };
        hoverItemWithoutDesc.item.tag['uid'] = 100;
        (component as any).getNodeToolTipListener()(null, (hoverItemWithoutDesc as any));
        expect(hoverItemWithoutDesc.handled).toBeTruthy();
        expect(hoverItemWithoutDesc.toolTip).toBe('dependencyGraph.nodeTooltipNotProvided');
    });

    xit('should return label for filter options', () => {
        component.graphInfo.nodeTypes = ['JCL PROC' as NodeType, 'ASSEMBLER ASSEMBLER' as NodeType, 'UNKNOWN UNKNOWN' as NodeType];
        component.graphInfo.relationships = ['CALLS' as NodeType, 'READ_WRITES' as NodeType];
        component.filterOptions = [];
        (component as any).createForm();
        const expectedLabels = ['JCL PROC', 'ASSEMBLER', 'UNKNOWN', 'CALLS', 'READ_WRITES'];
        const actualLabels = component.filterOptions.map(nodeType => nodeType.label);
        expect(actualLabels).toEqual(expectedLabels);
    });

    xit('should return label values for filter options', () => {
        const filterParams: FilterParameters = {
            moduleFilter: ['JCL_PROC', 'ASSEMBLER_ASSEMBLER', 'UNKNOWN_UNKNOWN'],
            relationshipFilter: ['CALLS', 'READ_WRITES']
        };
        const expectedValues = ['JCL PROC', 'ASSEMBLER ASSEMBLER', 'UNKNOWN UNKNOWN', 'CALLS', 'READ_WRITES'];
        const actualValues = getValuesFromUncheckedOptions(filterParams);
        expect(actualValues).toEqual(expectedValues);

        const emptyFilterParams: FilterParameters = {
            moduleFilter: [],
            relationshipFilter: []
        };
        const actualEmptyValues = getValuesFromUncheckedOptions(emptyFilterParams);
        expect(actualEmptyValues).toEqual([]);
    });

    xit('should check if filter is dirty', () => {
        component.graphFilter.setValue({
            typeControl: [component.filterOptions[0].label, component.filterOptions[3].label],
            depthControl: 2,
            selectedLayout: 'HIERARCHIC_LAYOUT'
        });
        component.graphFilter.markAsDirty();

        component.setOptionsBeforeChange();
        expect(component.isFilterDirty()).toBeFalsy();

        component.graphFilter.markAsPristine();
        component.graphFilter.setValue({
            typeControl: [component.filterOptions[0].label],
            depthControl: 2,
            selectedLayout: 'HIERARCHIC_LAYOUT'
        });
        expect(component.isFilterDirty()).toBeTruthy();

        component.graphFilter.setValue({
            typeControl: [component.filterOptions[3].label, component.filterOptions[0].label],
            depthControl: 2,
            selectedLayout: 'HIERARCHIC_LAYOUT'
        });
        expect(component.isFilterDirty()).toBeFalsy();

        component.graphFilter.setValue({
            typeControl: [component.filterOptions[3].label, component.filterOptions[1].label],
            depthControl: 2,
            selectedLayout: 'HIERARCHIC_LAYOUT'
        });
        expect(component.isFilterDirty()).toBeTruthy();

        component.filterOptionsBeforeChange = null;
        expect(component.isFilterDirty()).toBeTruthy();
    });

    xdescribe('graph with artificial edges', () => {
        beforeEach(() => {
            const ARTIFICIAL_GRAPH_LINKS: DependencyGraph = {
                modules: [
                    {
                        id: 100,
                        uid: '100',
                        name: 'ABC',
                        technology: 'CSD',
                        type: 'TRANSACTION',
                        description: 'Node ABC'
                    },
                    {
                        id: 200,
                        uid: '200',
                        name: 'BCD',
                        technology: 'JCL',
                        type: 'PROC'
                    },
                    {
                        id: 300,
                        uid: '300',
                        name: 'CDE',
                        technology: 'ASSEMBLER',
                        type: 'ADAPTVIEW'
                    }
                ],
                references: [
                    { srcModule: '100', dstModule: '200', relationship: 'ARTIFICIAL' as ModuleRelationshipPojo.RelationshipEnum },
                    { srcModule: '200', dstModule: '300', relationship: 'ARTIFICIAL' as ModuleRelationshipPojo.RelationshipEnum },
                    { srcModule: '300', dstModule: '100', relationship: 'ARTIFICIAL' as ModuleRelationshipPojo.RelationshipEnum }
                ],
                moduleTypes: [
                    'CSD TRANSACTION',
                    'JCL PROC',
                    'ASSEMBLER ASSEMBLER'
                ],
                relationshipTypes: []
            };
            component.layout = 'HIERARCHIC_LAYOUT' as LayoutType;
            const graphInfo = GraphUtility.getYFilesGraphInfo(ARTIFICIAL_GRAPH_LINKS);
            graphInfo.nodes[0].isRoot = true;
            component.graphInfo = graphInfo;
            fixture.detectChanges();
        });

        it('should create graph with artificial edges', () => {
            component.ngOnChanges();
            const actualStyle = component.graphComponent.graph.edges.first().style as PolylineEdgeStyle;
            const actualFillColor = (actualStyle.stroke.fill as any).color;
            expect(actualFillColor.r).toBe(0);
            expect(actualFillColor.g).toBe(0);
            expect(actualFillColor.b).toBe(255);
        });
    });

    xit('should open module in new browser tab', () => {
        component.selectedNode = { tag: { id: 2 } };
        component.openInNewBrowserTab('details/overview');
        expect(openedUrl).toContain('details/overview');
    });

    xit('should open module in eclipse', () => {
        component.selectedNode = { tag: { id: 2 } };
        spyOn((component as any).deepLinkService, 'showModuleInEclipse');
        component.openInEclipse();
        expect((component as any).deepLinkService.showModuleInEclipse).toHaveBeenCalledWith({ id: 2, path: 'test' });
    });

    xit('should hide node', () => {
        const visibilitySpy = jasmine.createSpy('setNodeVisibility');
        spyOnProperty(dependencyUtility, 'setNodeVisibility').and.returnValue(visibilitySpy);
        const node = component.graphComponent.graph.nodes.toArray().find(
            (node: INode) => node.tag.id !== 100
        );
        component.contextMenuHideNode({ node, visibility: false });
        expect(visibilitySpy).toHaveBeenCalled();
    });

    xit('should call methods on type filter opening and close', () => {
        spyOn((component), 'setOptionsBeforeChange');
        spyOn((component), 'filterGraph');

        component.filterTypeOpen(true);
        expect((component).setOptionsBeforeChange).toHaveBeenCalled();

        component.filterTypeOpen(false);
        expect((component).filterGraph).toHaveBeenCalled();
    });

    xit('should check onHoverItem when node clicked', () => {
        spyOn(component, 'onHoveredItemChanged').and.callThrough();
        spyOn(component, 'addHighlights').and.callThrough();
        component.selectedNode = { tag: { id: 2 } };
        const node: INode = component.getSelectedNode();
        component.onHoveredItemChanged(node, 'click');
        expect(component.addHighlights).toHaveBeenCalled();
    });

    xit('should check onHoverItem when empty', () => {
        spyOn(component, 'onHoveredItemChanged').and.callThrough();
        spyOn(component.graphComponent.highlightIndicatorManager, 'clearHighlights');
        component.onHoveredItemChanged(null, 'empty');
        expect(component.graphComponent.highlightIndicatorManager.clearHighlights).toHaveBeenCalled();
    });

    xit('should check when item is hovered', () => {
        spyOn(component, 'onHoveredItemChanged').and.callThrough();
        spyOn(component.graphComponent.highlightIndicatorManager, 'clearHighlights');
        spyOn(component, 'addHighlights').and.callThrough();
        component.onHoveredItemChanged(null, 'hover');
        expect(component.addHighlights).toHaveBeenCalled();
        expect(component.graphComponent.highlightIndicatorManager.clearHighlights).toHaveBeenCalled();
    });
});
