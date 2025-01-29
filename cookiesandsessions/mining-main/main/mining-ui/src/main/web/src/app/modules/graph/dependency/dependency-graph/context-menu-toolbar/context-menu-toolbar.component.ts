import {
    GraphComponent,
    ExteriorLabelModel,
    ExteriorLabelModelPosition,
    Point,
    INode,
    SimpleNode,
    SimpleLabel,
    Size,
    MutableRectangle,
    IOrientedRectangle,
    FilteredGraphWrapper,
    IEdge,
    IListEnumerable,
    GraphStructureAnalyzer,
    IGraph,
    ILabelModelParameter,
    IPort,
    IEnumerable,
    ListEnumerable,
    IModelItem
} from 'yfiles';
import {
    Component,
    Input,
    AfterViewInit,
    EventEmitter,
    Output,
    ViewChildren,
    QueryList,
    ElementRef,
    OnDestroy,
    ChangeDetectionStrategy,
    Optional,
    OnInit,
    ViewChild,
    TemplateRef
} from '@angular/core';
import { ContextToolbarType, EdgeOptions, FilterOptions, NodeVisibility, RemoveBranchType } from './context-menu-options';
import { last, Observable, Subscription } from 'rxjs';
import { GraphUtility } from '../../utils/dependency-graph-utility';
import { DependencyGraphComponent } from '../dependency-graph.component';
import { ModuleControllerService, ModulePojo, ReachabilityAnalysisRequest } from '@innowake/mining-api-angular-client';
import { ReachabilityGraphComponent } from '@app/modules/graph/reachability/reachability-graph/reachability-graph.component';
import { ActivatedRoute, Router } from '@angular/router';
import { NzDrawerRef, NzDrawerService } from 'ng-zorro-antd/drawer';
import { ReachabilityService } from '@app/modules/reachability-ui-product-vision/utils/reachability.service';
import { BlockViewDetailsComponent } from '@app/modules/reachability-ui-product-vision/block-view/view-block-details/block-view-details.component';
import { FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { MergeBlockData, ReachabilityBlocks, ReachabilityBlockState } from '@app/modules/reachability-ui-product-vision/utils/reachability-interface';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
const FilterTypeEnum = {
    ALL : 'all',
    INCOMING: 'incoming',
    OUTGOING: 'outgoing'
};
@Component({
    changeDetection: ChangeDetectionStrategy.OnPush,
    selector: 'graph-context-menu',
    templateUrl: './context-menu-toolbar.component.html'
})
export class ContextualToolbarComponent implements AfterViewInit, OnDestroy, OnInit {
    @Input() eclipseView = false;
    @Input() graphDepth: string;
    @Input() isEclipseFeatureActive: boolean;
    @Input() graphRootNode: Observable<INode>;
    @Input() resetFilterOptions: Observable<boolean>;
    @Input() resetGraph: Observable<boolean>;
    @Input() moduleDetails: ModulePojo;
    @Input() disableCodeViewer: boolean;
    @Input() codeViewerTooltip: string;
    @Input() projectId: number;
    @Input() contextMenuOptionType: string[] = [ContextToolbarType.DEFAULT];
    @Input() dependencyGraphComponent: DependencyGraphComponent | ReachabilityGraphComponent;
    @Input() explorable: boolean;
    @Output() openInNewBrowserTab: EventEmitter<string> = new EventEmitter();
    @Output() openInEclipse: EventEmitter<any> = new EventEmitter();
    @Output() contextMenuHideNode: EventEmitter<NodeVisibility> = new EventEmitter();
    @Output() toggleDPSidePanel: EventEmitter<boolean> = new EventEmitter();
    @Output() updateBlockStatus = new EventEmitter<boolean>();
    @Output() removeBranch = new EventEmitter<string>();
    @ViewChildren('graph') toolbarItem: QueryList<ElementRef>;
    @ViewChildren('optionsContainer') optionsContainer: QueryList<ElementRef>;
    @ViewChild('drawerTitleTemplate', { static: false }) drawerTitleTemplate?: TemplateRef<any>;

    selectedItem: INode;
    graphComponent: GraphComponent;
    container: HTMLElement;
    nodeLabelModelParameter: any;
    dirty: boolean;
    showToolbar = false;
    isPickerBottom = false;
    rootNode: INode;
    nodeMap: Map<string, IEdge[]> = new Map();
    incomingEdges: Map<number, Map<string, FilterOptions>> = new Map();
    outgoingEdges: Map<number, Map<string, FilterOptions>> = new Map();
    eventsSubscriptions = new Subscription();
    ContextToolbarMenuType = ContextToolbarType;
    removeBranchType = RemoveBranchType;
    canEditRB= false;
    selectedBranch: IEdge;
    selectedNodesArray: IModelItem[] = [];
    blockData: ReachabilityBlocks;
    blockState: ReachabilityBlockState;

    constructor(
        private moduleControllerService: ModuleControllerService,
        private router: Router,
        private nzDrawerService: NzDrawerService,
        @Optional() private drawerRef: NzDrawerRef<any>,
        private reachabilityService: ReachabilityService,
        private translateService: TranslateService,
        private functionalBlockControllerService: FunctionalBlockControllerService,
        private messageService: NzMessageService,
        private jobManagerService: JobManagerService,
        public route: ActivatedRoute
    ) {
    }

    /**
     * Gets the items to display information for.
     */
    get selectedNode(): INode {
        return this.selectedItem;
    }

    /**
     * Gets the items to display information for.
     */
     get selectedBranchToRemove(): IEdge {
        return this.selectedBranch;
    }

    /**
     * Gets the items to display information for.
     */
    get selectedNodes(): IModelItem[] {
        return this.selectedNodesArray;
    }

    /**
     * Sets the graph component where the context menu is to be displayed.
     *
     * @param graphComponent - graphComponent instance.
     */
    @Input() set graphComp(graphComponent: GraphComponent) {
        this.graphComponent = graphComponent;
        this.registerUpdateListeners();
    }
    /**
     * Sets the items to display the contextual toolbar for.
     * Setting this property to a value other than null r empty shows the toolbar.
     * Setting the property to null or empty hides the toolbar.
     */
    @Input() set selectedNode(node: INode) {
        if ( ! node) {
            this.hide();
            return;
        }
        if (node.tag.info?.isClue) {
            return;
        }
        this.selectedItem = node;
        if (this.selectedNodes.length <= 1) {
            this.selectedNodes = [node];
        }
        if ( ! this.explorable) {
            this.graphComponent.graph.nodes.forEach(item => {
                this.setEdgesOptionForToolbar(item);
            });
            /* Sets the state(checked, unchecked, indeterminate) for the checkbox for incoming/outgoing edge options.*/
            this.setOptionCheckboxState(node, FilterTypeEnum.ALL);
        }
    }

    /**
     * Sets the nodes and display the contextual toolbar for the selected nodes.
     * Hides the toolbar if nothing is selected.
     */
    @Input() set selectedNodes(array: IModelItem[]) {
        if (!array || array.length === 0) {
            this.hide();
        } else {
            this.showToolbar = false;
            this.selectedNodesArray = array;
            this.show();
        }
    }

    @Input() set selectedBranchToRemove(selectedBranch: IEdge) {
        this.showToolbar = false;
        if ( ! selectedBranch) {
            this.hide();
            return;
        }
        this.selectedBranch = selectedBranch;
        if (this.selectedBranch instanceof IEdge && this.selectedBranch.sourceNode) {
            this.showToolbar = true;
        }
    }


    ngOnInit(): void {
        this.canEditRB = JSON.parse(localStorage.getItem(`${this.projectId}-updateRB`));
    }

    setDependenciesForToolbar(event: MouseEvent): void {
        if (this.explorable) {
            const graph = (this.dependencyGraphComponent.graphComponent.graph as FilteredGraphWrapper).wrappedGraph;
            this.moduleControllerService.traverseModuleDependencies(this.projectId, this.selectedItem.tag.id as number,
                +this.graphDepth, GraphUtility.MAX_GRAPH_NODES, undefined, true)
                .subscribe(response => {
                    const graphInfo = GraphUtility.getYFilesGraphInfo(response);
                    if (response.modules.length) {
                        this.dependencyGraphComponent.updateGraph(graphInfo);
                        graph.nodes.forEach(item => {
                            if (graphInfo.nodes.find(createdNode => item.tag.id === createdNode.id)) {
                                this.setEdgesOptionForToolbar(item, false);
                            } else {
                                this.setEdgesOptionForToolbar(item);
                            }
                        });
                    }
                });
        }
        this.showPickerContainer(event);
    }

    /**
     * Helper function to show picker container and hide a node.
     *
     * @param event - MouseEvent.
     */
    hideNode(event: MouseEvent): void {
        this.showPickerContainer(event);
        this.toggleEdgeVisibility(FilterTypeEnum.ALL.toLocaleUpperCase(), FilterTypeEnum.INCOMING, true);
        this.toggleEdgeVisibility(FilterTypeEnum.ALL.toLocaleUpperCase(), FilterTypeEnum.OUTGOING, true);
    }

    /**
     * Toggles the visibility of the Edges of the selected node by calling setEdgeVisibility.
     * @param edgeRelation relationship of edge which need to be show/hide.
     * @param edgeType type of the edge incoming/outgoing needs to be show/hide.
     * @param hideNode Optional parameter to set visibility in case of node.
     */
    toggleEdgeVisibility(edgeRelation: string, edgeType: string, hideNode?: boolean): void {
        let edgesArr: any;
        let edgeOption: Map<string, FilterOptions>;

        if (edgeType === FilterTypeEnum.INCOMING) {
            edgeOption = this.incomingEdges.get(this.selectedNode.tag.id as number);
            edgesArr = this.getNodeMapEdges(this.selectedNode, 'inEdges');
        } else {
            edgeOption = this.outgoingEdges.get(this.selectedNode.tag.id as number);
            edgesArr = this.getNodeMapEdges(this.selectedNode, 'outEdges');
        }

        const edgeOptionVal = edgeOption.get(edgeRelation);
        const visibility = hideNode ? false : edgeOptionVal?.checked;
        let size: number;
        edgesArr.forEach((item: IEdge) => {
            if (item.tag.relationship === edgeRelation || edgeRelation === FilterTypeEnum.ALL.toLocaleUpperCase()) {
                item.tag.filtered = ! visibility;

                const node = edgeType === FilterTypeEnum.INCOMING ? item.sourceNode : item.targetNode;
                size = this.graphComponent.graph.edgesAt(node).size;
                if (size <= 1 && this.isNotRootNode(node)) {
                    this.contextMenuHideNode.emit({ node, visibility });
                }
            }
        });

        this.predicateGraphChange(this.graphComponent.graph);

        /* Set the state of the selected node options. */
        if (this.explorable) {
            this.setEdgesOptionForToolbar(this.selectedNode);
        } else {
            this.setOptionCheckboxState(this.selectedNode, edgeType);
        }

        size = this.graphComponent.graph.edgesAt(this.selectedNode).size;
        if (size === 0 && this.isNotRootNode(this.selectedNode)) {
            this.contextMenuHideNode.emit({ node: this.selectedNode, visibility: false });
            this.predicateGraphChange(this.graphComponent.graph);
        }
        this.checkConnected(edgesArr as IListEnumerable<IEdge>, edgeType);
        if (this.explorable) {
            void this.dependencyGraphComponent.runLayout(this.dependencyGraphComponent.graphComponent,
                this.selectedItem, true, false).then(() => this.dependencyGraphComponent.updateVisualClues());
        }
    }

    /**
     * Checks if the graph is connected or not.
     * @returns boolean value.
     */
    isGraphConnected(): boolean {
        const graphAnalyzer = new GraphStructureAnalyzer(this.graphComponent.graph);
        return graphAnalyzer.isConnected();
    }

    /**
     * Initializes the context menu toolbar.
     */
    ngAfterViewInit(): void {
        this.container = window.document.getElementById('contextualToolbar');
        // initialize a label model parameter that is used to position the node pop-up
        const nodeLabelModel = new ExteriorLabelModel({ insets: 10 });
        this.nodeLabelModelParameter = nodeLabelModel.createParameter(ExteriorLabelModelPosition.NORTH);
        this.dirty = false;
        this.eventsSubscriptions.add(this.graphRootNode.subscribe(node => this.rootNode = node));
        this.eventsSubscriptions.add(this.resetGraph.subscribe(() => {
            /* Reset existing previous-graph data */
            this.nodeMap.clear();
            this.incomingEdges.clear();
            this.outgoingEdges.clear();
        }));
        this.eventsSubscriptions.add(this.resetFilterOptions.subscribe(() => this.nodeMap.clear()));
    }


    /**
     * method to see the details of RB
     */
    viewRBDetails(): void {
        const selectedNodeTag = this.selectedNode.tag;
        const blockDetails = {
            uid: selectedNodeTag.uid,
            name: selectedNodeTag.name,
            description: selectedNodeTag.description,
            type: selectedNodeTag.info.TYPE,
            outdatedModule: selectedNodeTag.info.OUTDATED
        };
        this.drawerRef = this.nzDrawerService.create({
            nzTitle: this.drawerTitleTemplate,
            nzContent: BlockViewDetailsComponent,
            nzWrapClassName: 'block-view__side-drawer',
            nzContentParams: {
                blockDetails,
                projectId: this.projectId,
                canEditRB: this.canEditRB
            },
            nzWidth: '70vw',
            nzPlacement: 'right',
            nzClosable: true,
            nzMaskClosable: false
        });
        this.drawerRef.afterClose.subscribe(() => {
            if (this.reachabilityService.getUpdateBlocks()) {
                this.reachabilityService.setUpdateGraph(true);
                this.reachabilityService.setUpdateBlocks(false);
            }
        });
    }

    /**
     * Opens the TableView for the selected RB node
     */
    openRBTableView(isMultipleTable: boolean): void {
        let uIds: string[] = [this.selectedNode.tag.uid];
        let selectedBlocks: Array<{[key: string]: any}> = [];
        if(isMultipleTable) {
            // @yjs:keep
            localStorage.removeItem('ModulesWithErrorsAndWarnings');
            uIds = this.selectedNodes.map(node => node.tag.uid);
            selectedBlocks = this.selectedNodes.map(node => node.tag);
            this.reachabilityService.storeReachabilityDetails(this.selectedNode.tag as  {[key: string]: any}, this.projectId, false, selectedBlocks);
        }
        localStorage.setItem(`${this.projectId}-reachabilityIds`, JSON.stringify(uIds));
        void this.router.navigate(['/project-' + this.projectId + '/reachability/table']);
    }

    /**
     * Opens merge modal and merge the selected RB node
     */
    mergeRB(): void {
        this.reachabilityService.openModalToMergeReachabilityBlocks().afterClose.subscribe((data: MergeBlockData) => {
            const uids = this.selectedNodes.map(node => node.tag.uid);
            this.reachabilityService.mergeReachabilityBlocks(data, this.projectId, this.dependencyGraphComponent.networkUid, uids as string[]);
            this.reachabilityService.getMergeStatus().subscribe((status: string) => {
                if(status === this.translateService.instant('messageService.success')) {
                    this.reachabilityService.setUpdateGraph(true);
                }
            });
        });
    }

    /**
     * Removes only the current branch from the dependency graph.
     */
    removeBranches(removeType: string): void {
        this.removeBranch.emit(removeType);
    }

    /**
     * Opens the Graph for the selected RB node
     */
    openRBGraph(): void {
        void this.router.navigate(['/project-' + this.projectId + '/reachability/' + this.selectedNode.tag.uid + '/graph'],
        { queryParams: { type: this.selectedNode.tag.info.TYPE, name: this.selectedNode.tag.name } });
    }

    /**
     * Opens the modules table for a given tag.
     * @param tag The tag to open the modules table for error/warnings.
     */
    openModulesTable(tag: string): void {
        this.reachabilityService.openModulesTable(tag, this.projectId);
    }

    /**
     * It will get block details for the selected node and open the view as per the viewType.
     * @param isMultiple if multiple node(blocks) selected then true else false
     * @param viewType which view to open graph or table or details
     */
    getReachabilityDetails(isMultiple: boolean, viewType: string): void {
        if (isMultiple) {
            this.openRBTableView(isMultiple);
        } else {
            const selectedBlockId = this.selectedNode?.tag?.uid;
            if (this.blockData?.uid !== selectedBlockId) {
                const blockId = JSON.stringify(selectedBlockId);
                this.reachabilityService.getReachabilityBlockDetails(this.projectId, blockId).subscribe((blockDetails: ReachabilityBlocks) => {
                    if (blockDetails) {
                        this.blockData = blockDetails;
                        this.blockState = this.reachabilityService.getBlockState(this.blockData.resolvedModuleParts);
                        this.reachabilityService.storeReachabilityDetails(this.blockData, this.projectId, false);
                        this.openRBViewBasedOnType(viewType);
                     }
                 });
            } else {
                this.openRBViewBasedOnType(viewType);
            }
        }
    }

    openRBViewBasedOnType(viewType: string): void {
        if (viewType === 'graph') {
            this.openRBGraph();
        } else if (viewType === 'table') {
            this.openRBTableView(false);
        } else {
            this.viewRBDetails();
        }
    }

    /**
     * Helper function to show/hide a picker container.
     * @param event - MouseEvent.
     */
    showPickerContainer(event: MouseEvent): void {
        const toggleButton = event.target as HTMLInputElement;
        const pickerContainer = document.getElementById(toggleButton.getAttribute('data-container-id'));
        this.hideAllPickerContainer(toggleButton, pickerContainer);

        // position the container above/below the toggle button
        if (pickerContainer) {
            pickerContainer.style.display = 'block';
            const labelElement = document.querySelector(`label[for="${toggleButton.id}"]`);
            const labelBoundingRect = labelElement.getBoundingClientRect();
            const toolbarClientRect = this.container.getBoundingClientRect();
            const pickerClientRect = pickerContainer.getBoundingClientRect();
            const padding = 20;
            pickerContainer.style.left = `${labelBoundingRect.left +
                labelBoundingRect.width / 2 -
                pickerContainer.clientWidth / 2 -
                toolbarClientRect.left}px`;
            const gcAnchor = this.graphComponent.toPageFromView(new Point(0, 0));
            if (toolbarClientRect.top - gcAnchor.y < pickerClientRect.height + padding) {
                this.isPickerBottom = true;
            } else {
                pickerContainer.style.top = `-${pickerClientRect.height + 12}px`;
                this.isPickerBottom = false;
            }

            // timeout the fading animation to make sure that the element is visible
            setTimeout(() => {
                pickerContainer.style.opacity = '1';
            }, 0);
        }
    }

    /**
     * Checks if option for the incoming/outgoing edge available or not.
     * @params type the type of the edge.
     * @returns boolean value after checking for the options.
     */
    isOptionDisabled(type: string): boolean {
        const edges = type === FilterTypeEnum.INCOMING ? this.incomingEdges : this.outgoingEdges;
        return ! (this.selectedItem && edges.get(this.selectedItem.tag.id as number)?.size) && ! this.explorable;
    }

    /**
     * Checks if the passed node is root node or not.
     * @param node node to be checked.
     * @returns boolean value
     */
    isNotRootNode(node?: INode): boolean {
        if (node) {
            return node.tag.id !== this.rootNode.tag.id;
        } else if (this.selectedNode) {
            return this.selectedNode.tag.id !== this.rootNode.tag.id;
        }
    }

    /**
     * Method to toggle side panel
     */
    toggleSidePanel(): void {
        this.toggleDPSidePanel.emit();
    }

    ngOnDestroy(): void {
        this.drawerRef?.close();
        this.eventsSubscriptions.unsubscribe();
    }

   /**
    * Method to update the status of block
    * @param status is current status of the block
    */
    updateBlock(status: 'ACTIVE' | 'INACTIVE'): void {
        this.reachabilityService.updateBlockStatus(this.projectId, status, [this.selectedNode.tag.uid] as string[],
            this.translateService.instant('reachability.inactiveSuccess') as string,
            () => {
            this.drawerRef?.close();
            this.updateBlockStatus.emit(true);
            this.reachabilityService.setUpdateGraph(true);
        }, () => {
            this.drawerRef?.close();
        });
    }

    /**
     * Method to use JSON.stringify on string
     * @param data string to check block/network graph.
     */
    stringifyData(data: string): string | string[] {
        const uIds = this.selectedNodes.map(node => node.tag.uid);
        // this is because incase of block graph, selected item uid is different and filter will not be applied in FA.
        if (data === 'block') {
            // Sending both Ids and name as Ids will be used in graphQl filter & name to display.
            return JSON.stringify({ moduleIds: [this.selectedItem?.tag?.id], moduleName: [this.selectedItem?.tag?.name] });
        } else {
            return JSON.stringify({ reachabilityIds: uIds });
        }
    }

    /**
     * Method to recalculate the reachability analysis for the selected functional block In network View.
     */
    recalculateReachabilityAnalysis(): void {
        const reachabilityAnalysisRequest: ReachabilityAnalysisRequest = {
            analysisType: ReachabilityAnalysisRequest.AnalysisTypeEnum.TOP_DOWN,
            moduleTaxonomies: new Set([]),
            functionalBlockUids: new Set(this.selectedNodes.map(node => node.tag.uid)),
            recalculateAll: true
        };
        this.functionalBlockControllerService.executeReachabilityAnalysis(this.projectId, reachabilityAnalysisRequest).subscribe((result: string[]) => {
            if (result) {
                const remoteJob = {
                    jobId: result as unknown as string,
                    autoDownloadResult: false,
                    foreground: true,
                    cancellable: true
                };
                this.jobManagerService.register(remoteJob).status$.pipe(last()).subscribe(() => {
                    this.reachabilityService.setUpdateGraph(true);
                    this.reachabilityService.setNotifyAlertBannerForOutdatedOrRemovedRb(true);
                });
            }
          }, () => {
            const errorContent: string = this.translateService.instant('reachability.errorExecutingFB');
            this.messageService.error(errorContent);
          });
    }

    /**
     * Sets the incoming/Outgoing edge options for the toolbar and store the data
     * for the selected node to the {nodeMap} if not present already.
     * @param node the selected node
     * @param visibility initial state of the options.
     */
    private setEdgesOptionForToolbar(node: INode, visibility: boolean = true) {
        if ( ( ! this.explorable && this.nodeMap.has(node.tag.id + 'inEdges')) || node.tag.info?.isClue) {
            return;
        }
        const fg = this.explorable ? this.dependencyGraphComponent.graphComponent.graph as FilteredGraphWrapper
        : this.graphComponent.graph as FilteredGraphWrapper;
        const unwrapped = this.explorable ? fg.wrappedGraph : this.graphComponent.graph;
        const unwrappedNode = unwrapped.nodes.find(n => n.tag.id === node.tag.id);
        const inEdges = this.explorable ?
            new ListEnumerable(unwrapped.inEdgesAt(unwrappedNode).filter(edge => ! edge.tag.properties?.isClue))
            : fg.inEdgesAt(node);
        const outEdges = this.explorable ?
            new ListEnumerable(unwrapped.outEdgesAt(unwrappedNode).filter(edge => ! edge.tag.properties?.isClue))
            : fg.outEdgesAt(node);
        const inEdgesFiltered = this.explorable ? fg.inEdgesAt(node)
            : IListEnumerable.EMPTY as IListEnumerable<IEdge>;
        const outEdgesFiltered = this.explorable ? fg.outEdgesAt(node)
            : IListEnumerable.EMPTY as IListEnumerable<IEdge>;
        const inData = this.getEdgeOptions(inEdges, inEdgesFiltered, visibility);
        const outData = this.getEdgeOptions(outEdges, outEdgesFiltered, visibility);

        this.incomingEdges.set(node.tag.id as number, inData.filterOptions);
        this.outgoingEdges.set(node.tag.id as number, outData.filterOptions);

        this.nodeMap.set(node.tag.id + 'inEdges', inData.edges as IEdge[]);
        this.nodeMap.set(node.tag.id + 'outEdges', outData.edges as IEdge[]);
    }

    /**
     * Gets the options as per the given array of edges.
     * @param edgeEnumerable edges for which need to add filter options.
     * @param filteredEdgeArr array of edges in the filtered graph (only used when explorable feature-toggle is activated)
     * @param visibility checkbox initial value.
     * @returns Object of type {EdgeOptions}
     */
    private getEdgeOptions(edgeEnumerable: IEnumerable<IEdge>, filteredEdgeArr: IListEnumerable<IEdge>, visibility: boolean): EdgeOptions {
        const edges: any[] = [];
        /* Map mapping relationship types, f.e. "CALLS" to FilterOptions instances which contain num of relationships and whether option is checked */
        const filterOptions: Map<string, FilterOptions> = new Map();
        /* Sets the All option */
        filterOptions.set(FilterTypeEnum.ALL.toLocaleUpperCase(), { count: 0, checked: visibility, indeterminate: false });
        let count = 0;
        let indeterminate = false;
        edgeEnumerable.forEach((item) => {
            if (item.tag) {
                edges.push(item);
                const relation: string = item.tag.relationship;
                if (filterOptions.has(relation)) {
                    const val = filterOptions.get(relation);
                    val['count'] += 1;
                    if (val['checked'] !== this.arrContains(filteredEdgeArr, item)) {
                        val['indeterminate'] = true;
                        indeterminate = true;
                    }
                    val['checked'] = val['checked'] ? this.arrContains(filteredEdgeArr, item) : false;
                    filterOptions.set(relation, val);
                } else {
                    filterOptions.set(relation, { count: 1, checked: this.explorable ? this.arrContains(filteredEdgeArr, item) : visibility,
                        indeterminate: false });
                }
                count++;
            }
        });

        if (filterOptions.size <= 1) {
            filterOptions.clear();
        } else {
            let allChecked = false;
            if ( ! indeterminate) {
                allChecked = true;
                let first = true;
                filterOptions.forEach((value, key) => {
                    if (key !== FilterTypeEnum.ALL.toLocaleUpperCase()) {
                        if (first) {
                            first = false;
                        } else if (allChecked !== value.checked) {
                            indeterminate = true;
                        }
                        if (allChecked) {
                            allChecked = value.checked;
                        }
                    }
                });
            }
            filterOptions.set(FilterTypeEnum.ALL.toLocaleUpperCase(), { count, checked: this.explorable ? allChecked : visibility, indeterminate });
        }
        return { edges, filterOptions };
    }

    private arrContains(arr: IListEnumerable<IEdge>, edge: IEdge): boolean {
        return !! arr.find(e => e.sourceNode.tag.id === edge.sourceNode.tag.id && e.targetNode.tag.id === edge.targetNode.tag.id);
    }

    /**
     * Sets the checkbox state for the option checkbox.
     *
     * This method is only used without the dependencyGraphExplore feature-toggle enabled.
     *
     * @param node for which the option need to be set.
     * @param type of edges.
     */
    private setOptionCheckboxState(node: INode, type: string) {
        if (type === FilterTypeEnum.INCOMING || type === FilterTypeEnum.ALL) {
            const edgesArr = this.getNodeMapEdges(node, 'inEdges');
            const edgeOption = this.incomingEdges.get(node.tag.id as number);
            this.setCheckboxState(edgesArr, edgeOption);
        }

        if (type === FilterTypeEnum.OUTGOING || type === FilterTypeEnum.ALL) {
            const edgesArr = this.getNodeMapEdges(node, 'outEdges');
            const edgeOption = this.outgoingEdges.get(node.tag.id as number);
            this.setCheckboxState(edgesArr, edgeOption);
        }
    }

    /**
     * Determine the state of the edges and accordingly sets the checkbox option.
     * @param edgesArr Array of edges need to be evaluated.
     * @param edgeOption incoming/outgoing Options for the selected node.
     */
    private setCheckboxState(edgesArr: IEdge[], edgeOption: Map<string, FilterOptions>) {
        if (edgesArr.length) {
            const optionMap = new Map();
            /* set the option map */
            for (const item of edgesArr) {
                const name = item.tag.relationship;
                if (name !== FilterTypeEnum.ALL.toLocaleUpperCase()) {
                    const filter = item.tag.filtered === undefined ? false : item.tag.filtered;
                    if (optionMap.has(name)) {
                        const val = optionMap.get(name);
                        if (val !== filter) {
                            optionMap.set(name, 'indeterminate');
                            break;
                        }
                    } else {
                        optionMap.set(name, filter);
                    }
                }
            }

            /* Set state for options other than the All Option. */
            let allOption: any = [];
            for (const [key, value] of optionMap) {
                const optionVal = edgeOption.get(key as string);
                if (value === 'indeterminate') {
                    optionVal['checked'] = false;
                    optionVal['indeterminate'] = true;
                } else {
                    optionVal['checked'] = ! value;
                    optionVal['indeterminate'] = false;
                }
                allOption.push(value);
                edgeOption.set(key as string, optionVal);
            }

            /* Set the state for the ALL Option. */
            allOption = Array.from(new Set(allOption as Iterable<unknown>));
            const allOptionVal = edgeOption.get(FilterTypeEnum.ALL.toLocaleUpperCase());
            if (allOption.length > 1 || allOption[0] === 'indeterminate') {
                allOptionVal['checked'] = false;
                allOptionVal['indeterminate'] = true;
            } else {
                allOptionVal['checked'] = ! allOption[0];
                allOptionVal['indeterminate'] = false;
            }
            edgeOption.set(FilterTypeEnum.ALL.toLocaleUpperCase(), allOptionVal);
        }
    }

    /**
     * Closes all picker containers except for the given elements and set the active menu item.
     * @param exceptToggleButton html element for the selected menu item.
     * @param exceptContainer html element for the selected menu container.
     */
    private hideAllPickerContainer(exceptToggleButton?: HTMLInputElement, exceptContainer?: HTMLElement) {
        /* Hides all the menu-options except the selected container for the menu-item. */
        if (this.optionsContainer) {
            this.optionsContainer.forEach(container => {
                const ele = container.nativeElement;
                if (ele.style.opacity !== '0' && ele !== exceptContainer) {
                    ele.style.opacity = '0';
                    setTimeout(() => {
                        ele.style.display = 'none';
                    });
                }
            });
        }
    }

    /**
     * Makes this toolbar visible near the given items.
     */
    private show() {
        // we hide the picker containers such that we don't need to update their position if new elements are added to
        // the toolbar
        if (this.contextMenuOptionType.includes(this.ContextToolbarMenuType.REACHABILITY_BLOCK) ||
        this.contextMenuOptionType.includes(this.ContextToolbarMenuType.DEFAULT) ||
        this.contextMenuOptionType.includes(this.ContextToolbarMenuType.REACHABILITY_NETWORK)) {
        this.hideAllPickerContainer();

        // show hide UI for nodes and/or labels
        this.showToolbar = true;

        // place the contextual toolbar
        this.updateLocation();
        }
    }

    /**
     * Hides this toolbar.
     */
    private hide() {
        this.hideAllPickerContainer();
        if (this.container) {
            this.showToolbar = false;
        }
    }

    /**
     * Changes the location of toolbar to the location calculated by a label model parameter.
     * Depending on the selection, either an edge specific label model is used, or a node label model
     * that uses the union of all selected elements to place the toolbar above that union.
     */
    private updateLocation() {
        if (!this.selectedNodes.length) {
            return;
        }
        const width = this.container.clientWidth;
        const height = this.container.clientHeight;
        const zoom = this.graphComponent.zoom;

        const dummyOwner = new SimpleNode({
            layout: this.getEnclosingRect()
        });
        const labelModelParameter: ILabelModelParameter = this.nodeLabelModelParameter;

        // create a dummy label to let the LabelModelParameter compute the correct location
        const dummyLabel = new SimpleLabel(dummyOwner, '', labelModelParameter);
        if (labelModelParameter.supports(dummyLabel)) {
            dummyLabel.preferredSize = new Size(width / zoom, height / zoom);
            const newLayout = labelModelParameter.model.getGeometry(dummyLabel, labelModelParameter);
            /* eslint-disable @typescript-eslint/no-unnecessary-type-assertion */
            this.setLocation(newLayout.anchorX, newLayout.anchorY - (height + 10) / zoom, width, height);
        }
    }

    /**
     * Method to fetch the style for the icon to display in Filter dropdown.
     */
    private getEnclosingRect() {
        const enclosingRect = new MutableRectangle();
        for (const node of this.selectedNodes) {
            if (INode.isInstance(node)) {
                // we need the axis-parallel bounding rectangle, thus look out for oriented rectangles of the labels
                const bounds = node.layout instanceof IOrientedRectangle ? node.layout.bounds : node.layout;
                enclosingRect.setToUnion(enclosingRect, bounds);
            }
        }
        return enclosingRect;
    }

    /**
     * Sets the location of this pop-up to the given world coordinates.
     *
     * @param x - x coordinate location
     * @param y - y coordinate location
     * @param width - width of popup
     * @param height - height of popup
     */
    private setLocation(x: number, y: number, width: number, height: number) {
        // Calculate the view coordinates since we have to place the div in the regular HTML coordinate space
        const viewPoint = this.graphComponent.toViewCoordinates(new Point(x, y));
        const gcSize = this.graphComponent.innerSize;
        const padding = 15;
        const left = Math.min(gcSize.width - width - padding, Math.max(padding, viewPoint?.x));
        const top = Math.min(gcSize.height - height - padding, Math.max(padding, viewPoint?.y));
        this.container.style.left = `${left}px`;
        this.container.style.top = `${top}px`;
    }

    /**
     * Adds listeners for graph changes, to update the location or state of the toolbar accordingly.
     */
    private registerUpdateListeners() {
        if (this.graphComponent) {
            this.graphComponent.addViewportChangedListener(() => {
                if (this.selectedNodes.length) {
                    this.dirty = true;
                }
            });
            this.graphComponent.graph.addNodeLayoutChangedListener(() => {
                if (this.selectedNodes.length) {
                    this.dirty = true;
                }
            });
            this.graphComponent.addUpdatedVisualListener(() => {
                if (this.selectedNodes.length && this.dirty) {
                    this.dirty = false;
                    this.updateLocation();
                }
            });
        }
    }

    /**
     * Updates the graph for reflecting the new state of the graph.
     * @param graph graph which needs to be updated.
     */
    private predicateGraphChange(graph: IGraph) {
        const filteredGraph = graph as FilteredGraphWrapper;
        filteredGraph.nodePredicateChanged();
        filteredGraph.edgePredicateChanged();
    }

    /**
     * Checks if the graph is connected or not.
     * @param edgesArr Array of the edges.
     * @param edgeType type of the edge incoming/outgoing.
     */
    private checkConnected(edgesArr: IListEnumerable<IEdge>, edgeType: string) {
        const isConnected = this.isGraphConnected();
        if ( ! isConnected) {
            const nodesConnectedToRoot: Set<number> = new Set();
            nodesConnectedToRoot.add(this.rootNode.tag.id as number);

            edgesArr.forEach((item: any) => {
                const node: INode = edgeType === FilterTypeEnum.INCOMING ? item.sourceNode : item.targetNode;
                this.pathToRootNode(node, nodesConnectedToRoot);
            });
            this.pathToRootNode(this.selectedNode, nodesConnectedToRoot);
        }
    }

    /**
     * Traces the path to the root node and if not found removes the node from the graph.
     * Calls {disconnectedItemsFromGraph}.
     * @param node node from where start.
     * @param nodesConnectedToRoot Map of node connected to root node.
     */
    private pathToRootNode(node: INode, nodesConnectedToRoot: Set<number>) {
        const visitedNode: Map<string, boolean> = new Map();
        const nodesToBeRemoved: Map<number, INode> = new Map();

        if ( ! nodesConnectedToRoot.has(node.tag.id as number)) {
            const inEdges = this.graphComponent.graph.inEdgesAt(node);
            const outEdges = this.graphComponent.graph.outEdgesAt(node);

            if (inEdges.size > 0 && ! visitedNode.has('in-' + node.tag.id)) {
                visitedNode.set('in-' + node.tag.id, true);
                this.disconnectedItemsFromGraph(inEdges, FilterTypeEnum.INCOMING, visitedNode, nodesToBeRemoved, nodesConnectedToRoot);
            }

            if (outEdges.size > 0 && ! visitedNode.has('out-' + node.tag.id)) {
                visitedNode.set('out-' + node.tag.id, true);
                this.disconnectedItemsFromGraph(outEdges, FilterTypeEnum.OUTGOING, visitedNode, nodesToBeRemoved, nodesConnectedToRoot);
            }
            this.removeNodeFromGraph(nodesToBeRemoved, nodesConnectedToRoot);
        }
    }

    /**
     * Filters the node and edges from the graph.
     * @param nodesToBeRemoved All the nodes that needs to be removed.
     * @param nodesConnectedToRoot All the nodes connected to the root node.
     */
    private removeNodeFromGraph(nodesToBeRemoved: Map<number, INode>, nodesConnectedToRoot: Set<number>) {
        const visibility = false;
        if ( ! nodesToBeRemoved.has(this.rootNode.tag.id as number)) {
            nodesToBeRemoved.forEach((item: INode) => {
                this.contextMenuHideNode.emit({ node: item, visibility: false });
                const inEdgesArr = this.getNodeMapEdges(item, 'inEdges');
                const outEdgesArr = this.getNodeMapEdges(item, 'outEdges');

                inEdgesArr.forEach((edge: any) => {
                    edge.tag.filtered = ! visibility;
                });

                outEdgesArr.forEach((edge: any) => {
                    edge.tag.filtered = ! visibility;
                });
            });
            this.predicateGraphChange(this.graphComponent.graph);
        } else {
            nodesToBeRemoved.delete(this.rootNode.tag.id as number);
            nodesToBeRemoved.forEach((item: INode) => {
                nodesConnectedToRoot.add(item.tag.id as number);
            });
        }
    }

    /**
     * Sets the items which needs to be removed for the disconnected component of graph.
     * @param edgesArr Array of all the edges from the node.
     * @param type the type of the edges incoming/outgoing.
     * @param visitedNode nodes already visited while tracing.
     * @param nodesToBeRemoved nodes needed to be removed.
     * @param nodesConnectedToRoot nodes connected to root Node.
     */
    private disconnectedItemsFromGraph(edgesArr: IListEnumerable<IEdge>, type: string, visitedNode: any,
        nodesToBeRemoved: any, nodesConnectedToRoot: Set<number>) {
        edgesArr.forEach((item: any) => {
            let node: IPort;
            if (type === FilterTypeEnum.INCOMING) {
                if (nodesConnectedToRoot.has(item.sourceNode.tag.id as number) && item.tag) {
                    nodesToBeRemoved.set(this.rootNode.tag.id, {});
                    return;
                } else {
                    nodesToBeRemoved.set(item.sourceNode.tag.id, item.sourceNode);
                }
                node = item.sourceNode;
            } else if (type === FilterTypeEnum.OUTGOING) {
                if (nodesConnectedToRoot.has(item.targetNode.tag.id as number) && item.tag) {
                    nodesToBeRemoved.set(this.rootNode.tag.id, {});
                    return;
                } else {
                    nodesToBeRemoved.set(item.targetNode.tag.id, item.targetNode);
                }
                node = item.targetNode;
            }

            const inEdges = this.graphComponent.graph.inEdgesAt(node);
            const outEdges = this.graphComponent.graph.outEdgesAt(node);


            if (inEdges.size > 0 && ! visitedNode.has('in-' + node.tag.id)) {
                visitedNode.set('in-' + node.tag.id, true);
                this.disconnectedItemsFromGraph(inEdges, FilterTypeEnum.INCOMING, visitedNode, nodesToBeRemoved, nodesConnectedToRoot);
            }

            if (outEdges.size > 0 && ! visitedNode.has('out-' + node.tag.id)) {
                visitedNode.set('out-' + node.tag.id, true);
                this.disconnectedItemsFromGraph(outEdges, FilterTypeEnum.OUTGOING, visitedNode, nodesToBeRemoved, nodesConnectedToRoot);
            }
        });
    }

    /**
     * To get the NodeMap values.
     * @param node graph node.
     * @param type edge type.
     */
    private getNodeMapEdges(node: INode, type: string) {
        const edgesVal = this.nodeMap.get(node.tag.id + type);
        return edgesVal ? edgesVal : [];
    }
}
