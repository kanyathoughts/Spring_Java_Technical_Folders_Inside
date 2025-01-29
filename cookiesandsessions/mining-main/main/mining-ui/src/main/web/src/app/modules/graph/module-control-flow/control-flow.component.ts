import {
  Component,
  ViewChild,
  OnInit,
  ChangeDetectorRef,
  AfterContentChecked
} from '@angular/core';
import {
  ICommand,
} from 'yfiles';
import { ActivatedRoute } from '@angular/router';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ControlFlowUtility } from './utils/control-flow-utility';
import { CHARACTER_LIMIT } from './models/control-flow-node-details';
import { ControlFlowGraphInfo } from './models/control-flow-graph-info';
import { ControlFlowGraphComponent } from './control-flow-graph/control-flow-graph.component';
import 'svg2pdf.js';
import { TranslateService } from '@ngx-translate/core';
import { EclipseService } from '@app/core/authentication/eclipse.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { takeLast } from 'rxjs/operators';
import { HttpErrorResponse } from '@angular/common/http';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { ControlFlowControllerService, ControlFlowGraph, ControlFlowNode, ErrorMarker, JobInformation,
  ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';
import { CfgSupportedTypeService } from '@app/core/services/cfg-supported-type.service';
declare const java__callback__clicked: (arg0: number, arg1: number) => Promise<string>;

/**
 * Component for the creation of Intra-Module Control Flow Graph.
 */
@Component({
  selector: 'app-mining-module-control-flow',
  templateUrl: './control-flow.component.html'
})
export class ControlFlowComponent implements OnInit, AfterContentChecked {
  static SEVERITY_WARN = 'warn';
  static SEVERITY_ERROR = 'error';
  @ViewChild(ControlFlowGraphComponent) cfgComponent!: ControlFlowGraphComponent;
  projectId: number;
  module: ModulePojo;
  moduleId: number;
  moduleName: string;
  moduleLinkHash: string;
  loadState: LoaderState;
  errorMessage: string;
  eclipseView: boolean;
  controlFlowUtility: ControlFlowUtility;
  graphInfo: ControlFlowGraphInfo;
  isGraphEmpty = true;
  emptyGraphExportErrorMsg: string;
  cfgMetaData: any;
  isRecalculatingCFG: boolean;
  navigationUrl: string;
  linkName: string;
  code = '';
  initialSourceCode = '';
  annotationId = '';
  constructor(
    private route: ActivatedRoute,
    private controlFlowService: ControlFlowControllerService,
    private eclipseService: EclipseService,
    private cdref: ChangeDetectorRef,
    private translateService: TranslateService,
    private jobManager: JobManagerService,
    private cfgSupportedTypeService: CfgSupportedTypeService,
    private moduleControllerService: ModuleControllerService) {
    this.emptyGraphExportErrorMsg = this.translateService.instant('controlFlowGraph.emptyGraphExportError');
  }

  ngAfterContentChecked(): void {
    this.cdref.detectChanges();
  }

  ngOnInit(): void {
    this.annotationId = this.route.snapshot.queryParams['annotationId'];
    this.eclipseView = this.eclipseService.isEclipseView;
    this.controlFlowUtility = new ControlFlowUtility();
    this.route.data.subscribe((data: { module: ModulePojo }) => {
      this.projectId = data.module.projectId;
      this.module = data.module;
      this.moduleId = data.module.id;
      this.moduleName = data.module.name;
      this.moduleLinkHash = data.module.linkHash;
      if (!this.checkIfCfgSupported()) {
        this.errorMessage = this.generatedNotSupportedMessage();
      } else {
        this.controlFlowService.getControlFlow(this.projectId, this.module.id, null, true).subscribe(() => {
          this.loadState = LoaderState.loading;
          this.createGraph();
        }, () => {
          this.createOrCalculateCfg();
        });
      }
      this.fetchModule(data.module);
    });
  }

  zoomIn(): void {
    ICommand.INCREASE_ZOOM.execute(null, this.cfgComponent.graphComponent);
  }
  zoomOriginal(): void {
    ICommand.ZOOM.execute(1, this.cfgComponent.graphComponent);
  }
  zoomOut(): void {
    ICommand.DECREASE_ZOOM.execute(null, this.cfgComponent.graphComponent);
  }
  fitContent(): void {
    ICommand.FIT_GRAPH_BOUNDS.execute(null, this.cfgComponent.graphComponent);
  }

  /**
   * Expands all group nodes and relayouts the graph by calling the {@link ControlFlowGraphComponent.expandAllGroups|expandAllGroups}
   * method of {@link cfgComponent}
   */
  expandAllGroups(): void {
    this.cfgComponent.expandAllGroups();
  }

  /**
   * Collapses all group nodes and relayouts the graph by calling the {@link ControlFlowGraphComponent.collapseAllGroups|collapseAllGroups}
   * method of {@link cfgComponent}
   */
  collapseAllGroups(): void {
    this.cfgComponent.collapseAllGroups();
  }

  /**
   * Reobtains existing graph and recreates the graph in the UI.
   */
  reCreateGraph(): void {
    this.loadState = LoaderState.loading;
    this.isRecalculatingCFG = false;
    this.createGraph();
  }

  /**
   * Calculates the control graph in backend and recreates the graph in the UI for moduleId.
   */
  calculateCFG(): void {
    this.isRecalculatingCFG = true;
    this.controlFlowService.calculateControlFlowForModule(this.projectId, this.module.id, true)
      .subscribe((response: string[]) => {
        this.getJobStatus(response.toString());
      }, (error: any) => {
        this.isRecalculatingCFG = false;
        this.errorMessage = error.message;
      });
  }

  /**
   * Checks id re calculation to be enabled/disabled.
   * @returns boolean value to enable/disable recalculation button.
   */
  needsRecalculation(): boolean {
    let disableRecalculate = false;
    if (this.checkIfCfgSupported && ! this.module.sourceCodeAvailable) {
      disableRecalculate = true;
    }
    return disableRecalculate;
  }

  /**
   * Checks if control flow has caluclated for a module.
   * @returns boolean value.
   */
  needsCalculation(): boolean {
    return ( this.checkIfCfgSupported()
        && this.module.sourceCodeAvailable);
  }

  /**
   * Calls the java callback method (used in eclipse view)
   * @param offset offset for the module
   */
  onNodeClick(offset: number): void {
    void java__callback__clicked(this.moduleId, offset);
  }

  checkIfCfgSupported(): boolean {
    return this.cfgSupportedTypeService.checkIfSupported(this.module.technology, this.module.type);
  }

  private generatedNotSupportedMessage(): string {
    let controlFlowMessage = '';
    const actuallySupportedModuleTypes = new Set(this.cfgSupportedTypeService.supportedTypes
      .map(nodeType => nodeType['technology'].toString() + ' ' + nodeType['type'].toString()));
    controlFlowMessage = this.translateService.instant('controlFlow.actuallyNotSupported',
      { supportedTypes: [...actuallySupportedModuleTypes].join(', ') });
    return controlFlowMessage;
  }

  private getJobStatus(jobId: string) {
    const remoteJob = {
      jobId,
      foreground: true
    };
    this.jobManager.register(remoteJob).status$.pipe(takeLast(1)).subscribe((status:
      JobInformation.StatusEnum) => {
        if (status === JobInformation.StatusEnum.SUCCESS) {
          this.reCreateGraph();
        } else {
          this.isRecalculatingCFG = false;
          this.createNavigationUrl();
        }
    }, (error: HttpErrorResponse) => {
      this.isRecalculatingCFG = false;
      this.loadState = LoaderState.nocontent;
      this.errorMessage = error.message;
    });
  }

  // @yjs:keep
  private createGraph() {
    this.controlFlowService.getControlFlow(this.projectId, this.moduleId, CHARACTER_LIMIT)
      .subscribe((response: ControlFlowGraph) => {
        if (response && response.nodes.length > 0) {
          const astNode: ControlFlowNode[] = response.nodes.filter(
            (element: ControlFlowNode) => ! (element.entity === ControlFlowNode.EntityEnum.ANNOTATION && element.parent === null)
          );
          response.nodes = astNode;
          this.graphInfo = this.controlFlowUtility.getGraphInfo(response, this.moduleName);
          if (this.graphInfo === null) {
            this.loadState = LoaderState.error;
            return;
          }
          this.loadState = LoaderState.success;
          this.isGraphEmpty = false;
          const metaData: any = {};
          response.nodes.forEach((node) => {
            if (node?.properties && (node.properties['inputFileIds'] || node.properties['outputFileIds'])) {
              const inputFileIds = response.relatedModules.filter((module) => (node.properties['inputFileIds'] as any).includes(module.id as any));
              const inputFiles = inputFileIds.map((module) => ({
                name: module.name,
                id: module.id
              }));
              const outputFileIds = response.relatedModules.filter((module) => (node.properties['outputFileIds'] as any).includes(module.id as any));
              const outputFiles = outputFileIds.map((module) => ({
                name: module.name,
                id: module.id
              }));
              metaData[node.id] = [
                {
                  'Output Files': outputFiles,
                },
                {
                  'Input Files': inputFiles,
                }
              ];
            }
          });
          if (metaData) {
            this.cfgMetaData = metaData;
          }
        } else {
          this.createNavigationUrl();
        }
      }, () => {
        this.loadState = LoaderState.error;
        this.errorMessage = 'unableToRenderGraph';
      });
  }

  private getErrorMarkers() {
    this.moduleControllerService.findErrorMarkers(this.projectId, this.moduleId)
      .subscribe((response: ErrorMarker[]) => {
        if (response) {
          this.errorMessage = this.translateService.instant('controlFlow.errorMarkersMsg') + this.moduleName;
          this.linkName = this.translateService.instant('controlFlow.detailsErrorTab');
        }
      }, (error: HttpErrorResponse) => {
        this.errorMessage = error.message;
      });
  }

  private createNavigationUrl() {
    this.navigationUrl = RouteBuilder.buildDetailsRoute(this.projectId, this.moduleLinkHash, 'errors');
    this.getErrorMarkers();
    this.loadState = LoaderState.error;
  }

  private fetchModule(module: ModulePojo) {
    this.moduleName = module.name;
    if (module.content) {
      this.initialSourceCode = module.content;
      this.code = module.content;
    }
  }

  private createOrCalculateCfg() {
    if (this.needsCalculation()) {
      this.loadState = LoaderState.loading;
      this.calculateCFG();
    } else {
      this.loadState = LoaderState.loading;
      this.createGraph();
    }
  }
}
