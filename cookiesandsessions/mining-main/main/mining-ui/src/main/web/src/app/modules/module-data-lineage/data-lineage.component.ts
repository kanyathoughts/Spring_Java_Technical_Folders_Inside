import { Component, ElementRef, Inject, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { ModulePojo, ModuleControllerService, JobControllerService,
  JobInformation, DataFlowGraphNode, DataDictionaryControllerService } from '@innowake/mining-api-angular-client';
import { GraphComponent, ICommand, IFoldingView } from 'yfiles';
import { DataFlowUtility } from './utils/data-flow-utility';
import { DataLineageGraphInfo } from './models/data-lineage-graph.info';
import HierarchicGrouping from '@app/modules/graph/utils/yfiles-util/hierarchic-grouping';
import { DataFlowGraphComponent } from './data-flow-graph/data-flow-graph.component';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { Logger } from '@app/core';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { WindowToken } from '@app/core/utils/window';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { openInNewTab } from '@app/core/utils/window-open.utils';

const log = new Logger('DataLineageComponent');

@Component({
  selector: 'mn-data-lineage',
  templateUrl: './data-lineage.component.html'
})

export class DataLineageComponent implements OnInit {
  @ViewChild(DataFlowGraphComponent) dfgComponent!: DataFlowGraphComponent;

  @ViewChild('graphComponentRef') graphComponentRef!: ElementRef;
  projectId: number;
  module: ModulePojo;
  graphComponent!: GraphComponent;
  dataFlowUtility: DataFlowUtility;
  graphInfo: DataLineageGraphInfo;
  foldingView: IFoldingView;
  hierarchicGrouping: HierarchicGrouping;
  offsetNumber: number;
  assembled: boolean;
  includingModule: string;
  loadState: LoaderState;
  errorMessage: string;
  fieldName: string;
  type: any;
  columnName: string;
  inputOutputFields: string[] = [];

  constructor(private route: ActivatedRoute,
    private moduleController: ModuleControllerService,
    private jobManagerService: JobManagerService,
    private jobControllerService: JobControllerService,
    protected router: Router,
    @Inject(WindowToken) private $window: Window,
    private messageService: NzMessageService,
    private translateService: TranslateService,
    private dataDictionaryControllerService: DataDictionaryControllerService) { }

  ngOnInit(): void {
    this.loadState = LoaderState.loading;
    const offset = this.route.queryParams['_value']?.offset;
    this.offsetNumber = offset ? Number(offset) : -1;
    this.assembled = this.route.queryParams['_value']?.assembled ? true : false;
    this.fieldName = this.route.queryParams['_value']?.field;
    this.includingModule = this.route.queryParams['_value']?.includingModule;
    this.route.data.subscribe((data: { module: ModulePojo }) => {
      this.projectId = data.module.projectId;
      this.module = data.module;
    });
    this.columnName = this.route.queryParams['_value']?.name;
    this.type = this.route.queryParams['_value']?.type;
    this.dataFlowUtility = new DataFlowUtility();
    this.getDataFlowGraph(this.module.id);
    this.getJobResult();
  }

  /**
   * method to zoom in the graph.
   */
  zoomIn(): void {
    ICommand.INCREASE_ZOOM.execute(null, this.dfgComponent.graphComponent);
  }

  /**
   * method to zoom out the graph.
   */
  zoomOut(): void {
    ICommand.DECREASE_ZOOM.execute(null, this.dfgComponent.graphComponent);
  }

  /**
   * method to zoom the graph.
   */
  zoomOriginal(): void {
    ICommand.ZOOM.execute(1, this.dfgComponent.graphComponent);
  }

  /**
   * method to fit the content of the graph.
   */
  fitContent(): void {
    ICommand.FIT_GRAPH_BOUNDS.execute(null, this.dfgComponent.graphComponent);
  }

  /**
   * Expands all group nodes and relayouts the graph by calling the {@link DataFlowGraphComponent.expandAllGroups|expandAllGroups}
   * method of {@link dfgComponent}
   */
  expandAllGroups(): void {
    this.dfgComponent.expandAllGroups();
  }

  /**
   * Collapses all group nodes and relayouts the graph by calling the {@link DataFlowGraphComponent.collapseAllGroups|collapseAllGroups}
   * method of {@link dfgComponent}
   */
  collapseAllGroups(): void {
    this.dfgComponent.collapseAllGroups();
  }

  /**
   * Method to redirect to the Data Dictionary table if I/O fields present.
   */
  redirectToDDTable(): void {
    this.dataDictionaryControllerService.findDataDictionaryIdsBasedOnDataFlowIds(this.projectId, this.module.id, this.inputOutputFields).subscribe((dDIds) => {
      if (dDIds.length) {
        const randomValue = new Uint8Array(5);
        crypto.getRandomValues(randomValue);
        const uniqueIdsKeys = `linkedDDIds-${Array.from(randomValue).map(byte => byte.toString(36)).join('')}`;
        const uniqueIdsValue = { DDIds: dDIds.map((item: any) => item.nid) };
        localStorage.setItem(uniqueIdsKeys, JSON.stringify(uniqueIdsValue));
        const path = 'data-dictionary?preFilter=' + uniqueIdsKeys;
        openInNewTab(this.projectId, null, path, this.$window);
      } else {
        this.messageService.create('warning', `${this.translateService.instant('dlI/OFieldsMsg', { data: this.module.name })}`);
      }
    });
  }

  private getDataFlowGraph(moduleId: number): void {
    /* eslint-disable @typescript-eslint/no-unsafe-argument */
    this.moduleController.getCodeViewerDataFlowGraphJob(this.projectId, moduleId, this.offsetNumber, this.assembled,
      this.includingModule, this.columnName, this.type)
    .subscribe((jobId: any) => {
      const remoteJob = {
        jobId,
        foreground: true,
        cancellable: true,
        autoDownloadResult: false,
      };
      this.jobManagerService.register(remoteJob);
    });
  }

  private getJobResult(): void {
    this.jobManagerService.jobNotification.subscribe(notification => {
      if ( ! notification || notification?.status !== JobInformation.StatusEnum.SUCCESS) {
        if (notification?.status === JobInformation.StatusEnum.CANCELED ||
          notification?.status === JobInformation.StatusEnum.CANCEL_REQUESTED) {
          this.loadState = LoaderState.success;
        }
        return;
      }
      const remoteJob = this.jobManagerService.getRemoteJob(notification.jobId);
      if (remoteJob) {
        this.jobControllerService.getJobResult(notification.jobId).subscribe((jobResult) => {
          const linkModel = jobResult.object;
          if (linkModel && linkModel['nodes'].length > 0) {
            const nodes = linkModel['nodes'];
            this.graphInfo = this.dataFlowUtility.getDataLineageGraphInfo(nodes as DataFlowGraphNode[]);
            this.inputOutputFields = this.dataFlowUtility.inputOutputFields;
            this.loadState = LoaderState.success;
          } else {
            this.loadState = LoaderState.error;
            this.errorMessage = 'codeViewer.DLErrorMessage';
          }
        }, (error) => {
          log.error(error);
          this.loadState = LoaderState.error;
          this.errorMessage = 'codeViewer.DLErrorMessage';
        });
      }
    });
  }
}
