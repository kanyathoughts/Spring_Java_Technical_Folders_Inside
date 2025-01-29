import { Component, ElementRef, Inject, Input, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { UntypedFormBuilder, UntypedFormControl } from '@angular/forms';
import { NzMessageService } from 'ng-zorro-antd/message';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { Subscription } from 'rxjs';
import { TranslateService } from '@ngx-translate/core';
import { RemoteJob } from '@app/core/services/job-manager/job-manager-service.interface';
import { Logger } from '@app/core';
import { ListDetail, ModuleValueChange } from './call-chain-export.interface';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ModuleSelectionComponent } from './module-selection/module-selection.component';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import {
  AggregationRequestModuleFieldName,
  AggregationResultModuleFieldName,
  DependencyGraph,
  JobControllerService,
  JobInformation,
  ModuleControllerService,
  ModulePojo,
  EntityId
} from '@innowake/mining-api-angular-client';
import { WindowToken } from '@app/core/utils/window';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
interface ErrorModule {
  errors: number;
  id: EntityId;
}

/** Supported different file format */
const callChainExportFormats = [['GRAPHML', 'GraphML'], ['CSVZIP', 'CSV ZIP'], ['CSV', 'CSV']];
/** Token used to recognize call chain exporter jobs */
const CALL_CHAIN_EXPORTER_TOKEN = 'call-chain-exporter';
const log = new Logger('Module Detail Call Chain');

@Component({
  selector: 'mn-call-chain-export',
  templateUrl: './call-chain-export.component.html',
})
export class CallChainExportComponent implements OnInit, OnDestroy {
  @Input() currentModuleDetails: ListDetail[];
  @ViewChild(ModuleSelectionComponent) moduleSelection: ModuleSelectionComponent;
  loadingBtn = false;
  disableBtn = false;
  onLoading: boolean;
  onCancelLoading: boolean;
  hiddenFromCallChainLoading = true;
  selectedDepth = -1;
  isGraphRender = false;

  direction = [
    { directionValue: ['IN'], imageText: 'arrow-left', showText: 'Inbound' },
    { directionValue: ['OUT'], imageText: 'arrow-right', showText: 'Outbound' },
  ];

  exportForm = this.fb.group({
    direction: this.fb.control([]),
    depth: this.fb.control([]),
    filters: this.fb.control([]),
    dataAccessBased: [false],
    conditionalDependencies: [false]
  });

  listModuleTypeOptions: Array<{ value: string, label: string }> = [];
  listDepthOptions: Array<{ value: number, label: string }> = [{ value: -1, label: 'infinite' }];
  isJobCancelDisabled: boolean;
  taxonomyIds: string[] = [];
  additionalExportFormats: string[][] = callChainExportFormats;
  dependencyGraph: DependencyGraph;
  emptyDependencyGraph: boolean;
  projectId: number;
  startModule: string[];
  endModule: string[];
  selectedStartRadio: string;
  selectedEndRadio: string;
  startModuleDetails: Array<{ label: string, value: number[] }> = [];
  endModuleDetails: Array<{ label: string, value: number[] }> = [];
  considerConditionalDependenciesActive: boolean;
  graphLoadingMessage = '';
  considerDataFlowActive: boolean;
  showErrorCount = false;
  showMissingDependencyCount = false;
  errCount = 0;
  errMissingDependency = 0;
  errModuleIds: number[];
  missingDependencyModuleIds: number[];

  private exportJob: RemoteJob | undefined;
  private jobSubscription: Subscription;
  private clientProjectSubscription: Subscription;

  constructor(
    private fb: UntypedFormBuilder,
    private elRef: ElementRef,
    private messageService: NzMessageService,
    private jobController: JobControllerService,
    private jobManagerService: JobManagerService,
    private translateService: TranslateService,
    private moduleControllerService: ModuleControllerService,
    private labelMappingService: LabelMappingService,
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private featureToggleService: FeatureToggleService,
    @Inject(WindowToken) private $window: Window
  ) {}

  get filters(): UntypedFormControl {
    return this.exportForm.get('filters') as UntypedFormControl;
  }

  get directionValue(): UntypedFormControl {
    return this.exportForm.get('direction') as UntypedFormControl;
  }

  ngOnInit(): void {
    this.featureToggleService.isActive('callChainConditionalDependencies').subscribe((isActive: boolean) => {
      this.considerConditionalDependenciesActive = isActive;
    });
    this.featureToggleService.isActive('considerDataFlow').subscribe((isActive: boolean) => {
      this.considerDataFlowActive = isActive;
    });
    this.clientProjectSubscription = this.clientProjectRelationshipService.getClientProjectObservable()
      .subscribe((clientProjectRelationship: ClientProjectRelationship) => {
        this.projectId = clientProjectRelationship.getProjectId();
      });
    this.isJobCancelDisabled = true;
    this.setModuleTypeOptions();
    for (let i = 1; i <= 10; i++) {
      this.listDepthOptions.push({ value: i, label: '' + i });
    }
    this.setDefault();
    this.subscribeToJobNotification();
    /* Whenever the Job status changes the cancel & csv button changes are handled here */
    this.jobSubscription = this.jobManagerService.jobs$.subscribe(jobs => {
      this.exportJob = jobs.filter(job => job.uiToken === CALL_CHAIN_EXPORTER_TOKEN)[0];
      if (this.exportJob === undefined || ! JobManagerService.isJobRunning(this.exportJob.jobInfo?.status)) {
        this.isJobCancelDisabled = true;
        this.onLoading = false;
      } else if (this.exportJob.jobInfo?.status === JobInformation.StatusEnum.CANCEL_REQUESTED) {
        this.isJobCancelDisabled = true;
        this.onLoading = true;
      } else { /* job is running */
        this.isJobCancelDisabled = false;
        this.onLoading = true;
      }
    });
  }

  ngOnDestroy(): void {
    this.jobSubscription.unsubscribe();
    this.clientProjectSubscription?.unsubscribe();
  }
  redirectToModule(btnType: string): void {
    const randomValue = new Uint8Array(5);
    crypto.getRandomValues(randomValue);
    const uniqueIdsKeys = `call-chain-${Array.from(randomValue)
      .map((byte) => byte.toString(36))
      .join('')}`;

    if (btnType === 'Error') {
      const uniqueIdsValue = { createdTime: dateFormatter(new Date()), moduleIds: this.errModuleIds };
      localStorage.setItem(uniqueIdsKeys, JSON.stringify(uniqueIdsValue));
      const path = 'modules?preFilter=' + `${uniqueIdsKeys}&columns=Module.errorCount`;
      openInNewTab(this.projectId, null , path, this.$window);
    } else {
      const uniqueIdsValue = { createdTime: dateFormatter(new Date()), moduleIds: this.missingDependencyModuleIds };
      localStorage.setItem(uniqueIdsKeys, JSON.stringify(uniqueIdsValue));
      const path = 'modules?preFilter=' + `${uniqueIdsKeys}&columns=Module.missingDependencyNames`;
      openInNewTab(this.projectId, null , path, this.$window);
    }
  }
  /**
   * Method to get updated values for start/end input field.
   * @param event event emitted when values are updated.
   */
  moduleChange(event: ModuleValueChange): void {
    if (event.hasOwnProperty('startModule')) {
      this.startModule = event.startModule;
      this.selectedStartRadio = event.selectedRadio;
      this.startModuleDetails = event.moduleDetails;
    } else {
      this.endModule = event.endModule;
      this.selectedEndRadio = event.selectedRadio;
      this.endModuleDetails = event.moduleDetails;
    }
  }

  /**
   * Method to get updated values for depth.
   * @param depthLevel depthLevel emitted by the depth dropdown with the updated depth value
   */
  onDepthChange(depthLevel: number): void {
    this.selectedDepth = depthLevel;
  }

  /**
   * Subscribe to the job notifications. If a dependency-graph job finishes successfully, we want to set {@linkplain dependencyGraph}
   * in order to display it on the UI.
   */
  subscribeToJobNotification(): void {
    this.jobManagerService.subscribeToJobNotification();
    this.jobManagerService.jobResult.subscribe((result: any) => {
      if (this.isGraphRender) {
        this.dependencyGraph = result;
        this.isGraphRender = false;
      }

      this.errMissingDependency = result.modulesWithMissingDependencies?.length;
      this.emptyDependencyGraph = this.dependencyGraph?.modules?.length === 0;
      this.missingDependencyModuleIds = result.modulesWithMissingDependencies;
      this.errModuleIds = result.modules?.filter((m: { errors: number }) => m.errors > 0).map((m: ErrorModule) => m.id);
      this.errCount = this.errModuleIds?.length;
      if (this.errCount > 0) {
        this.showErrorCount = true;
      }
      if (this.missingDependencyModuleIds?.length > 0) {
        this.showMissingDependencyCount = true;
      }
    });
    this.jobManagerService.jobStatus.subscribe((response: any) => {
      this.disableBtn = this.loadingBtn = response;
    });
  }

  setDefault(): void {
    this.exportForm.get('direction').patchValue(this.direction[1].directionValue);
    this.exportForm.get('depth').setValue(-1);
    this.filters.setValue([ModulePojo.TypeEnum.COPYBOOK, ModulePojo.TypeEnum.EXEC, ModulePojo.TypeEnum.EXEC_PGM]);
  }

  /**
   * Gets Label for start box based on direction value.
   */
  getStartLabel(): void {
    this.moduleSelection.changeStartLabel(this.directionValue.value as string[]);
  }

  /**
   * Returns the Label for Direction dropdown
   * @param selectedOption - The selected direction.
   */
  getSelectedDirection(selectedDir: string[]): string {
    if (selectedDir.length === 1) {
      return selectedDir[0];
    } else {
      return selectedDir.join(',');
    }
  }

  /**
   * method to get updated taxonomy ids based on the selection of the filter
   * @param  event event emitted by taxonomy filter component with details
   */
  updateTaxonomySelection(event: TaxonomyFilterSelected[]): void {
    this.taxonomyIds.length = 0;
    event.forEach((taxonomyId: TaxonomyFilterSelected) => {
      this.taxonomyIds.push(...taxonomyId.toString().split('_')[1].split(','));
    });
  }

  /**
   * method to create job depending upon the selection of the export format
   * @param  exportFormat it could be CSV , CSVZIP or GraphML
   */
  startCallChainJob(exportFormat: string): void {
    this.isGraphRender = true;
    if (exportFormat === 'DEPENDENCY_GRAPH') {
      this.disableBtn = false;
      this.loadingBtn = true;
      this.emptyDependencyGraph = false;
      this.dependencyGraph = null;
      this.graphLoadingMessage = '';
      setTimeout(() => {
        this.graphLoadingMessage = this.translateService.instant('callChain.graphLoadingMessage');
      }, 3000);
    } else {
      this.disableBtn = true;
      this.loadingBtn = false;
    }
    const requestObject = this.buildRequestObject(exportFormat);
    this.jobManagerService.jobExtension(exportFormat, this.projectId, requestObject);
  }

  /*
   * Makes the back-end call to cancel the job and updates the button & message functionalities accordingly
   */
  cancelCallChainJob(): void {
    if (this.exportJob === undefined) {
      /* no active job, so can't cancel */
      return;
    }
    this.onCancelLoading = true;
    this.jobController.cancelJob(this.exportJob.jobId).subscribe(() => {
      this.isJobCancelDisabled = true;
      this.onCancelLoading = false;
    }, () => {
      this.isJobCancelDisabled = false;
      this.onCancelLoading = false;
      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
      this.messageService.error(this.translateService.instant('job.jobCancelError'));
    });
  }

  private buildRequestObject(exportFormat: string): {[key: string]: string[]} {
    const requestObject = {};
    const filteredTypeValues: string[] = [];
    const endModuleIdValues: string[] = [];
    const endModuleTypeValues: string[] = [];
    let startModuleIdValues: string[] = this.currentModuleDetails && !this.startModule?.length ?
      this.currentModuleDetails.map((module: ListDetail) => (module.value) as string) : [];
    const startModuleTypeValues: string[] = [];
    requestObject['direction'] = this.exportForm.get('direction').value;
    requestObject['dataAccessBased'] = [this.exportForm.get('dataAccessBased').value];
    requestObject['considerConditionalDependencies'] = [this.exportForm.get('conditionalDependencies').value];
    requestObject['depth'] = [this.exportForm.get('depth').value];
    requestObject['ignoredTaxonomy'] = this.taxonomyIds;
    if (exportFormat === 'CSVZIP') {
      requestObject['exportFormat'] = ['CSV'];
      requestObject['compressed'] = ['true'];
    } else {
      requestObject['compressed'] = ['false'];
      requestObject['exportFormat'] = [exportFormat];
    }
    if (this.filters.value) {
      this.filters.value.forEach((element: string) => {
        filteredTypeValues.push(element);
      });
    }
    if (this.endModule && this.endModule[0] !== this.moduleSelection.allModuleOption.value) {
      switch (this.selectedEndRadio) {
        case 'name':
          this.endModule?.forEach((module: any) => {
            const findModule = this.endModuleDetails.find(x => x.label === module);
            endModuleIdValues.push(...findModule.value as unknown as string);
          });
          break;
        case 'type':
          this.endModule?.forEach((endModuleItem: string) => {
            endModuleTypeValues.push(this.listModuleTypeOptions[this.listModuleTypeOptions.findIndex(x => x.label === endModuleItem)]?.value);
          });
          break;
      }
    }
    if (this.startModule && this.startModule[0] !== this.moduleSelection.allModuleOption.value) {
      startModuleIdValues = [];
      switch (this.selectedStartRadio) {
        case 'name':
          this.startModule?.forEach((module: any) => {
            const findModule = this.startModuleDetails.find(x => x.label === module);
            startModuleIdValues.push(...findModule.value as unknown as string);
          });
          break;
        case 'type':
          this.startModule?.forEach((startModuleItem: string) => {
            startModuleTypeValues.push(this.listModuleTypeOptions[this.listModuleTypeOptions.findIndex(x => x.label === startModuleItem)]?.value);
          });
      }
    }
    requestObject['startModuleId'] = startModuleIdValues;
    requestObject['startModuleType'] = startModuleTypeValues;
    requestObject['filteredType'] = filteredTypeValues;
    requestObject['endModuleId'] = endModuleIdValues;
    requestObject['endModuleType'] = endModuleTypeValues;
    return requestObject;
  }

  private setModuleTypeOptions(): void {
    const typeRequest: AggregationRequestModuleFieldName = {
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY, AggregationRequestModuleFieldName.GroupByEnum.TYPE]),
      orderBy: [AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY, AggregationRequestModuleFieldName.GroupByEnum.TYPE],
      fields: {
        [AggregationRequestModuleFieldName.GroupByEnum.ID]: 'COUNT'
      }
    };
    this.moduleControllerService.getAggregatedValues2(this.projectId, typeRequest).subscribe(
      (resp: AggregationResultModuleFieldName[]) => {
        this.hiddenFromCallChainLoading = false;
        if (resp.length) {
          const typeOptions = resp.map((result: AggregationResultModuleFieldName) => {
            let typeLabel = `${this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, result.group.TECHNOLOGY + '')}`;
            if (result.group.TYPE !== result.group.TECHNOLOGY) {
              typeLabel = `${this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, result.group.TECHNOLOGY + '')}`
              + ' ' + `${this.labelMappingService.mapLabel(LabelType.TYPE, result.group.TYPE + '')}`;
            }

            /* Sets the default value for Hide from call chain field if type available for the project */
            if ((result.group.TYPE as any) in [ModulePojo.TypeEnum.COPYBOOK, ModulePojo.TypeEnum.EXEC, ModulePojo.TypeEnum.EXEC_PGM]) {
              this.filters.patchValue(result.group.TYPE);
            }
            return { value: result.group.TYPE, label: typeLabel };
          });
          /* Removes duplicate objects from array for which type is same and change label to type only. */
          this.listModuleTypeOptions = typeOptions.reduce((unique, type) => {
            if ( ! unique.some(obj => obj.value === type.value)) {
              unique.push(type);
            } else {
              const index = unique.findIndex(item => item.value === type.value);
              unique[index] = {
                value: type.value, label: this.labelMappingService.mapLabel(LabelType.TYPE, type.value + '')
              };
            }
            return unique;
          }, []);
          this.listModuleTypeOptions.sort((obj1, obj2) => obj1.label.localeCompare(obj2.label));
        }
      }, (err) => {
        this.hiddenFromCallChainLoading = false;
        log.error('Error while fetching types: ' + err.message);
      });
  }
}
