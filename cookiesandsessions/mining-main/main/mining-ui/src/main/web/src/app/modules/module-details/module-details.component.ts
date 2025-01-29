import { ChangeDetectorRef, Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Logger } from '@app/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NODE_CONFIG } from '../graph/utils/node-configurations';
import { BadgeCountUpdateOperation, ModuleBadgesSum, ModuleComplexityDetails } from './models/module-complexity-details';
import { Subscription } from 'rxjs';
import { ModuleBadgeUpdateService } from '@app/core/services/module-details/module-badge-update.service';
import { ModuleOverViewTaxonomyComponent } from '@app/shared/components/module-overview-taxonomy/module-overview-taxonomy.component';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { calculateModuleComplexity } from '@app/shared/components/shared-module-details/shared-module-details.util';
import { LabelType } from '@app/core/utils/mapping.utils';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { DnaCard } from '../dna-analysis/dna-card.interface';
import { DNAAnalysisService } from '@app/core/services/dna-analysis.service';
import { HttpErrorResponse } from '@angular/common/http';
import { ModuleDetailsDPSidePanelService } from '@app/core/services/module-details-and-dp-panel-state.service';
import { DataLineageExportComponent } from '@app/shared/components/data-lineage-export/data-lineage-export.component';
import { ListDetail } from './call-chain-export/call-chain-export.interface';
import { MiningToolbarButtonComponent } from '@app/shared/mining-toolbar/mining-toolbar-button/mining-toolbar-button.component';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import {
  AggregationRequestModuleFieldName,
  AggregationResultRelationshipFieldName,
  DataSchemaControllerService,
  DiscoveryControllerService,
  ErrorMarker,
  FieldInfoPojo,
  IoControllerService,
  ModelBelongsToClusters,
  ModelDna,
  ModuleControllerService,
  ModulePojo,
  ProjectRole,
  TaxonomyAssignmentsGetResponse,
  TaxonomyControllerService,
  TaxonomyPojo
} from '@innowake/mining-api-angular-client';
import { CfgSupportedTypeService } from '@app/core/services/cfg-supported-type.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { getBasePath } from '@app/core/utils/base-path.utils';

const log = new Logger('BrowseMiningModulesDetailsComponent');

@Component({
  selector: 'app-module-details',
  templateUrl: './module-details.component.html'
})
export class ModuleDetailsComponent implements OnInit, OnDestroy {
  @ViewChild(ModuleOverViewTaxonomyComponent) moduleOverViewTaxonomyComponent: ModuleOverViewTaxonomyComponent;
  @ViewChild(MiningToolbarButtonComponent) miningToolbarButton: MiningToolbarButtonComponent;
  taxonomyListLength: number;
  moduleId: number;
  moduleHash: string;
  projectId: number;
  projectName: string;
  moduleName: string;
  clientId: number;
  selectedModule: ModulePojo;
  isEditor: boolean;
  moduleComplexity: ModuleComplexityDetails;
  controlFlowMessage: string;
  isCalculatingCFG = false;
  modal: any;
  dnaCardList: DnaCard[] = [];
  codeViewerTooltip: string;
  disableCodeViewer = false;
  badgeObject: ModuleBadgesSum = { annotationCount: 0, dataDictionaryCount: 0, dependencyCount: 0, schemaFieldCount: 0 };
  annotationDataDictionarySubs: Subscription;
  moduleSubtitle: string;
  routeDependencies: string;
  routeControlFlow: string;
  virtualModuleDetail: AggregationResultRelationshipFieldName[];
  showModal: boolean;
  moduleIdArray: number[];
  taxonomyResponse: TaxonomyAssignmentsGetResponse;
  modulePath: string;
  labels: { [key: string]: { [key: string]: string } };
  schemaTabHeader: string;
  currentClient: ClientProjectRelationship;
  loadState: LoaderState;
  dnaChartsData: ModelDna;
  viewCFG: boolean;
  isDataLineageAvailable: boolean;
  isDataLineageExtensionAvailable: boolean;
  moduleProjectDetails: { moduleDetails: ListDetail[], project: number};
  isEclipseLinkActive: boolean;
  errorResponse: ErrorMarker[] = [];
  activeTabAsPerRoute: string;
  moduleIdForPreFiltering: number[];
  undiscoveredDependencies: ErrorMarker[] = [];
  code: string;

  private  dataLineageModalInstance: NzModalRef;
  private clientProjectSubscription: Subscription;
  private assignTaxonomiesModalSubscription: Subscription;
  private dnaSubscriptions: Subscription[] = [];

  constructor(
    private customTablesParametersService: CustomizableTableParametersService,
    private route: ActivatedRoute,
    private moduleControllerService: ModuleControllerService,
    private deeplinkService: DeepLinkService,
    protected router: Router,
    private messageService: NzMessageService,
    private authorizationService: KeycloakAuthorizationService,
    private relationshipService: ClientProjectRelationshipService,
    private translateService: TranslateService,
    private modalService: NzModalService,
    private taxonomyControllerService: TaxonomyControllerService,
    private labelMappingService: LabelMappingService,
    private dataSchemaControllerService: DataSchemaControllerService,
    private moduleDetailsDPSidePanelService: ModuleDetailsDPSidePanelService,
    private discoveryControllerService: DiscoveryControllerService,
    private dnaAnalysis: DNAAnalysisService,
    private ioService: IoControllerService,
    private cdk: ChangeDetectorRef,
    private moduleBadgeUpdateService: ModuleBadgeUpdateService,
    private cfgSupportedTypeService: CfgSupportedTypeService,
    private graphQlControllerService: GraphQlControllerService
  ) {
    this.activeTabAsPerRoute = router.url.split('?')[0].split('/').pop();
  }

  ngOnInit(): void {
    this.route.data.subscribe((data: { module: ModulePojo }) => {
      /*
       To make sure we call the methods and logic only when the module is changed, rather than doing it every time
       when navigate through the tabs(child routes).
      */
      if (data.module.id !== this.moduleId) {
        this.code = data.module.content;
        this.moduleId = data.module.id;
        this.moduleHash = data.module.linkHash;
        this.routeDependencies = RouteBuilder.buildModuleRoute(this.projectId, this.moduleId, 'dependencies');
        this.projectId = data.module.projectId;
        this.moduleName = data.module.name;
        this.modulePath = data.module.path;
        this.moduleIdArray = [this.moduleId];
        this.moduleControllerService.findErrorMarkers(this.projectId, this.moduleId).subscribe((errorResponse: ErrorMarker[]) => {
          this.errorResponse = errorResponse;
          this.undiscoveredDependencies = this.errorResponse.filter((error: ErrorMarker) => error.key === 'UNDISCOVERED_DEPENDENCY');
        });
        this.getAssignmentData();
        this.clientProjectSubscription = this.relationshipService.getClientProjectObservable().subscribe(currentClient => {
          this.projectName = currentClient.getProjectName();
          this.currentClient = currentClient;
          this.isEditor = this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.EDITOR);
        });
        this.moduleProjectDetails = {moduleDetails :[{value: this.moduleId, label: this.moduleName}], project : this.projectId};
        this.selectedModule = data.module;
        this.moduleComplexity = calculateModuleComplexity(this.selectedModule);
        if (this.moduleComplexity === undefined) {
          this.moduleComplexity = { description: 'notAvailable' };
        }
        this.moduleComplexity.description = this.translateService.instant(this.moduleComplexity.description);
        this.handleControlFlowButton();
        this.getCountsBadges();
        this.getSchemaTabHeader();
        this.fetchVirtualModuleDetails();
        this.moduleSubtitle =
          this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, this.selectedModule?.technology)
          + ' ' + this.labelMappingService.mapLabel(LabelType.TYPE, this.selectedModule?.type);

        this.fetchDNAData();

        this.routeDependencies = RouteBuilder.buildModuleRoute(this.projectId, this.moduleHash, 'dependencies');
        this.routeControlFlow = RouteBuilder.buildModuleRoute(this.projectId, this.moduleHash, 'control-flow');
      }
    });
    this.ioService.getExportFormats(this.projectId).subscribe(exportExtensions => {
      const dataLineageExtension = exportExtensions.find(x => x.id === 'datalineage-gml');
      this.isDataLineageExtensionAvailable = dataLineageExtension ? true : false;
      this.cdk.detectChanges();
      this.miningToolbarButton?.checkDropdownNeeded();
    });
    this.moduleControllerService.getDataLineageAvailableForModule(this.projectId, this.moduleId).subscribe((result: boolean) => {
      this.isDataLineageAvailable = result;
    });
    this.annotationDataDictionarySubs = this.moduleBadgeUpdateService.getAnnotationDataDictionary().subscribe((response:
      {operation: string, count: number}) => {
      if (response.count <= 0) {
      return;
      }
      const countMap = {
        [BadgeCountUpdateOperation.ANNOTATION_DELETED]: 'annotationCount',
        [BadgeCountUpdateOperation.DATA_DICTIONARY_DELETED]: 'dataDictionaryCount'
      };
      const key = countMap[response.operation];
      if (this.badgeObject[key] > 0) {
        this.badgeObject[key] -= response.count;
      }
    });
    this.getAssignmentData();
    this.deeplinkService.featureIsActive().subscribe(resp => {
      this.isEclipseLinkActive = resp;
      this.cdk.detectChanges();
      this.miningToolbarButton?.checkDropdownNeeded();
    });
  }

  /**
   * Method to handle(enable/disable) open control flow button.
   */
  handleControlFlowButton(): void {
    if (this.cfgSupportedTypeService.checkIfSupported(this.selectedModule.technology, this.selectedModule.type)) {
      if ( ! this.selectedModule.sourceCodeAvailable) {
        this.viewCFG = false;
        this.controlFlowMessage = this.translateService.instant('controlFlow.sourceNotAvailable');
      } else {
        this.viewCFG = true;
      }
    } else {
      this.viewCFG = false;
      const actuallySupportedModuleTypes = new Set(this.cfgSupportedTypeService.supportedTypes.map(
        (item) => this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, item['technology'] + '').toString() + ' '
          + this.labelMappingService.mapLabel(LabelType.TYPE, item['type'] + '').toString()));
      this.controlFlowMessage = this.translateService.instant('controlFlow.actuallyNotSupported',
        { supportedTypes: [...actuallySupportedModuleTypes].join(', ') });
    }
  }

  /**
   * Call the deeplink service to open the selected module in Eclipse
   */
  openInEclipse(): void {
    this.deeplinkService.showModuleInEclipse(this.selectedModule);
  }

  /**
   * Method to return the source of the Module's icon.
   */
  getIconSource(): string {
    const nodeType = this.selectedModule?.technology.toUpperCase() + ' ' + this.selectedModule?.type.toUpperCase();
    return getBasePath() + (NODE_CONFIG[nodeType] ? NODE_CONFIG[nodeType].imageUrl : NODE_CONFIG.GENERIC.imageUrl);
  }

  /**
   * Method to create a routing link
   *
   * @param routeTo the end section of the url
   * @return the string of routing link
   */
  routeToDetails(routeTo: string): string {
    return RouteBuilder.buildDetailsRoute(this.projectId, this.moduleHash, routeTo);
  }

  /**
   * Opens a modal window as a popup to edit details
   * @param event the mouse event
   */
  editModuleDetails(event: MouseEvent): void {
    event.preventDefault();
    const updateModuleDesc: () => ModuleDetailsDPSidePanelService = this.updateModuleDesc.bind(this);
    this.moduleDetailsDPSidePanelService.editModuleDescription(this.selectedModule, updateModuleDesc);
  }

  /**
   * Get the length of taxonomyList to show edit button.
   * @param length length of taxonomyList.
   */
  getTaxonomyListLength(length: number): void {
    this.taxonomyListLength = length;
  }

  /**
   * Get the flag for setting showModal to false.
   *
   * @param closeModal flag with false value.
   */
  getShowModalfromOverviewTaxonomy(closeModal: boolean): void {
    this.showModal = closeModal;
  }

  /**
   * Opens modal for assigning taxonomy.
   */
  assignTaxonomies(): void {
    this.getAssignmentData();
    this.showModal = true;
  }

  /**
   * Method to check if requiresReview is true.
   */
  hasRequiresReviewFlag(): boolean {
    return this.selectedModule.requiresReview;
  }

  /**
   * Method to change and update the requiresReview status.
   */
  updateRequiresReviewStatus(): void {
    this.selectedModule.requiresReview = ! this.selectedModule.requiresReview;
    this.moduleControllerService.updateModule(this.projectId, this.moduleId, this.selectedModule).subscribe(response => {
      if (response) {
        this.moduleBadgeUpdateService.updateModuleToBeReviewed(BadgeCountUpdateOperation.ANNOTATION_DELETED);
      }
    });
  }

  /**
   * Gets the taxonomy assignment data through TaxonomyControllerService.
   */
  getAssignmentData(): void {
    this.taxonomyControllerService.getAssignedTaxonomyByModule(this.projectId, { 'modules': { 'ids': this.moduleIdArray } })
      .subscribe((taxonomyAssignmentResponse: TaxonomyAssignmentsGetResponse) => {
        if (taxonomyAssignmentResponse?.taxonomies.length) {
          const taxonomyData: TaxonomyPojo[] = [];
          taxonomyAssignmentResponse.taxonomies.forEach((response) => {
            if (response.state === 'ALL' || response.state === 'SOME') {
              taxonomyData.push(response.taxonomy);
            }
          });
          this.taxonomyListLength = taxonomyData.length;
          this.taxonomyResponse = taxonomyAssignmentResponse;
          this.moduleOverViewTaxonomyComponent?.getTaxonomyData(taxonomyData);
        }
      });
  }

  /**
   * method to create modal for the Data lineage
   */
  openModalForLineageExport(): void {
    this.dataLineageModalInstance = this.modalService.create({
      nzTitle: this.translateService.instant('dataLineage.modalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzWrapClassName: 'vertical-center-modal',
      nzKeyboard: true,
      nzWidth: '40vw',
      nzContent: DataLineageExportComponent
    });
    const modalComponent = this.dataLineageModalInstance.getContentComponent();
    modalComponent.projectId = this.projectId;
    modalComponent.moduleId = this.moduleId;
  }

  ngOnDestroy(): void {
    this.customTablesParametersService.resetSavedSearchForModuleId(this.moduleId);
    this.clientProjectSubscription?.unsubscribe();
    this.assignTaxonomiesModalSubscription?.unsubscribe();
    this.annotationDataDictionarySubs?.unsubscribe();
    this.dnaSubscriptions.forEach(subscription => subscription?.unsubscribe());
  }

  private getSchemaTabHeader(): void {
    const fieldLabel = this.labelMappingService.mapLabel(LabelType.TYPE, this.selectedModule?.type);
    const value: string = fieldLabel.toUpperCase();
    this.schemaTabHeader = (value === ModulePojo.TypeEnum.TABLE.toString()) ?
      'Table Columns' : (value === 'STORED PROCEDURE') ?
        'Stored Procedure' : (value === ModulePojo.TypeEnum.INDEX.toString()) ?
          'Index' : (value === ModulePojo.TypeEnum.VIEW.toString()) ?
            'View' : (value === ModulePojo.TypeEnum.TRIGGER.toString()) ?
              'Trigger' : fieldLabel;
  }

  /**
   * update module description.
   *
   * @param updatedParams Updated module description
   */
  private updateModuleDesc(updatedParams: any) {
    const oldDescription = this.selectedModule.description;
    this.selectedModule.description = updatedParams.Description;
    this.moduleControllerService.updateModule(this.projectId, this.moduleId, this.selectedModule).subscribe(() => {
      this.messageService.success(`${this.translateService.instant('updateSuccessful')}`);
      this.moduleDetailsDPSidePanelService.descriptionModal.destroy();
    }, error => {
      this.selectedModule.description = oldDescription;
      this.messageService.error(`${this.translateService.instant('updateError')}`);
      log.error(error.message);
    });
  }

  private getCountsBadges(): void {
    const requestQuery = {
      'query': `{ modules(projectId: ${this.projectId}, filterObject: { content_id: {eq: ${this.moduleId}}}) {
        content { annotationCount dependencyCount }}}`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response) => {
      if (response && response['data']?.modules?.content.length) {
        const counts = response['data']?.modules?.content[0];
        this.badgeObject.annotationCount = counts.annotationCount;
        this.badgeObject.dependencyCount = counts.dependencyCount;
      }
    }, (error: any) => {
      log.error(error.message);
    });
    this.moduleControllerService.findIncludedModuleIds(this.projectId, this.moduleId).subscribe((moduleIdsOfIncludedCopyBooks: number[]) => {
      this.badgeObject.dataDictionaryCount = 0;
      this.moduleIdForPreFiltering = moduleIdsOfIncludedCopyBooks;
      const ddRequestQuery = {
        'query': `{ modules(projectId: ${this.projectId}, filterObject: { content_id: {in: [${this.moduleIdForPreFiltering}]}}) {
          content { dataDictionaryEntryCount }}}`
      };
      this.graphQlControllerService.graphQl(ddRequestQuery).subscribe((response) => {
        if (response && response['data']?.modules?.content.length) {
          const ddCounts = response['data']?.modules?.content;
          ddCounts.forEach((counts: any) => {
            this.badgeObject.dataDictionaryCount+= counts.dataDictionaryEntryCount;
          });
        }
      }, (error: any) => {
        log.error(error.message);
      });
   });
    this.dataSchemaControllerService.findFieldInfos(this.projectId, this.moduleId)
      .subscribe((response: FieldInfoPojo[]) => {
        this.badgeObject.schemaFieldCount = response.length;
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
  }

  private fetchVirtualModuleDetails(): void {
    const id: any = this.selectedModule.parent ? this.selectedModule.parentId : this.moduleId;
    const requestCodeMetrics: AggregationRequestModuleFieldName = {
      filterObject: {
        CONTAINING_MODULE_ID: { eq: id }
      },
      groupBy: new Set([
        AggregationRequestModuleFieldName.GroupByEnum.CONTAINING_MODULE_ID,
        AggregationRequestModuleFieldName.GroupByEnum.CONTAINING_MODULE_NAME
      ]),
      fields: {
        ID: 'LIST',
        NAME: 'LIST'
      }
    };
    this.moduleControllerService.getAggregatedValues2(this.projectId, requestCodeMetrics)
      .subscribe((response: AggregationResultRelationshipFieldName[]) => {
        this.virtualModuleDetail = response;
      }, (error) => {
        log.error(error);
      });
  }

  private fetchDNAData() {
    this.dnaSubscriptions.push(this.discoveryControllerService.modelDNAForLatestTimestamp(this.projectId).subscribe((dnaChartsData: ModelDna) => {
      this.loadState = LoaderState.success;
      this.dnaChartsData = dnaChartsData;
      this.dnaSubscriptions.push(this.discoveryControllerService.belongsToCluster(this.projectId, this.moduleId)
        .subscribe((clusterData: ModelBelongsToClusters) => {
          this.dnaCardList = this.dnaAnalysis.createChartData(this.dnaChartsData, clusterData.clusters, false);
        }, () => {
          this.dnaCardList = [];
        }));
    }, (err: HttpErrorResponse) => {
      log.error(err);
      this.loadState = LoaderState.error;
    }));
  }
}
